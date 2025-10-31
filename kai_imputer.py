# kai_imputer.py
# Author: Daniel R. Sarmiento
# Paper: Impute or Not to Impute? A Scalable Kalman-Attention Framework...
# GitHub: https://github.com/DanielSs99/Kalman-Attention-Imputer-KAI-.git

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch import optim
from typing import Tuple, Optional
import numpy as np
from pykalman import KalmanFilter  # pip install pykalman


class KalmanSmoother(nn.Module):
    """
    Wrapper around pykalman to compute smoothed estimates.
    Differentiable via interpolation fallback for training.
    """
    def __init__(self, dim: int, em_vars='all'):
        super().__init__()
        self.dim = dim
        self.em_vars = em_vars
        self.kf = None

    def fit(self, X_obs: np.ndarray, mask: np.ndarray):
        """Fit Kalman filter on observed data."""
        X_filled = np.where(mask, X_obs, 0)  # Fill missing with 0 for EM
        self.kf = KalmanFilter(
            transition_matrices=np.eye(self.dim),
            observation_matrices=np.eye(self.dim),
            em_vars=self.em_vars
        )
        self.kf = self.kf.em(X_filled, n_iter=10)
        return self

    def smooth(self, X: np.ndarray, mask: np.ndarray) -> np.ndarray:
        """Return smoothed estimate E[x_t | X_1:T]"""
        if self.kf is None:
            raise RuntimeError("Must call fit() first.")
        X_filled = np.where(mask, X, 0)
        smoothed, _ = self.kf.smooth(X_filled)
        return smoothed

    def forward(self, X: torch.Tensor, mask: torch.Tensor) -> torch.Tensor:
        """
        During training: use interpolation fallback (differentiable)
        During inference: use fitted Kalman
        """
        if not self.training and self.kf is not None:
            with torch.no_grad():
                X_np = X.cpu().numpy()
                mask_np = mask.cpu().numpy()
                smoothed = self.smooth(X_np, mask_np)
                return torch.tensor(smoothed, device=X.device, dtype=X.dtype)
        else:
            # Differentiable fallback: linear interpolation
            return self._interpolate(X, mask)

    def _interpolate(self, X: torch.Tensor, mask: torch.Tensor) -> torch.Tensor:
        """Linear interpolation (differentiable)"""
        X_interp = X.clone()
        for i in range(X.shape[1]):  # per feature
            x = X[:, i]
            m = mask[:, i]
            idx = torch.arange(X.size(0), device=X.device)
            valid = m.bool()
            if valid.any():
                X_interp[:, i] = torch_interp1d(idx, x[valid], idx, m)
        return X_interp


def torch_interp1d(idx_known: torch.Tensor, values: torch.Tensor, idx_query: torch.Tensor, mask: torch.Tensor):
    """1D linear interpolation for missing values"""
    filled = values[0].item()  # start with first
    result = []
    prev_idx, prev_val = None, None
    for i, q in enumerate(idx_query):
        if mask[i]:
            result.append(values[torch.where(idx_known == q)[0]])
        else:
            if prev_idx is None:
                result.append(torch.tensor(filled, device=values.device))
            else:
                # find next known
                next_known = torch.where(idx_known > q)[0]
                if len(next_known) > 0:
                    n_idx = idx_known[next_known[0]]
                    n_val = values[next_known[0]]
                    ratio = (q - prev_idx) / (n_idx - prev_idx)
                    val = prev_val + ratio * (n_val - prev_val)
                    result.append(val)
                else:
                    result.append(prev_val)
        if mask[i]:
            prev_idx, prev_val = q, values[torch.where(idx_known == q)[0]]
    return torch.stack(result).squeeze()


class MultiHeadAttention(nn.Module):
    def __init__(self, d_model: int, n_heads: int = 4, dropout: float = 0.1):
        super().__init__()
        self.attn = nn.MultiheadAttention(d_model, n_heads, dropout=dropout, batch_first=True)
        self.norm = nn.LayerNorm(d_model)
        self.dropout = nn.Dropout(dropout)

    def forward(self, x: torch.Tensor, mask: Optional[torch.Tensor] = None):
        attn_out, _ = self.attn(x, x, x, key_padding_mask=~mask if mask is not None else None)
        return self.dropout(attn_out) + x


class KAI(nn.Module):
    """
    Kalman-Attention Imputer (KAI)
    Hybrid: Kalman Smoother + Residual Attention Correction
    """
    def __init__(self, d_model: int = 12, n_heads: int = 4, window: int = 24, lr: float = 1e-3):
        super().__init__()
        self.d_model = d_model
        self.window = window
        self.kalman = KalmanSmoother(d_model)
        self.proj_in = nn.Linear(d_model, d_model)
        self.attention = MultiHeadAttention(d_model, n_heads)
        self.ffn = nn.Sequential(
            nn.Linear(d_model, d_model * 2),
            nn.GELU(),
            nn.Linear(d_model * 2, d_model),
            nn.Dropout(0.1)
        )
        self.norm = nn.LayerNorm(d_model)
        self.optimizer = optim.Adam(self.parameters(), lr=lr)

    def forward(self, X: torch.Tensor, mask: torch.Tensor) -> torch.Tensor:
        """
        X: (T, D) or (B, T, D)
        mask: (T, D) or (B, T, D) - 1 if observed
        """
        if X.dim() == 2:
            X = X.unsqueeze(0)  # (1, T, D)
            mask = mask.unsqueeze(0)

        B, T, D = X.shape

        # Step 1: Kalman smoothed estimate (non-diff during train)
        with torch.set_grad_enabled(False):
            X_kalman = self.kalman(X.squeeze(0).cpu().numpy(), mask.squeeze(0).cpu().numpy())
            X_kalman = torch.tensor(X_kalman, device=X.device, dtype=X.dtype).unsqueeze(0)

        # Step 2: Residual attention over local window
        X_res = X_kalman
        imputed = []

        for t in range(T):
            start = max(0, t - self.window // 2)
            end = min(T, t + self.window // 2 + 1)
            window_slice = X_res[:, start:end, :]  # (1, W, D)
            window_mask = mask[:, start:end, :]

            # Pad if needed
            if window_slice.size(1) < self.window:
                pad = self.window - window_slice.size(1)
                window_slice = F.pad(window_slice, (0, 0, 0, pad))
                window_mask = F.pad(window_mask, (0, 0, 0, pad))

            h = self.proj_in(window_slice)
            h = self.attention(h, window_mask.squeeze(0))
            h = self.ffn(h)
            imputed.append(h[:, -pad if pad > 0 else (t - start), :])

        X_att = torch.stack(imputed, dim=1)  # (1, T, D)
        X_out = self.norm(X_kalman + X_att)

        # Only impute where missing
        return torch.where(mask.bool(), X, X_out)

    def fit_kalman(self, X: np.ndarray, mask: np.ndarray):
        """Fit Kalman on full data (once, before training)"""
        self.kalman.fit(X, mask)
        return self

    def train_step(self, X: torch.Tensor, mask: torch.Tensor, epochs: int = 100):
        self.train()
        criterion = nn.MSELoss()
        for epoch in range(epochs):
            self.optimizer.zero_grad()
            X_imp = self(X, mask)
            loss = criterion(X_imp * ~mask.bool(), X * ~mask.bool())
            loss.backward()
            self.optimizer.step()
            if epoch % 20 == 0:
                print(f"Epoch {epoch}, Loss: {loss.item():.6f}")
        return self