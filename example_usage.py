# example_usage.py
import torch
import numpy as np
import pandas as pd
from kai_imputer import KAI
import matplotlib.pyplot as plt

# Load Bogotá data (example: one station)
df = pd.read_excel("Usaquen.xlsx", parse_dates=['Fecha'])
X_full = df[['PM10', "OZONO", "Temperatura", "Precipitacion", "PM2.5"]].values
dates = df['Fecha'].values

# Introduce 20% MCAR missing
np.random.seed(42)
mask = np.random.rand(*X_full.shape) < 0.8
X_obs = X_full.copy()
X_obs[~mask] = np.nan

# Normalize
mean, std = X_full.mean(0), X_full.std(0)
X_norm = (X_full - mean) / std
X_obs_norm = (X_obs - mean) / std
X_obs_norm[np.isnan(X_obs)] = 0  # PyKalman needs no NaN

# To torch
X_torch = torch.tensor(X_obs_norm, dtype=torch.float32)
mask_torch = torch.tensor(mask, dtype=torch.float32)

# Initialize KAI
kai = KAI(d_model=12, window=24, lr=3e-3)
kai.fit_kalman(X_obs_norm, mask)  # Fit Kalman once

# Train attention residual
kai.train_step(X_torch, mask_torch, epochs=150)

# Impute
kai.eval()
with torch.no_grad():
    X_imp = kai(X_torch, mask_torch).numpy().squeeze()

# Denormalize
X_imp_denorm = X_imp * std + mean
X_true = X_full

# Plot
plt.figure(figsize=(12, 6))
plt.plot(dates, X_true[:, 0], label='True PM2.5', alpha=0.7)
plt.plot(dates, X_imp_denorm[:, 0], label='KAI Imputed', alpha=0.9)
plt.scatter(dates[~mask[:, 0]], X_true[~mask[:, 0], 0], c='red', s=10, label='Missing')
plt.legend()
plt.title("KAI Imputation on Bogotá PM2.5 (Suba Station)")
plt.savefig("kai_imputation.png")
plt.show()

# RMSE on missing
from sklearn.metrics import mean_squared_error
missing_idx = ~mask[:, 0]
rmse = mean_squared_error(X_true[missing_idx, 0], X_imp_denorm[missing_idx, 0], squared=False)
print(f"RMSE on missing: {rmse:.3f}")