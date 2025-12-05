# kai_imputer.py
# Author: Daniel R. Sarmiento
# Paper: Impute or Not to Impute? A Scalable Kalman-Attention Framework...
# GitHub: https://github.com/DanielSs99/Kalman-Attention-Imputer-KAI-.git


SDataset <- dataset(
  name = "TSDatasetKAI",
  initialize = function(x, m, e = NULL, window = NULL, norm = TRUE) {
    # x: [N,T,dx], m: [N,T,dx], e: [N,T,de] (opcional)
    self$x <- x; self$m <- m; self$e <- e; self$window <- window
    if (norm) {
      # normaliza cada dimensión usando SOLO observados (m==1)
      mu <- apply(x * m, 3, function(z) mean(z[z != 0], na.rm = TRUE))
      sd <- apply(x * m, 3, function(z) sd(z[z != 0], na.rm = TRUE))
      sd[sd == 0] <- 1
      self$x <- sweep(sweep(x, 3, mu, "-"), 3, sd, "/")
    }
  },
  .length = function() {
    dim(self$x)[1]  # N muestras
  },
  .getitem = function(i) {
    xi <- self$x[i,,,drop=FALSE]    # [1,T,dx]
    mi <- self$m[i,,,drop=FALSE]    # [1,T,dx]
    ei <- if (!is.null(self$e)) self$e[i,,,drop=FALSE] else NULL
    list(x = xi, m = mi, e = ei)
  }
)

# Construye dataset/dataloader (usa num_workers > 0 si tu IO es pesado)
ds <- TSDataset(x, m, e)
dl <- dataloader(ds, batch_size = 16, shuffle = TRUE, num_workers = 0)  # [3](https://www.rdocumentation.org/packages/torch/versions/0.14.2/topics/dataloader)

# --- 2) RTS diferenciable con máscara ---
RTSModule <- nn_module(
  "RTSModule",
  initialize = function(dx) {
    # Parámetros (ej.: A,H,Q,R) — versión diagonal para estabilidad
    self$A <- nn_parameter(torch_eye(dx))
    self$H <- nn_parameter(torch_eye(dx))
    self$logQ <- nn_parameter(torch_zeros(dx))
    self$logR <- nn_parameter(torch_zeros(dx))
    self$eps <- 1e-5
  },
  forward = function(x, m) {
    # x,m: [B,T,dx]
    B <- x$size(1); Tt <- x$size(2); dx <- x$size(3)
    Q <- torch_diag(torch_softplus(self$logQ)) + self$eps * torch_eye(dx)
    R <- torch_diag(torch_softplus(self$logR)) + self$eps * torch_eye(dx)

    # (1) Kalman forward con máscara: ignora observaciones faltantes (m==0)
    # (2) RTS backward: usa ganancias J_t para suavizar
    # Devuelve: xK [B,T,dx], SigK_diag [B,T,dx]
    xK <- x # placeholder: reemplaza por tu KF+RTS
    SigK_diag <- torch_ones_like(x)
    list(xK = xK, SigK_diag = SigK_diag)
  }
)

# --- 3) Atención consciente de máscara + FFN ---
MaskAwareAttn <- nn_module(
  "MaskAwareAttn",
  initialize = function(d_model, n_heads) {
    self$Wx <- nn_linear(in_features = d_model, out_features = d_model)
    self$mha <- nn_transformer_encoder(
      encoder_layer = nn_transformer_encoder_layer(
        d_model = d_model, nhead = n_heads
      ),
      num_layers = 1
    )  # usa MHA interna; puedes inyectar sesgos por máscara en los inputs/attn_mask
    self$ffn <- nn_sequential(
      nn_linear(d_model, 4*d_model), nn_gelu(),
      nn_linear(4*d_model, d_model)
    )
    self$ln <- nn_layer_norm(d_model)
  },
  forward = function(u, m) {
    # u: [B,T,d_model], m: [B,T,dx] -> proyecta máscara a [B,T,1] si requieres
    # Construye un attn_mask (True=oculto) penalizando posiciones con m==0
    # En torch R, puedes preparar un "src_mask" o sesgar u con embeddings de missingness.
    h <- self$mha(u)
    out <- self$ln(self$ffn(h))
    out
  }
)

# --- 4) KAI: gating + var-head + pérdida ---
KAIModel <- nn_module(
  "KAIModel",
  initialize = function(dx, d_model, n_heads = 4) {
    self$rts <- RTSModule(dx)
    self$Wx <- nn_linear(dx, d_model)
    self$We <- nn_linear(d_model, d_model)  # si tienes e_t; ajusta de->d_model
    self$attn <- MaskAwareAttn(d_model, n_heads)
    self$gate <- nn_sequential(
      nn_linear(2*dx, dx), nn_sigmoid()
    )
    self$var_head <- nn_sequential(
      nn_linear(d_model + dx, dx),
      nn_softplus()
    )
  },
  forward = function(x, m, e = NULL) {
    # 1) RTS
    rts <- self$rts(x, m)
    xK <- rts$xK                     # [B,T,dx]
    SigK <- rts$SigK_diag            # [B,T,dx]

    # 2) Embeddings
    u <- self$Wx(xK)
    if (!is.null(e)) {
      u <- u + self$We(e)            # asume e ya en d_model
    }
    # 3) Atención + FFN
    h <- self$attn(u, m)             # [B,T,d_model]
    # 4) Gate (usa incertidumbre)
    g_in <- torch_cat(list(xK, SigK), dim = 3) # [B,T,2dx]
    g <- self$gate(g_in)             # [B,T,dx]
    ffn_out <- torch_narrow(h, dim = 3, start = 1, length = x$size(3)) # o proyéctalo a dx
    # 5) Imputación (solo faltantes)
    xhat <- xK + (g * ffn_out) * (1 - m)
    # 6) Var head
    var <- self$var_head(torch_cat(list(h, xK), dim = 3)) + 1e-6
    list(xhat = xhat, var = var, xK = xK, SigK = SigK)
  }
)

# --- 5) Pérdida híbrida ---
loss_kai <- function(pred, target, mask, lambda_smooth = 0.0) {
  # NLL/MSE solo en observados
  mse_obs <- ((pred - target)$pow(2) * mask)$mean()  # simple MSE observado
  # suavidad temporal (L2) opcional
  dt <- pred$diff(dim = 2)
  smooth <- if (dt$ndimension() > 0) dt$pow(2)$mean() else torch_tensor(0)
  mse_obs + lambda_smooth * smooth
}

# --- 6) Entrenamiento: loop  con Adam ---
model <- KAIModel(dx = dim(x)[3], d_model = 128, n_heads = 4)
optimizer <- optim_adam(model$parameters, lr = 1e-3)  # [7](https://www.rdocumentation.org/packages/torch/versions/0.14.2/topics/optim_adam)

num_epochs <- 10
for (epoch in 1:num_epochs) {
  coro::loop(for (batch in dl) {
    xb <- batch$x$squeeze(1)  # [T,dx] -> ajusta shapes según tu dataset
    mb <- batch$m$squeeze(1)
    eb <- if (!is.null(batch$e)) batch$e$squeeze(1) else NULL

    out <- model(xb, mb, eb)
    loss <- loss_kai(out$xhat, xb, mb, lambda_smooth = 1e-3)

    optimizer$zero_grad()
    loss$backward()
    optimizer$step()
  })
  cat(sprintf("epoch %d | loss: %.4f\n", epoch, as.numeric(loss)))
}

