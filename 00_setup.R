# 00_setup.R
# Global setup for "Executive Confidence and Firm Performance" analysis

required_pkgs <- c(
  "tidyverse",
  "plm",
  "sandwich",
  "lmtest",
  "modelsummary",
  "quantreg"
)

for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

options(scipen = 999, digits = 4)

# Firm-clustered VCOV for linear panel models
vc_firm <- function(model, firm_ids) {
  sandwich::vcovCL(model, cluster = firm_ids)
}

# Firm-clustered bootstrap VCOV for quantile regression
vcov_cluster_qr <- function(model, data, cluster_var,
                            B = 200, seed = 123) {
  set.seed(seed)
  cl <- data[[cluster_var]]
  ucl <- unique(cl)
  
  beta_hat <- coef(model)
  K <- length(beta_hat)
  
  boot_coefs <- matrix(NA_real_, nrow = B, ncol = K)
  
  form <- formula(model)
  tau  <- model$tau
  
  for (b in seq_len(B)) {
    samp_cl <- sample(ucl, length(ucl), replace = TRUE)
    idx <- unlist(lapply(samp_cl, function(g) which(cl == g)))
    boot_data <- data[idx, , drop = FALSE]
    
    mb <- try(
      quantreg::rq(form, data = boot_data, tau = tau, method = "fn"),
      silent = TRUE
    )
    if (inherits(mb, "try-error")) next
    
    cb <- coef(mb)
    if (length(cb) == K) {
      boot_coefs[b, ] <- cb
    }
  }
  
  boot_coefs <- boot_coefs[complete.cases(boot_coefs), , drop = FALSE]
  if (nrow(boot_coefs) < 10) {
    stop("Too few successful bootstrap replications for clustered SE.")
  }
  
  vcov_mat <- stats::cov(boot_coefs)
  dimnames(vcov_mat) <- list(names(beta_hat), names(beta_hat))
  vcov_mat
}
