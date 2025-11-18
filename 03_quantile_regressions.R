# 03_quantile_regressions.R

# Fixed-effects quantile regressions (TobinQ, ROA, ROE)

source("00_setup.R")

data_path <- "data/executive_confidence_panel.rds"
if (!file.exists(data_path)) {
  stop("Data file 'data/executive_confidence_panel.rds' not found.")
}

panel <- readRDS(data_path)

taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# Build FE quantile regression models
build_qr_models <- function(yvar) {
  fml <- as.formula(
    paste0(
      yvar,
      " ~ Confidence + Size + Lev + SalesGrowth + Age + Gen + FOP",
      " + factor(firm_id) + factor(year)"
    )
  )
  mods <- lapply(taus, function(tau_val) {
    quantreg::rq(fml, data = panel, tau = tau_val, method = "fn")
  })
  names(mods) <- paste0("tau_", taus)
  mods
}

qr_TobinQ <- build_qr_models("TobinQ")
qr_ROA    <- build_qr_models("ROA")
qr_ROE    <- build_qr_models("ROE")

# Clustered SE via bootstrap function
vcov_qr_TobinQ <- lapply(
  qr_TobinQ,
  function(m) vcov_cluster_qr(m, data = panel, cluster_var = "firm_id", B = 200)
)
vcov_qr_ROA <- lapply(
  qr_ROA,
  function(m) vcov_cluster_qr(m, data = panel, cluster_var = "firm_id", B = 200)
)
vcov_qr_ROE <- lapply(
  qr_ROE,
  function(m) vcov_cluster_qr(m, data = panel, cluster_var = "firm_id", B = 200)
)

# Export: TobinQ
modelsummary(
  qr_TobinQ,
  vcov     = vcov_qr_TobinQ,
  estimate = "{estimate} ({std.error})",
  statistic = NULL,
  output   = "output/tables/table5_QR_TobinQ.html"
)

# Export: ROA
modelsummary(
  qr_ROA,
  vcov     = vcov_qr_ROA,
  estimate = "{estimate} ({std.error})",
  statistic = NULL,
  output   = "output/tables/table6_QR_ROA.html"
)

# Export: ROE
modelsummary(
  qr_ROE,
  vcov     = vcov_qr_ROE,
  estimate = "{estimate} ({std.error})",
  statistic = NULL,
  output   = "output/tables/table7_QR_ROE.html"
)
