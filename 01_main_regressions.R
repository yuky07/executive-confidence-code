# 01_main_regressions.R

source("00_setup.R")

data_path <- "data/executive_confidence_panel.rds"
if (!file.exists(data_path)) stop("Data file missing (not shared in repository).")

panel <- readRDS(data_path)
pdata <- pdata.frame(panel, index = c("firm_id", "year"))

run_fe <- function(yvar) {
  fml <- as.formula(
    paste0(
      yvar,
      " ~ Confidence + Size + Lev + SalesGrowth + Age + Gen + FOP"
    )
  )
  plm(fml, data = pdata, model = "within", effect = "twoways")
}

yvars <- c("TobinQ", "ROA", "ROE")

models_main <- lapply(yvars, run_fe)
names(models_main) <- yvars

vcovs_main <- lapply(models_main, vc_firm, firm_ids = panel$firm_id)

modelsummary(
  models_main,
  vcov     = vcovs_main,
  output   = "output/tables/table3_main_FE.html",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
  stars    = TRUE
)
