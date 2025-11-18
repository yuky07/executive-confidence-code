# 02_covid_dummy_regressions.R

source("00_setup.R")

data_path <- "data/executive_confidence_panel.rds"
if (!file.exists(data_path)) stop("Data file missing (not shared).")

panel <- readRDS(data_path)

panel <- panel %>%
  mutate(
    D2020 = if_else(year == 2020L, 1L, 0L),
    D2021 = if_else(year == 2021L, 1L, 0L),
    Conf_D2020 = Confidence * D2020,
    Conf_D2021 = Confidence * D2021
  )

pdata <- pdata.frame(panel, index = c("firm_id", "year"))

run_covid <- function(yvar) {
  fml <- as.formula(
    paste0(
      yvar,
      " ~ Confidence + Size + Lev + SalesGrowth + Age + Gen + FOP",
      " + D2020 + D2021 + Conf_D2020 + Conf_D2021"
    )
  )
  plm(fml, data = pdata, model = "within", effect = "twoways")
}

yvars <- c("TobinQ", "ROA", "ROE")
models_covid <- lapply(yvars, run_covid)
names(models_covid) <- yvars

vcovs_covid <- lapply(models_covid, vc_firm, firm_ids = panel$firm_id)

modelsummary(
  models_covid,
  vcov     = vcovs_covid,
  output   = "output/tables/table4_covid_interactions.html",
  stars    = TRUE
)
