# 04_robustness_checks.R

source("00_setup.R")

data_path <- "data/executive_confidence_panel.rds"
if (!file.exists(data_path)) {
  stop("Data file 'data/executive_confidence_panel.rds' not found.")
}

panel <- readRDS(data_path)
yvars <- c("TobinQ", "ROA", "ROE")

## 4.1 Industry × Year fixed effects (Table 9) ----------------------------

run_ind_year <- function(yvar) {
  fml <- as.formula(
    paste0(
      yvar,
      " ~ Confidence + Size + Lev + SalesGrowth + Age + Gen + FOP",
      " + factor(firm_id) + factor(year)",
      " + factor(Industry):factor(year)"
    )
  )
  lm(fml, data = panel)
}

models_indyr <- lapply(yvars, run_ind_year)
names(models_indyr) <- paste0(yvars, "_indYear")

vcovs_indyr <- lapply(models_indyr, vc_firm, firm_ids = panel$firm_id)

cat("\n=== Robustness 1: Industry × Year FE ===\n")
for (i in seq_along(models_indyr)) {
  cat("\nOutcome:", names(models_indyr)[i], "\n")
  print(lmtest::coeftest(models_indyr[[i]], vcov = vcovs_indyr[[i]]))
}

modelsummary(
  models_indyr,
  vcov     = vcovs_indyr,
  output   = "output/tables/table9_industry_year_FE.html",
  stars    = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE"
)

modelsummary(
  models_indyr,
  vcov     = vcovs_indyr,
  output   = "output/tables/table9_industry_year_FE.tex",
  stars    = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE"
)

## 4.2 Alternative size proxy: log sales (Appendix A1) --------------------
## Assumes your dataset includes Size_sales = log(sales) as the alternative.

run_alt_size <- function(yvar) {
  fml <- as.formula(
    paste0(
      yvar,
      " ~ Confidence + Size_sales + Lev + SalesGrowth + Age + Gen + FOP"
    )
  )
  pdata_alt <- pdata.frame(panel, index = c("firm_id", "year"))
  plm(fml, data = pdata_alt, model = "within", effect = "twoways")
}

models_alt_size <- lapply(yvars, run_alt_size)
names(models_alt_size) <- paste0(yvars, "_altSize")

vcovs_alt_size <- lapply(models_alt_size, vc_firm, firm_ids = panel$firm_id)

cat("\n=== Robustness 2: Alternative size (log sales) ===\n")
for (i in seq_along(models_alt_size)) {
  cat("\nOutcome:", names(models_alt_size)[i], "\n")
  print(lmtest::coeftest(models_alt_size[[i]], vcov = vcovs_alt_size[[i]]))
}

modelsummary(
  models_alt_size,
  vcov     = vcovs_alt_size,
  output   = "output/tables/appendix_A1_alt_size.html",
  stars    = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE"
)

## 4.3 Placebo: lead Confidence (t+1) (Appendix A2) -----------------------

panel_lead <- panel %>%
  arrange(firm_id, year) %>%
  group_by(firm_id) %>%
  mutate(Confidence_lead1 = dplyr::lead(Confidence, 1L)) %>%
  ungroup()

pdata_lead <- pdata.frame(panel_lead, index = c("firm_id", "year"))

run_lead <- function(yvar) {
  fml <- as.formula(
    paste0(
      yvar,
      " ~ Confidence_lead1 + Size + Lev + SalesGrowth + Age + Gen + FOP"
    )
  )
  plm(fml, data = pdata_lead, model = "within", effect = "twoways")
}

models_lead <- lapply(yvars, run_lead)
names(models_lead) <- paste0(yvars, "_lead")

vcovs_lead <- lapply(models_lead, vc_firm, firm_ids = panel_lead$firm_id)

cat("\n=== Robustness 3: Lead Confidence (t+1 placebo) ===\n")
for (i in seq_along(models_lead)) {
  cat("\nOutcome:", names(models_lead)[i], "\n")
  print(lmtest::coeftest(models_lead[[i]], vcov = vcovs_lead[[i]]))
}

modelsummary(
  models_lead,
  vcov     = vcovs_lead,
  output   = "output/tables/appendix_A2_lead_confidence.html",
  stars    = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE"
)

## 4.4 Interactions: Confidence×Gen and Confidence×FOP (Appendix A3) ------

panel_int_GF <- panel %>%
  mutate(
    Conf_Gen = Confidence * Gen,
    Conf_FOP = Confidence * FOP
  )

pdata_GF <- pdata.frame(panel_int_GF, index = c("firm_id", "year"))

run_int_GF <- function(yvar) {
  fml <- as.formula(
    paste0(
      yvar,
      " ~ Confidence + Size + Lev + SalesGrowth + Age + Gen + FOP",
      " + Conf_Gen + Conf_FOP"
    )
  )
  plm(fml, data = pdata_GF, model = "within", effect = "twoways")
}

models_GF <- lapply(yvars, run_int_GF)
names(models_GF) <- paste0(yvars, "_GenFOP")

vcovs_GF <- lapply(models_GF, vc_firm, firm_ids = panel_int_GF$firm_id)

cat("\n=== Robustness 4: Confidence×Gen and Confidence×FOP ===\n")
for (i in seq_along(models_GF)) {
  cat("\nOutcome:", names(models_GF)[i], "\n")
  print(lmtest::coeftest(models_GF[[i]], vcov = vcovs_GF[[i]]))
}

modelsummary(
  models_GF,
  vcov     = vcovs_GF,
  output   = "output/tables/appendix_A3_Gen_FOP_interactions.html",
  stars    = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE"
)

## 4.5 Interaction: Confidence×Closeheld (Appendix A4) --------------------
## Requires Closeheld variable in your dataset.

panel_CH <- panel %>%
  mutate(
    Conf_Closeheld = Confidence * Closeheld
  )

pdata_CH <- pdata.frame(panel_CH, index = c("firm_id", "year"))

run_int_CH <- function(yvar) {
  fml <- as.formula(
    paste0(
      yvar,
      " ~ Confidence + Size + Lev + SalesGrowth + Age + Gen + FOP",
      " + Closeheld + Conf_Closeheld"
    )
  )
  plm(fml, data = pdata_CH, model = "within", effect = "twoways")
}

models_CH <- lapply(yvars, run_int_CH)
names(models_CH) <- paste0(yvars, "_Closeheld")

vcovs_CH <- lapply(models_CH, vc_firm, firm_ids = panel_CH$firm_id)

cat("\n=== Robustness 5: Confidence×Closeheld (ownership concentration) ===\n")
for (i in seq_along(models_CH)) {
  cat("\nOutcome:", names(models_CH)[i], "\n")
  print(lmtest::coeftest(models_CH[[i]], vcov = vcovs_CH[[i]]))
}

modelsummary(
  models_CH,
  vcov     = vcovs_CH,
  output   = "output/tables/appendix_A4_Closeheld_interactions.html",
  stars    = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE"
)
