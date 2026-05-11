#####################
# Author: Mike Liang
#####################

library(dplyr)
library(tidyr)
library(exuber)
library(tibble)

options(
  exuber.parallel = FALSE,
  exuber.show_progress = TRUE
)

metro_panel <- readRDS("data/monthly_metropolitan_panel.rds") |>
  select(date, geo, cpi_geo, Composite_Benchmark_SA_real_2023, composite_benchmark_income_per_capita_ratio)

prov_panel <- readRDS("data/monthly_provincial_panel.rds") |>
  select(date, geo, cpi_geo, Composite_Benchmark_SA_real_2023, composite_benchmark_income_per_capita_ratio)

metro_hp <- metro_panel |>
  select(date, geo, value = Composite_Benchmark_SA_real_2023) |>
  pivot_wider(
    names_from = geo,
    values_from = value
  ) |>
  arrange(date)

prov_hp <- prov_panel |>
  select(date, geo, value = Composite_Benchmark_SA_real_2023) |>
  pivot_wider(
    names_from = geo,
    values_from = value
  ) |>
  arrange(date)

metro_ratio <- metro_panel |>
  filter(date <= "2024-12") |>
  select(date, geo, value = composite_benchmark_income_per_capita_ratio) |>
  pivot_wider(
    names_from = geo,
    values_from = value
  ) |>
  arrange(date)

prov_ratio <- prov_panel |>
  filter(date <= "2024-12") |>
  select(date, geo, value = composite_benchmark_income_per_capita_ratio) |>
  pivot_wider(
    names_from = geo,
    values_from = value
  ) |>
  arrange(date)

metro_hp_dates <- metro_hp$date
metro_ratio_dates <- metro_ratio$date
prov_hp_dates <- prov_hp$date
prov_ratio_dates <- prov_ratio$date

m1 <- metro_hp |>
  select(-date) |>
  as.data.frame() |>
  log()

m2 <- metro_ratio |>
  select(-date) |>
  as.data.frame() |>
  log()

p1 <- prov_hp |>
  select(-date) |>
  as.data.frame() |>
  log()

p2 <- prov_ratio |>
  select(-date) |>
  as.data.frame() |>
  log()

m1_minw <- exuber::psy_minw(nrow(m1))
m2_minw <- exuber::psy_minw(nrow(m2))
p1_minw <- exuber::psy_minw(nrow(p1))
p2_minw <- exuber::psy_minw(nrow(p2))

m1_radf <- exuber::radf(
  m1,
  minw = m1_minw,
  lag = 1L
)

m2_radf <- exuber::radf(
  m2,
  minw = m2_minw,
  lag = 1L
)

p1_radf <- exuber::radf(
  p1,
  minw = p1_minw,
  lag = 1L
)

p2_radf <- exuber::radf(
  p2,
  minw = p2_minw,
  lag = 1L
)

exuber::index(m1_radf) <- metro_hp_dates
exuber::index(m2_radf) <- metro_ratio_dates
exuber::index(p1_radf) <- prov_hp_dates
exuber::index(p2_radf) <- prov_ratio_dates

# ------------------------------------------------------------------------------
# Get critical values
# ------------------------------------------------------------------------------

m1_mc_cv <- exuber::radf_mc_cv(
  n = nrow(m1),
  minw = m1_minw,
  nrep = 2000,
  seed = 15893
)
m1_sb_cv <- exuber::radf_sb_cv(
  data = m1,
  minw = m1_minw,
  lag = 1L,
  nboot = 1000,
  seed = 15893
)

m2_mc_cv <- exuber::radf_mc_cv(
  n = nrow(m2),
  minw = m2_minw,
  nrep = 2000,
  seed = 15893
)
m2_sb_cv <- exuber::radf_sb_cv(
  data = m2,
  minw = m2_minw,
  lag = 1L,
  nboot = 1000,
  seed = 15893
)

p1_mc_cv <- exuber::radf_mc_cv(
  n = nrow(p1),
  minw = p1_minw,
  nrep = 2000,
  seed = 15893
)
p1_sb_cv <- exuber::radf_sb_cv(
  data = p1,
  minw = p1_minw,
  lag = 1L,
  nboot = 1000,
  seed = 15893
)

p2_mc_cv <- exuber::radf_mc_cv(
  n = nrow(p2),
  minw = p2_minw,
  nrep = 2000,
  seed = 15893
)
p2_sb_cv <- exuber::radf_sb_cv(
  data = p2,
  minw = p2_minw,
  lag = 1L,
  nboot = 1000,
  seed = 15893
)

# ------------------------------------------------------------------------------
# Get results
# ------------------------------------------------------------------------------

metro_hp_stats <- exuber::tidy(m1_radf)
metro_ratio_stats <- exuber::tidy(m2_radf)
prov_hp_stats <- exuber::tidy(p1_radf)
prov_ratio_stats <- exuber::tidy(p2_radf)

metro_hp_bsadf <- exuber::augment(
  m1_radf,
  cv = m1_mc_cv
)
metro_ratio_bsadf <- exuber::augment(
  m2_radf,
  cv = m2_mc_cv
)

prov_hp_bsadf <- exuber::augment(
  p1_radf,
  cv = p1_mc_cv
)
prov_ratio_bsadf <- exuber::augment(
  p2_radf,
  cv = p2_mc_cv
)

metro_hp_univariate_datestamp <- exuber::datestamp(
  m1_radf,
  cv = m1_mc_cv,
  min_duration = exuber::psy_ds(nrow(m1)),
  option = "gsadf"
) |>
  exuber::tidy()

metro_ratio_univariate_datestamp <- exuber::datestamp(
  m2_radf,
  cv = m2_mc_cv,
  min_duration = exuber::psy_ds(nrow(m2)),
  option = "gsadf"
) |>
  exuber::tidy()

prov_hp_univariate_datestamp <- exuber::datestamp(
  p1_radf,
  cv = p1_mc_cv,
  min_duration = exuber::psy_ds(nrow(p1)),
  option = "gsadf"
) |>
  exuber::tidy()

prov_ratio_univariate_datestamp <- exuber::datestamp(
  p2_radf,
  cv = p2_mc_cv,
  min_duration = exuber::psy_ds(nrow(p2)),
  option = "gsadf"
) |>
  exuber::tidy()

# ------------------------------------------------------------------------------
# Save objects
# ------------------------------------------------------------------------------

dir.create("outputs/bsadf_gsadf", recursive = TRUE, showWarnings = FALSE)

make_panel_bsadf <- function(radf_obj, dates) {
  tibble::tibble(
    date = tail(dates, length(radf_obj$bsadf_panel)),
    bsadf_panel = radf_obj$bsadf_panel
  )
}

make_result_bundle <- function(
    label,
    y,
    dates,
    minw,
    radf_obj,
    mc_cv,
    sb_cv,
    stats,
    bsadf,
    univariate_datestamp
) {
  list(
    label = label,
    metadata = list(
      n = nrow(y),
      n_geographies = ncol(y),
      start_date = min(dates),
      end_date = max(dates),
      minw = minw,
      min_duration = exuber::psy_ds(nrow(y)),
      lag = 1L,
      mc_nrep = 2000L,
      sb_nboot = 1000L,
      seed = 15893L,
      exuber_version = as.character(utils::packageVersion("exuber"))
    ),
    radf = radf_obj,
    mc_cv = mc_cv,
    sb_cv = sb_cv,
    stats = stats,
    bsadf = bsadf,
    univariate_datestamp = univariate_datestamp,
    panel_bsadf = make_panel_bsadf(radf_obj, dates),
    panel_gsadf = tibble::tibble(
      gsadf_panel = radf_obj$gsadf_panel
    ),
    panel_summary = summary(radf_obj, sb_cv),
    panel_diagnostics = diagnostics(radf_obj, sb_cv)
  )
}

results <- list(
  metro_hp = make_result_bundle(
    label = "Metropolitan log real composite benchmark",
    y = m1,
    dates = metro_hp_dates,
    minw = m1_minw,
    radf_obj = m1_radf,
    mc_cv = m1_mc_cv,
    sb_cv = m1_sb_cv,
    stats = metro_hp_stats,
    bsadf = metro_hp_bsadf,
    univariate_datestamp = metro_hp_univariate_datestamp
  ),
  metro_ratio = make_result_bundle(
    label = "Metropolitan log composite benchmark income per capita ratio",
    y = m2,
    dates = metro_ratio_dates,
    minw = m2_minw,
    radf_obj = m2_radf,
    mc_cv = m2_mc_cv,
    sb_cv = m2_sb_cv,
    stats = metro_ratio_stats,
    bsadf = metro_ratio_bsadf,
    univariate_datestamp = metro_ratio_univariate_datestamp
  ),
  prov_hp = make_result_bundle(
    label = "Provincial log real composite benchmark",
    y = p1,
    dates = prov_hp_dates,
    minw = p1_minw,
    radf_obj = p1_radf,
    mc_cv = p1_mc_cv,
    sb_cv = p1_sb_cv,
    stats = prov_hp_stats,
    bsadf = prov_hp_bsadf,
    univariate_datestamp = prov_hp_univariate_datestamp
  ),
  prov_ratio = make_result_bundle(
    label = "Provincial log composite benchmark income per capita ratio",
    y = p2,
    dates = prov_ratio_dates,
    minw = p2_minw,
    radf_obj = p2_radf,
    mc_cv = p2_mc_cv,
    sb_cv = p2_sb_cv,
    stats = prov_ratio_stats,
    bsadf = prov_ratio_bsadf,
    univariate_datestamp = prov_ratio_univariate_datestamp
  )
)

saveRDS(results, "outputs/bsadf_gsadf/all_results.rds")

for (name in names(results)) {
  saveRDS(
    results[[name]],
    file.path("outputs/bsadf_gsadf", paste0(name, "_results.rds"))
  )
}
