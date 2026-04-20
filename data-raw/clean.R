# 96.3 - 2022
# 97.5 - 2022
# 98.2 - 2023
# 99.4 - 2023
# 100.3 - 2024
# 101.5 - 2024

library(rgesis)
library(haven)
library(tidyverse)
library(countrycode)
library(eurostat)
library(lme4)
library(future)
library(furrr)
library(giscoR)
library(sf)
options(gesis_download_purpose = "lecturer")

# Harmonized Eurobarometer
eb <- read_dta(gesis_data("SDN-10.7802-2539", select = "\\.zip", path = "data_raw")) |>
  select(
    year, country,
    mem, benefit, treu,
    gender, age, occup, educ
  ) |>
  mutate(
    non_working = occup %in% c(521, 530, 540),
    unempl = occup == 540
  ) |>
  as_factor()


eb96 <- read_dta(gesis_data("ZA7848", select = "\\.dta", path = "data_raw")) |>
  select(
    year = p1,
    country = isocntry,
    mem = qa7b,
    benefit = qa7c,
    treu = qa6b_10,
    gender = d10,
    age = d11,
    occup = d15a
  ) |>
  mutate(year = year(as.POSIXct(as.character(as_factor(year)), format = "%d-%b-%Y")))
eb97 <- read_dta(gesis_data("ZA7902", select = "\\.dta", path = "data_raw")) |>
  select(
    year = p1,
    country = isocntry,
    mem = qa6b,
    benefit = qa6c,
    treu = qa6a_10,
    gender = d10,
    age = d11,
    occup = d15a
  ) |>
  mutate(year = year(as.POSIXct(as.character(as_factor(year)), format = "%d-%b-%Y")))
eb98 <- read_dta(gesis_data("ZA7953", select = "\\.dta", path = "data_raw")) |>
  select(
    year = p1,
    country = isocntry,
    mem = qa9a,
    benefit = qa9b,
    treu = qa6_11,
    gender = d10,
    age = d11,
    occup = d15a
  ) |>
  mutate(year = year(as.POSIXct(as.character(as_factor(year)), format = "%d-%b-%Y")))
eb99 <- read_dta(gesis_data("ZA7997", select = "\\.dta", path = "data_raw")) |>
  select(
    year = p1,
    country = isocntry,
    mem = qa9a,
    benefit = qa9b,
    treu = qa6_11,
    gender = d10,
    age = d11,
    occup = d15a
  ) |>
  mutate(year = year(as.POSIXct(as.character(as_factor(year)), format = "%d-%b-%Y")))
eb100 <- read_dta(gesis_data("ZA8843", select = "\\.dta", path = "data_raw")) |>
  select(
    year = p1,
    country = isocntry,
    mem = qa8a,
    benefit = qa8b,
    treu = qa6_10,
    gender = d10,
    age = d11,
    occup = d15a
  ) |>
  mutate(year = year(as.POSIXct(as.character(as_factor(year)), format = "%d-%b-%Y")))
eb101 <- read_dta(gesis_data("ZA8843", select = "\\.dta", path = "data_raw")) |>
  select(
    year = p1,
    country = isocntry,
    mem = qa8a,
    benefit = qa8b,
    treu = qa6_10,
    gender = d10,
    age = d11,
    occup = d15a
  ) |>
  mutate(year = year(as.POSIXct(as.character(as_factor(year)), format = "%d-%b-%Y")))

eb_later <- bind_rows(eb96, eb97, eb98, eb99, eb100, eb101) %>%
  mutate(
    non_working = occup %in% 2:4,
    unempl = occup == 3
  ) |>
  as_factor() |>
  mutate(
    mem = factor(case_match(
      mem,
      "A good thing" ~ "Good thing",
      "Neither good nor bad" ~ "Neither good nor bad",
      "A bad thing" ~ "Bad thing",
      .default = NA
    ), levels = levels(eb$mem)),

    benefit = factor(case_match(
      benefit,
      "Would benefit" ~ "Benefitted",
      "Would not benefit" ~ "Not benefitted",
      .default = NA
    ), levels = c("Benefitted", "Not benefitted")),

    treu = na_if(treu, "Don't know (SPONTANEOUS)"),

    gender = factor(case_match(
      gender,
      "Man" ~ "Male",
      "Woman" ~ "Female",
      .default = NA
    )),
    age = age |>
      as.character() |>
      na_if("Refusal") |>
      str_remove(fixed(" years")) |>
      as.numeric(),

    country = case_when(
      country == "CY-TCC" ~ "CY",
      country == "DE-E" ~ "DE",
      country == "DE-W" ~ "DE",
      country == "RS-KM" ~ "RS",
      TRUE ~ country
    ),
    country = countrycode(country, "iso2c", "country.name")
  )

eb <- eb |>
  mutate(
    benefit = factor(case_match(
      as.character(benefit),
      "benefited" ~ "Benefitted",
      "not benefited" ~ "Not benefitted",
      .default = NA
    ), levels = levels(eb_later$benefit)),

    treu = factor(case_match(
      as.character(treu),
      "trust" ~ "Tend to trust",
      "no trust" ~ "Tend not to trust",
      .default = NA
    ), levels = levels(eb_later$treu)),

    gender = factor(case_match(
      as.character(gender),
      "male" ~ "Male",
      "female" ~ "Female",
      .default = NA
    ), levels = c("Male", "Female"))
  )

eb_cmb <- eb |>
  bind_rows(eb_later) |>
  select(-occup, -educ, -gender) |>
  mutate(
    mem = mem == "Good thing",
    benefit = mem == "Benefitted",
    treu = treu == "Tend to trust",
    country = case_match(
      country,
      "Czech Republic" ~ "Czechia",
      "Cyprus (TCC)" ~ "Cyprus",
      "Great Britain" ~ "United Kingdom",
      "Northern Ireland" ~ "United Kingdom",
      "Macedonia (FYROM)" ~ "North Macedonia",
      "Macedonia" ~ "North Macedonia",
      "Bosnia and Herzegovina" ~ "Bosnia & Herzegovina",
      .default = country
    ),
    age = factor(case_when(
      age >= 15 & age <= 24 ~ "15-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 55 ~ "45-54",
      age >= 45 & age <= 64 ~ "55-64",
      age >= 65 ~ "65+",
      TRUE ~ NA
    ), levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  ) |>
  filter(!country %in% c(
    "Albania", "Moldova", "Georgia", "Kosovo",
    "Switzerland", "Serbia", "Norway", "North Macedonia",
    "Montenegro", "Iceland", "Bosnia & Herzegovina"
  ))

unemp <- get_eurostat(
  "lfst_r_lfu3rt",
  filters = list(
    isced11 = "TOTAL",
    age = "Y_GE15",
    sex = "T"
  )
) |>
  filter(nchar(geo) == 2) |>
  mutate(
    geo = countrycode(geo, "eurostat", "country.name"),
    year = year(time)
  ) |>
  select(-freq, -isced11, -unit, -age, -sex, -time, country = geo)

lt_unemp <- get_eurostat(
  "lfst_r_lfu2ltu",
  filters = list(
    isced11 = "TOTAL",
    age = "Y_GE15",
    sex = "T",
    unit = "PC_ACT"
  )
) |>
  filter(nchar(geo) == 2) |>
  mutate(
    geo = countrycode(geo, "eurostat", "country.name"),
    year = year(time)
  ) |>
  select(-freq, -isced11, -unit, -age, -sex, -time, country = geo)

eb_cmb <- left_join(
  eb_cmb,
  unemp,
  by = c("country", "year")
) |>
  rename(unempl_share = values) |>
  left_join(
    lt_unemp,
    by = c("country", "year")
  ) |>
  rename(lt_unempl_share = values)

params <- expand.grid(
  outcome = "treu",
  predictor = c("unempl", "non_working", "unempl_share", "lt_unempl_share"),
  age = c(NA, levels(eb_cmb$age)),
  gender = c(NA, levels(eb_cmb$gender))
)
fit_names <- pmap_chr(params, \(...) paste(as.character(c(...)), collapse = "."))

plan(multisession, workers = 8)
fits <- future_pmap(params, function(outcome, predictor, age, gender) {
  .data <- eb_cmb

  if (!is.na(age)) {
    .data <- filter(.data, age == !!age)
  }

  if (!is.na(gender)) {
    .data <- filter(.data, gender == !!gender)
  }

  fm <- as.formula(sprintf(
    "%s ~ %s + (1 + %s | country / year)",
    outcome, predictor, predictor
  ))

  glmer(fm, data = .data, family = "binomial")
}, .progress = TRUE, .options = furrr_options(seed = TRUE))
names(fits) <- fit_names


invlogit <- function(x) 1 / (1 + exp(-x))

clean_re <- function(re) {
  nest <- row.names(re)
  row.names(re) <- NULL
  names(re) <- c("intercept", "slope")

  cntr <- gsub("^.+:", "", nest)
  cntr <- factor(cntr, levels = sort(unique(cntr)))
  if (any(grepl(":", nest))) {
    year <- gsub(":.+$", "", nest)
    year <- factor(year, levels = as.character(sort(as.numeric(unique(year)))))
    data.frame(
      year = year,
      country = cntr,
      intercept = re$intercept,
      slope = re$slope,
      effect = 0
    )
  } else {
    data.frame(
      country = cntr,
      intercept = re$intercept,
      slope = re$slope,
      effect = 0
    )
  }
}

get_ind_probs <- function(fit) {
  int <- fixef(fit)["(Intercept)"]
  coef <- fixef(fit)[2]

  re_country <- ranef(fit)[["country"]] |>
    clean_re() |>
    mutate(effect = int + slope) |>
    rename(
      intercept_country = intercept,
      slope_country = slope,
      effect_country = effect
    )

  re_country_year <- ranef(fit)[["year:country"]] |>
    clean_re() |>
    mutate(effect = int + slope)

  ranef <- left_join(re_country_year, re_country, by = "country")
  ranef$effect <- ranef$effect + ranef$slope_country

  ranef_intercept <- int + ranef$intercept + ranef$intercept_country
  ranef$bl_prob <- invlogit(ranef_intercept)
  ranef$prob <- invlogit(ranef_intercept + ranef$effect)
  ranef$prob_change <- ranef$prob - ranef$bl_prob
  ranef
}

get_contextual_preds <- function(fit) {
  # Fixed effects
  int <- fixef(fit)["(Intercept)"]
  coef <- fixef(fit)[2]

  # Random effects
  re_country <- ranef(fit)[["country"]] |>
    clean_re() |>
    select(-effect) |>
    rename(intercept_country = intercept, slope_country = slope)
  re_country_year <- ranef(fit)[["year:country"]] |>
    clean_re() |>
    select(-effect)

  unemp_range <- c(0.025, 0.05, 0.1, 0.15, 0.2, 0.25)
  preds <- expand.grid(
    context = unemp_range,
    year = unique(re_country_year$year),
    country = unique(re_country_year$country)
  )

  preds <- left_join(
    preds,
    re_country_year,
    by = c("country", "year")
  ) |>
    left_join(re_country, by = "country")

  ranef_intercept <- int + preds$intercept + preds$intercept_country
  ranef_effect <- coef + preds$slope_country + preds$slope
  preds$logit <- ranef_intercept + ranef_effect * preds$context
  preds$bl_prob <- invlogit(ranef_intercept)
  preds$prob <- invlogit(preds$logit)
  preds$prob_change <- preds$prob - preds$bl_prob
  preds
}


ranef_ind <- lapply(fits[params$predictor %in% c("unempl", "non_working")], get_ind_probs) |>
  bind_rows(.id = "fit") |>
  separate_wider_delim("fit", delim = ".", names = c("outcome", "predictor", "age", "gender")) |>
  mutate(across(c(age, gender), ~if_else(.x == "NA", NA, .x))) |>
  select(outcome, predictor, age, gender, year, country, bl_prob, prob, prob_change)

ranef_cxt <- lapply(fits[params$predictor %in% c("unempl_share", "lt_unempl_share")], get_contextual_preds) |>
  bind_rows(.id = "fit") |>
  separate_wider_delim("fit", delim = ".", names = c("outcome", "predictor", "age", "gender")) |>
  mutate(across(c(age, gender), ~if_else(.x == "NA", NA, .x))) |>
  select(outcome, predictor, age, gender, context, year, country, bl_prob, prob, prob_change)

cc <- countrycode(levels(ranef_ind$country), "country.name", "eurostat")
polys <- gisco_get_nuts(
  country = cc,
  nuts_level = 0,
  year = "2016",
  resolution = "60",
  epsg = 3035
) |>
  mutate(geo = countrycode(geo, "eurostat", "country.name")) |>
  select(country = geo)

usethis::use_data(ranef_cxt, ranef_ind, countries, internal = TRUE)
