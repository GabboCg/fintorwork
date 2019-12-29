library(dplyr, warn.conflicts = FALSE)
library(janitor)
library(magrittr)
library(openxlsx)
library(usethis)
library(readr)

predictors_annual <- read.xlsx('data-raw/PredictorData2018.xlsx', sheet = 3)

predictors_annual %<>%
  as_tibble() %>%
  clean_names() %>%
  mutate_each(list(~ as.numeric(.)), b_m:ntis, infl:crsp_s_pvwx)

write_csv(predictors_annual, 'data-raw/predictors_monthly.csv')
usethis::use_data(predictors_annual, overwrite = TRUE)




