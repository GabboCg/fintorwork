library(dplyr, warn.conflicts = FALSE)
library(janitor)
library(magrittr)
library(openxlsx)
library(usethis)
library(readr)

predictors_quarterly <- read.xlsx('data-raw/PredictorData2018.xlsx', sheet = 2)

predictors_quarterly %<>%
  as_tibble() %>%
  clean_names() %>%
  mutate_each(list(~ as.numeric(.)), b_m:ntis, infl:crsp_s_pvwx)

write_csv(predictors_quarterly, 'data-raw/predictors_quarterly.csv')
usethis::use_data(predictors_quarterly, overwrite = TRUE)



