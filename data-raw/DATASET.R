# Scripts used to generate package datasets

library(dplyr)
library(tidyquant)


inflation <- tq_get('CPIAUCSL', 'economic.data', from="1900-01-01") %>%
  mutate(inf_yoy = price / lag(price, 12) - 1) %>%
  mutate(inf_mon = (price / lag(price) - 1) * 12) %>%
  filter(!is.na(inf_yoy)) %>%
  transmute(date = zoo::as.yearmon(date),
            cpi = price,
            inf_mon = inf_mon,
            inf_yoy = inf_yoy)

short_rates <- tq_get('DGS3MO', 'economic.data', from="1900-01-01") %>%
  tq_transmute(mutate_fun = to.monthly) %>%
  left_join(inflation) %>%
  transmute(date = date,
            nominal = price / 100,
            real = nominal - inf_mon)

long_rates <- tq_get('DGS10', 'economic.data', from="1900-01-01") %>%
  tq_transmute(mutate_fun = to.monthly) %>%
  left_join(inflation) %>%
  transmute(date = date,
            nominal = price / 100,
            real = nominal - inf_mon)

equity <- tq_get("^GSPC", from="1900-01-01") %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly", type = "log", col_rename = "logreturn")


usethis::use_data(inflation, overwrite = T)
usethis::use_data(short_rates, overwrite = T)
usethis::use_data(long_rates, overwrite = T)
usethis::use_data(equity, overwrite = T)

