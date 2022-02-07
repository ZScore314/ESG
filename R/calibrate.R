# Would like to avoid explicit calls to library but doing this in the meantime
# library(tidyquant)

#' Default Inflation Parameters (CAS-SOA)
#'
#' List of stochastic process parameters
#'
#' Convenience list of stochastic process parameters for Vasicek Model
#' @export
cas_inflation_vas1f <- list(r0 = 0.01, m=0.048, k=0.4, v=0.04, rmin=-0.02)


#' Default 2 factor Vasicek Real Interest Rate Parameters (CAS-SOA)
#'
#' List of stochastic process parameters
#'
#' Convenience list of stochastic process parameters for two-factor Vasicek Model
#' @export
cas_rates_vas2f <- list(param_long = list(r0 = 0.007, m = 0.028, k = 0.1, v = 0.0165, rmin = NULL),
                      param_short = list(r0 = 0, k = 1, v = 0.01, rmin = -0.05))


#' Calibrate Equity Returns using
#'
#' Get Historical mean and standard deviation of monthly log returns
#'
#' @param n_years number of historical years to include in calibration
#' @param to a character string representing a end date in YYYY-MM-DD format
#' @param return.data
#'
#' @return list of ML parameters and data
#' @export
#'
#' @examples
CalEquityILN <- function(n_years = 20, from=NULL, to=NULL, return.data = FALSE, symbol = "^GSPC"){

  if(is.null(to))
    to <- as.character(lubridate::today())

  if(is.null(from))
    from <- as.character(lubridate::ymd(to) - lubridate::years(n_years))

  dat <- tidyquant::tq_get(symbol, from=from, to=to)

  # convert to xts and calculate monthly log-return
  dat <- dat %>%
    dplyr::select(date, adjusted) %>%
    timetk::tk_xts_(silent = T) %>%
    quantmod::monthlyReturn(type = "log")

  # convert to tibble
  dat <- tibble::as_tibble(dat) %>%
    tibble::add_column(date = zoo::index(dat), .before = 1, )

  dat_sum <- dat %>%
    dplyr::summarise(meanlog = mean(monthly.returns),
                     sdlog = sd(monthly.returns))

  mean_ann <- exp((dat_sum$meanlog + 0.5 * dat_sum$sdlog^2) * 12) - 1
  sdlog_ann <- dat_sum$sdlog * sqrt(12)

  parms <- list(mean = mean_ann, vol = sdlog_ann)

  print(paste("Parameters calibrated with", nrow(dat), "observations from", head(dat$date, 1), "through", tail(dat$date, 1)))

  if(return.data)
    return(list(parms = parms,
                dat = list(dat)))
  else
    return(parms)

}

#' Calibrate 1-factor Vasicek Model using the Maximum Likelihood Estimator
#'
#' @param data numerical vector of time series data
#' @param dt time step, default is monthly (1/12)
#'
#' @return a named list with parameters r0 - initial value,
#' m - mean reversion level, k - speed of mean reversion (annual), v - annual volatility
#' @export
#'
#' @examples
CalVasicek1f <- function(dat, dt = 1/12) {

  # r1 ~ Normal(r0 + k(m - r0), sigma)

  if(!is.numeric(dat))
    stop("Data input must be in the form of a numeric vector")

  n <- length(dat)

  x <- dat[-n]
  y <- dat[-1]

  f <- lm(y ~ x)

  k <- as.numeric((1 - f$coefficients[2]) / dt)
  mu <- as.numeric(f$coefficients[1] / (1 - f$coefficients[2]))
  sigma <- sqrt(mean(summary(f)$residuals^2) * (1/dt))

  parms <- list(r0 = tail(dat, 1), m = mu, k = k, v = sigma)

  return(parms)


}

#' Calibrate 1-factor CIR Model using the Maximum Likelihood Estimator
#'
#' @param data numerical vector of time series data
#' @param dt time step, default is monthly (1/12)
#' @param shift optional shift to apply prior to fitting. This helps if negatie values may cause errors.
#'
#' @return a named list with parameters r0 - initial value,
#' m - mean reversion level, k - speed of mean reversion (annual), v - annual volatility
#' @export
#'
#' @examples
CalCIR1f <- function(dat, dt = 1/12, shift = NULL) {

  if(!is.numeric(dat))
    stop("Data input must be in the form of a numeric vector")

  # Apply shift to avoid negative interest rates
  if (!is.null(shift))
    dat <- dat + shift

  dat <- tibble::tibble(r = dat)

  dat <- dat %>%
    dplyr::mutate(y = (r - dplyr::lag(r))/dplyr::lag(sqrt(r)),
                  x1 = 1 / dplyr::lag(sqrt(r)),
                  x2 = dplyr::lag(sqrt(r)))

  f <- lm(y ~ 0 + x1 + x2, data = dat)

  k <- as.numeric(- f$coefficients[2] / dt)
  mu <- as.numeric(- f$coefficients[1] / f$coefficients[2])
  sigma <- sqrt(mean(summary(f)$residuals^2) * (1/dt))

  parms <- list(r0 = tail(dat$r, 1), m = mu, k = k, v = sigma)

  return(parms)

}


CalVasicekHist2 <- function(n_years = 99, to=NULL, from=NULL, return.data = FALSE){

  if(is.null(to))
    to <- as.character(lubridate::today())

  if(is.null(from))
    from <- as.character(lubridate::ymd(to) - lubridate::years(n_years))

  dat <- tidyquant::tq_get('DGS3MO', 'economic.data', from=from, to=to) %>%
    tidyquant::tq_transmute(mutate_fun = to.monthly) %>%
    dplyr::mutate(price = price / 100)

  f <- glm(price ~ lag(price), data = dat)

  k <-(1 - f$coefficients[2]) * 12
  mu <- f$coefficients[1] / (1 - f$coefficients[2])
  sigma <- sqrt(summary(f)$dispersion) * sqrt(12)

  parms <- list(r0 = tail(dat$price, 1), k = as.numeric(k), m = as.numeric(mu), v = sigma)

  print(paste("Parameters calibrated with", nrow(dat), "observations from", head(dat$date, 1), "through", tail(dat$date, 1)))

  if(return.data)
    return(list(parms = parms,
                dat = list(dat)))
  else
    return(parms)

}
