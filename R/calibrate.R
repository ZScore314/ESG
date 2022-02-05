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

#' Calibrate Inflation
#'
#' Get Historical Inflation and estimate parameters of 1-factor Vasicek Model
#'
#' @param n_years number of historical years to include in calibration
#' @param to a character string representing a end date in YYYY-MM-DD format
#' @param return.data
#'
#' @return list of ML parameters and data
#' @export
#'
#' @examples
CalInflation <- function(n_years = 20, from=NULL, to=NULL, return.data = FALSE){

  if(is.null(to))
    to <- as.character(lubridate::today())

  if(is.null(from))
    from <- as.character(lubridate::ymd(to) - lubridate::years(n_years))

  dat <- tidyquant::tq_get('CPIAUCSL', 'economic.data', from=from, to=to) %>%
    dplyr::mutate(inf = price / dplyr::lag(price, 12) - 1)

  f <- glm(inf ~ dplyr::lag(inf), data = dat)

  k <-(1 - f$coefficients[2]) * 12
  mu <- f$coefficients[1] / (1 - f$coefficients[2])
  sigma <- sqrt(summary(f)$dispersion) * sqrt(12)

  parms <- list(r0 = tail(dat$inf, 1), k = as.numeric(k), m = as.numeric(mu), v = sigma)

  print(paste("Parameters calibrated with", nrow(dat), "observations from", head(dat$date, 1), "through", tail(dat$date, 1)))

  if(return.data)
    return(list(parms = parms,
                dat = list(dat)))
  else
    return(parms)

}

CalVasicek1f <- function(data, dt = 1/12) {

  n <- length(data)

  x <- data[-n]
  y <- data[-1]

  Sx <- sum(x)
  Sy <- sum(y)
  Sxx <- as.numeric(crossprod(x, x))
  Sxy <- as.numeric(crossprod(x, y))
  Syy <- as.numeric(crossprod(y, y))

  mu  <- (Sy * Sxx - Sx * Sxy) / (n * (Sxx - Sxy) - (Sx^2 - Sx*Sy) )
  kappa <- -log((Sxy - mu * Sx - mu * Sy + n * mu^2) /   (Sxx - 2 * mu * Sx + n * mu^2)) / dt
  kappa2 <- -(1/dt) * log((n * Sxy - Sx * Sy)/(n * Sxx - Sx ^ 2))
  a <- exp(-kappa*dt)
  sigmah2 <- (Syy - 2 * a * Sxy + a^2 * Sxx - 2 * mu * (1-a) * (Sy - a * Sx) + n * mu^2 * (1 - a)^2)/n
  sigma <- sqrt(sigmah2 * 2 * kappa / (1 - a^2))

  parms <- list(r0 = tail(data,1),
                m = mu,
                k = kappa,
                k2 = kapp2,
                v = sigma)

  return(parms)

}

#' Vasicek Historical Calibration
#'
#' Calibrates the vasicek model using the maximum likelihood estimator
#
# TODO - Add in start and end dates.
#
# Args:
#   fred.ticker: Ticker used to download the historical rates from the Federal
#                Reserve Bank of St Louis. Defaults to DSG3MO, the 3-Month
#                Treasury Constant Maturity Rate.
#   dt: The change in time between observations. Defaults to 1/252 because
#       we assume generation of daily rates and there are 252 trading days
#       per year.
#
# Returns:
#   A vector of the form c(kappa, theta, sigma, r0), where kappa is the mean
#   reversion rate, theta is the long-term rate/mean, sigma is the volatility
#   and r0 is the last observed rate.
#
# Requires:
#   quantmod
#' @param fred.ticker
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
CalVasicekHist <- function(n_years = 99, to=NULL, from=NULL, return.data = F) {


  dt <- 1/12

  if(is.null(to))
    to <- as.character(lubridate::today())

  if(is.null(from))
    from <- as.character(lubridate::ymd(to) - lubridate::years(n_years))

  dat <- tidyquant::tq_get('DGS3MO', 'economic.data', from=from, to=to) %>%
    tidyquant::tq_transmute(mutate_fun = to.monthly) %>%
    dplyr::mutate(price = price / 100)

  n <- length(dat$price)

  x <- dat$price[-n]
  y <- dat$price[-1]

  Sx <- sum(x)
  Sy <- sum(y)
  Sxx <- as.numeric(crossprod(x, x))
  Sxy <- as.numeric(crossprod(x, y))
  Syy <- as.numeric(crossprod(y, y))

  mu  <- (Sy * Sxx - Sx * Sxy) / (n * (Sxx - Sxy) - (Sx^2 - Sx*Sy) )
  kappa <- -log((Sxy - mu * Sx - mu * Sy + n * mu^2) /   (Sxx - 2 * mu * Sx + n * mu^2)) / dt
  kappa2 <- -(1/dt) * log((n * Sxy - Sx * Sy)/(n * Sxx - Sx ^ 2))
  a <- exp(-kappa*dt)
  sigmah2 <- (Syy - 2 * a * Sxy + a^2 * Sxx - 2 * mu * (1-a) * (Sy - a * Sx) + n * mu^2 * (1 - a)^2)/n
  sigma <- sqrt(sigmah2 * 2 * kappa / (1 - a^2))

  parms <- list(r0 = tail(dat$price,1),
                m = mu,
                k = kappa,
                k2 = kappa2,
                v = sigma)

  if(return.data)
    return(list(parms = parms,
                dat = list(dat)))
  else
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

#' CIR Historical Calibration
#'
#' @param n_years
#' @param to
#' @param from
#' @param shift
#' @param return.data
#'
#' @return
#' @export
#'
#' @examples
CalCIRHist <- function(n_years = 99, to = NULL, from = NULL, shift = NULL, return.data = FALSE){

  if(is.null(to))
    to <- as.character(lubridate::today())

  if(is.null(from))
    from <- as.character(lubridate::ymd(to) - lubridate::years(n_years))

  dat <- tidyquant::tq_get('DGS3MO', 'economic.data', from=from, to=to) %>%
    tidyquant::tq_transmute(mutate_fun = to.monthly) %>%
    dplyr::mutate(r = price / 100)

  # Apply shift to avoid negative interest rates
  if (!is.null(shift))
    dat <- dat %>% dplyr::mutate(r = r + shift)

  dat <- dat %>%
    dplyr::mutate(y = (r - dplyr::lag(r))/dplyr::lag(sqrt(r)),
                  x1 = 1 / dplyr::lag(sqrt(r)),
                  x2 = dplyr::lag(sqrt(r)))

  f <- glm(y ~ 0 + x1 + x2, data = dat)

  k <- - f$coefficients[2] * 12
  mu <- - f$coefficients[1] / f$coefficients[2]
  sigma <- sqrt(summary(f)$dispersion) * sqrt(12)

  parms <- list(r0 = tail(dat$r, 1), k = as.numeric(k), m = as.numeric(mu), v = sigma)

  print(paste("Parameters calibrated with", nrow(dat), "observations from", head(dat$date, 1), "through", tail(dat$date, 1)))

  if(return.data)
    return(list(parms = parms,
                dat = list(dat)))
  else
    return(parms)

}

CIRobjective <- function(params, dat){
  Data = Model.Data;
  DataF = Data(2:end);
  DataL = Data(1:end-1);
  Nobs = length(Data);
  TimeStep = Model.TimeStep;
  alpha = Params(1);
  mu = Params(2);
  sigma = Params(3);
  c = 2*alpha/(sigma^2*(1-exp(-alpha*TimeStep)));
  q = 2*alpha*mu/sigma^2-1;
  u = c*exp(-alpha*TimeStep)*DataL;
  v = c*DataF;
  z = 2*sqrt(u.*v);
  bf = besseli(q,z,1);
  lnL= -(Nobs-1)*log(c) + sum(u + v - 0.5*q*log(v./u) - log(bf) - z);


}
