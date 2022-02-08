#' Historical CPI & Inflation
#'
#' A dataset containing historical seasonally adjusted CPI and inflation.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{date}{Calendar Month}
#'   \item{cpi}{CPI}
#'   \item{inf_mon}{Monthly inflation annualized}
#'   \item{inf_yoy}{YoY % change in CPI}
#'   ...
#' }
#' @source \url{https://fred.stlouisfed.org/series/CPIAUCSL}
"inflation"

#' Historical S&P500 Monthly Returns
#'
#' A dataset containing historical log (continous) returns of the S&P500 index.
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{date}{Calendar Month}
#'   \item{return}{log return}
#'   ...
#' }
#' @source \url{https://finance.yahoo.com/quote/%5EGSPC}
"equity"

#' Historical Yield on U.S. Treasury Securities at 3-Month Constant Maturity
#'
#' A dataset containing historical 3-month treasury yields (nominal)
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{date}{Calendar Month}
#'   \item{nominal}{nominal yield}
#'   \item{real}{real yield estimated as nominal less monthly inflation}
#'   ...
#' }
#' @source \url{https://fred.stlouisfed.org/series/DGS3MO}
"short_rates"

#' Historical Yield on U.S. Treasury Securities at 10-Year Constant Maturity
#'
#' A dataset containing historical 10-year treasury yields (nominal)
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{date}{Calendar Month}
#'   \item{nominal}{nominal yield}
#'   \item{real}{real yield estimated as nominal less monthly inflation}
#'   ...
#' }
#' @source \url{https://fred.stlouisfed.org/series/DGS10}
"long_rates"
