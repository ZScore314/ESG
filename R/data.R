#' Historical CPI & Inflation
#'
#' A dataset containing historical seasonally adjusted CPI and inflation.
#'
#' @format A data frame with 888 rows and 3 variables:
#' \describe{
#'   \item{date}{Calendar Month}
#'   \item{price}{CPI}
#'   \item{inf}{YoY % change in CPI}
#'   ...
#' }
#' @source \url{https://fred.stlouisfed.org/series/CPIAUCSL}
"inflation"

#' Historical S&P500 Monthly Returns
#'
#' A dataset containing historical log (continous) returns of the S&P500 index.
#'
#' @format A data frame with 1128 rows and 2 variables:
#' \describe{
#'   \item{date}{Calendar Month}
#'   \item{return}{log return}
#'   ...
#' }
#' @source \url{https://finance.yahoo.com/quote/%5EGSPC}
"equity"

#' Historical Yield on U.S. Treasury Securities at 3-Month Constant Maturity
#'
#' A dataset containing historical seasonally adjusted CPI and inflation.
#'
#' @format A data frame with 484 rows and 2 variables:
#' \describe{
#'   \item{date}{Calendar Month}
#'   \item{yield}{yield}
#'   ...
#' }
#' @source \url{https://fred.stlouisfed.org/series/DGS3MO}
"short_rates"
