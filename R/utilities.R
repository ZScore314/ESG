#' Create Piecewise Linear from Simulation
#'
#' @param dat numeric vector conatining simulation data
#' @param bins optional number of bins
#' @param xmin optional minimum value
#' @param xmax optional maximum value
#' @param remove.dup optional argument to remove bins with no probability mass
#'
#' @return tibble with columns x and Fx
#' @export
#'
#' @examples
CreatePWL <- function(dat, bins = 100, xmin = NULL, xmax = NULL, remove.dup = TRUE) {

  if(is.null(xmin))
    xmin <- min(dat)

  if(is.null(xmax))
    xmax <- max(dat)

  x <- seq(xmin, xmax, length.out=bins)

  ret <- tibble::tibble(x = x, Fx = ecdf(dat)(x))

  # increments and rescale
  ret <- ret %>%
    dplyr::mutate(fx = dplyr::if_else(dplyr::row_number() == 1, Fx, Fx - dplyr::lag(Fx))) %>%
    dplyr::mutate(fx = fx / sum(fx)) %>%
    dplyr::mutate(Fx = cumsum(fx))

  if(remove.dup)
    ret <- ret %>% dplyr::filter(fx != 0)

  ret <- ret %>% dplyr::select(-fx)

  # # Add zero row
  ret <- tibble::tibble(x=xmin, Fx=0) %>%
    dplyr::bind_rows(ret)

  return(ret)

}

#' Quantile Summary of distribution
#'
#' @param x data to be summarized
#' @param q vector of quantiles
#'
#' @return tibble with mean, standard deviation and quantiles
#' @export
#'
#' @examples
quibble <- function(x, q = c(.005, .01, .025, .05, .1, .5, .9, .95, .975, .99, .995)) {
  tibble(metric = c("Mean", "Stddev", "CV", paste0("VaR", as.character(q))),
         value = c(mean(x), sd(x), sd(x)/mean(x), quantile(x, q)))
}
