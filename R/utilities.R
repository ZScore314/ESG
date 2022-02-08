#' Get Treasury Yield Curve as of given date
#'
#' @param date a character string representing a date in YYYY-MM-DD format
#'
#' @return tibble with columns date, symbol, yield
#' @export
#'
#' @examples GetTreasuries("2022-02-04")
GetTreasuries <- function(date = NULL){

  if(is.null(date))
    date <- paste0(year(Sys.Date())-1,"-12-31")

  yields <- tidyquant::tq_get(c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"), "economic.data",
                   to = date, from = date)

  yields <- yields %>%
    dplyr::transmute(date = date,
              symbol = symbol,
              yield = price / 100)

  return(yields)

}

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

#' Funnel Uncertainty Plot
#'
#' @param data_forecast tibble with columns time, value
#' @param data_actual numeric vector of historical monthly
#' @param init_value value at time 0
#' @param variable_name A string or character vector containing name of projected variable
#' @param time_offset numeric offset for x-axis (e.g. time 0 = 2022)
#'
#' @return ggplot object
#' @export
#'
#' @examples
gg_funnel <- function(data_forecast, data_actual, init_value, variable_name = NULL, time_offset = 0){

  funnel <- function(x, q = c(.01, .25, .5, .75, .99)) {
    tibble(metric = c(paste0("VaR", as.character(q * 100))),
           value = c(quantile(x, q)))
  }

  q_sum <- data_forecast %>%
    group_by(time) %>%
    summarise(funnel(value)) %>%
    ungroup()

  q_sum <- q_sum %>%
    distinct(metric) %>%
    mutate(time = 0, value = init_value) %>%
    bind_rows(q_sum)

  # calc offset for graph
  gg_offset <- ifelse(min(q_sum$value) < 0, -min(q_sum$value), 0)

  q_med <- q_sum %>%
    filter(metric == "VaR50") %>%
    mutate(gg_value = value + gg_offset) %>%
    mutate(time = time + time_offset)

  q_sum_gg <- q_sum %>%
    mutate(gg_value = value + gg_offset) %>%
    mutate(time = time + time_offset) %>%
    group_by(time) %>%
    mutate(gg_value = if_else(metric == "VaR1", gg_value, gg_value - lag(gg_value))) %>%
    mutate(metric = fct_rev(fct_inorder(metric))) %>%
    ungroup()

  # historical actual
  hist <- tibble(value = data_actual) %>%
    mutate(time = -rev(1:n() - 1)/12) %>%
    mutate(gg_value = value + gg_offset) %>%
    mutate(time = time + time_offset)

  q_sum_gg %>%
    ggplot() +
    geom_area(aes(time, gg_value, fill = metric)) +
    scale_fill_manual(values = c("darkblue", "lightblue", "lightblue", "darkblue", "white")) +
    geom_line(aes(time, gg_value), data = q_med) +
    geom_line(aes(time, gg_value), data = hist) +
    scale_y_continuous(labels = ~ AonECM::pct_format(. - gg_offset, digits = 1), breaks = seq(-.02, .30, .02) + gg_offset) +
    geom_hline(yintercept = 0 + gg_offset, size = 0.5, color = "grey") +
    labs(x = "t",
         y = "Yield",
         title = paste("Forecast & Actual", variable_name),
         subtitle = "Shaded Regions indicate 25-75 and 1-99 percentiles") +
    theme_classic() +
    theme(axis.line.x = element_blank()) +
    theme(legend.position="none")


}
