#' Simulate Independent Lognormal (ILN)
#'
#' Simulate monthly returns assuming log returns in non-overlapping are iid
#' normal with constant mean and volatility
#'
#' @param n number of simulated trials
#' @param param Vector of length two containing [1] - expected annual (arithmetic) return
#' [2] volatility of stock price per year (sigma of log returns)
#' @param t number of future monthly timesteps
#' @param type type of output returns: arithmetic (discrete) or log (continuous)
#' @param seed optional starting seed
#'
#' @return data in the form of a `tibble` object with columns trial, time, mo_return, cum_return, accum
#' @export
#'
#' @examples SimEquityILN(n=10, c(.08, .16))
SimEquityILN <- function(n = 1, param = c(.08, .16),
                         t = 12,
                         type = c('arithmetic', 'log'),
                         seed = NULL){

  # Check for required parameters
  if (length(param) < 2)
    stop("parameter vector not length 2, does not contain both mean and volatility")

  # Check seed input
  if (!is.null(seed)) {
    if (!is.vector(seed, mode = "numeric") || length(seed) !=
        1 || seed != trunc(seed) || seed < 0 || seed > 3000)
      stop("'seed' must be  an integer between 0 and 3000")
    set.seed(seed)
  }

  meanlog <- (log(1 + param[1]) - .5 * param[2] ** 2) * 1/12
  sdlog <- param[2] * sqrt(1/12)

  sim <- tidyr::crossing(trial = 1:n,
                         time = 1:t / 12) %>%
    dplyr::mutate(mo_return = rnorm(dplyr::n(), meanlog, sdlog))

  sim <- sim %>%
    dplyr::group_by(trial) %>%
    dplyr::mutate(cum_return = cumsum(mo_return)) %>%
    dplyr::mutate(accum = exp(cum_return)) %>%
    dplyr::ungroup()

  if (type[1] == 'arithmetic'){
    sim <- sim %>%
      dplyr::mutate(mo_return = exp(mo_return) - 1) %>%
      dplyr::mutate(cum_return = exp(cum_return) - 1)
  }


  return(sim)


}


#' Regime Switching LogNormal Equity Return Model
#'
#' Simulate monthly equity returns from a 2-state RSLN model
#' @param n number of simulated trials
#' @param pswitch vector of length two with transition probabilities for each state
#' @param means vector of length two with monthly mean returns conditional on state
#' (added to optional unconditional mean)
#' @param vols vector of length two with monthly volatility conditional on state
#' @param t number of future monthly timesteps
#' @param state0 Optional intial state. If unchanged will simulate from unconditional state probabilities
#' @param mean_uncond Optional vector of length (n * t) additional ANNUAL return unconditional on state (e.g. real return + inflation)
#' @param z_rand Optional normal standard deviate vector of length (n * t)
#' @param type type of output returns: arithmetic (discrete) or log (continuous)
#' @param .detail Include additional detail state, mean, volatility
#'
#' @return data in the form of a `tibble` object with columns trial, time, mo_return, cum_return, accum
#' @export
#'
#' @examples SimEquityRSLN(n=10, pswitch=c(.011, .059), means=c(.008, -.011), vols = c(.039, .113))
SimEquityRSLN <- function(n = 1,
                          pswitch = c(.011, .059),
                          means = c(.008, -.011),
                          vols = c(.039, .113),
                          t = 12,
                          state0 = c(0, 1),
                          mean_uncond = 0,
                          z_rand = NULL,
                          type = c('arithmetic', 'log'),
                          .detail = FALSE) {

  # allow for user supplied random numbers
  if (is.null(z_rand))
    z_rand <- rnorm(n * t)

  if(length(z_rand) != n * t)
    stop("Length of supplied random variables inconsistent with required size n * t")

  # z_rand <- matrix(z_rand, ncol = n)

  if(!length(mean_uncond) %in% c(1, n*t))
    stop("Length of supplied mean reversion parameters inconsistent with required size n * t")

  # matrix of mean reversion parameters
  mean_uncond <- matrix(mean_uncond/12, t, n) %>%
    as.vector()

  s <- .GetStates(n, t, pswitch, state0) %>% as.vector()

  # vector of conditional means
  m_xs <- purrr::map_dbl(s, ~ means[.+1])

  # vector of conditional volatilities
  v <-  purrr::map_dbl(s, ~ vols[.+1])

  S <- m_xs + mean_uncond + v * z_rand

  sim <- tidyr::crossing(trial = 1:n,
                         time = 1:t / 12) %>%
    dplyr::mutate(state = s, mean_uncond = mean_uncond, mean_xs = m_xs, vol = v, z = z_rand, mo_return = S)

  sim <- sim %>%
    dplyr::group_by(trial) %>%
    dplyr::mutate(cum_return = cumsum(mo_return)) %>%
    dplyr::mutate(accum = exp(cum_return)) %>%
    dplyr::ungroup()

  if (type[1] == 'arithmetic'){
    sim <- sim %>%
      dplyr::mutate(mo_return = exp(mo_return) - 1) %>%
      dplyr::mutate(cum_return = exp(cum_return) - 1)
  }

  if (!.detail)
    sim <- dplyr::select(sim, trial, time, mo_return, cum_return, accum)

  return(sim)

}

#' Get states vector of regime switching process
#'
#' @param n number of simulated trials
#' @param t number of future monthly timesteps
#' @param pswitch vector of length two with transition probabilities for each state
#' @param state0 Optional intial state. If unchanged will simulate from unconditional state probabilities
#'
#' @return
#' @keywords internal
.GetStates <- function(n = 1, t = 12, pswitch = c(.011, .059), state0 = c(0, 1)){

  s <- matrix(0, t+1, n) # matrix of states

  for(j in 1:n){

    # get states
    # simulate random starting regime if not supplied
    if (length(state0) > 1)
      state0 <- 1 - rbinom(1, 1, pswitch[2] / sum(pswitch))

    s[1, j] <- state0

    for(i in 2:(t+1)){

      transition <- rbinom(1, 1, pswitch[s[i-1, j] + 1])

      if (transition == 1)
        s[i, j] <- 1 - s[i-1, j]
      else
        s[i, j] <- s[i-1, j]
    }

  }

  s <- s[-1,]

  return(s)

}


