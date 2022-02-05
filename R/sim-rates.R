#' Simulate mean reverting diffusion process
#'
#' Monthly Simulation of rates using Ornstein-Uhlenbeck Model
#'
#' @param n number of simulated trials
#' @param t number of future monthly timesteps
#' @param param_list a named list with parameters r0 - initial value,
#' m - mean reversion level, k - speed of mean reversion (annual), v - annual volatility
#' @param rmin - optional rate lower bound
#' @param g optional proportional conditional volatility exponent (0 = Vasicek, 1/2 = CIR)
#' @param z_rand optional vector standard normal random variables of length n * t
#' @param col_rename optional name of output column
#'
#' @return data in the form of a `tibble` object with columns trial, time, r (or col_rename)
#' @keywords internal
.SimDiffusion <- function(n = 1, param_list = list(r0, m, k, v),
                          rmin = NULL, t = 12, g = 0, z_rand = NULL,
                          col_rename = NULL, seed = NULL){


  if (!is.list(param_list))
    stop("'param_list' must be a list.")

  # Check for required parameters
  check_param_list <- c("r0", "m", "k", "v") %in% names(param_list)
  if (sum(check_param_list) < 4){
    stop(paste0("'param_list' does not contain required parameters. Missing: ",
                paste(c("r0", "m", "k", "v")[check_param_list == FALSE], collapse = ", ")))
  }

  # Check seed input
  if (!is.null(seed)) {
    if (!is.vector(seed, mode = "numeric") || length(seed) !=
        1 || seed != trunc(seed) || seed < 0 || seed > 3000)
      stop("'seed' must be  an integer between 0 and 3000")
    set.seed(seed)
  }

  # allow for user supplied random numbers
  if (is.null(z_rand))
    z_rand <- rnorm(n * t)

  if(length(z_rand) != n * t)
    stop("Length of supplied random variables inconsistent with required size n * t")

  z_rand <- matrix(z_rand, ncol = n)

  if(!length(param_list$m) %in% c(1, n*t))
    stop("Length of supplied mean reversion parameters inconsistent with required size n * t")

  m <- matrix(param_list$m, t, n) # matrix of mean reversion parameters

  dt <- 1/12 # timestep

  r <- matrix(0, t+1, n) # matrix to hold simulated paths
  r[1,] <- param_list$r0

  for(j in 1:n){
    for(i in 2:(t+1)){
      dr <- param_list$k * (m[i-1, j] - r[i-1, j]) * dt + param_list$v * r[i-1, j] ^ g * z_rand[i-1, j] * sqrt(dt)
      r[i, j] <- max(r[i-1, j] + dr, rmin)
    }
  }

  sim <- tidyr::crossing(trial = 1:n, time = 1:t / 12) %>%
      dplyr::bind_cols(r = as.vector(r[-1,])) # remove initial state, convert to vector

  if (!is.null(col_rename))
    names(sim)[3] <- col_rename

  return(sim)

}

#' Simulate 1 factor Vasicek model
#'
#' Monthly Simulation of rates using 1 factor Vasicek model
#'
#' @param n number of simulated trials
#' @param t number of future monthly timesteps
#' @param param_list a named list with parameters r0 - initial value,
#' m - mean reversion level, k - speed of mean reversion (annual), v - annual volatility
#' @param rmin - optional rate lower bound
#' @param shift optional additive shift post simulation
#' @param z_rand optional vector standard normal random variables of length n * t
#' @param col_rename optional name of output column
#' @param seed optional starting seed
#'
#' @return data in the form of a `tibble` object with columns trial, time, r (or col_rename)
#' @export
#'
#' @examples SimVasicek1F(param_list = list(r0=.01, m=.05, k=.5, v=.05))
SimVasicek1F <- function(n = 1, param_list = list(r0=.01, m=.05, k=.5, v=.05),
                         rmin = NULL, t = 12, z_rand = NULL, col_rename = NULL,
                         seed = NULL){

  .SimDiffusion(n=n, param_list=param_list, rmin=rmin, t=t, g=0,
                z_rand=z_rand, col_rename=col_rename, seed=seed)

}


#' Simulate 2 factor Vasicek Model (stochastic mean reverting term)
#'
#' Monthly Simulation of Inflation using 2-factor Vasicek (Hull White) Model
#'
#' @param n number of simulated trials
#' @param t number of future monthly timesteps
#' @param param_short (short rate ) a named list with parameters r0 - initial value,
#' m - mean reversion level, k - speed of mean reversion (annual), v - annual volatility, rmin - optional rate lower bound
#' @param param_long (mean reversion term) a named list with parameters r0 - initial value,
#' m - mean reversion level, k - speed of mean reversion (annual), v - annual volatility, rmin - optional rate lower bound
#' @param rcorr correlation between long and short processes
#' @param seed optional starting seed
#'
#' @return data in the form of a `tibble` object with columns trial, time, r1, r2
#' @export
#'
#' @examples SimVasicek2F(param_short = list(r0=.01, m=.05, k=.5, v=.05), param_long = list(r0=.01, m=.05, k=.5, v=.05))
SimVasicek2F <- function(n = 1,
                         param_short = list(r0=.01, m=.05, k=.5, v=.05, rmin=NULL),
                         param_long = list(r0=.01, m=.05, k=.5, v=.05, rmin=NULL),
                         t = 12, rcorr = 0.5, seed = NULL){

  # get correlated random normals
  z_rand <-mvtnorm::rmvnorm(n * t, sigma = matrix(c(1,rcorr,rcorr,1), ncol=2), method = "chol")

  # First get long rates
  r2 <- .SimDiffusion(n=n, param_list=list(r0=param_long$r0, m=param_long$m, k=param_long$k, v=param_long$v), rmin=param_long$rmin, t=t, g=0, z_rand=z_rand[,1], col_rename = "r2")
  r2_mat <- matrix(r2$r2, ncol = n)

  # Simulate short rates using stochastic mean reversion parameter
  r1 <- .SimDiffusion(n=n, param_list=list(r0=param_short$r0, m=r2_mat,  k=param_short$k, v=param_short$v), rmin=param_short$rmin, t=t, g=0, z_rand=z_rand[,2], col_rename = "r1")

  sim <- r1 %>%
    dplyr::left_join(r2, by = c("trial", "time"))

  return(sim)

}

#' Simulate 1 factor Cox Ingersoll Ross (CIR) model
#'
#' Monthly Simulation of rates using 1 factor CIR model
#'
#' @param n number of simulated trials
#' @param t number of future monthly timesteps
#' @param param_list a list with parameters r0 - initial value,
#' m - mean reversion level, k - speed of mean reversion (annual), v - annual volatility
#' @param rmin - optional rate lower bound
#' @param shift optional additive shift post simulation
#' @param z_rand optional vector standard normal random variables of length n * t
#' @param col_rename optional name of output column
#' @param seed optional starting seed
#'
#' @return data in the form of a `tibble` object with columns trial, time, r (or col_rename)
#' @export
#'
#' @examples SimCIR1F(param_list = list(r0=.01, m=.05, k=.5, v=.05))
SimCIR1F <- function(n = 1, param_list = list(r0=.01, m=.05, k=.5, v=.05),
                     rmin = NULL, t = 12, z_rand = NULL, col_rename = NULL,
                     seed = NULL){

  .SimDiffusion(n=n, param_list=param_list, rmin=rmin, t=t, g=0.5,
                z_rand=z_rand, col_rename=col_rename, seed=seed)

}
