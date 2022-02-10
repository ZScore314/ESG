#' Price of bond under 1-factor Vasicek Model
#'
#' @param r
#' @param a
#' @param b
#' @param s
#' @param t
#'
#' @return
#' @export
#'
#' @examples
Vasicek1fPrice <- function(r, a, b, v, t){
  # returns the zero bond price that pays $1 at time t under a 1-factor Vasicek model
  # see Hull p.685

  # B term
  B <-  (1 - exp(-a * t)) / a

  # A Term
  A1 <- ((B - t) * (a * a * b - v^2 / 2)) / (a^2)
  A2 <- (v^2 * (B^2) / (4 * a))
  A <- exp(A1 - A2)

  # Bond Price
  P <- A * exp(-B * r)

  return(P)

}

#' Bond Yield under 1-factor Vasicek Model
#'
#' @param r
#' @param a
#' @param b
#' @param v
#' @param t
#'
#' @return
#' @export
#'
#' @examples
VasicekYield <- function(r, a, b, v, t){

  p <- VasicekPrice(r, a, b, v, t)

  -log(p)/t

}

#' Bond Price under 1-factor CIR Model
#'
#' @param r
#' @param a
#' @param b
#' @param v
#' @param t
#'
#' @return
#' @export
#'
#' @examples
CIR1fPrice <- function(r, a, b, v, t){
  # returns the zero bond price that pays $1 at time t under a 1-factor CIR model
  # see Hull p.685

  y <- sqrt(a^2 + 2 * v^2)

  # B term
  temp1 <- 2 * (exp(y * t) - 1)
  temp2 <- (y + a) * (exp(y * t) - 1) + 2 * y
  B <- temp1 / temp2

  temp3 <- 2 * y * exp(0.5 * (a + y) * t)
  A <- (temp3 / temp2) ^ (2 * a * b / v^2)

  # Bond Price
  P <- A * exp(-B * r)

  return(P)
}

#' Bond Yield under 1-factor CIR Model
#'
#' @param r
#' @param a
#' @param b
#' @param v
#' @param t
#'
#' @return
#' @export
#'
#' @examples
CIR1fYield <- function(r, a, b, v, t){

  p <- CIRPrice(r, a, b, v, t)

  -log(p)/t

}

#' Bond Price under 2-factor Vasicek Model
#'
#' @param r1 short rate at time t
#' @param r2 mean reversion level for the short rate at time t
#' @param a1 strength of mean reversion for short rate
#' @param a2 strength of mean reversion for short rate mean reversion
#' @param v1 annualized volatility of short rate
#' @param v2 annualized volatility of short rate mean
#' @param u mean reversion level for r2
#' @param t maturity of zero-coupon bond
#'
#' @return
#' @export
#'
#' @examples
Vasicek2fPrice <- function(r1, r2, a1, a2, v1, v2, u, t){
  # returns the zero bond price under a 2-factor Vasicek yield curve
  # see hibbert, mowbray, and turnbull p.18

  .Vas2fA <- function(a1, a2, v1, v2, u, s){
    # see hibbert, mowbray, and turnbull p.18
    # computes the function A in the bond pricing equation
    temp1 <- (.Vas2fB1(a1, s) - s) * (u - (v1 * v1) / (2 * a1 * a1))
    temp2 <- .Vas2fB2(a1, a2, s) * u - (v1 * v1 * .Vas2fB1(a1, s) * .Vas2fB1(a1, s)) / (4 * a1)
    temp3 <- (s / (a2 * a2) - 2 * (.Vas2fB2(a1, a2, s) + .Vas2fB1(a1, s)) / (a2 * a2))
    temp4 <- 1 / ((a1 - a2) * (a1 - a2)) * (1 - exp(-2 * a1 * s)) / (2 * a1)
    temp5 <- -2 * a1 / (a2 * (a1 - a2) * (a1 - a2)) * (1 - exp(-(a1 + a2) * s)) / (a1 + a2)
    temp6 <- a1 * a1 / (a2 * a2 * (a1 - a2) * (a1 - a2)) * (1 - exp(-2 * a2 * s)) / (2 * a2)

    ret <- temp1 + temp2 + (v2 * v2 / 2) * (temp3 + temp4 + temp5 + temp6)

    return(ret)

  }

  .Vas2fB1 <- function(a, s) (1 - exp(-1 * a * s)) / a

  .Vas2fB2 <- function(a1, a2, s){

    temp1 <- (1 - exp(-1 * a2 * s)) / a2 - (1 - exp(-1 * a1 * s)) / a1

    a1 / (a1 - a2) * temp1

  }

  exp(.Vas2FA(a1, a2, v1, v2, u, t) - .Vas2FB1(a1, t) * r1 - .Vas2FB2(a1, a2, t) * r2)

}

#' Bond Yield under 2-factor Vasicek Model
#'
#' @param r1 short rate at time t
#' @param r2 mean reversion level for the short rate at time t
#' @param a1 strength of mean reversion for short rate
#' @param a2 strength of mean reversion for short rate mean reversion
#' @param v1 annualized volatility of short rate
#' @param v2 annualized volatility of short rate mean
#' @param u mean reversion level for r2
#' @param t maturity of zero-coupon bond
#'
#' @return
#' @export
#'
#' @examples
Vasicek2fYield <- function(r1, r2, a1, a2, v1, v2, u, t){

  p <- Vasicek2fPrice(r1, r2, a1, a2, v1, v2, u, t)

  -log(p)/t

}
