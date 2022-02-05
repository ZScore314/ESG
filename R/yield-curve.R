#' Price of bond under Vasicek Model
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
VasicekPrice <- function(r, a, b, s, t){
  # returns the zero bond price that pays $1 at time t under a 1-factor Vasicek model
  # see Hull p.685

  # B term
  B <-  (1 - exp(-a * t)) / a

  # A Term
  A1 <- ((B - t) * (a * a * b - s * s / 2)) / (a^2)
  A2 <- (s * s * (B^2) / (4 * a))
  A <- exp(A1 - A2)

  # Bond Price
  P <- A * exp(-B * r)

  return(P)

}

#' Bond Yield under Vasicek Model
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
VasicekYield <- function(r, a, b, s, t){

  p <- VasicekPrice(r, a, b, s, t)

  -log(p)/t

}

#' Bond Price under CIR Model
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
CIRPrice <- function(r, a, b, s, t){
  # returns the zero bond price that pays $1 at time t under a 1-factor CIR model
  # see Hull p.685

  y <- sqrt(a^2 + 2 * s^2)

  # B term
  temp1 <- (2 * (exp(y * t) - 1))
  temp2 <- (y + a) * (exp(y * t) - 1) + 2 * y
  B <- temp1 / temp2

  temp3 <- 2 * y * exp(0.5 * (a + y) * t)
  A <- (temp3 / temp2) ^ (2 * a * b * s ^-0.5)

  # Bond Price
  P <- A * exp(-B * r)

  return(P)
}

#' Bond Yield under CIR Model
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
CIRYield <- function(r, a, b, s, t){

  p <- CIRPrice(r, a, b, s, t)

  -log(p)/t

}

#' Bond Price using 2 factor Vasicek Model
#'
#' @param r1
#' @param r2
#' @param a1
#' @param a2
#' @param s1
#' @param s2
#' @param u
#' @param t
#'
#' @return
#' @export
#'
#' @examples
Vas2FBondPrice <- function(r1, r2, a1, a2, s1, s2, u, t){
  # returns the zero bond price under a 2-factor Vasicek yield curve with a known
  # see hibbert, mowbray, and turnbull p.18

  exp(.Vas2FA(a1, a2, s1, s2, u, t) - .Vas2FB1(a1, t) * r1 - .Vas2FB2(a1, a2, t) * r2)

}

#' Bond Yield using 2 factor Vasicek Model
#'
#' @param r1
#' @param r2
#' @param a1
#' @param a2
#' @param s1
#' @param s2
#' @param u
#' @param t
#'
#' @return
#' @export
#'
#' @examples
Vas2FBondYield <- function(r1, r2, a1, a2, s1, s2, u, t){

  p <- Vas2FBondPrice(r1, r2, a1, a2, s1, s2, u, t)

  -log(p)/t

}

#' Calculate A parameter of 2 factor Vasicek Model
#'
#' @param a1
#' @param a2
#' @param s1
#' @param s2
#' @param u
#' @param s
#' @keywords internal
.Vas2FA <- function(a1, a2, s1, s2, u, s){
  # see hibbert, mowbray, and turnbull p.18
  # computes the function A in the bond pricing equation
  temp1 <- (.Vas2FB1(a1, s) - s) * (u - (s1 * s1) / (2 * a1 * a1))
  temp2 <- .Vas2FB2(a1, a2, s) * u - (s1 * s1 * .Vas2FB1(a1, s) * .Vas2FB1(a1, s)) / (4 * a1)
  temp3 <- (s / (a2 * a2) - 2 * (.Vas2FB2(a1, a2, s) + .Vas2FB1(a1, s)) / (a2 * a2))
  temp4 <- 1 / ((a1 - a2) * (a1 - a2)) * (1 - exp(-2 * a1 * s)) / (2 * a1)
  temp5 <- -2 * a1 / (a2 * (a1 - a2) * (a1 - a2)) * (1 - exp(-(a1 + a2) * s)) / (a1 + a2)
  temp6 <- a1 * a1 / (a2 * a2 * (a1 - a2) * (a1 - a2)) * (1 - exp(-2 * a2 * s)) / (2 * a2)

  ret <- temp1 + temp2 + (s2 * s2 / 2) * (temp3 + temp4 + temp5 + temp6)

  return(ret)

}

#' Calculate B1 parameter of 2 factor Vasicek Model
#'
#' @param a1
#' @param a2
#' @param s1
#' @param s2
#' @param u
#' @param s
#' @keywords internal
.Vas2FB1 <- function(a, s){
  # see hibbert, mowbray, and turnbull p.18

  (1 - exp(-1 * a * s)) / a

}

#' Calculate B2 parameter of 2 factor Vasicek Model
#'
#' @param a1
#' @param a2
#' @param s1
#' @param s2
#' @param u
#' @param s
#' @keywords internal
.Vas2FB2 <- function(a1, a2, s){
  # see hibbert, mowbray, and turnbull p.18

  temp1 <- (1 - exp(-1 * a2 * s)) / a2 - (1 - exp(-1 * a1 * s)) / a1

  a1 / (a1 - a2) * temp1


}


