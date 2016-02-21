#' A cube-root transformation for scales, e.g., color gradients.
#'
#' @export
cubroot_trans <- function(){
  trans_new('cubroot', transform = function(x) x^(1/3), inverse = function(x) x^3)
}

#' A signed square-root transformation for scales, e.g., color gradients.
#'
#' @export
sqrt_sign_trans <- function(){
  trans_new('sqrt_sign', transform = function(x) sign(x) * sqrt(abs(x)), inverse = function(x) sign(x) * x^2)
}

#' A "not in" operator.
#'
#' @export
`%ni%` <- Negate(`%in%`)