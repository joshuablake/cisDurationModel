#' Sensitivity fixed to a given value
#'
#' @param sensitivity The value to fix the sensitivity to
#' @export
fixed_sensitivity = function(sensitivity = 1) {
  stopifnot(sensitivity >= 0)
  stopifnot(sensitivity <= 1)
  list(
    data_args = list(sensitivity = sensitivity),
    data_code = list("real<lower=0,upper=1> sensitivity;")
  )
}

#' Sensitivity fixed to a given value
#'
#' @param prior The prior for the sensitivity
#' @export
infer_sensitivity = function(prior = "beta(1.5, 5)") {
  list(
    parameters = list("real<lower=0,upper=1> sensitivity;"),
    model = list(paste0("sensitivity ~ ", prior, ";"))
  )
}
