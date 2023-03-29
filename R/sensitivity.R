#' Sensitivity fixed to a given value
#'
#' @param sensitivity The value to fix the sensitivity to.
#' @param n_pos Not used, included for compatibility with other sensitivities only.
#' @param n_intermittent_neg Not used, included for compatibility with other sensitivities only.
#' @export
fixed_sensitivity = function(sensitivity = 1, n_pos = 0, n_intermittent_neg = 0) {
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
#' @param n_pos Number of positive tests observed
#' @param n_intermittent_neg Number of intermittent negative tests observed
#' @export
infer_sensitivity = function(prior = "beta(1.5, 5)", n_pos = 0, n_intermittent_neg = 0) {
  list(
    data_args = list(
      n_pos = n_pos,
      n_intermittent_neg = n_intermittent_neg
    ),
    parameters = list("real<lower=0,upper=1> sensitivity;"),
    model = list(
      paste0("sensitivity ~ ", prior, ";"),
      glue::glue("target += {n_pos} * log(sensitivity) + {n_intermittent_neg} * log1m(sensitivity);")
    )
  )
}
