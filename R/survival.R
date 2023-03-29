#' @export
surv_prior_independent = function(prior = "beta(0.1, 1.9)") {
  list(
    parameters = list("real<lower=0, upper=1> lambda[max_S-1]; // Hazard of recovery"),
    transformed_parameters_declare = list("vector[max_S] S;"),
    transformed_parameters_code = list(
      "S[1] = 1;",
      "for (i in 1:max_S-1) {",
      "\tS[i+1] = S[i] * (1 - lambda[i]);",
      "}"
    ),
    model = list(paste0("lambda ~", prior, ";"))
  )
}
