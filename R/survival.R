base_survival = list(
  parameters = list("real<lower=0, upper=1> lambda[max_S-1]; // Hazard of recovery"),
  transformed_parameters_declare = list("vector[max_S] S;"),
  transformed_parameters_code = list(
    "S[1] = 1;",
    "for (i in 1:max_S-1) {",
    "\tS[i+1] = S[i] * (1 - lambda[i]);",
    "}"
  )
)

#' @export
surv_prior_independent = function(prior = "beta(0.1, 1.9)") {
  merge_lists(
    base_survival,
    list(model = list(paste0("lambda ~", prior, ";")))
  )
}

#' @export
surv_prior_informative_hiearchy = function(logit_prior_mean, logit_prior_covar, c,
                                           alpha0 = 0.1, beta0 = 1.9) {
  length_informative_prior = length(logit_prior_mean)
  stopifnot(dim(logit_prior_covar) == c(length_informative_prior, length_informative_prior))
  stopifnot(length(c) == length_informative_prior)
  merge_lists(
    base_survival,
    list(
      data_args = list(
        length_informative_prior = length_informative_prior,
        logit_h_mean = logit_prior_mean,
        logit_h_covar = logit_prior_covar,
        c = c
      ),
      data_code = list(
        "int<lower=0> length_informative_prior;",
        "vector[length_informative_prior] c; // Controls precision of hazard prior",
        "vector[length_informative_prior] logit_h_mean;",
        "cov_matrix[length_informative_prior] logit_h_covar;"
      ),
      transformed_data_declare = list(
        "matrix[length_informative_prior, length_informative_prior] logit_h_covar_decomposed = cholesky_decompose(logit_h_covar);"
      ),
      parameters = list("vector[length_informative_prior] logit_h_raw;"),
      model = list(
        "// Using parameterisation of multivariate normal recommended by Stan manual",
        "// https://mc-stan.org/docs/2_29/stan-users-guide/reparameterization.html",
        "vector[length_informative_prior] h = inv_logit(logit_h_mean + logit_h_covar_decomposed * logit_h_raw);",
        "logit_h_raw ~ std_normal();",
        "// Prior for lambda conditional on h",
        glue::glue("lambda[:length_informative_prior] ~ beta(c .* h + {alpha0}, c .* (1 - h) + {beta0});"),
        glue::glue("lambda[length_informative_prior+1:] ~ beta({alpha0}, {beta0});")
      )
    )
  )
}
