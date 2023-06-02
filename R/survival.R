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

#' @export
surv_prior_RW1_sigma_fixed = function(lambda1_alpha = 0.1, lambda1_beta = 1.9, sigma = 0.1) {
  list(
    data_args = list(sigma_steps = sigma, lambda1_alpha = lambda1_alpha, lambda1_beta = lambda1_beta),
    data_code = list(
      "real<lower=0> sigma_steps;",
      "real<lower=0> lambda1_alpha;",
      "real<lower=0> lambda1_beta;"
    ),
    parameters = list(
      "vector[max_S-2] z_logit_lambda_steps;",
      "real<lower=0, upper=1> lambda1;"
    ),
    transformed_parameters_declare = list(
      "vector [max_S-1] lambda;",
      "vector[max_S] S;"
    ),
    transformed_parameters_code = c(
      list(
        "lambda[1] = lambda1;",
        "lambda[2:max_S-1] = inv_logit(cumulative_sum(sigma_steps * z_logit_lambda_steps) + logit(lambda1));"
      ),
      base_survival[["transformed_parameters_code"]]
    ),
    model = list(
      "lambda1 ~ beta(lambda1_alpha, lambda1_beta);",
      "z_logit_lambda_steps ~ std_normal();"
    )
  )
}

#' @export
surv_prior_RW2_sigma = function(
    prior,
    logit_lambda1_mean = -17.5,
    logit_lambda1_sd = 6,
    prior_gradient_mean = 1.09,
    prior_gradient_sd = 0.03
  ) {
  list(
    parameters = list(
      "vector[max_S-3] z_logit_lambda_gradient;",
      "real logit_lambda1;",
      "real gradient1;",
      "real<lower=0> sigma;"
    ),
    transformed_parameters_declare = list(
      "vector[max_S-3] gradients;",
      "vector[max_S-1] lambda;",
      "vector[max_S] S;"
    ),
    transformed_parameters_code = c(
      list(
        "gradients = inv_logit(cumulative_sum(sigma * z_logit_lambda_gradient) + gradient1);",
        "lambda[1] = inv_logit(logit_lambda1);",
        "lambda[2] = inv_logit(logit_lambda1 + gradient1);",
        "lambda[3:max_S-1] = inv_logit(logit_lambda1 + gradient1 + cumulative_sum(gradients));"
      ),
      base_survival[["transformed_parameters_code"]]
    ),
    model = list(
      glue::glue("logit_lambda1 ~ normal({logit_lambda1_mean}, {logit_lambda1_sd});"),
      "z_logit_lambda_gradient ~ std_normal();",
      glue::glue("gradient1 ~ normal({prior_gradient_mean}, {prior_gradient_sd});"),
      glue::glue("sigma ~ {prior};")
    )
  )
}

#' @export
surv_prior_RW2_sigma_fixed = function(
    logit_lambda1_mean = -17.5,
    logit_lambda1_sd = 6,
    prior_gradient_mean = 1.09,
    prior_gradient_sd = 0.03,
    sigma = 0.1
  ) {
  list(
    parameters = list(
      "vector[max_S-3] z_logit_lambda_gradient;",
      "real logit_lambda1;",
      "real gradient1;"
    ),
    transformed_parameters_declare = list(
      "vector[max_S-3] gradients;",
      "vector[max_S-1] lambda;",
      "vector[max_S] S;"
    ),
    transformed_parameters_code = c(
      list(
        glue::glue(
          "gradients = inv_logit(cumulative_sum({sigma} * z_logit_lambda_gradient) + gradient1);"
        ),
        "lambda[1] = inv_logit(logit_lambda1);",
        "lambda[2] = inv_logit(logit_lambda1 + gradient1);",
        "lambda[3:max_S-1] = inv_logit(logit_lambda1 + gradient1 + cumulative_sum(gradients));"
      ),
      base_survival[["transformed_parameters_code"]]
    ),
    model = list(
      glue::glue("logit_lambda1 ~ normal({logit_lambda1_mean}, {logit_lambda1_sd});"),
      "z_logit_lambda_gradient ~ std_normal();",
      glue::glue("gradient1 ~ normal({prior_gradient_mean}, {prior_gradient_sd});")
    )
  )
}
