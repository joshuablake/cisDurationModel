test_that("component extraction", {
  components = list(
    base = list(
      data_args = list(
        l_b = 1,
        r_b = 2
      ),
      data_code = list(
        "int<lower=0> N; // Number of episodes"
      )
    ),
    survival_prior = list(
      data_code = list(
        "int max_S;"
      )
    )
  )
  expect_equal(extract_component(components, "data_code"), "\t int<lower=0> N; // Number of episodes\n\t int max_S;")
})

test_that("utils basic characteristics", {
  util_code = get_util_stan_code()
  expect_type(util_code, "character")
  expect_length(util_code, 1)
})

check_args_run = function(args) {
  args_full = c(args, iter = 5, chains = 1, refresh = 0, cores = 1, warmup = 2)
  result = suppressWarnings(do.call(rstan::stan, args_full))
  S = rstan::extract(result, "S[1]")[[1]]
  expect_length(S, 3)
}

test_that("basic individual model compiles", {
  infer_duration(
      pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
      pt_model = pt_individual(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4)))
  ) |>
    check_args_run()
})

test_that("basic total model compiles", {
  infer_duration(
      pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
      pt_model = pt_total(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4)), mu_n = 4, r_n = 1)
    ) |>
      check_args_run()
})

test_that("inferring sensitivity compiles", {
  infer_duration(
      pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
      pt_model = pt_individual(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4))),
      sensitivity_model = infer_sensitivity(n_pos = 2, n_intermittent_neg = 3)
    ) |>
      check_args_run()
})

test_that("hiearchical prior compiles", {
  infer_duration(
    pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
    pt_model = pt_total(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4)), mu_n = 4, r_n = 1),
    survival_prior = surv_prior_informative_hiearchy(c(0, 1), matrix(c(2, 0.5, 0.5, 2), nrow = 2, ncol = 2), c(1, 1))
  ) |>
    check_args_run()
})

test_that("RW1 prior compiles", {
  infer_duration(
    pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
    pt_model = pt_total(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4)), mu_n = 4, r_n = 1),
    survival_prior = surv_prior_RW1_sigma_fixed()
  ) |>
    check_args_run()
})

test_that("RW2 prior compiles", {
  infer_duration(
    pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
    pt_model = pt_total(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4)), mu_n = 4, r_n = 1),
    survival_prior = surv_prior_RW2_sigma_fixed()
  ) |>
    check_args_run()
})

test_that("RW2 prior with varying sigma compiles", {
  infer_duration(
    pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
    pt_model = pt_total(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4)), mu_n = 4, r_n = 1),
    survival_prior = surv_prior_RW2_sigma("exponential(10)")
  ) |>
    check_args_run()
})

test_that("Two tS_matrices compiles with total model", {
  infer_duration(
    pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
    pt_model = pt_total(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4), matrix(rep(2, 8), nrow = 2, ncol = 4)), mu_n = 4, r_n = 1),
    survival_prior = surv_prior_independent()
  ) |>
    check_args_run()
})

test_that("Two tS_matrices compiles with individual model", {
  infer_duration(
    pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
    pt_model = pt_individual(tS_matrices = list(matrix(rep(1, 8), nrow = 2, ncol = 4), matrix(rep(2, 8), nrow = 2, ncol = 4)), mu_n = 4, r_n = 1),
    survival_prior = surv_prior_independent()
  ) |>
    check_args_run()
})

