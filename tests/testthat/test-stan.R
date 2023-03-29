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
