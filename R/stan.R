#' @export
get_util_stan_code = function() {
  # system.file("stan", "functions.stan", package = packageName()) |>
  #   readLines() |>
  #   paste(collapse = "\n")
  return("")
}

extract_component = function(components, component_name, indent = "\t") {
  extracted = purrr::map(components, component_name) |>
    unlist() |>
    purrr::keep(~!is.null(.x))
  paste(
    indent,
    extracted
  ) |>
    paste(collapse = "\n")
}

#' @export
infer_duration = function(
    pa_model,
    pt_model,
    survival_prior = surv_prior_independent(),
    sensitivity_model = fixed_sensitivity(1),
    stan_args = list(),
    run_stan = FALSE
) {
  components = list(
    pa_model,
    pt_model,
    survival_prior,
    sensitivity_model
  )

  code = paste(
    get_util_stan_code(),
    "data {",
      extract_component(components, "data_code"),
    "}",
    "transformed data {",
      extract_component(components, "transformed_data_declare"),
      extract_component(components, "transformed_data_code"),
    "}",
    "parameters {",
      extract_component(components, "parameters"),
    "}",
    "transformed parameters {",
      extract_component(components, "transformed_parameters_declare"),
      extract_component(components, "transformed_parameters_code"),
    "}",
    "model {",
      extract_component(components, "model"),
    "}",
    sep = "\n"
  )

  args = c(
    stan_args,
    list(
      model_code = code,
      data = purrr::map(components, "data_args")
    )
  )

  if (run_stan) {
    return(do.call(rstan::stan, args))
  } else {
    return(args)
  }
}
