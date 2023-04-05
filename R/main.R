#' Currently unused function to read a global file of Stan functions
#'
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

#' Perform inference
#'
#' @param pa_model Model for the p_{ia}s
#' @param pt_model Model for the p_{it}s
#' @param survival_prior What prior to place on the survival/hazard
#' @param sensitivity_model How to model the sensitivity over time
#' @param stan_args A list of extra arguments to pass to `rstan::stan`
#' @param run_stan Whether to run Stan or return a list of arguments
#' @returns if run_stan is `FALSE`, a list of arguments ready to pass to `rstan::stan`; otherwise, the return value from said call
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
    survival_prior,
    sensitivity_model,
    pa_model,
    pt_model
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
      data = purrr::map(components, "data_args") |>
        unlist(recursive = FALSE)
    )
  )

  if (run_stan) {
    return(do.call(rstan::stan, args))
  } else {
    return(args)
  }
}

# Utility function written by ChatGPT
merge_lists = function(list1, list2) {
  # Get all unique names
  all_names <- unique(c(names(list1), names(list2)))

  # Merge lists and concatenate elements with the same name
  merged_list <- lapply(all_names, function(name) {
    el1 <- if (name %in% names(list1)) list1[[name]] else NULL
    el2 <- if (name %in% names(list2)) list2[[name]] else NULL
    c(el1, el2)
  })

  # Assign names to the merged list
  names(merged_list) <- all_names

  return(merged_list)
}
