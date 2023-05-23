#' Likelihood contribution for the individual model for p_t
#'
#' @param tS_matrices Matrix/ces of size (n_a x max_S) as returned by form_tS_matrices
#' @param mu_ni Prior mean for the negative binomial prior on n_i
#' @param r_ni Prior size for the negative binomial prior on n_i. Set to zero for an improper prior proportion to 1/n_i
#' @export
pt_individual = function(tS_matrices, mu_ni = 1, r_ni = 0) {
  stopifnot(length(tS_matrices) <= 2)
  if (length(tS_matrices) == 1) {
    result = list(
      data_args = list(tS = tS_matrices[[1]]),
      data_code = list(
        data = list("matrix[n_a, max_S] tS;")
      ),
      transformed_parameters_code = list(
        "one_minus_pt = sensitivity * tS * S;"
      )
    )
  } else {
    result = list(
      data_args = list(
        tS1 = tS_matrices[[1]],
        tS2 = tS_matrices[[2]]
      ),
      data_code = list(
        data = list(
          "matrix[n_a, max_S] tS1;",
          "matrix[n_a, max_S] tS2;"
        )
      ),
      transformed_parameters_code = list(
        "one_minus_pt = sensitivity * tS1 * S + (1 - sensitivity) * tS2 * S;"
      )
    )
  }

  result$transformed_parameters_declare = list("vector[n_a] one_minus_pt;")
  result$model = list(
    glue::glue("target += -({r_ni} + 1) * sum(log({mu_ni} * one_minus_pt + {r_ni}));")
  )

  return(result)
}

#' Likelihood contribution for the total model for p_t
#'
#' @param tS_matrices Matrix/ces of size (n_a x max_S) as returned by form_tS_matrices
#' @param mu_n Prior mean for the negative binomial prior on n.
#' @param r_n Prior size for the negative binomial prior on n.
#' @export
pt_total = function(tS_matrices, mu_n = 1, r_n = 0) {
  stopifnot(length(tS_matrices) <= 2)
  if (length(tS_matrices) == 1) {
    result = list(
      data_args = list(tS = colMeans(tS_matrices[[1]])),
      data_code = list(
        data = list("row_vector[max_S] tS;")
      ),
      transformed_parameters_code = list(
        "one_minus_pt = sensitivity * tS * S;"
      )
    )
  } else {
    result = list(
      data_args = list(
        tS1 = colMeans(tS_matrices[[1]]),
        tS2 = colMeans(tS_matrices[[2]])
      ),
      data_code = list(
        data = list(
          "row_vector[max_S] tS1;",
          "row_vector[max_S] tS2;"
        )
      ),
      transformed_parameters_code = list(
        "one_minus_pt = sensitivity * tS1 * S + (1 - sensitivity) * tS2 * S;"
      )
    )
  }
  result$transformed_parameters_declare = list("real one_minus_pt;")
  result$model = list(
    glue::glue("target += -lmultiply({r_n} + n_a, {r_n} + {mu_n} * one_minus_pt);")
  )
  return(result)
}
