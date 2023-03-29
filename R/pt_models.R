#' Likelihood contribution for the individual model for p_t
#'
#' @param tS Matrix of size (n_a x max_S) where element (i, j) is the contribution of S_j for individual i
#' @param mu_ni Prior mean for the negative binomial prior on n_i
#' @param r_ni Prior size for the negative binomial prior on n_i. Set to zero for an improper prior proportion to 1/n_i
#' @export
pt_individual = function(tS, mu_ni = 1, r_ni = 0) {
  list(
    data_args = list(tS = tS),
    data_code = list(
      data = list("matrix[n_a, max_S] tS;")
    ),
    model = list(
      glue::glue("target += -({r_ni} + 1) * sum(log({mu_ni} * tS * S + {r_ni}));")
    )
  )
}
