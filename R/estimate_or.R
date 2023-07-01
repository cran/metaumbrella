#' Estimate Odds Ratio using sample sizes
#'
#' @param n_cases_exp number of cases in the exposed group
#' @param n_cases_nexp number of cases in the non exposed group
#' @param n_controls_exp number of controls in the exposed group
#' @param n_controls_nexp number of controls in the non exposed group
#'
#' @noRd
.estimate_or_from_n = function (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) {
  zero = which(n_cases_exp == 0 | n_cases_nexp == 0 | n_controls_exp == 0 | n_controls_nexp == 0)
  n_cases_exp[zero] = n_cases_exp[zero] + 0.5
  n_cases_nexp[zero] = n_cases_nexp[zero] + 0.5
  n_controls_exp[zero] = n_controls_exp[zero] + 0.5
  n_controls_nexp[zero] = n_controls_nexp[zero] + 0.5
  returned_df = data.frame(
    value = (n_cases_exp / n_cases_nexp) / (n_controls_exp / n_controls_nexp),
    se = sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp)
  )
  return(returned_df)
}

#' Estimate standard error of Odds ratio using the effect size and sample sizes.
#'
#' @param or OR
#' @param n_cases number of cases
#' @param n_controls number of controls
#'
#' @noRd
.estimate_se_from_or = function (or, n_cases, n_controls) {
  ca_ex = 1:(n_cases - 1)
  ca_ne = n_cases - ca_ex
  co_ex = round(n_controls / (1 + ca_ne * or / ca_ex))
  co_ex[which(co_ex < 1 | co_ex > n_controls - 1)] = NA
  co_ne = n_controls - co_ex
  v_or_mean = mean(1/ca_ex + 1/ca_ne + 1/co_ex + 1/co_ne, na.rm = TRUE)
  returned_df = data.frame(
    value = or,
    var = v_or_mean,
    se = sqrt(v_or_mean)
  )
  return(returned_df)
}


#' Convert a Cohen's d to an Odds Ratio
#'
#' @param d SMD value
#'
#' @noRd
.d_to_or = function (d) {
  return(exp(d * pi / sqrt(3)))
}

#' Convert an odds ratio to a p-value
#'
#' @param o OR value
#'
#' @noRd
.odds_to_p = function (o) {
  return(o / (1 + o))
}
