#' Estimate Risk Ratio using sample sizes
#'
#' @param n_cases_exp number of cases in the exposed group
#' @param n_exp number of participants in the exposed grouo
#' @param n_cases_nexp number of cases in the exposed group
#' @param n_nexp number of participants in the non-exposed group
#'
#' @noRd
.estimate_rr_from_n = function (n_cases_exp, n_exp, n_cases_nexp, n_nexp) {
  zero = which(n_cases_exp == 0 | n_exp - n_cases_exp == 0 | n_cases_nexp == 0 | n_nexp - n_cases_nexp == 0)
  n_cases_exp[zero] = n_cases_exp[zero] + 0.5
  n_exp[zero] = n_exp[zero] + 1
  n_cases_nexp[zero] = n_cases_nexp[zero] + 0.5
  n_nexp[zero] = n_nexp[zero] + 1
  returned_df = data.frame(
    value = (n_cases_exp / n_exp) / (n_cases_nexp / n_nexp),
    se = sqrt(1 / n_cases_exp - 1 / n_exp + 1 / n_cases_nexp - 1 / n_nexp)
  )
  return(returned_df)
}


#' Estimate Incident Risk Ratio using sample sizes
#'
#' @param n_cases_exp number of cases in the exposed group
#' @param time_exp  person-time of disease-free observation in the exposed group
#' @param n_cases_nexp number of cases in the non-exposed group
#' @param time_nexp person-time of disease-free observation in the non-exposed group
#'
#' @noRd
.estimate_irr_from_n = function (n_cases_exp, time_exp, n_cases_nexp, time_nexp) {
  returned_df = data.frame(
    value = (n_cases_exp / time_exp) / (n_cases_nexp / time_nexp),
    se = sqrt(1 / n_cases_exp + 1 / n_cases_nexp)
  )
  return(returned_df)
}
