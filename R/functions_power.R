#' Calculate power of a SMD
#'
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param true_d SMD of the largest study
#' @param se standard error of the SMD
#'
#' @noRd
.power_d = function (n_cases, n_controls, true_d, se = NULL) {

   se = ifelse(is.null(se), sqrt(1/n_cases + 1/n_controls), se)

   t = true_d / se

   cv = qt(1 - 0.05/2, n_cases + n_controls - 2)

   power = suppressWarnings(1 - pt(cv, n_cases + n_controls - 2, t) + pt(-cv, n_cases + n_controls - 2, t))

   power_cor = ifelse(power > 1, 1, power)

   return(power_cor)
}

#' Calculate power of a Hazard ratio
#'
#' @param xi dataset
#' @param hr HR of the largest study
#'
#' @noRd
.power_hr = function (xi, hr, seed) {
  study_p.value = xi$p.value
  n_cases = xi$n_cases
  study_hr = exp(xi$value)
  k = optimize(function (k) {
    (powerSurvEpi::powerCT.default0(k, n_cases, study_hr, study_p.value) - 0.5)^2
  }, 0.01:100)$minimum
  return(powerSurvEpi::powerCT.default0(k, n_cases, hr))
}

#' Calculate power of incidence risk ratio
#'
#' @param xi dataset
#' @param irr incidence rate ratio of the largest study
#'
#' @noRd
.power_irr = function (xi, irr, seed) {
  time_exp = xi$time_exp
  time_nexp = xi$time_nexp
  ir_exp = xi$n_cases_exp / time_exp
  ir_nexp = xi$n_cases_nexp / time_nexp
  ir_nexp_estimated_from_exp = ir_exp / irr
  irhat_nexp = weighted.mean(
    c(ir_nexp, ir_nexp_estimated_from_exp),
    c(time_nexp, time_exp)
  )
  irhat_exp = irr * irhat_nexp
  tmp = .estimate_irr_from_n(
    rpois(10000, irhat_exp * time_exp), time_exp,
    rpois(10000, irhat_nexp * time_nexp), time_nexp
  )
  return(mean(.two_tail(pnorm(log(tmp$value) / tmp$se)) < 0.05, na.rm = TRUE))
}
#' Calculate power of an odds ratio
#'
#' @param xi dataset
#' @param or OR of the largest study
#'
#' @noRd
.power_or = function (xi, or, seed) {
  n_cases_exp = xi$n_cases_exp
  n_cases_nexp = xi$n_cases_nexp
  n_controls_exp = xi$n_controls_exp
  n_controls_nexp = xi$n_controls_nexp
  n_cases = n_cases_exp + n_cases_nexp
  n_controls = n_controls_exp + n_controls_nexp

  # These corrections are only used in the estimation of ohat_cases
  # and ohat_controls, as otherwise some odds could be 0 or infinity.
  if (n_cases_exp == 0 || n_cases_nexp == 0 || n_controls_exp == 0 || n_controls_nexp == 0) {
    n_cases_exp = n_cases_exp + 0.5
    n_cases_nexp = n_cases_nexp + 0.5
    n_controls_exp = n_controls_exp + 0.5
    n_controls_nexp = n_controls_nexp + 0.5
  }
  o_cases = n_cases_exp / n_cases_nexp
  o_controls = n_controls_exp / n_controls_nexp
  o_controls_estimated_from_cases = o_cases / or
  ohat_controls = weighted.mean(
    c(o_controls, o_controls_estimated_from_cases),
    # We use (n_controls_exp + n_controls_nexp) and (n_cases_exp + n_cases_nexp)
    # because they may include the +0.5 correction, which is not included
    # in n_controls and n_cases because they must be the original numbers
    # in the rbinom below
    c(n_controls_exp + n_controls_nexp, n_cases_exp + n_cases_nexp)
  )
  ohat_cases = or * ohat_controls

  n_cases_exp = rbinom(10000, round(n_cases), .odds_to_p(ohat_cases))
  n_controls_exp = rbinom(10000, round(n_controls), .odds_to_p(ohat_controls))
  tmp = .estimate_or_from_n(n_cases_exp, n_cases - n_cases_exp, n_controls_exp, n_controls - n_controls_exp)
  return(mean(.two_tail(pnorm(log(tmp$value) / tmp$se)) < 0.05, na.rm = TRUE))
}
#' Calculate power of a relative risk
#'
#' @param xi dataset
#' @param rr RR of the largest study
#'
#' @noRd
.power_rr = function (xi, rr) {
  n_exp = xi$n_exp
  n_nexp = xi$n_nexp
  ci_exp = xi$n_cases_exp / n_exp
  ci_nexp = xi$n_cases_nexp / n_nexp
  ci_nexp_estimated_from_exp = ci_exp / rr
  phat_cases_in_nexp = weighted.mean(
    c(ci_nexp, ci_nexp_estimated_from_exp),
    c(n_nexp, n_exp)
  )
  phat_cases_in_exp = rr * phat_cases_in_nexp

  phat_cases_in_nexp = ifelse(phat_cases_in_nexp > 1 , 1, phat_cases_in_nexp)
  phat_cases_in_exp = ifelse(phat_cases_in_exp > 1 , 1, phat_cases_in_exp)

  tmp = .estimate_rr_from_n(
    rbinom(10000, round(n_exp), phat_cases_in_exp), rep(n_exp, 10000),
    rbinom(10000, round(n_nexp), phat_cases_in_nexp), rep(n_nexp, 10000)
  )
  return(mean(.two_tail(pnorm(log(tmp$value) / tmp$se)) < 0.05, na.rm = TRUE))
}
