#' Calculate meta-analysis for d value
#'
#' @param x a well formatted dataset
#' @param method.var the method used
#'
#' @noRd
.meta_d = function (x, method.var) {

  n_cases = x$n_cases
  mean_cases = x$mean_cases
  sd_cases = x$sd_cases
  n_controls = x$n_controls
  mean_controls = x$mean_controls
  sd_controls = x$sd_controls

  if (method.var == "hksj") {
     m = meta::metacont(n.e = n_cases, mean.e = mean_cases, sd.e = sd_cases,
                         n.c = n_controls, mean.c = mean_controls, sd.c = sd_controls,
                         sm = "SMD", method.smd = "Hedges", method.tau = "DL")
     m = .hksj_meta(m)
  } else {
     m = meta::metacont(n.e = n_cases, mean.e = mean_cases, sd.e = sd_cases,
                         n.c = n_controls, mean.c = mean_controls, sd.c = sd_controls,
                         sm = "SMD", method.smd = "Hedges", method.tau = method.var)
  }

  return(m)
}

#' Calculate meta-analysis by calling metagen correecting for bias corrected SMD
#'
#' @param x a well formatted dataset
#' @param method.var the method used
#'
#' @noRd
.meta_gen_smd = function (x, method.var) {

  # value = .estimate_g_from_d(d = x$value, n_cases = x$n_cases, n_controls = x$n_controls, se = x$se)$value
  # se = .estimate_g_from_d(d = x$value, n_cases = x$n_cases, n_controls = x$n_controls, se = x$se)$se

  value = x$value
  se = x$se

  if (method.var == "hksj") {
    m = meta::metagen(value, se, rownames(x), method.tau = "DL")
    m = .hksj_meta(m)
  } else {
    m = meta::metagen(value, se, rownames(x), method.tau = method.var)
  }

  return(m)
}

#' Calculate meta-analysis by calling metagen on bias corrected SMD
#'
#' @param x a well formatted dataset
#' @param method.var the method used
#'
#' @noRd
.meta_gen_g = function (x, method.var) {

  if (method.var == "hksj") {
    m = meta::metagen(x$value, x$se, rownames(x), method.tau = "DL")
    m = .hksj_meta(m)
  } else {
    m = meta::metagen(x$value, x$se, rownames(x), method.tau = method.var)
  }

  return(m)
}

#' Calculate meta-analysis by calling metagen with the logarithm of the value
#'
#' @param x a well formatted dataset
#' @param method.var the method used
#'
#' @noRd
.meta_gen_log = function (x, method.var) {
  x$value = log(x$value)

  if (method.var == "hksj") {
    m = .hksj_meta(meta::metagen(x$value, x$se, rownames(x), method.tau = "DL"))
  } else {
    m = meta::metagen(x$value, x$se, rownames(x), method.tau = method.var)
  }
  return(m)
}

#' Calculate meta-analysis for incidence rate ratio
#'
#' @param x a well formatted dataset
#' @param method.var the method used
#'
#' @noRd
.meta_irr = function (x, method.var) {

  if (method.var == "hksj") {
    m = meta::metainc(x$n_cases_exp, x$time_exp, x$n_cases_nexp, x$time_nexp,
                      rownames(x), sm = "IRR", method.tau = "DL")
    m = .hksj_meta(m)
  } else {
    m = meta::metainc(x$n_cases_exp, x$time_exp, x$n_cases_nexp, x$time_nexp,
                      rownames(x), sm = "IRR", method.tau = method.var)
  }
  return(m)
}
#' Calculate meta-analysis for odds ratio measures. Calls metabin
#'
#' @param x a well formatted dataset
#' @param method.var the method used
#'
#' @noRd
.meta_or = function (x, method.var) {

  if (method.var == "hksj") {
    m = meta::metabin(x$n_cases_exp, x$n_cases_exp + x$n_controls_exp,
                      x$n_cases_nexp, x$n_cases_nexp + x$n_controls_nexp,
                      rownames(x), sm = "OR", method.tau = "DL")
    m = .hksj_meta(m)
  } else {
    m = meta::metabin(x$n_cases_exp, x$n_cases_exp + x$n_controls_exp,
                      x$n_cases_nexp, x$n_cases_nexp + x$n_controls_nexp,
                      rownames(x), sm = "OR", method.tau = method.var)
  }
  return(m)
}

#' Calculate metaanalysis of relative risk.
#'
#' @param x a well formatted dataset
#' @param method.var the method used
#'
#' @noRd
.meta_rr = function (x, method.var) {

  if (method.var == "hksj") {
    m = meta::metabin(x$n_cases_exp, x$n_exp,
                      x$n_cases_nexp, x$n_nexp,
                      rownames(x), sm = "RR", method.tau = "DL")
    m = .hksj_meta(m)
  } else {
    m = meta::metabin(x$n_cases_exp, x$n_exp,
                      x$n_cases_nexp, x$n_nexp,
                      rownames(x), sm = "RR", method.tau = method.var)
  }

  return(m)
}

#' Calculate the Hartung-knapp-Sidik-Jonkman method for random effects meta-analysis for a meta object
#'
#' @param m a meta object
#'
#' @noRd
.hksj_meta = function (m) {
  k = m$k
  if (k == 1) {
    m$seTE.random = NA
    m$pval.random = NA
    m$lower.random = NA
    m$upper.random = NA
  } else {
    coef = m$TE.random
    df = k - 1
    v = weighted.mean((m$TE - coef)^2, m$w.random) / df
    se = sqrt(v)
    m$seTE.random = se
    m$pval.random = .two_tail(pt(coef / se, df))
    m$lower.random = coef + se * qt(0.025, df)
    m$upper.random = coef + se * qt(0.975, df)
  }
  return(m)
}

