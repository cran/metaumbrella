#' Calculate meta-analysis by calling metagen
#'
#' @param x a well formatted dataset
#' @param method.var the method used
#'
#' @noRd
.meta_gen = function (x, method.var) {

  if (method.var == "hksj") {
    m = meta::metagen(x$value, x$se, rownames(x), method.tau = "DL")
    m = .hksj_meta(m)
  } else if (method.var == "FE") {
    m = meta::metagen(x$value, x$se, rownames(x), method.tau = "DL")
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
  } else if (method.var == "FE") {
    m = meta::metagen(x$value, x$se, rownames(x), method.tau = "DL")
  } else {
    m = meta::metagen(x$value, x$se, rownames(x), method.tau = method.var)
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
