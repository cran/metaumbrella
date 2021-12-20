test_that(".estimate_or_from_n() correctly estimates Odds Ratio and standard error", {

  df <- df.OR
  df$value <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                           n_controls_exp, n_controls_nexp)$value)
  df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                        n_controls_exp, n_controls_nexp)$se)

  df.OR.mfr <- metafor::escalc(measure = "OR",
                               ai = n_cases_exp, bi = n_cases_nexp,
                               ci = n_controls_exp, di = n_controls_nexp,
                               data = df)

  OR_mfr <- exp(as.numeric(as.character(df.OR.mfr$yi)))
  se_mfr <- sqrt(df.OR.mfr$vi)

  expect_equal(df$value, OR_mfr, tolerance = 1e-15)
  expect_equal(df$se, se_mfr, tolerance = 1e-15)
})

test_that(".estimate_se_from_or() correctly estimates standard error from Odds Ratio and sample size in each group", {

  df <- df.OR

  df.OR.mfr <- metafor::escalc(measure = "OR",
                               ai = n_cases_exp, bi = n_cases_nexp,
                               ci = n_controls_exp, di = n_controls_nexp,
                               data = df)

  OR_mfr <- exp(as.numeric(as.character(df.OR.mfr$yi)))
  se_mfr <- sqrt(as.numeric(as.character(df.OR.mfr$vi)))
  se_umb <- or_umb <- NA
  for (i in 1:nrow(df)) {
    se_umb[i] <- .estimate_se_from_or(or = df$value[i],
                                      n_cases = df$n_cases[i],
                                      n_controls = df$n_controls[i])$se
    or_umb[i] <- .estimate_se_from_or(or = df$value[i],
                                      n_cases = df$n_cases[i],
                                      n_controls = df$n_controls[i])$value
}
  expect_equal(or_umb, OR_mfr, tolerance = 1e-15)
  # tol 1e-1 leads to failure
  expect_equal(se_umb, se_mfr, tolerance = 5e-1)
})
