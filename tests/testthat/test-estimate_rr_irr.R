test_that(".estimate_rr_from_n() correctly estimates Risk Ratio and standard error", {
  df <- df.RR
  df <- df.RR
  df$value <- with(df, .estimate_rr_from_n(n_cases_exp, n_exp,
                                           n_cases_nexp, n_nexp)$value)
  df$se <- with(df, .estimate_rr_from_n(n_cases_exp, n_exp,
                                        n_cases_nexp, n_nexp)$se)


  df <- df[, c("value", "se", "n_cases_exp", "n_cases_nexp", "n_exp","n_nexp")]

  df.RR.mfr <- metafor::escalc(measure = "RR",
                               ai = n_cases_exp, n1i = n_exp,
                               ci = n_cases_nexp, n2i = n_nexp,
                               data = df)

  RR_mfr <- exp(as.numeric(as.character(df.RR.mfr$yi)))
  se_mfr <- sqrt(df.RR.mfr$vi)

  expect_equal(df$value, RR_mfr, tolerance = 1e-15)
  expect_equal(df$se, se_mfr, tolerance = 1e-15)
})

test_that(".estimate_irr_from_n() correctly estimates Incident Risk Ratio and standard error", {
  df <- df.IRR

  df$value <- with(df, .estimate_irr_from_n(n_cases_exp, time_exp,
                                            n_cases_nexp, time_nexp)$value)
  df$se <- with(df, .estimate_irr_from_n(n_cases_exp, time_exp,
                                         n_cases_nexp, time_nexp)$se)



  df <- df[, c("value", "se", "n_cases_exp", "n_cases_nexp", "time_exp","time_nexp")]

  df.IRR.mfr <- metafor::escalc(measure = "IRR",
                                x1i = n_cases_exp, t1i = time_exp,
                                x2i = n_cases_nexp, t2i = time_nexp,
                                data = df)

  IRR_mfr <- exp(as.numeric(as.character(df.IRR.mfr$yi)))
  se_mfr <- sqrt(df.IRR.mfr$vi)

  # tolerance 1e-15 leads to failure
  expect_equal(df$value, IRR_mfr, tolerance = 1e-14)
  expect_equal(df$se, se_mfr, tolerance = 1e-14)
})
