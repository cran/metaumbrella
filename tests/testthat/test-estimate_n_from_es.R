tol_large = 1e-10
tol_med = 1e-6
tol_ok = 1e-3
tol_limit = 1e-2
tol_obs = 1e-1
tol_dif = 5e-1
tol_force = 1

test_that(".estimate_n_from_irr correctly converts sample size", {
  skip_on_cran()

  irr <- se <- irr_conv <- se_conv <-
    n_cases_exp_conv <- n_cases_nexp_conv <- time_exp_conv <- time_nexp_conv <- NA

  df <- df.IRR

  df$value <- with(df, .estimate_irr_from_n(n_cases_exp, time_exp,
                                                n_cases_nexp, time_nexp)$value)
  df$se <- with(df, .estimate_irr_from_n(n_cases_exp, time_exp,
                                             n_cases_nexp, time_nexp)$se)


  # convert n from irr
  for (i in 1:nrow(df)) {
    n_cases_exp_conv[i] <- .estimate_n_from_irr(irr = df$value[i], var = df$se[i]^2,
                                          n_cases = df$n_cases[i])$n_cases_exp
    n_cases_nexp_conv[i] <- .estimate_n_from_irr(irr = df$value[i], var = df$se[i]^2,
                                           n_cases = df$n_cases[i])$n_cases_nexp
    time_exp_conv[i] <- .estimate_n_from_irr(irr = df$value[i], var = df$se[i]^2,
                                          n_cases = df$n_cases[i])$time_exp
    time_nexp_conv[i] <- .estimate_n_from_irr(irr = df$value[i], var = df$se[i]^2,
                                            n_cases = df$n_cases[i])$time_nexp
  }

  # re estimate value from converted n
  for (i in 1:nrow(df)) {
    irr_conv[i] <- .estimate_irr_from_n(n_cases_exp_conv[i], time_exp_conv[i],
                                        n_cases_nexp_conv[i], time_nexp_conv[i])$value
    se_conv[i] <- .estimate_irr_from_n(n_cases_exp_conv[i], time_exp_conv[i],
                                       n_cases_nexp_conv[i], time_nexp_conv[i])$se
  }
  expect_equal(df$value, irr_conv, tolerance = tol_large)
  expect_equal(df$se, se_conv, tolerance = tol_med)
})

test_that(".estimate_n_from_or_and_n_cases correctly converts sample size", {
  skip_on_cran()


    or <- se <- or_conv <- se_conv <-
      n_cases_exp_conv <- n_cases_nexp_conv <-
      n_controls_exp_conv <- n_controls_nexp_conv <- NA

    df <- df.OR
    df$value <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                                      n_controls_exp, n_controls_nexp)$value)
    df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                                 n_controls_exp, n_controls_nexp)$se)


  for (i in 1:nrow(df)) {
    n_transit_i <- NA
    n_transit_i <- .estimate_n_from_or_and_n_cases(or = df$value[i],
                                                   var = df$se[i]^2,
                                                   n_cases = df$n_cases[i],
                                                   n_controls = df$n_controls[i])

    n_cases_exp_conv[i] = n_transit_i$n_cases_exp
    n_cases_nexp_conv[i] = n_transit_i$n_cases_nexp
    n_controls_exp_conv[i] = n_transit_i$n_controls_exp
    n_controls_nexp_conv[i] = n_transit_i$n_controls_nexp
  }

  # re estimate n from converted n
  for (i in 1:nrow(df)) {
    or_transit_i <- .estimate_or_from_n(n_cases_exp = n_cases_exp_conv[i],
                                        n_cases_nexp = n_cases_nexp_conv[i],
                                        n_controls_exp = n_controls_exp_conv[i],
                                        n_controls_nexp = n_controls_nexp_conv[i])
    or_conv[i] <- or_transit_i$value
    se_conv[i] <- or_transit_i$se
  }

  expect_equal(df$n_cases_exp, n_cases_exp_conv, tolerance = tol_large)
  expect_equal(df$n_cases_nexp, n_cases_nexp_conv, tolerance = tol_large)
  expect_equal(df$n_controls_exp, n_controls_exp_conv, tolerance = tol_large)
  expect_equal(df$n_controls_nexp, n_controls_nexp_conv, tolerance = tol_large)
  expect_equal(df$value, or_conv, tolerance = tol_large)
  expect_equal(df$se, se_conv, tolerance = tol_large)
})

test_that(".estimate_n_from_or_and_n_exp correctly converts sample size", {

  skip_on_cran()

  or <- se <- or_conv <- se_conv <-
    n_cases_exp_conv <- n_cases_nexp_conv <-
    n_controls_exp_conv <- n_controls_nexp_conv <- NA

  df <- df.OR
  df$value <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                           n_controls_exp, n_controls_nexp)$value)
  df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                        n_controls_exp, n_controls_nexp)$se)


  for (i in 1:nrow(df)) {
    n_transit_i <- NA
    n_transit_i <- .estimate_n_from_or_and_n_exp(or = df$value[i],
                                                 var = df$se[i]^2,
                                                 n_exp = df$n_exp[i],
                                                 n_nexp = df$n_nexp[i])
    n_cases_exp_conv[i] = n_transit_i$n_cases_exp
    n_cases_nexp_conv[i] = n_transit_i$n_cases_nexp
    n_controls_exp_conv[i] = n_transit_i$n_controls_exp
    n_controls_nexp_conv[i] = n_transit_i$n_controls_nexp
  }

  # re estimate n from converted n
  for (i in 1:nrow(df)) {
    or_transit_i <- .estimate_or_from_n(n_cases_exp = n_cases_exp_conv[i],
                                        n_cases_nexp = n_cases_nexp_conv[i],
                                        n_controls_exp = n_controls_exp_conv[i],
                                        n_controls_nexp = n_controls_nexp_conv[i])
    or_conv[i] <- or_transit_i$value
    se_conv[i] <- or_transit_i$se
  }

  expect_equal(df$value, or_conv, tolerance = tol_limit)
  expect_equal(df$se, se_conv, tolerance = tol_large)
})

test_that(".estimate_n_from_rr correctly converts sample size", {

  skip_on_cran()

  rr <- se <- rr_conv <- se_conv <-
    n_cases_exp_conv <- n_cases_nexp_conv <-
    n_exp_conv <- n_nexp_conv <- NA

  df <- df.RR
  df$value <- with(df, .estimate_rr_from_n(n_cases_exp, n_exp,
                                           n_cases_nexp, n_nexp)$value)
  df$se <- with(df, .estimate_rr_from_n(n_cases_exp, n_exp,
                                        n_cases_nexp, n_nexp)$se)



    df.RR$n_cases <- df.RR$n_cases_exp + df.RR$n_cases_nexp
    df.RR$n_controls <- df.RR$n_exp + df.RR$n_nexp - df.RR$n_cases_exp - df.RR$n_cases_nexp
    df.RR$se <- with(df.RR, .estimate_rr_from_n(n_cases_exp, n_exp, n_cases_nexp, n_nexp)$se)

  for (i in 1:nrow(df.RR)) {
    n_transit_i <- NA
    n_transit_i <- .estimate_n_from_rr(rr = df.RR$value[i], var = df.RR$se[i]^2,
                                       n_cases = df.RR$n_cases[i],
                                       n_controls = df.RR$n_controls[i])
    n_cases_exp_conv[i] = n_transit_i$n_cases_exp
    n_cases_nexp_conv[i] = n_transit_i$n_cases_nexp
    n_exp_conv[i] = n_transit_i$n_exp
    n_nexp_conv[i] = n_transit_i$n_nexp
  }



  # re estimate n from converted n
  for (i in 1:nrow(df.RR)) {
    rr_transit_i <- .estimate_rr_from_n(n_cases_exp = n_cases_exp_conv[i],
                                        n_cases_nexp = n_cases_nexp_conv[i],
                                        n_exp = n_exp_conv[i],
                                        n_nexp = n_nexp_conv[i])
    rr_conv[i] <- rr_transit_i$value
    se_conv[i] <- rr_transit_i$se
  }

  expect_equal(df$value, rr_conv, tolerance = 0.01)
  expect_equal(df$se, se_conv, tolerance = 0.1)

})
