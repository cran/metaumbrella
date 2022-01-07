# We check that the umbrella function correctly estimates value + se + CI + samples sizes ---


##### from raw information -------------------------------------------------------------------

# SMD

test_that("format_dataset from raw information: SMD", {
  skip_on_cran()
  df <- df.SMD[df.SMD$factor == "Pharmacological", ]
  df.red <- subset(df, select = -c(value, se, ci_lo, ci_up))
  df_format <- .quiet(.format_dataset(attr(.check_data(df.red), "data")))
  df_umb <- umbrella(df.red)[[1]]$x

  G = .estimate_g_from_d(d = df$value, n_cases = df$n_cases, n_controls = df$n_controls)$value
  se = .estimate_g_from_d(d = df$value, n_cases = df$n_cases, n_controls = df$n_controls)$se
  ci_lo <- G - qt(0.975, df$n_cases + df$n_controls - 2) * se
  ci_up <- G + qt(0.975, df$n_cases + df$n_controls - 2) * se

  expect_equal(G, df_format$value, tolerance = 1e-13)
  expect_equal(se, df_format$se, tolerance = 1e-13)
  expect_equal(ci_lo, df_format$ci_lo, tolerance = 1e-13)
  expect_equal(ci_up, df_format$ci_up, tolerance = 1e-13)
})

# OR
test_that("format_dataset from raw information: OR", {
  skip_on_cran()
  df <- df.OR[df.OR$factor == "ADHD", ]
  df.red <- subset(df, select = -c(value, ci_lo, ci_up,
                                   n_cases, n_controls,
                                   n_exp, n_nexp))
    df_format <- .quiet(.format_dataset(attr(.check_data(df.red), "data")))

  se = with(df, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))

  df$ci_lo <- df$value / exp(qnorm(0.975) * se)
  df$ci_up <- df$value * exp(qnorm(0.975) * se)

  expect_equal(df$value, df_format$value, tolerance = 1e-13)
  expect_equal(se, df_format$se, tolerance = 1e-13)
  expect_equal(df$ci_lo, df_format$ci_lo, tolerance = 1e-13)
  expect_equal(df$ci_up, df_format$ci_up, tolerance = 1e-13)
  expect_equal(df$n_cases, df_format$n_cases)
  expect_equal(df$n_controls, df_format$n_controls)
})

# RR

test_that("format_dataset from raw information: RR", {
  skip_on_cran()
  df <- df.RR[df.RR$factor == "SSRI", ]
  df.red <- subset(df, select = -c(value, ci_lo, ci_up))
    df_format <- .quiet(.format_dataset(attr(.check_data(df.red), "data")))

  se <- with(df, sqrt(1 / n_cases_exp - 1 / n_exp + 1 / n_cases_nexp - 1 / n_nexp))

  df$ci_lo <- df$value / exp(qnorm(0.975) * se)
  df$ci_up <- df$value * exp(qnorm(0.975) * se)

  expect_equal(df$value, df_format$value, tolerance = 1e-13)
  expect_equal(se, df_format$se, tolerance = 1e-13)
  expect_equal(df$ci_lo, df_format$ci_lo, tolerance = 1e-13)
  expect_equal(df$ci_up, df_format$ci_up, tolerance = 1e-13)
  expect_equal(df$n_cases_exp, df_format$n_cases_exp)
  expect_equal(df$n_exp, df_format$n_exp)
  expect_equal(df$n_cases_nexp, df_format$n_cases_nexp)
  expect_equal(df$n_nexp, df_format$n_nexp)
})

# IRR
test_that("format_dataset from raw information: IRR", {
  skip_on_cran()
  df <- df.IRR[df.IRR$factor == "Smoking", ]
  df.red <- subset(df, select = -c(value, ci_lo, ci_up,
                                   time, n_cases))
    df_format <- .quiet(.format_dataset(attr(.check_data(df.red), "data")))

  se <- with(df, sqrt(1 / n_cases_exp + 1 / n_cases_nexp))
  df$ci_lo <- df$value / exp(qnorm(0.975) * se)
  df$ci_up <- df$value * exp(qnorm(0.975) * se)

  expect_equal(df$value, df_format$value, tolerance = 1e-13)
  expect_equal(se, df_format$se, tolerance = 1e-13)
  expect_equal(df$ci_lo, df_format$ci_lo, tolerance = 1e-13)
  expect_equal(df$ci_up, df_format$ci_up, tolerance = 1e-13)
  expect_equal(df$time, df_format$time)
  expect_equal(df$n_cases, df_format$n_cases)
})

#### some information not indicated --------------------------

############################
# value + SE not INDICATED #
############################

# SMD
test_that("format_dataset from CI: SMD", {
  skip_on_cran()
  df <- subset(df.SMD, factor == "Pharmacological", select = -c(mean_cases, mean_controls, sd_cases, sd_controls))
  df$ci_lo <- df$ci_up <- NA

  df$se <- with(df, sqrt(1/n_cases + 1/n_controls))
  df$ci_lo = with(df, value - se * qt(0.975, n_cases + n_controls - 2))
  df$ci_up = with(df, value + se * qt(0.975, n_cases + n_controls - 2))


  dfred <- subset(df, factor == "Pharmacological", select = -c(value, se))

  df_format <- .quiet(.format_dataset(attr(.check_data(dfred), "data")))

  value = (dfred$ci_up + dfred$ci_lo)  / 2
  se = (dfred$ci_up - dfred$ci_lo) / (2 * qt(0.975, df$n_cases + df$n_controls - 2))
  G = .estimate_g_from_d(d = value, n_cases = df$n_cases, n_controls = df$n_controls, se = se)$value
  se = .estimate_g_from_d(d = value, n_cases = df$n_cases, n_controls = df$n_controls, se = se)$se
  ci_lo <- G - qt(0.975, df$n_cases + df$n_controls - 2) * se
  ci_up <- G + qt(0.975, df$n_cases + df$n_controls - 2) * se

  expect_equal(G, df_format$value, tolerance = 1e-6)
  expect_equal(se, df_format$se, tolerance = 1e-6)
  expect_equal(ci_lo, df_format$ci_lo, tolerance = 1e-6)
  expect_equal(ci_up, df_format$ci_up, tolerance = 1e-6)
  expect_equal(df$n_cases, df_format$n_cases)
  expect_equal(df$n_controls, df_format$n_controls)
})

### OR
test_that("format_dataset from CI: OR", {
  skip_on_cran()
  dfsave <- df.OR[df.OR$factor == "ADHD", ]
  dfsave$se <- with(dfsave, sqrt(1/n_cases_exp +1/ n_cases_nexp +1/n_controls_exp +1/ n_controls_nexp))
  df <- df.OR[df.OR$factor == "ADHD", ]
  df <- subset(df, select = -c(value, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  df_format <- .quiet(.format_dataset(attr(.check_data(df), "data")))

  expect_equal(dfsave$value, df_format$value, tolerance = 1e-6)
  expect_equal(dfsave$se, df_format$se, tolerance = 1e-4)
  expect_equal(dfsave$ci_lo, df_format$ci_lo, tolerance = 1e-6)
  expect_equal(dfsave$ci_up, df_format$ci_up, tolerance = 1e-6)
  expect_equal(dfsave$n_cases, df_format$n_cases)
  expect_equal(dfsave$n_controls, df_format$n_controls)
  expect_equal(dfsave$n_cases_exp, df_format$n_cases_exp)
  expect_equal(dfsave$n_cases_nexp, df_format$n_cases_nexp)
  expect_equal(dfsave$n_controls_exp, df_format$n_controls_exp)
  expect_equal(dfsave$n_controls_nexp, df_format$n_controls_nexp)
})

test_that("format_dataset from CI: RR", {
  skip_on_cran()
  dfsave <- df.RR[df.RR$factor == "SSRI", ]

  df <- df.RR[df.RR$factor == "SSRI", ]
  df$n_cases <- df$n_cases_exp + df$n_cases_nexp
  df$n_controls <- df$n_exp + df$n_nexp - df$n_cases_exp - df$n_cases_nexp
  dfsave <- df
  df <- subset(df, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp))
  df_format <- .quiet(.format_dataset(attr(.check_data(df), "data")))

  dfsave$se <- with(dfsave, sqrt(1 / n_cases_exp - 1 / n_exp + 1 / n_cases_nexp - 1 / n_nexp))
  expect_equal(dfsave$value, df_format$value, tolerance = 1e-6)
  expect_equal(dfsave$se, df_format$se, tolerance = 1e-4)
  expect_equal(dfsave$ci_lo, df_format$ci_lo, tolerance = 1e-6)
  expect_equal(dfsave$ci_up, df_format$ci_up, tolerance = 1e-6)

  expect_equal(dfsave$n_cases, df_format$n_cases)
  expect_equal(dfsave$n_controls, df_format$n_controls)

  # Incongruities due to approximations made in .estimate_n_from_rr
  expect_equal(dfsave$n_cases_exp, df_format$n_cases_exp, tolerance = 1)
  expect_equal(dfsave$n_cases_nexp, df_format$n_cases_nexp, tolerance = 1)
  expect_equal(dfsave$n_exp, df_format$n_exp, tolerance = 1)
  expect_equal(dfsave$n_nexp, df_format$n_nexp, tolerance = 1)
})

test_that("format_dataset from CI: IRR", {
  skip_on_cran()
  dfsave <- df.IRR[df.IRR$factor == "Smoking", ]
  df <- df.IRR[df.IRR$factor == "Smoking", ]
  df <- subset(df, select = -c(value, n_cases_exp, n_cases_nexp, time_exp, time_nexp))

  df_format <- .quiet(.format_dataset(attr(.check_data(df), "data")))

  dfsave$se <- with(dfsave, sqrt(1/n_cases_exp +1/ n_cases_nexp))

  expect_equal(dfsave$value, df_format$value, tolerance = 1e-6)
  expect_equal(dfsave$se, df_format$se, tolerance = 1e-4)
  expect_equal(dfsave$ci_lo, df_format$ci_lo, tolerance = 1e-6)
  expect_equal(dfsave$ci_up, df_format$ci_up, tolerance = 1e-6)
  expect_equal(dfsave$n_cases, df_format$n_cases)
  expect_equal(dfsave$time, df_format$time)
  # Incongruities due to approximations made in .estimate_n_from_rr
  expect_equal(dfsave$n_cases_exp, round(df_format$n_cases_exp), tolerance = 1)
  expect_equal(dfsave$n_cases_nexp, round(df_format$n_cases_exp), tolerance = 1)
  expect_equal(dfsave$time_exp, df_format$time_exp, tolerance = 1)
  expect_equal(dfsave$time_nexp, df_format$time_nexp, tolerance = 1)
})



#########################
# SE + CI not indicated #
#########################

# SMD
test_that("format_dataset from value and N: SMD", {
  skip_on_cran()
  df <- df.SMD[df.SMD$factor == "Pharmacological", ]
  df.red <- subset(df, select = -c(ci_lo, ci_up, se,
                               mean_cases, mean_controls, sd_cases, sd_controls))

  df_format <- .quiet(.format_dataset(attr(.check_data(df.red), "data")))


  G = .estimate_g_from_d(d = df$value, n_cases = df$n_cases, n_controls = df$n_controls)$value
  se = .estimate_g_from_d(d = df$value, n_cases = df$n_cases, n_controls = df$n_controls)$se
  ci_lo <- G - qt(0.975, df$n_cases + df$n_controls - 2) * se
  ci_up <- G + qt(0.975, df$n_cases + df$n_controls - 2) * se

  expect_equal(G, df_format$value, tolerance = 1e-6)
  expect_equal(se, df_format$se, tolerance = 1e-6)
  expect_equal(ci_lo, df_format$ci_lo, tolerance = 1e-6)
  expect_equal(ci_up, df_format$ci_up, tolerance = 1e-6)
  expect_equal(df$n_cases, df_format$n_cases)
  expect_equal(df$n_controls, df_format$n_controls)
})

# G
test_that("format_dataset from value and N: G", {
  df <- df.SMD[df.SMD$factor == "Pharmacological", ]
  df_sauv <- df.SMD[df.SMD$factor == "Pharmacological", ]

  # G
  G = with(df, .estimate_g_from_d(value, n_cases, n_controls)$value)
  se = with(df, .estimate_g_from_d(value, n_cases, n_controls)$se)
  df$value = G; df$se = se
  df$ci_lo <- df$value - qt(0.975, df$n_cases + df$n_controls - 2) * (df$se)
  df$ci_up <- df$value + qt(0.975, df$n_cases + df$n_controls - 2) * (df$se)

  df_sauv <- df

  df$measure <- "G"

  df.red <- subset(df, select = -c(ci_lo, ci_up, se,
                                   mean_cases, mean_controls, sd_cases, sd_controls))

  df_format <- .quiet(.format_dataset(attr(.check_data(df.red), "data")))
  expect_equal(df_sauv$value, df_format$value, tolerance = 1e-6)
  expect_equal(df_sauv$se, df_format$se, tolerance = 1e-6)
  expect_equal(df_sauv$ci_lo, df_format$ci_lo, tolerance = 1e-6)
  expect_equal(df_sauv$ci_up, df_format$ci_up, tolerance = 1e-6)
  expect_equal(df_sauv$n_cases, df_format$n_cases)
  expect_equal(df_sauv$n_controls, df_format$n_controls)
})

# OR
test_that("format_dataset from value and N: OR", {
  skip_on_cran()
  df <- df.OR[df.OR$factor == "ADHD", ]
  df.red <- subset(df, select = -c(ci_lo, ci_up,
                                   n_exp, n_nexp,
                                   n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
    df_format <- .quiet(.format_dataset(attr(.check_data(df.red), "data")))


  se = with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp)$se)
  ci_lo = exp(log(df$value) - qnorm(0.975) * se)
  ci_up = exp(log(df$value) + qnorm(0.975) * se)

  expect_equal(df$value, df_format$value, tolerance = 1e-6)
  expect_equal(df$n_cases, df_format$n_cases)
  expect_equal(df$n_controls, df_format$n_controls)

  # errors due to imprecision in .estimate_se_from_or
  expect_equal(df$ci_lo, df_format$ci_lo, tolerance = 5e-1)
  expect_equal(df$ci_up, df_format$ci_up, tolerance = 5e-1)
  expect_equal(df$n_cases_exp, df_format$n_cases_exp, tolerance = 1)
  expect_equal(df$n_cases_nexp, df_format$n_cases_nexp, tolerance = 1)
  expect_equal(df$n_controls_exp, df_format$n_controls_exp, tolerance = 1)
  expect_equal(df$n_controls_nexp, df_format$n_controls_nexp, tolerance = 1)
})

#----------------------------------------------------------


test_that("format_dataset converts and back converts G correctly", {
  df <- df.SMD[df.SMD$factor == "Pharmacological", ]
  df <- subset(df, select = -c(mean_cases, mean_controls, sd_cases, sd_controls, ci_lo, ci_up))
  df$se = with(df, .estimate_se_from_d(n_cases, n_controls, value))$se

  G = with(df, .estimate_g_from_d(value, n_cases, n_controls)$value)
  se = with(df, .estimate_g_from_d(value, n_cases, n_controls)$se)
  ci_lo = G - qt(0.975, df$n_cases + df$n_controls - 2) * (se)
  ci_up = G + qt(0.975, df$n_cases + df$n_controls - 2) * (se)

  df$value = G
  df$se = se
  df$measure <- "G"

  df_G_SE <- df_G_CI <- df
  df_G_CI$ci_lo = ci_lo
  df_G_CI$ci_up = ci_up
  df_SMD_SE_format = subset(df.SMD, factor == "Pharmacological", select = -c(mean_cases, mean_controls, sd_cases, sd_controls, ci_lo, ci_up))
  df_SMD_SE_format$se = with(df_SMD_SE_format, .estimate_se_from_d(n_cases, n_controls, value))$se
  df_G_SE <- .quiet(.format_dataset(attr(.check_data(df_G_SE), "data")))
  df_G_CI <- .quiet(.format_dataset(attr(.check_data(df_G_CI), "data")))
  df_SMD_SE <- .quiet(.format_dataset(attr(.check_data(df_SMD_SE_format), "data")))


  expect_equal(G, df_G_SE$value, tolerance = 1e-6)
  expect_equal(se, df_G_SE$se, tolerance = 1e-6)
  expect_equal(ci_lo, df_G_SE$ci_lo, tolerance = 1e-6)
  expect_equal(ci_up, df_G_SE$ci_up, tolerance = 1e-6)

  expect_equal(G, df_G_CI$value, tolerance = 1e-6)
  expect_equal(se, df_G_CI$se, tolerance = 1e-6)
  expect_equal(ci_lo, df_G_CI$ci_lo, tolerance = 1e-6)
  expect_equal(ci_up, df_G_CI$ci_up, tolerance = 1e-6)

  expect_equal(G, df_SMD_SE$value, tolerance = 1e-6)
  expect_equal(se, df_SMD_SE$se, tolerance = 1e-6)
  expect_equal(ci_lo, df_SMD_SE$ci_lo, tolerance = 1e-6)
  expect_equal(ci_up, df_SMD_SE$ci_up, tolerance = 1e-6)
})

test_that("format_dataset converts and back converts G correctly", {
  df <- df.SMD[df.SMD$factor == "Pharmacological", ]
  df <- subset(df, select = -c(mean_cases, mean_controls, sd_cases, sd_controls, se))

  G = with(df, .estimate_g_from_d(value, n_cases, n_controls)$value)
  se = with(df, .estimate_g_from_d(value, n_cases, n_controls)$se)
  ci_lo = G - qt(0.975, df$n_cases + df$n_controls - 2) * (se)
  ci_up = G + qt(0.975, df$n_cases + df$n_controls - 2) * (se)

  df$value = G
  df$ci_lo = ci_lo
  df$ci_up = ci_up
  df$measure <- "G"

  df_G_CI <- .quiet(.format_dataset(attr(.check_data(df), "data")))

  df_SMD_SE_format = subset(df.SMD, factor == "Pharmacological", select = -c(mean_cases, mean_controls, sd_cases, sd_controls, ci_lo, ci_up))
  df_SMD_SE <- .quiet(.format_dataset(attr(.check_data(df_SMD_SE_format), "data")))


  expect_equal(G, df_G_CI$value, tolerance = 1e-6)
  expect_equal(se, df_G_CI$se, tolerance = 1e-6)
  expect_equal(ci_lo, df_G_CI$ci_lo, tolerance = 1e-6)
  expect_equal(ci_up, df_G_CI$ci_up, tolerance = 1e-6)

  expect_equal(G, df_SMD_SE$value, tolerance = 1e-6)
  expect_equal(se, df_SMD_SE$se, tolerance = 1e-6)
  expect_equal(ci_lo, df_SMD_SE$ci_lo, tolerance = 1e-6)
  expect_equal(ci_up, df_SMD_SE$ci_up, tolerance = 1e-6)
})
