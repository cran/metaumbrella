tol_large = 1e-10
###########
### SMD ###
###########

# SMD - standard analysis
test_that(".meta_gen correctly estimates the pooled effect size for d values with raw information", {
  df <- subset(df.SMD, factor == "Pharmacological", select = -c(value, se, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$ma_results

  df_mfr = metafor::escalc(m1i = mean_cases, m2i = mean_controls,
                           sd1i = sd_cases, sd2i = sd_controls,
                           n1i = n_cases, n2i = n_controls,
                           data = df, measure = "SMD", vtype = "UB")

  meta <- metafor::rma.uni(yi = yi, vi = vi, data = df_mfr, method = "REML")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

test_that(".meta_gen correctly estimates the pooled effect size for d values with raw information", {
  df <- subset(df.SMD, factor == "Pharmacological", select = -c(value, se, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "FE")[[1]]$ma_results

  df_mfr = metafor::escalc(m1i = mean_cases, m2i = mean_controls,
                           sd1i = sd_cases, sd2i = sd_controls,
                           n1i = n_cases, n2i = n_controls,
                           data = df, measure = "SMD", vtype = "UB")

  meta <- metafor::rma.uni(yi = yi, vi = vi, data = df_mfr, method = "FE")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})


# SMD - generic
test_that(".meta_gen correctly estimates the pooled effect size for d values value/SE", {


  df <- subset(df.SMD, factor == "Pharmacological", select = -c(ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$ma_results)

  meta <- metafor::rma.uni(yi = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)$value,
                           sei = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)$se,
                           data = df, method = "REML", measure = "SMD")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})


# SMD - hksj estimator
test_that(".meta_gen correctly estimates the pooled effect size for d values and hksj estimator", {

  df <- subset(df.SMD, factor == "Pharmacological")

  meta_umb <- umbrella(df, method.var = "hksj")
  meta <- meta::metagen(TE = meta_umb[[1]]$x$value, seTE = meta_umb[[1]]$x$se,
                        hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb[[1]]$ma_results$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$ma_results$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})

###########
### SMC ###
###########

# SMC - standard analysis
test_that(".meta_gen correctly estimates the pooled effect size for d values with change information", {
  df <- subset(df.SMC, factor == "Physically_active", select = -c(value, se, ci_lo, ci_up, mean_pre_cases, mean_pre_controls))

  umb <- umbrella(df, method.var = "REML")[[1]]$ma_results

  df$r = 0
  smc_cases <- metafor::escalc(m1i = df$mean_change_cases,
                               sd1i = df$sd_change_cases,
                               ni = df$n_cases,
                               m2i = df$r,
                               sd2i = df$r,
                               ri = df$r,
                               measure = "SMCC", vtype = "LS")
  smc_controls <- metafor::escalc(m1i = df$mean_change_controls,
                                  sd1i = df$sd_change_controls,
                                  ni = df$n_controls,
                                  m2i = df$r,
                                  sd2i = df$r,
                                  ri = df$r,
                                  measure = "SMCC", vtype = "LS")

  smcc = smc_cases$yi - smc_controls$yi
  se = sqrt(smc_cases$vi + smc_controls$vi)

  meta <- metafor::rma.uni(yi = smcc, sei = se, method = "REML")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

test_that(".meta_gen correctly estimates the pooled effect size for d values with raw information", {
  df <- subset(df.SMC, factor == "Physically_active", select = -c(value, se, ci_lo, ci_up, mean_change_cases, sd_change_cases, sd_change_controls))

  umb <- suppressWarnings(umbrella(df, method.var = "REML", pre_post_cor = 0.5)[[1]]$ma_results)

  df$pre_post_cor = 0.5
  smc_cases <- metafor::escalc(m1i = df$mean_cases,
                               sd1i = df$sd_cases,
                               ni = df$n_cases,
                               m2i = df$mean_pre_cases,
                               sd2i = df$sd_pre_cases,
                               ri = df$pre_post_cor,
                               measure = "SMCC", vtype = "LS")
  smc_controls <- metafor::escalc(m1i = df$mean_controls,
                                  sd1i = df$sd_controls,
                                  ni = df$n_controls,
                                  m2i = df$mean_pre_controls,
                                  sd2i = df$sd_pre_controls,
                                  ri = df$pre_post_cor,
                                  measure = "SMCC", vtype = "LS")

  smcc = smc_cases$yi - smc_controls$yi
  se = sqrt(smc_cases$vi + smc_controls$vi)

  meta <- metafor::rma.uni(yi = smcc, sei = se, method = "REML")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

# SMC - hksj estimator
test_that(".meta_gen correctly estimates the pooled effect size for d values and hksj estimator", {

  df <- subset(df.SMC, factor == "Physically_active")

  meta_umb <- suppressWarnings(umbrella(df, method.var = "hksj"))
  meta <- meta::metagen(TE = meta_umb[[1]]$x$value, seTE = meta_umb[[1]]$x$se,
                        hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb[[1]]$ma_results$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$ma_results$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})


###########
#### Z ####
###########

test_that(".meta_gen correctly estimates the pooled effect size for z values from r", {
  df <- subset(df.R, factor == "gestational_diabetes")

  umb <- suppressWarnings(umbrella(df, method.var = "REML")[[1]]$ma_results)

  z_mfr <- metafor::escalc(ri = df$value,
                           ni = df$n_sample,
                           measure = "ZCOR")

  meta <- metafor::rma.uni(yi = .as_numeric(z_mfr$yi), sei = sqrt(z_mfr$vi), method = "REML")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})
test_that(".meta_gen correctly estimates the pooled effect size for z values and hksj estimator", {

  df <- subset(df.R, factor == "gestational_diabetes")

  meta_umb <- suppressWarnings(umbrella(df, method.var = "hksj"))
  meta <- meta::metagen(TE = meta_umb[[1]]$x$value, seTE = meta_umb[[1]]$x$se,
                        hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb[[1]]$ma_results$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$ma_results$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})


##########
### OR ###
##########

# OR - standard analysis
test_that("meta_gen_log correctly estimates the pooled effect size from 2x2 table", {

  df <- subset(df.OR, factor == "ASD", select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$ma_results
  meta <- metafor::rma.uni(ai = n_cases_exp, bi = n_cases_nexp,
                           ci = n_controls_exp, di = n_controls_nexp,
                           data = df, method = "REML", measure = "OR")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})


# OR - hksj estimator
test_that("meta_gen_log correctly estimates the pooled effect size from 2x2 table and hksj estimator", {

  df <- subset(df.OR, factor == "ASD")

  meta_umb <- umbrella(df, method.var = "hksj")
  meta <- meta::metagen(TE = log(meta_umb[[1]]$x$value), seTE = meta_umb[[1]]$x$se,
                        hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb[[1]]$ma_results$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$ma_results$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})

# OR - generic
test_that(".meta_gen_log correctly estimates the pooled effect size for OR values", {

  se <- with(df.OR, .estimate_or_from_n(n_cases_exp,n_cases_nexp,
                            n_controls_exp, n_controls_nexp))$se

  df <- subset(df.OR, factor == "ASD", select = -c(n_cases_exp, n_cases_nexp,
                                                   n_controls_exp, n_controls_nexp))
  df$se <- se[which(df.OR$factor=="ASD")]

  umb <- umbrella(df, method.var = "REML")[[1]]$ma_results

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML", measure = "OR")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

##########
### RR ###
##########

# RR - standard analysis
test_that(".meta_gen_log correctly estimates the pooled effect size", {

  df <- subset(df.RR, select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$ma_results

  meta <- metafor::rma.uni(ai = n_cases_exp, n1i = n_exp,
                           ci = n_cases_nexp, n2i = n_nexp,
                           data = df, method = "REML", measure = "RR")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})


# RR - hksj estimator
test_that(".meta_gen_log correctly estimates the pooled effect size with hksj estimator", {

  df <- df.RR

  meta_umb <- umbrella(df, method.var = "hksj")
  meta <- meta::metagen(TE = log(meta_umb[[1]]$x$value), seTE = meta_umb[[1]]$x$se,
                        hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb[[1]]$ma_results$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$ma_results$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})

# RR - generic for multilevel
test_that(".meta_gen_log correctly estimates the pooled effect size for RR values", {

  se <- with(df.RR, .estimate_rr_from_n(n_cases_exp,n_exp,
                                        n_cases_nexp, n_nexp))$se

  df.RR$n_cases = df.RR$n_cases_exp + df.RR$n_cases_nexp
  df.RR$n_controls = with(df.RR, n_exp + n_nexp - n_cases_exp - n_cases_nexp)

  df <- subset(df.RR, select = -c(n_cases_exp, n_cases_nexp,
                                  n_exp, n_nexp))
  df$se <- se

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$ma_results)

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML", measure = "RR")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

###########
### IRR ###
###########

# IRR - standard analysis
test_that(".meta_gen_log correctly estimates the pooled effect size", {

  df <- df.IRR

  umb <- umbrella(df, method.var = "REML")

  meta <- metafor::rma.uni(x1i = n_cases_exp, t1i = time_exp,
                           x2i = n_cases_nexp, t2i = time_nexp,
                           data = df, method = "REML", measure = "IRR")

  expect_equal(as.numeric(as.character(umb[[1]]$ma_results$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb[[1]]$ma_results$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})


# irr - hksj estimator
test_that(".meta_irr correctly estimates the pooled effect size with hksj estimator", {
  df <- df.IRR

  meta_umb <- umbrella(df, method.var = "hksj")
  meta <- meta::metagen(TE = log(meta_umb[[1]]$x$value), seTE = meta_umb[[1]]$x$se,
                        sm = "IRR", hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb[[1]]$ma_results$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$ma_results$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})

# IRR - standard analysis
test_that(".meta_irr correctly estimates the pooled effect size", {

  df <- subset(df.IRR, select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$ma_results

  meta <- metafor::rma.uni(x1i = n_cases_exp, t1i = time_exp,
                           x2i = n_cases_nexp, t2i = time_nexp,
                           data = df, method = "REML", measure = "IRR")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})


# IRR - hksj estimator
test_that(".meta_irr correctly estimates the pooled effect size with hksj estimator", {

  df <- subset(df.IRR, select = -c(value, ci_lo, ci_up))

  meta_umb <- umbrella(df, method.var = "hksj")
  meta <- meta::metagen(TE = log(meta_umb[[1]]$x$value), seTE = meta_umb[[1]]$x$se,
                        sm = "IRR", hakn = TRUE, method.tau = "DL")

  expect_equal(as.numeric(as.character(meta_umb[[1]]$ma_results$value)), as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(meta_umb[[1]]$ma_results$p.value)), as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})

# IRR - generic for multilevel
test_that(".meta_gen_log correctly estimates the pooled effect size for IRR values", {

  se <- with(df.IRR, sqrt(1/n_cases_exp + 1/n_cases_nexp))

  df <- subset(df.IRR, select = -c(n_cases_exp, n_cases_nexp,
                                  time_exp, time_nexp))
  df$se <- se

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$ma_results)

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML")

  #
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})


# HR - generic for multilevel
test_that(".meta_gen_log correctly estimates the pooled effect size for HR values", {

  se <- with(df.HR, ((log(ci_up) - log(ci_lo))/3.92))
  df <- subset(df.HR, factor == "Yoga")
  df$se <- se[which(df.HR$factor == "Yoga")]

  umb <- umbrella(df, method.var = "REML", verbose = FALSE)[[1]]$ma_results

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML")

  #
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

#######################################################################################

##########
### R ###
##########

# R - standard analysis
test_that("meta_gen correctly estimates the pooled effect size from R", {

  df <- subset(df.R, factor == "Gestational_hypertension")

  umb <- umbrella(df, method.var = "REML")[[1]]$ma_results
  meta <- metafor::rma.uni(ri = value, ni = n_sample,
                           data = df, method = "REML", measure = "ZCOR")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

test_that("meta_gen correctly estimates the prediction interval size from R", {

  df <- subset(df.R, factor == "Gestational_hypertension")

  umb <- umbrella(df, method.var = "REML")[[1]]$ma_results
  meta <- meta::metacor(cor = value, n = n_sample,
                           data = df, method.tau = "REML", prediction = TRUE)

  expect_equal(as.numeric(as.character(umb$pi_lo)), as.numeric(as.character(meta$lower.predict)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$pi_up)), as.numeric(as.character(meta$upper.predict)), tolerance = tol_large)
})

