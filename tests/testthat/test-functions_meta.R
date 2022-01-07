###########
### SMD ###
###########

# SMD - standard analysis
test_that(".meta_d correctly estimates the pooled effect size for d values", {
  df <- subset(df.SMD, factor == "Pharmacological", select = -c(value, se, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$random
  meta_umb <- .meta_d(df, method.var = "REML")
  meta <- metafor::rma.uni(m1i = mean_cases, m2i = mean_controls,
                           sd1i = sd_cases, sd2i = sd_controls,
                           n1i = n_cases, n2i = n_controls,
                           data = df, method = "REML", measure = "SMD", vtype = "UB")

  expect_equal(as.numeric(as.character(meta_umb$TE.random)), as.numeric(as.character(meta$beta)), tolerance = 1e-5)
  expect_equal(as.numeric(as.character(meta_umb$pval.random)), as.numeric(as.character(meta$pval)), tolerance = 5e-5)
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-5)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 5e-5)
})


# SMD - hksj estimator
test_that(".meta_d correctly estimates the pooled effect size for d values and hksj estimator", {

  df <- subset(df.SMD, factor == "Pharmacological")
  umb <- umbrella(df, method.var = "hksj")[[1]]$random
  meta_umb <- .meta_d(df, method.var = "hksj")

  meta <- meta::metacont(n_cases, mean_cases, sd_cases,
                         n_controls, mean_controls, sd_controls,
                         method.tau = "DL", data = df,
                         sm ="SMD", method.smd = "Hedges", hakn = TRUE)


  expect_equal(meta_umb$TE.random, as.numeric(as.character(meta$TE.random)), tolerance = 1e-6)
  expect_equal(meta_umb$pval.random, as.numeric(as.character(meta$pval.random)), tolerance = 1e-6)
  expect_equal(umb$value, as.numeric(as.character(meta$TE.random)), tolerance = 1e-6)
  expect_equal(umb$p.value, as.numeric(as.character(meta$pval.random)), tolerance = 1e-6)
})

# SMD - generic for multilevel
test_that(".meta_gen_smd correctly estimates the pooled effect size for d values", {


  df <- subset(df.SMD, factor == "Pharmacological", select = -c(ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$random)
  df_form <- .quiet(umbrella(df, method.var = "REML")[[1]]$x)

  meta_gen <- .meta_gen_smd(df_form, method.var = "REML")

  meta <- metafor::rma.uni(yi = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)$value,
                           sei = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)$se,
                           data = df, method = "REML", measure = "SMD")

  expect_equal(as.numeric(as.character(meta_gen$TE.random)), as.numeric(as.character(meta$beta)), tolerance = 1e-10)
  expect_equal(as.numeric(as.character(meta_gen$seTE.random)), as.numeric(as.character(meta$se)), tolerance = 1e-10)
  expect_equal(as.numeric(as.character(meta_gen$pval.random)), as.numeric(as.character(meta$pval)), tolerance = 1e-10)
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-10)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 1e-10)

})

##########
### OR ###
##########

# OR - standard analysis
test_that(".meta_or correctly estimates the pooled effect size from 2x2 table", {

  df <- subset(df.OR, factor == "ASD", select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$random
  meta_umb <- .meta_or(df, method.var = "REML")
  meta <- metafor::rma.uni(ai = n_cases_exp, bi = n_cases_nexp,
                           ci = n_controls_exp, di = n_controls_nexp,
                           data = df, method = "REML", measure = "OR")

  expect_equal(as.numeric(as.character(meta_umb$TE.random)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(meta_umb$seTE.random)), as.numeric(as.character(meta$se)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(meta_umb$pval.random)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
})


# OR - hksj estimator
test_that(".meta_or correctly estimates the pooled effect size from 2x2 table and hksj estimator", {

  df <- subset(df.OR, factor == "ASD")

  umb <- umbrella(df, method.var = "hksj")
  meta_umb <- umb[[1]]$random
  meta <- meta::metabin(event.e = n_cases_exp, n.e = n_exp,
                        event.c = n_cases_nexp, n.c = n_nexp,
                        data = df, sm = "OR", hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb$value, as.numeric(as.character(meta$TE.random)), tolerance = 1e-5)
  expect_equal(meta_umb$p.value, as.numeric(as.character(meta$pval.random)), tolerance = 1e-5)
})

# OR - generic
test_that(".meta_gen_log correctly estimates the pooled effect size for OR values", {

  se <- with(df.OR, .estimate_or_from_n(n_cases_exp,n_cases_nexp,
                            n_controls_exp, n_controls_nexp))$se

  df <- subset(df.OR, factor == "ASD", select = -c(n_cases_exp, n_cases_nexp,
                                                   n_controls_exp, n_controls_nexp))
  df$se <- se[which(df.OR$factor=="ASD")]

  umb <- umbrella(df, method.var = "REML")[[1]]$random

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML", measure = "OR")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
})

##########
### RR ###
##########

# RR - standard analysis
test_that(".meta_rr correctly estimates the pooled effect size", {

  df <- subset(df.RR, select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$random
  meta_umb <- .meta_rr(df, method.var = "REML")

  meta <- metafor::rma.uni(ai = n_cases_exp, n1i = n_exp,
                           ci = n_cases_nexp, n2i = n_nexp,
                           data = df, method = "REML", measure = "RR")

  expect_equal(as.numeric(as.character(meta_umb$TE.random)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(meta_umb$seTE.random)), as.numeric(as.character(meta$se)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(meta_umb$pval.random)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
})


# RR - hksj estimator
test_that(".meta_rr correctly estimates the pooled effect size with hksj estimator", {

  df <- df.RR

  umb <- umbrella(df, method.var = "hksj")
  meta_umb <- umb[[1]]$random
  meta <- meta::metabin(event.e = n_cases_exp, n.e = n_exp,
                        event.c = n_cases_nexp, n.c = n_nexp,
                        data = df, sm = "RR", hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb$value, as.numeric(as.character(meta$TE.random)), tolerance = 1e-3)
  expect_equal(meta_umb$p.value, as.numeric(as.character(meta$pval.random)), tolerance = 1e-10)
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

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$random)

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML", measure = "RR")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
})

###########
### IRR ###
###########

# IRR - standard analysis
test_that(".meta_irr correctly estimates the pooled effect size", {

  df <- df.IRR

  umb <- .meta_irr(df, "DL")

  meta <- metafor::rma.uni(x1i = n_cases_exp, t1i = time_exp,
                           x2i = n_cases_nexp, t2i = time_nexp,
                           data = df, method = "DL", measure = "IRR")

  expect_equal(as.numeric(as.character(umb$TE.random)), as.numeric(as.character(meta$beta)), tolerance = 1e-5)
  expect_equal(as.numeric(as.character(umb$seTE.random)), as.numeric(as.character(meta$se)), tolerance = 1e-5)
  expect_equal(as.numeric(as.character(umb$pval.random)), as.numeric(as.character(meta$pval)), tolerance = 1e-5)
})


# irr - hksj estimator
test_that(".meta_irr correctly estimates the pooled effect size with hksj estimator", {
  df <- df.IRR

  umb <- umbrella(df, method.var = "hksj")
  meta_umb <- umb[[1]]$random
  meta <- meta::metainc(event.e = n_cases_exp, time.e = time_exp,
                        event.c = n_cases_nexp, time.c = time_nexp,
                        data = umb[[1]]$x, sm = "IRR", hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb$value, as.numeric(as.character(meta$TE.random)), tolerance = 1e-6)
  expect_equal(meta_umb$p.value, as.numeric(as.character(meta$pval.random)), tolerance = 1e-6)
})

# irr - generic for multilevel
test_that(".meta_gen_log correctly estimates the pooled effect size for IRR values", {


  df <- df.IRR

  df$value <- with(df, .estimate_irr_from_n(n_cases_exp, time_exp,
                                            n_cases_nexp, time_nexp)$value)
  df$se <- with(df, .estimate_irr_from_n(n_cases_exp, time_exp,
                                         n_cases_nexp, time_nexp)$se)



  df <- df[, c("value", "se", "n_cases_exp", "n_cases_nexp", "time_exp","time_nexp")]

  meta_gen <- .meta_gen_log(df, method.var = "REML")
  meta_umb <- .meta_irr(df, method.var = "REML")

  expect_equal(as.numeric(as.character(meta_gen$TE.random)), as.numeric(as.character(meta_umb$TE.random)), tolerance = 1e-4)
  expect_equal(as.numeric(as.character(meta_gen$seTE.random)), as.numeric(as.character(meta_umb$seTE.random)), tolerance = 1e-2)
  expect_equal(as.numeric(as.character(meta_gen$pval.random)), as.numeric(as.character(meta_umb$pval.random)), tolerance = 1e-2)
})




##########
### IRR ###
##########

# IRR - standard analysis
test_that(".meta_irr correctly estimates the pooled effect size", {

  df <- subset(df.IRR, select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$random
  meta_umb <- .meta_irr(df, method.var = "REML")

  meta <- metafor::rma.uni(x1i = n_cases_exp, t1i = time_exp,
                           x2i = n_cases_nexp, t2i = time_nexp,
                           data = df, method = "REML", measure = "IRR")

  expect_equal(as.numeric(as.character(meta_umb$TE.random)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(meta_umb$seTE.random)), as.numeric(as.character(meta$se)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(meta_umb$pval.random)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
})


# IRR - hksj estimator
test_that(".meta_irr correctly estimates the pooled effect size with hksj estimator", {

  df <- subset(df.IRR, select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "hksj")[[1]]$random
  meta_umb <- .meta_irr(df, method.var = "hksj")
  meta <- meta::metainc(event.e = n_cases_exp, time.e = time_exp,
                        event.c = n_cases_nexp, time.c = time_nexp,
                        data = df, measure = "IRR", hakn = TRUE, method.tau = "DL")

  expect_equal(as.numeric(as.character(meta_umb$TE.random)), as.numeric(as.character(meta$TE.random)), tolerance = 1e-6)
  expect_equal(as.numeric(as.character(meta_umb$seTE.random)), as.numeric(as.character(meta$seTE.random)), tolerance = 1e-6)
  expect_equal(as.numeric(as.character(meta_umb$pval.random)), as.numeric(as.character(meta$pval.random)), tolerance = 1e-6)
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$TE.random)), tolerance = 1e-6)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval.random)), tolerance = 1e-6)

})

# IRR - generic for multilevel
test_that(".meta_gen_log correctly estimates the pooled effect size for IRR values", {

  se <- with(df.IRR, sqrt(1/n_cases_exp + 1/n_cases_nexp))

  df <- subset(df.IRR, select = -c(n_cases_exp, n_cases_nexp,
                                  time_exp, time_nexp))
  df$se <- se

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$random)

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML", measure = "IRR")

  #
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
})


# HR - generic for multilevel
test_that(".meta_gen_log correctly estimates the pooled effect size for HR values", {

  se <- with(df.HR, ((log(ci_up) - log(ci_lo))/3.92))
  df <- subset(df.HR, factor == "Yoga")
  df$se <- se[which(df.HR$factor == "Yoga")]

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$random)

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML", measure = "IRR")

  #
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = 1e-13)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = 1e-13)
})
