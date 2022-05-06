tol_large = 1e-10
###########
### SMD ###
###########

# SMD - standard analysis
test_that(".meta_gen correctly estimates the pooled effect size for d values with raw information", {
  df <- subset(df.SMD, factor == "Pharmacological", select = -c(value, se, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$random

  df_mfr = metafor::escalc(m1i = mean_cases, m2i = mean_controls,
                           sd1i = sd_cases, sd2i = sd_controls,
                           n1i = n_cases, n2i = n_controls,
                           data = df, measure = "SMD", vtype = "UB")

  meta <- metafor::rma.uni(yi = yi, vi = vi, data = df_mfr, method = "REML")

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

# SMD - generic
test_that(".meta_gen correctly estimates the pooled effect size for d values value/SE", {


  df <- subset(df.SMD, factor == "Pharmacological", select = -c(ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$random)

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

  expect_equal(meta_umb[[1]]$random$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$random$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})


##########
### OR ###
##########

# OR - standard analysis
test_that("meta_gen_log correctly estimates the pooled effect size from 2x2 table", {

  df <- subset(df.OR, factor == "ASD", select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$random
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

  expect_equal(meta_umb[[1]]$random$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$random$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
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

  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

##########
### RR ###
##########

# RR - standard analysis
test_that(".meta_gen_log correctly estimates the pooled effect size", {

  df <- subset(df.RR, select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$random

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

  expect_equal(meta_umb[[1]]$random$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$random$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
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

  expect_equal(as.numeric(as.character(umb[[1]]$random$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb[[1]]$random$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})


# irr - hksj estimator
test_that(".meta_irr correctly estimates the pooled effect size with hksj estimator", {
  df <- df.IRR

  meta_umb <- umbrella(df, method.var = "hksj")
  meta <- meta::metagen(TE = log(meta_umb[[1]]$x$value), seTE = meta_umb[[1]]$x$se,
                        sm = "IRR", hakn = TRUE, method.tau = "DL")

  expect_equal(meta_umb[[1]]$random$value, as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(meta_umb[[1]]$random$p.value, as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})

# IRR - standard analysis
test_that(".meta_irr correctly estimates the pooled effect size", {

  df <- subset(df.IRR, select = -c(value, ci_lo, ci_up))

  umb <- umbrella(df, method.var = "REML")[[1]]$random

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

  expect_equal(as.numeric(as.character(meta_umb[[1]]$random$value)), as.numeric(as.character(meta$TE.random)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(meta_umb[[1]]$random$p.value)), as.numeric(as.character(meta$pval.random)), tolerance = tol_large)
})

# IRR - generic for multilevel
test_that(".meta_gen_log correctly estimates the pooled effect size for IRR values", {

  se <- with(df.IRR, sqrt(1/n_cases_exp + 1/n_cases_nexp))

  df <- subset(df.IRR, select = -c(n_cases_exp, n_cases_nexp,
                                  time_exp, time_nexp))
  df$se <- se

  umb <- .quiet(umbrella(df, method.var = "REML")[[1]]$random)

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

  umb <- umbrella(df, method.var = "REML", verbose = FALSE)[[1]]$random

  meta <- metafor::rma.uni(yi = log(value), sei = se,
                           data = df, method = "REML")

  #
  expect_equal(as.numeric(as.character(umb$value)), as.numeric(as.character(meta$beta)), tolerance = tol_large)
  expect_equal(as.numeric(as.character(umb$p.value)), as.numeric(as.character(meta$pval)), tolerance = tol_large)
})

#######################################################################################

# test_that(".format_dataset selects appropriate meta analysis type: OR", {
#
#   # simple level
#   df1 = subset(df.OR, factor = unique(df.OR$factor)[1])
#   format1 = .quiet(.format_dataset(attr(.check_data(df1), "data")))
#   expect_equal(attr(format1, "meta"), "OR_standard_raw_information")
#
#   n_cases = with(df.OR, n_cases_exp + n_cases_nexp)
#   n_controls = with(df.OR, n_controls_exp + n_controls_nexp)
#   df2 = subset(df.OR, factor = unique(df.OR$factor)[1], select = -c(n_cases_exp, n_controls_exp, n_cases_nexp, n_controls_nexp))
#   df2$n_cases = n_cases
#   df2$n_controls = n_controls
#   format2 = .quiet(.format_dataset(attr(.check_data(df2), "data")))
#   expect_equal(attr(format2, "meta"), "OR_standard_generic")
#
#   # multilevel
#   df3 = subset(df.OR.multi, factor = unique(df.OR.multi$factor)[1])
#   format3 = .quiet(.format_dataset(attr(.check_data(df3), "data"), mult.level = TRUE))
#   expect_equal(attr(format3, "meta"), "OR_multilevel_raw_information")
#
#   n_cases = with(df.OR.multi, n_cases_exp + n_cases_nexp)
#   n_controls = with(df.OR.multi, n_controls_exp + n_controls_nexp)
#   df4 = subset(df.OR.multi, factor = unique(df.OR.multi$factor)[1], select = -c(n_cases_exp, n_controls_exp, n_cases_nexp, n_controls_nexp))
#   df4$n_cases = n_cases
#   df4$n_controls = n_controls
#   format4 = .quiet(.format_dataset(attr(.check_data(df4), "data"), mult.level = TRUE))
#   expect_equal(attr(format4, "meta"), "OR_multilevel_generic")
# })
#
# test_that(".format_dataset selects appropriate meta analysis type: SMD", {
#
#   # when SMD/MD/G is used only metagen is used to allow a unique estimation of G
#
#   # simple level
#   df1 = subset(df.SMD, factor = unique(df.SMD$factor)[1])
#   format1 = .quiet(.format_dataset(attr(.check_data(df1), "data")))
#   expect_equal(attr(format1, "meta"), "SMD_standard_generic")
#
#   df2 = subset(df.SMD, factor = unique(df.SMD$factor)[1], select = -c(mean_cases, mean_controls))
#   format2 = .quiet(.format_dataset(attr(.check_data(df2), "data")))
#   expect_equal(attr(format2, "meta"), "SMD_standard_generic")
#
#
#   # multi level
#   df1$author[2] <- df1$author[1]
#   df1$year[2] <- df1$year[1]
#   df1$multiple_es <- NA
#   df1$multiple_es[1:2] <- "outcomes"
#   format3 = .quiet(.format_dataset(attr(.check_data(df1), "data"), mult.level = TRUE))
#   expect_equal(attr(format3, "meta"), "SMD_multilevel_generic")
#
#   df2$author[2] <- df2$author[1]
#   df2$year[2] <- df2$year[1]
#   df2$multiple_es <- NA
#   df2$multiple_es[1:2] <- "outcomes"
#   format4 = .quiet(.format_dataset(attr(.check_data(df2), "data"), mult.level = TRUE))
#   expect_equal(attr(format4, "meta"), "SMD_multilevel_generic")
# })
#
# test_that(".format_dataset selects appropriate meta analysis type: RR", {
#
#   # simple level
#   df1 = subset(df.RR, factor = unique(df.RR$factor)[1])
#   format1 = .quiet(.format_dataset(attr(.check_data(df1), "data")))
#   expect_equal(attr(format1, "meta"), "RR_standard_raw_information")
#
#   n_cases = with(df.RR, n_cases_exp + n_cases_nexp)
#   n_controls = with(df.RR, n_exp + n_nexp - n_cases_exp - n_cases_nexp)
#   df2 = subset(df.RR, factor = unique(df.RR$factor)[1], select = -c(n_cases_exp, n_cases_nexp, n_exp, n_nexp))
#   df2$n_cases = n_cases
#   df2$n_controls = n_controls
#   format2 = .quiet(.format_dataset(attr(.check_data(df2), "data")))
#   expect_equal(attr(format2, "meta"), "RR_standard_generic")
#
#
#   # multi level
#   df1$author[2] <- df1$author[1]
#   df1$year[2] <- df1$year[1]
#   df1$multiple_es <- NA
#   df1$multiple_es[1:2] <- "outcomes"
#   format3 = .quiet(.format_dataset(attr(.check_data(df1), "data"), mult.level = TRUE))
#   expect_equal(attr(format3, "meta"), "RR_multilevel_raw_information")
#
#   df2$author[2] <- df2$author[1]
#   df2$year[2] <- df2$year[1]
#   df2$multiple_es <- NA
#   df2$multiple_es[1:2] <- "outcomes"
#   format4 = .quiet(.format_dataset(attr(.check_data(df2), "data"), mult.level = TRUE))
#   expect_equal(attr(format4, "meta"), "RR_multilevel_generic")
# })
#
# test_that(".format_dataset selects appropriate meta analysis type: HR", {
#
#   # simple level
#   df1 = subset(df.HR, factor = unique(df.HR$factor)[1])
#   format1 = .quiet(.format_dataset(attr(.check_data(df1), "data")))
#   expect_equal(attr(format1, "meta"), "HR_standard_generic")
#
#
#   # multi level
#   df1$author[2] <- df1$author[1]
#   df1$year[2] <- df1$year[1]
#   df1$multiple_es <- NA
#   df1$multiple_es[1:2] <- "outcomes"
#   format2 = .quiet(.format_dataset(attr(.check_data(df1), "data"), mult.level = TRUE))
#   expect_equal(attr(format2, "meta"), "HR_multilevel_generic")
#
#   df2 <- df.RR
#   df2$measure <- "HR"
#   df2$n_cases = with(df2, n_cases_exp + n_cases_nexp)
#   df2$n_controls = with(df2, n_exp + n_nexp - n_cases_exp - n_cases_nexp)
#
#   format3 = .quiet(.format_dataset(attr(.check_data(df2), "data")))
#   expect_equal(attr(format3, "meta"), "HR_standard_generic")
# })
#
#
# test_that(".format_dataset selects appropriate meta analysis type: IRR", {
#
#   # simple level
#   df1 = subset(df.IRR, factor = unique(df.IRR$factor)[1])
#   format1 = .quiet(.format_dataset(attr(.check_data(df1), "data")))
#   expect_equal(attr(format1, "meta"), "IRR_standard_raw_information")
#
#   df2 = subset(df.IRR, factor = unique(df.IRR$factor)[1], select = -c(time_exp, time_nexp))
#   format2 = .quiet(.format_dataset(attr(.check_data(df2), "data")))
#   expect_equal(attr(format2, "meta"), "IRR_standard_generic")
#
#   # multi level
#   df1$author[2] <- df1$author[1]
#   df1$year[2] <- df1$year[1]
#   df1$multiple_es <- NA
#   df1$multiple_es[1:2] <- "outcomes"
#   format3 = .quiet(.format_dataset(attr(.check_data(df1), "data"), mult.level = TRUE))
#   expect_equal(attr(format3, "meta"), "IRR_multilevel_raw_information")
#   umb3 = umbrella(df1, mult.level = TRUE)
#
#   df2$author[2] <- df2$author[1]
#   df2$year[2] <- df2$year[1]
#   df2$multiple_es <- NA
#   df2$multiple_es[1:2] <- "outcomes"
#   format4 = .quiet(.format_dataset(attr(.check_data(df2), "data"), mult.level = TRUE))
#   expect_equal(attr(format4, "meta"), "IRR_multilevel_generic")
#
# })

