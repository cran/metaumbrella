test_that("mult.level creates message for multilevel studies", {
  skip_on_cran()
  df <- df.OR.multi
  expect_message(invisible(force(umbrella(df, mult.level = TRUE))), "contains multiple outcomes")
  expect_message(invisible(force(umbrella(df, mult.level = TRUE))), "contains multiple groups")
})

test_that("error: mult.level = FALSE", {
  skip_on_cran()
  df <- df.OR.multi
  expect_error(.quiet(umbrella(df)), "Please, check that it is not a repeated entry. If not, indicate that you have multivariate data by specfying 'mult.level = TRUE' as an argument of the 'umbrella' function.")
})

##########################
#### different inputs ####
##########################

##### SMD -----------------
test_that("different measures lead to similar results: CI vs means/SD", {
  skip_on_cran()
  dfsmd <- df.SMD

  tmp = with(dfsmd, .estimate_d_from_means(n_cases, n_controls,
                                            mean_cases, sd_cases,
                                            mean_controls, sd_controls))

  dfsmd$value = tmp$value
  dfsmd$se = tmp$se
  dfsmd$ci_lo = dfsmd$value - dfsmd$se * qt(0.975, dfsmd$n_cases + dfsmd$n_controls - 2)
  dfsmd$ci_up = dfsmd$value + dfsmd$se * qt(0.975, dfsmd$n_cases + dfsmd$n_controls - 2)


  dfwoVAL <- subset(dfsmd, select = -c(value, se, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfwoVAL, seed = 4321))
  expect_equal(umb1$Pharmacological$random, umb2$Pharmacological$random, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$heterogeneity, umb2$Pharmacological$heterogeneity, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: SMD + SE vs means/SD", {
  dfsmd <- df.SMD
  tmp = with(dfsmd, .estimate_d_from_means(n_cases, n_controls,
                                           mean_cases, sd_cases,
                                           mean_controls, sd_controls))

  dfsmd$value = tmp$value
  dfsmd$se = tmp$se
  dfsmd$ci_lo = dfsmd$value - dfsmd$se * qt(0.975, dfsmd$n_cases + dfsmd$n_controls - 2)
  dfsmd$ci_up = dfsmd$value + dfsmd$se * qt(0.975, dfsmd$n_cases + dfsmd$n_controls - 2)

  dfwomean <- subset(df.SMD, select = -c(ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfwomean, seed = 4321))
  expect_equal(umb1$Pharmacological$random, umb2$Pharmacological$random, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$heterogeneity, umb2$Pharmacological$heterogeneity, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: SMD + var vs means/SD", {
  skip_on_cran()
  dfsmd <- df.SMD
  dfwomean <- subset(df.SMD, select = -c(se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  dfwomean$var <- with(dfwomean, 1 / n_cases + 1 / n_controls)
  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfwomean, seed = 4321))
  expect_equal(umb1$Pharmacological$random, umb2$Pharmacological$random, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$heterogeneity, umb2$Pharmacological$heterogeneity, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: SMD w/o CI vs means/SD", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$measure <- "SMD"

  dfwoCI <- subset(df.SMD, select = -c(se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfwoCI, seed = 4321))
  expect_equal(umb1$Pharmacological$random, umb2$Pharmacological$random, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$heterogeneity, umb2$Pharmacological$heterogeneity, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: SMD v MD+CI", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$measure <- "SMD"
  dfmd <- df.SMD; dfmd$measure <- "MD"

  dfmd <- metafor::escalc(m1i = mean_cases,
                          sd1i = sd_cases,
                          n1i = n_cases,
                          m2i = mean_controls,
                          sd2i = sd_controls,
                          n2i = n_controls,
                          measure = "MD",
                          data = dfmd)
  dfmd <- subset(dfmd, select = -c(se, mean_cases, mean_controls, sd_cases, sd_controls))

  dfmd$value <- as.numeric(as.character(dfmd$yi))
  dfmd$ci_lo <- dfmd$value - qt(0.975, (dfmd$n_cases + dfmd$n_controls - 2)) * sqrt(dfmd$vi)
  dfmd$ci_up <- dfmd$value + qt(0.975, (dfmd$n_cases + dfmd$n_controls - 2)) * sqrt(dfmd$vi)

  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfmd, seed = 4321))
  expect_equal(umb1$Pharmacological$random, umb2$Pharmacological$random, tolerance = 5e-2)
  expect_equal(umb1$Pharmacological$heterogeneity, umb2$Pharmacological$heterogeneity, tolerance = 6e-2)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: SMD v MD+SE", {
  skip_on_cran()
  dfsmd <- df.SMD;
  dfmd <- df.SMD; dfmd$measure <- "MD"

  dfmd <- metafor::escalc(m1i = mean_cases,
                          sd1i = sd_cases,
                          n1i = n_cases,
                          m2i = mean_controls,
                          sd2i = sd_controls,
                          n2i = n_controls,
                          measure = "MD",
                          data = dfmd)
  dfmd <- subset(dfmd, select = -c(value, se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  dfmd$value <- as.numeric(as.character(dfmd$yi))
  dfmd$var <- as.numeric(as.character(dfmd$vi))

  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfmd, seed = 4321))
  expect_equal(umb1$Pharmacological$random, umb2$Pharmacological$random, tolerance = 5e-2)
  expect_equal(umb1$Pharmacological$heterogeneity, umb2$Pharmacological$heterogeneity, tolerance = 6e-2)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: SMD v G", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$measure <- "SMD"

  dfg <- dfsmd; dfg$measure <- "G"

  dfg$value = .estimate_g_from_d(d = dfsmd$value, n_cases = dfsmd$n_cases, n_controls = dfsmd$n_controls)$value
  dfg$se = .estimate_g_from_d(d = dfsmd$value, n_cases = dfsmd$n_cases, n_controls = dfsmd$n_controls)$se

  dfg <- subset(dfsmd, select = -c(se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfg, seed = 4321))
  expect_equal(umb1$Pharmacological$random, umb2$Pharmacological$random, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$heterogeneity, umb2$Pharmacological$heterogeneity, tolerance = 1e-4)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: SMD v G+SE", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$measure <- "SMD"
  tmp = with(dfsmd, .estimate_d_from_means(n_cases, n_controls,
                                           mean_cases, sd_cases,
                                           mean_controls, sd_controls))

  dfsmd$value = tmp$value
  dfsmd$se = tmp$se
  dfsmd$ci_lo = dfsmd$value - dfsmd$se * qt(0.975, dfsmd$n_cases + dfsmd$n_controls - 2)
  dfsmd$ci_up = dfsmd$value + dfsmd$se * qt(0.975, dfsmd$n_cases + dfsmd$n_controls - 2)

  dfg <- dfsmd; dfg$measure <- "G"
  dfg$value = .estimate_g_from_d(d = dfg$value, n_cases = dfg$n_cases, n_controls = dfg$n_controls)$value
  dfg$se = .estimate_g_from_d(d = dfg$value, n_cases = dfg$n_cases, n_controls = dfg$n_controls)$se

  dfg$ci_lo = dfg$value - dfg$se * qt(0.975, dfg$n_cases + dfg$n_controls - 2)
  dfg$ci_up = dfg$value + dfg$se * qt(0.975, dfg$n_cases + dfg$n_controls - 2)


  dfg <- subset(dfg, select = -c(ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfg, seed = 4321))
  expect_equal(umb1$Pharmacological$random, umb2$Pharmacological$random, tolerance = 1e-3)
  expect_equal(umb1$Pharmacological$heterogeneity, umb2$Pharmacological$heterogeneity, tolerance = 1e-3)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-15)
})
test_that("different measures lead to similar results: G with / without CI", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$measure <- "SMD"
  dfg <- df.SMD; dfg$measure <- "G"

  dfg$value = .estimate_g_from_d(d = dfsmd$value, n_cases = dfsmd$n_cases, n_controls = dfsmd$n_controls)$value
  dfg$se = .estimate_g_from_d(d = dfsmd$value, n_cases = dfsmd$n_cases, n_controls = dfsmd$n_controls)$se

  dfg$ci_lo <- dfg$value - qt(0.975, (dfg$n_cases + dfg$n_controls - 2)) * dfg$se
  dfg$ci_up <- dfg$value + qt(0.975, (dfg$n_cases + dfg$n_controls - 2)) * dfg$se
  dfg.comp <- subset(dfg, select = -c(se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- .quiet(umbrella(dfg, seed = 4321))
  umb2 <- .quiet(umbrella(dfg.comp, seed = 4321))
  expect_equal(dfg$Pharmacological$random, dfg.comp$Pharmacological$random, tolerance = 1e-10)
  expect_equal(dfg$Pharmacological$heterogeneity, dfg.comp$Pharmacological$heterogeneity, tolerance = 1e-10)
  expect_equal(umb1$Pharmacological$esb$p.value, umb2$Pharmacological$esb$p.value, tolerance = 1e-16)
})

##### OR -----------------

test_that("different measures lead to similar results: OR n_cases/controls vs. n_exp/n_nexp", {
  skip_on_cran()
  dfor1 <- subset(df.OR, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(n_cases, n_controls, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))

  umb1 <- .quiet(umbrella(dfor1, seed = 4321))
  umb2 <- .quiet(umbrella(dfor2, seed = 4321))
  expect_equal(umb1$ID$random, umb2$ID$random, tolerance = 1e-10)
  expect_equal(umb1$ID$heterogeneity, umb2$ID$heterogeneity, tolerance = 1e-10)
  expect_equal(umb1$ID$esb$p.value, umb2$ID$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: OR n_cases/controls vs. 2x2 table", {
  skip_on_cran()
  dfor1 <- subset(df.OR, factor == "ID", select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, factor == "ID", select = -c(value, ci_lo, ci_up, n_cases, n_controls, n_exp, n_nexp))

  umb1 <- .quiet(umbrella(dfor1, seed = 4321))
  umb2 <- .quiet(umbrella(dfor2, seed = 4321))
  expect_equal(umb1$ID$random, umb2$ID$random, tolerance = 1e-4)
  expect_equal(umb1$ID$heterogeneity, umb2$ID$heterogeneity, tolerance = 1e-3)
  expect_equal(umb1$ID$esb$p.value, umb2$ID$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: OR n_cases/controls SE vs. 2x2 table", {
  skip_on_cran()
  dfor1 <- subset(df.OR, factor == "ID", select = -c(ci_lo, ci_up, n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, factor == "ID", select = -c(value, ci_lo, ci_up, n_cases, n_controls, n_exp, n_nexp))

  dfor1$se <- with(dfor2, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))
  umb1 <- .quiet(umbrella(dfor1, seed = 4321))
  umb2 <- .quiet(umbrella(dfor2, seed = 4321))
  expect_equal(umb1$ID$random, umb2$ID$random, tolerance = 1e-10)
  expect_equal(umb1$ID$heterogeneity, umb2$ID$heterogeneity, tolerance = 1e-10)
  expect_equal(umb1$ID$esb$p.value, umb2$ID$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: OR n_exp/n_nexp vs. 2x2 table", {
  skip_on_cran()
  dfor1 <- subset(df.OR, select = -c(n_cases, n_controls, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(value, ci_lo, ci_up, n_cases, n_controls, n_exp, n_nexp))

  umb1 <- .quiet(umbrella(dfor1, seed = 4321))
  umb2 <- .quiet(umbrella(dfor2, seed = 4321))
  expect_equal(umb1$ID$random, umb2$ID$random, tolerance = 1e-4)
  expect_equal(umb1$ID$heterogeneity, umb2$ID$heterogeneity, tolerance = 1e-3)
  expect_equal(umb1$ID$esb$p.value, umb2$ID$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: OR n_exp/n_nexp SE vs. 2x2 table", {
  skip_on_cran()
  dfor1 <- subset(df.OR, factor == "ID", select = -c(ci_lo, ci_up, n_controls, n_cases, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, factor == "ID", select = -c(value, ci_lo, ci_up, n_cases, n_controls, n_exp, n_nexp))

  dfor1$se <- with(dfor2, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))
  umb1 <- .quiet(umbrella(dfor1, seed = 4321))
  umb2 <- .quiet(umbrella(dfor2, seed = 4321))
  expect_equal(umb1$ID$random, umb2$ID$random, tolerance = 1e-10)
  expect_equal(umb1$ID$heterogeneity, umb2$ID$heterogeneity, tolerance = 1e-10)
  expect_equal(umb1$ID$esb$p.value, umb2$ID$esb$p.value, tolerance = 1e-16)
})

test_that("different measures lead to similar results: OR n_cases/controls with / without CI", {
  skip_on_cran()
  dfor1 <- subset(df.OR, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(ci_lo, ci_up, n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))

  umb1 <- .quiet(umbrella(dfor1, seed = 4321))
  umb2 <- .quiet(umbrella(dfor2, seed = 4321))
  expect_equal(umb1$ID$random, umb2$ID$random, tolerance = 0.8)
  expect_equal(umb1$ID$esb$p.value, umb2$ID$esb$p.value, tolerance = 1e-2)
})

##### RR -----------------

test_that("different measures lead to similar results: RR n_cases/controls vs. 2x2 table", {
  skip_on_cran()
  dfrr1 <- df.RR
  dfrr1$n_cases = dfrr1$n_cases_exp + dfrr1$n_cases_nexp
  dfrr1$n_controls = dfrr1$n_exp + dfrr1$n_nexp - dfrr1$n_cases
  dfrr1 <- subset(dfrr1, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp))
  dfrr2 <- df.RR

  umb1 <- .quiet(umbrella(dfrr1, seed = 4321))
  umb2 <- .quiet(umbrella(dfrr2, seed = 4321))
  expect_equal(umb1$SSRI$random, umb2$SSRI$random, tolerance = 1e-4)
  expect_equal(umb1$SSRI$esb$p.value, umb2$SSRI$esb$p.value, tolerance = 1e-8)
})

test_that("different measures lead to similar results: RR n_cases/controls SE vs. RR n_cases/controls CI", {
  skip_on_cran()
  dfrr1 <- df.RR
  dfrr1$n_cases = dfrr1$n_cases_exp + dfrr1$n_cases_nexp
  dfrr1$n_controls = dfrr1$n_exp + dfrr1$n_nexp - dfrr1$n_cases
  dfrr1$se = with(dfrr1, sqrt(1 / n_cases_exp - 1 / n_exp + 1 / n_cases_nexp - 1 / n_nexp))
  dfrr1$ci_lo = with(dfrr1, value / exp(qnorm(0.975) * se))
  dfrr1$ci_up = with(dfrr1, value * exp(qnorm(0.975) * se))
  dfrr2 <- subset(dfrr1, select = -c(se, n_exp, n_nexp, n_cases_exp, n_cases_nexp))
  dfrr1 <- subset(dfrr1, select = -c(ci_lo, ci_up, n_exp, n_nexp, n_cases_exp, n_cases_nexp))

  umb1 <- .quiet(umbrella(dfrr1, seed = 4321))
  umb2 <- .quiet(umbrella(dfrr2, seed = 4321))
  expect_equal(umb1$SSRI$random, umb2$SSRI$random, tolerance = 1e-7)
  expect_equal(umb1$SSRI$esb$p.value, umb2$SSRI$esb$p.value, tolerance = 1e-10)
})


test_that("different measures lead to similar results: RR n_cases/controls vs. classic RR inputs (cases1/N1 and cases2/N2)", {
  skip_on_cran()
  dfrr1 <- df.RR
  dfrr1$n_cases = dfrr1$n_cases_exp + dfrr1$n_cases_nexp
  dfrr1$n_controls = dfrr1$n_exp + dfrr1$n_nexp - dfrr1$n_cases
  dfrr1 <- subset(dfrr1, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp))

  dfrr2 <- df.RR

  umb1 <- .quiet(umbrella(dfrr1, seed = 4321))
  umb2 <- .quiet(umbrella(dfrr2, seed = 4321))
  expect_equal(umb1$SSRI$random, umb2$SSRI$random, tolerance = 1e-4)
  expect_equal(umb1$SSRI$esb$p.value, umb2$SSRI$esb$p.value, tolerance = 1e-8)
})

##### IRR -----------------

test_that("different measures lead to similar results: IRR n_cases/time vs. n_cases_exp-nexp/time-exp/nexp", {
  skip_on_cran()
  dfirr1 <- subset(df.IRR, select = -c(n_cases_exp, n_cases_nexp, time_exp, time_nexp))
  dfirr2 <- subset(df.IRR, select = -c(n_cases, time))

  umb1 <- .quiet(umbrella(dfirr1, seed = 4321))
  umb2 <- .quiet(umbrella(dfirr2, seed = 4321))
  expect_equal(umb1$Smoking$random, umb2$Smoking$random, tolerance = 1e-5)
  expect_equal(umb1$Smoking$esb$p.value, umb2$Smoking$esb$p.value, tolerance = 1e-10)
})

test_that("different measures lead to similar results: IRR n_cases/time vs. n_cases_exp-nexp/time-exp/nexp", {
  skip_on_cran()
  dfirr1 <- subset(df.IRR, select = -c(n_cases_exp, n_cases_nexp, time_exp, time_nexp))
  dfirr2 <- subset(df.IRR, select = -c(ci_lo, ci_up, n_cases_exp, n_cases_nexp, time_exp, time_nexp))
  dfirr2$var = with(df.IRR, 1 / n_cases_exp + 1 / n_cases_nexp)

  umb1 <- .quiet(umbrella(dfirr1, seed = 4321))
  umb2 <- .quiet(umbrella(dfirr2, seed = 4321))
  expect_equal(umb1$Smoking$random, umb2$Smoking$random, tolerance = 1e-5)
  expect_equal(umb1$Smoking$esb$p.value, umb2$Smoking$esb$p.value, tolerance = 1e-10)
})



### REVERSE ES

test_that("reverse ES leads to similar results: SMD", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfsmd))
  umb2 <- .quiet(umbrella(df.SMD))
  expect_equal(umb1[[1]]$random[,1], -umb2[[1]]$random[,1], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,2], -umb2[[1]]$random[,2], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,3], umb2[[1]]$random[,3], tolerance = 1e-8)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 1e-8)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 1e-8)
})

test_that("reverse ES leads to similar results: OR", {
  skip_on_cran()
  dfor <- df.OR; dfor$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfor, seed = 4321))
  umb2 <- .quiet(umbrella(df.OR, seed = 4321))
  expect_equal(umb1[[1]]$random[,1], -umb2[[1]]$random[,1], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,2], -umb2[[1]]$random[,2], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,3], umb2[[1]]$random[,3], tolerance = 1e-8)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 1e-8)
  # verify
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 5e-3)
})

test_that("reverse ES leads to similar results: RR", {
  skip_on_cran()
  dfrr <- df.RR; dfrr$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfrr, seed = 4321))
  umb2 <- .quiet(umbrella(df.RR, seed = 4321))
  expect_equal(umb1[[1]]$random[,1], -umb2[[1]]$random[,1], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,2], -umb2[[1]]$random[,2], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,3], umb2[[1]]$random[,3], tolerance = 1e-8)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 1e-8)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 1e-8)
})

test_that("reverse ES leads to similar results: HR", {
  skip_on_cran()
  dfhr <- df.HR; dfhr$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfhr, seed = 4321))
  umb2 <- .quiet(umbrella(df.HR, seed = 4321))
  expect_equal(umb1[[1]]$random[,1], -umb2[[1]]$random[,1], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,2], -umb2[[1]]$random[,2], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,3], umb2[[1]]$random[,3], tolerance = 1e-8)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 1e-8)
  # verify
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 1e-1)
})

test_that("reverse ES leads to similar results: IRR", {
  skip_on_cran()
  dfirr <- df.IRR; dfirr$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfirr, seed = 4321))
  umb2 <- .quiet(umbrella(df.IRR, seed = 4321))
  expect_equal(umb1[[1]]$random[,1], -umb2[[1]]$random[,1], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,2], -umb2[[1]]$random[,2], tolerance = 1e-8)
  expect_equal(umb1[[1]]$random[,3], umb2[[1]]$random[,3], tolerance = 1e-8)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 1e-8)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 1e-8)
})


### COMBINED MULTIPLE_ES + SHARED_controls

test_that("umbrella correctly handle multiple_es + shared_controls", {
  skip_on_cran()
  df <- df.SMD[1:4,]
  df$shared_controls <- rep(1, 4)
  df$multiple_es <- rep("outcomes", 4)
  df$author[c(2, 4)] <- df$author[c(1,3)]
  df$year[c(2, 4)] <- df$year[c(1,3)]
  df$n_controls <- 60
  df$n_cases <- 100

  umb1 <- .quiet(umbrella(df, mult.level = TRUE, seed = 4321))


  d = .estimate_d_from_means(df$n_cases, df$n_controls / 2, df$mean_cases,
                         df$sd_cases, df$mean_controls, df$sd_controls)

  # test that umbrella correctly estimate SMD from corrected sample size
  expect_equal(umb1[[1]]$x_multi$value, d$value, tolerance = 1e-13)
  expect_equal(umb1[[1]]$x_multi$se, d$se, tolerance = 1e-13)


  df.m <- df; df.m$n_controls = df.m$n_controls / 2


  g.umb = .estimate_g_from_d(umb1[[1]]$x_multi$value, umb1[[1]]$x_multi$n_cases, umb1[[1]]$x_multi$n_controls)
  ## metafor
  df.mfr <- metafor::escalc(m1i = mean_cases,
                                sd1i = sd_cases,
                                n1i = n_cases,
                                m2i = mean_controls,
                                sd2i = sd_controls,
                                n2i = n_controls, data = df.m, measure = "SMD", vtype = "UB")

  expect_equal(g.umb$value, .as_numeric(df.mfr$yi), tolerance = 1e-13)
  expect_equal(g.umb$se, sqrt(df.mfr$vi), tolerance = 1e-13)


  df.agg.mfr <- metafor::aggregate.escalc(df.mfr,
                                          cluster = author,
                                          struct = "CS",
                                          weighted = FALSE,
                                          rho = 0.5)
  g.umb2 = .estimate_g_from_d(umb1[[1]]$x$value, umb1[[1]]$x$n_cases, umb1[[1]]$x$n_controls)

  expect_equal(g.umb2$value, .as_numeric(df.agg.mfr$yi), tolerance = 1e-13)
  expect_equal(g.umb2$se, sqrt(df.agg.mfr$vi), tolerance = 5e-1)

  df_test <- umb1[[1]]$x_multi
  df_test$value <- g.umb$value
  df_test$se <- g.umb$se

  agg <- .agg_data(df_test, measure = "SMD", r = 0.5)

  # approximations in previous calculations are due to the fact that we aggregate SMD while metafor aggregate G
  expect_equal(agg$se, sqrt(df.agg.mfr$vi), tolerance = 1e-13)

})

test_that("umbrella correctly handle multiple_es + shared_controls", {
  skip_on_cran()
  df <- df.OR[1:5,]
  df$shared_controls <- c(rep(1, 4), NA)
  df$multiple_es <- c(rep("outcomes", 4), NA)
  df$author[c(2, 4)] <- df$author[c(1,3)]
  df$year[c(2, 4)] <- df$year[c(1,3)]

  umb1 <- .quiet(umbrella(df, mult.level = TRUE, seed = 4321))


  or = .estimate_or_from_n(df$n_cases_exp[1:4], df$n_cases_nexp[1:4] ,
                           df$n_controls_exp[1:4]/ 2,
                           df$n_controls_nexp[1:4] / 2)

  # test that umbrella correctly estimate SMD from corrected sample size
  expect_equal(umb1[[1]]$x_multi$value[1:4], or$value, tolerance = 1e-13)
  expect_equal(umb1[[1]]$x_multi$se[1:4], or$se, tolerance = 1e-13)

  df.m <- df
  df.m$n_controls_exp[1:4] = df.m$n_controls_exp[1:4] / 2
  df.m$n_controls_nexp[1:4] = df.m$n_controls_nexp[1:4] / 2

  ## metafor
  df.mfr <- metafor::escalc(ai = n_cases_exp,
                            bi = n_cases_nexp,
                            ci = n_controls_exp,
                            di = n_controls_nexp,
                            data = df.m, measure = "OR")

  expect_equal(umb1[[1]]$x_multi$value, exp(.as_numeric(df.mfr$yi)), tolerance = 1e-13)
  expect_equal(umb1[[1]]$x_multi$se, sqrt(df.mfr$vi), tolerance = 1e-13)


  df.agg.mfr <- metafor::aggregate.escalc(df.mfr,
                                          cluster = author,
                                          struct = "CS",
                                          weighted = FALSE,
                                          rho = 0.5)

  expect_equal(umb1[[1]]$x$value, exp(.as_numeric(df.agg.mfr$yi)), tolerance = 1e-13)
  expect_equal(umb1[[1]]$x$se, sqrt(df.agg.mfr$vi), tolerance =1e-13)

})



# x<-dfmd; method.var = "REML"; mult.level = FALSE; r = 0.5; true_effect = "largest"; factor="Pharmacological"

