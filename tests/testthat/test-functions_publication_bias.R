test_that("publication bias correclty performs egger's test", {
  df.OR$se <- with(df.OR, (log(ci_up) - log(ci_lo)) / qnorm(0.975))

  df.or <- subset(df.OR, factor == "ADHD")
  df.smd <- subset(df.SMD, factor == "Pharmacological")


  df_mfr = metafor::escalc(m1i = mean_cases, m2i = mean_controls,
                           sd1i = sd_cases, sd2i = sd_controls,
                           n1i = n_cases, n2i = n_controls,
                           data = df.smd, measure = "SMD", vtype = "UB")

  rma1 <- metafor::rma.uni(yi = yi, vi = vi, data = df_mfr, method = "REML")



  rma2 <- metafor::rma.uni(ai = n_cases_exp, bi = n_cases_nexp,
                           ci = n_controls_exp, di = n_controls_nexp,
                           data = df.or, method = "REML", measure = "OR")

  umb1 <- umbrella(df.smd)
  umb2 <- umbrella(df.or)

  res_rma1 <- metafor::regtest(rma1, model="lm", predictor="sei")
  res_rma2 <- metafor::regtest(rma2, model="lm", predictor="sei")

  expect_equal(umb1[[1]]$egger$p.value, res_rma1$pval)

  expect_equal(umb2[[1]]$egger$p.value, res_rma2$pval)

  df.smd2 <- subset(df.SMD, factor == "Pharmacological", select = -c(mean_cases, mean_controls))
  df.or2 <- subset(df.OR, factor == "ADHD", select = -c(n_cases_exp, n_cases_nexp, n_controls_exp))

  umb <- umbrella(df.smd2, method.var = "REML")
  expect_equal(umb[[1]]$egger$p.value, res_rma1$pval)


  umb <- umbrella(df.or2, method.var = "REML")
  expect_equal(umb[[1]]$egger$p.value, res_rma2$pval)


  df.R$factor = "A"
  umb = umbrella(df.R)


  rma_r <- metafor::rma(ri = df.R$value,
                           ni = df.R$n_sample,
                           measure = "ZCOR")

  res_rma_r <- metafor::regtest(rma_r, model="lm", predictor="sei")
  expect_equal(umb[[1]]$egger$p.value, res_rma_r$pval)

})

test_that("publication bias for different inputs", {

  df1 <- df2 <- subset(df.SMD, factor == "Pharmacological",
                       select = -c(mean_cases, mean_controls, ci_lo, ci_up))

  G = .estimate_g_from_d(df2$value, df2$n_cases, df2$n_controls)$value
  se = .estimate_g_from_d(df2$value, df2$n_cases, df2$n_controls)$se

  df2$measure = "G"
  df2$value = G
  df2$se = se

  umb1 <- umbrella(df1, method.var = "REML")
  umb2 <- umbrella(df2, method.var = "REML")
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value)
})

test_that("github bug for 3-level MA", {
  dat = structure(list(study = c("Enock 2014", "Enock 2014", "Pham 2016",  "Pham 2016", "Stolz 2018"), measure = c("SMD", "SMD", "SMD",  "SMD", "SMD"), factor = c("Weisel 2020", "Weisel 2020", "Weisel 2020",  "Weisel 2020", "Weisel 2020"), meta_review = c("Weisel 2020",  "Weisel 2020", "Weisel 2020", "Weisel 2020", "Weisel 2020"),  value = c(0.645, -0.008, 0.0276355552083566, -0.084, 0.694 ), se = c(0.226, 0.14, 0.249, 0.249, 0.228), rob = c("high",  "high", "unclear", "unclear", "low"), n_cases = c(53, 110,  32, 32, 42), n_controls = c(53, 110, 32, 32, 23), multiple_es = c("outcome",  "outcome", "outcome", "outcome", "outcome")), row.names = c(NA,  -5L), class = "data.frame")
  x = dat
  expect_no_error(umbrella(dat, mult.level=TRUE))
})


