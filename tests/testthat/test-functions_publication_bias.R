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
})

test_that("publication bias for different inputs", {

  df1 <- df2 <- subset(df.SMD, factor == "Pharmacological", select = -c(mean_cases, mean_controls, ci_lo, ci_up))

  G = .estimate_g_from_d(df2$value, df2$n_cases, df2$n_controls)$value
  se = .estimate_g_from_d(df2$value, df2$n_cases, df2$n_controls)$se

  df2$measure = "G"
  df2$value = G
  df2$se = se

  umb1 <- umbrella(df1, method.var = "REML")
  umb2 <- umbrella(df2, method.var = "REML")
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value)
})
