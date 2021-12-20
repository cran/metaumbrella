test_that("publication bias correclty performs egger's test", {
  df.OR$se <- with(df.OR, (log(ci_up) - log(ci_lo)) / 3.92)

  df.or <- subset(df.OR, factor == "ADHD")
  df.smd <- subset(df.SMD, factor == "Pharmacological")

  rma1 <-  metafor::rma.uni(m1i = mean_cases, m2i = mean_controls,
                            sd1i = sd_cases, sd2i = sd_controls,
                            n1i = n_cases, n2i = n_controls,
                            data = df.smd, method = "REML", measure = "SMD", vtype="UB")

  rma2 <- metafor::rma.uni(ai = n_cases_exp, bi = n_cases_nexp,
                           ci = n_controls_exp, di = n_controls_nexp,
                           data = df.or, method = "REML", measure = "OR")

  umb1 <- umbrella(df.smd)
  umb2 <- umbrella(df.or)

  res_rma1 <- metafor::regtest(rma1, model="lm", predictor="sei")
  res_rma2 <- metafor::regtest(rma2, model="lm", predictor="sei")

  expect_equal(umb1[[1]]$egger$p.value, res_rma1$pval)

  expect_equal(umb2[[1]]$egger$p.value, res_rma2$pval)

  res1 <- .egger_pb(.as_numeric(rma1$yi), sqrt(rma1$vi), measure = "G")
  res2 <- .egger_pb(.as_numeric(rma2$yi), sqrt(rma2$vi), measure = "G")

  df.smd2 <- subset(df.SMD, factor == "Pharmacological", select = -c(mean_cases, mean_controls))
  df.or2 <- subset(df.OR, factor == "ADHD", select = -c(n_cases_exp, n_cases_nexp, n_controls_exp))

  df.smd2$value <- .as_numeric(rma1$yi)
  df.smd2$se <- sqrt(rma1$vi)
  df.smd2$measure = "G"

  umb <- umbrella(df.smd2, method.var = "REML")

  expect_equal(umb[[1]]$egger$p.value, res_rma1$pval)


  umb <- umbrella(df.or2, method.var = "REML")

  expect_equal(umb[[1]]$egger$p.value, res_rma2$pval)
})
