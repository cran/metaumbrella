tol_large = 1e-10
tol_med = 1e-6
tol_ok = 1e-3
tol_limit = 1e-2
tol_obs = 1e-1
tol_dif = 5e-1
tol_force = 1

test_that("esb.test produced same results for different inputs: SMD", {
    skip_on_cran()
    df <- df.SMD[df.SMD$factor == "Surgical", ]

    metasmd <- meta::metacont(n_cases, mean_cases, sd_cases,
                              n_controls, mean_controls, sd_controls,
                              method.tau = "REML", data = df,
                              sm = "SMD", method.smd = "Hedges")

    metasmd2 <- meta::metacont(n_cases, mean_cases, sd_cases,
                              n_controls, mean_controls, sd_controls,
                              method.tau = "REML", data = df,
                              sm = "SMD", method.smd = "Cohen")

    rmasmd <- metafor::rma.uni(m1i = mean_cases, m2i = mean_controls,
                                sd1i = sd_cases, sd2i = sd_controls,
                                n1i = n_cases, n2i = n_controls,
                                data = df, method = "REML", measure = "SMD", vtype = "UB")

    df$value = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)$value
    df$se = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)$se

    df$measure <- "G"
    esb.df.chisq <- suppressWarnings(.quiet(esb.test(df, input = "dataframe", measure = "G", method.esb = "IT.chisq", seed = 4321, true_effect = "largest")))
    esb.df.binom <- .quiet(esb.test(df, input = "dataframe", measure = "G", method.esb = "IT.binom",
                                    seed = 4321, true_effect = "largest"))

    meta.df.chisq1 <- suppressWarnings(.quiet(esb.test(metasmd, input = "meta", method.esb = "IT.chisq", seed = 4321, true_effect = "largest")))
    meta.df.binom1 <- .quiet(esb.test(metasmd, input = "meta", method.esb = "IT.binom", seed = 4321, true_effect = "largest"))

    meta.df.chisq2 <- suppressWarnings(.quiet(esb.test(metasmd2, input = "meta", method.esb = "IT.chisq", seed = 4321, true_effect = "largest")))
    meta.df.binom2 <- .quiet(esb.test(metasmd2, input = "meta", method.esb = "IT.binom", seed = 4321, true_effect = "largest"))

    rma.df.chisq <- suppressWarnings(.quiet(esb.test(rmasmd, input = "rma", n_cases = df$n_cases, method.esb = "IT.chisq", seed = 4321, true_effect = "largest")))
    rma.df.binom <- .quiet(esb.test(rmasmd, input = "rma", n_cases = df$n_cases, method.esb = "IT.binom", seed = 4321, true_effect = "largest"))

    umb <- .quiet(umbrella(df, seed = 4321, method.esb = "IT.binom", true_effect = "largest"))
    umb2 <- .quiet(umbrella(subset(df, select = -c(mean_cases, mean_controls, sd_cases, sd_controls, ci_lo, ci_up)),
                            method.esb = "IT.binom", seed = 4321, true_effect = "largest"))

    # meta
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(meta.df.chisq1$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(meta.df.chisq2$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(meta.df.chisq1$p.value), tolerance = tol_large)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(meta.df.chisq2$p.value), tolerance = tol_large)


    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(meta.df.binom1$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(meta.df.binom2$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(meta.df.binom1$p.value), tolerance = tol_large)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(meta.df.binom2$p.value), tolerance = tol_large)

    # rma
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(rma.df.chisq$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(rma.df.chisq$p.value), tolerance = tol_large)

    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(rma.df.binom$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(rma.df.binom$p.value), tolerance = tol_large)

    # umbrella
    expect_equal(.as_numeric(umb[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(umb[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_large)
    expect_equal(.as_numeric(umb2[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(umb2[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = 1e-3)

})

test_that("esb.test produced same results for generic inputs: SMD", {
    skip_on_cran()
    df <- df.SMD[df.SMD$factor == "Surgical", ]
    df$se <- with(df, .estimate_d_from_means(n_cases, n_controls, mean_cases, sd_cases, mean_controls, sd_controls)$se)
    df$value = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)$value
    df$se = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)$se

    metasmd1 <- meta::metagen(TE = value, seTE = se ,
                              method.tau = "REML", data = df,
                              sm = "SMD")

    metasmd2 <- meta::metagen(TE = value, seTE = se, n.e = n_cases, n.c = n_controls,
                             method.tau = "REML", data = df,
                             sm = "SMD")

    rmasmd <- metafor::rma.uni(yi = value, sei = se, ni = n_cases + n_controls,
                               data = df, method = "REML", measure = "SMD")

    esb.df.chisq <- .quiet(esb.test(df, input = "dataframe", measure = "SMD", method.esb = "IT.chisq", seed = 4321, true_effect = "largest"))
    esb.df.binom <- .quiet(esb.test(df, input = "dataframe", measure = "SMD", method.esb = "IT.binom", seed = 4321, true_effect = "largest"))

    meta.df.chisq1 <- .quiet(esb.test(metasmd1, input = "meta", n_cases = df$n_cases, n_controls = df$n_controls, method.esb = "IT.chisq", seed = 4321, true_effect = "largest"))
    meta.df.binom1 <- .quiet(esb.test(metasmd1, input = "meta", n_cases = df$n_cases, n_controls = df$n_controls, method.esb = "IT.binom", seed = 4321, true_effect = "largest"))
    meta.df.chisq2 <- .quiet(esb.test(metasmd2, input = "meta", method.esb = "IT.chisq", seed = 4321, true_effect = "largest"))
    meta.df.binom2 <- .quiet(esb.test(metasmd2, input = "meta", method.esb = "IT.binom", seed = 4321, true_effect = "largest"))

    rma.df.chisq1 <- .quiet(esb.test(rmasmd, input = "rma", n_cases = df$n_cases, method.esb = "IT.chisq", seed = 4321, true_effect = "largest"))
    rma.df.chisq2 <- .quiet(esb.test(rmasmd, input = "rma", n_controls = df$n_controls, method.esb = "IT.chisq", seed = 4321, true_effect = "largest"))
    rma.df.binom1 <- .quiet(esb.test(rmasmd, input = "rma", n_cases = df$n_cases, method.esb = "IT.binom", seed = 4321, true_effect = "largest"))
    rma.df.binom2 <- .quiet(esb.test(rmasmd, input = "rma", n_controls = df$n_controls, method.esb = "IT.binom", seed = 4321, true_effect = "largest"))

    umb <- .quiet(umbrella(df, seed = 4321,  method.esb = "IT.binom", true_effect = "largest"))

    # meta
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(meta.df.chisq1$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(meta.df.chisq2$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(meta.df.chisq1$p.value), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(meta.df.chisq2$p.value), tolerance = tol_med)


    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(meta.df.binom1$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(meta.df.binom2$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(meta.df.binom1$p.value), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(meta.df.binom2$p.value), tolerance = tol_med)

    # rma
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(rma.df.chisq1$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(rma.df.chisq1$p.value), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(rma.df.chisq2$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(rma.df.chisq2$p.value), tolerance = tol_med)

    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(rma.df.binom1$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(rma.df.binom1$p.value), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(rma.df.binom2$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(rma.df.binom2$p.value), tolerance = tol_med)

    # umbrella
    expect_equal(.as_numeric(umb[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(umb[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_med)
})

test_that("esb.test produced same results for different inputs: OR", {
    skip_on_cran()
    df <- df.OR[df.OR$factor == "ASD", ]
    metaor <- meta::metabin(event.e = n_cases_exp, n.e = n_exp, event.c = n_cases_nexp, n.c = n_nexp,
                              data = df, sm = "OR", method.tau = "REML")

    rmaor <- metafor::rma.uni(ai = n_cases_exp, bi = n_controls_exp,
                               ci = n_cases_nexp, di = n_controls_nexp,
                               n1i = n_cases, n2i = n_controls,
                               data = df, method = "REML", measure = "OR")

    esb.df.chisq <- suppressWarnings(.quiet(esb.test(df, input = "dataframe", measure = "OR", method.esb = "IT.chisq", true_effect = "largest", seed = 4321)))
    esb.df.binom <- .quiet(esb.test(df, input = "dataframe", measure = "OR", method.esb = "IT.binom", true_effect = "largest", seed = 4321))

    meta.df.chisq <- suppressWarnings(.quiet(esb.test(metaor, input = "meta", method.esb = "IT.chisq", true_effect = "largest", seed = 4321)))
    meta.df.binom <- .quiet(esb.test(metaor, input = "meta", method.esb = "IT.binom", true_effect = "largest", seed = 4321))

    rma.df.chisq <- suppressWarnings(.quiet(esb.test(rmaor, input = "rma", n_cases = df$n_cases, method.esb = "IT.chisq", true_effect = "largest", seed = 4321)))
    rma.df.binom <- .quiet(esb.test(rmaor, input = "rma", n_cases = df$n_cases, method.esb = "IT.binom", true_effect = "largest", seed = 4321))

    umb <- .quiet(umbrella(df, seed = 4321,method.esb = "IT.binom", true_effect = "largest"))

    # meta
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(meta.df.chisq$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(meta.df.chisq$p.value), tolerance = tol_med)


    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(meta.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(meta.df.binom$p.value), tolerance = tol_med)

    # rma
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(rma.df.chisq$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(rma.df.chisq$p.value), tolerance = tol_med)

    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(rma.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(rma.df.binom$p.value), tolerance = tol_med)

    # umbrella
    expect_equal(.as_numeric(umb[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(umb[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_med)

})

test_that("esb.test produced same results for generic inputs: OR", {
    skip_on_cran()
    df <- df.OR[df.OR$factor == "ASD", ]
    df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp)$se)

    metaor <- meta::metagen(TE = log(df$value), seTE = df$se,
                            data = df, sm = "OR", method.tau = "REML")

    rmaor <- metafor::rma.uni(yi = log(df$value), sei = df$se,
                              data = df, method = "REML", measure = "OR")

    esb.df.chisq <- suppressWarnings(.quiet(esb.test(df, input = "dataframe", measure = "OR", method.esb = "IT.chisq",
                                                     true_effect = "largest",seed = 4321)))
    esb.df.binom <- .quiet(esb.test(df, input = "dataframe", measure = "OR", method.esb = "IT.binom",
                                    true_effect = "largest",seed = 4321))

    meta.df.chisq <- suppressWarnings(.quiet(esb.test(metaor, n_cases = df$n_cases, n_controls = df$n_controls, input = "meta", method.esb = "IT.chisq",
                                                      true_effect = "largest", seed = 4321)))
    meta.df.binom <- .quiet(esb.test(metaor, input = "meta", n_cases = df$n_cases, n_controls = df$n_controls, method.esb = "IT.binom",
                                     true_effect = "largest", seed = 4321))

    rma.df.chisq <- suppressWarnings(.quiet(esb.test(rmaor, input = "rma", n_cases = df$n_cases, n_controls = df$n_controls, method.esb = "IT.chisq",
                                                     true_effect = "largest", seed = 4321)))
    rma.df.binom <- .quiet(esb.test(rmaor, input = "rma", n_cases = df$n_cases, n_controls = df$n_controls, method.esb = "IT.binom",
                                    true_effect = "largest", seed = 4321))

    umb <- .quiet(umbrella(df, seed = 4321, method.esb = "IT.binom",true_effect = "largest"))
    umb2 <- .quiet(umbrella(subset(df, select = -c(n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp)), method.esb = "IT.binom",
                            true_effect = "largest", seed = 4321))

    # meta
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(meta.df.chisq$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(meta.df.chisq$p.value), tolerance = tol_med)


    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(meta.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(meta.df.binom$p.value), tolerance = tol_med)

    # rma
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(rma.df.chisq$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(rma.df.chisq$p.value), tolerance = tol_med)

    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(rma.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(rma.df.binom$p.value), tolerance = tol_med)

    # umbrella
    expect_equal(.as_numeric(umb[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(umb[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_med)
    # umbrella
    expect_equal(.as_numeric(umb2[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(umb2[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_med)
})

test_that("esb.test produced correct results with reversed inputs", {
    skip_on_cran()
    df <- df.SMD[df.SMD$factor == "Surgical", ]
    df$reverse_es <- NA; df$reverse_es[1:5] <- "reverse"

    gen <- .quiet(esb.test(df, input = "dataframe", measure = "SMD", seed = 4321, method.esb = "IT.binom", true_effect = "largest"))

    umb <- .quiet(umbrella(df, seed = 4321, method.esb = "IT.binom", true_effect = "largest"))

    expect_equal(.as_numeric(gen$statistic), .as_numeric(umb[[1]]$esb$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(gen$p.value), .as_numeric(umb[[1]]$esb$p.value), tolerance = tol_med)
})

test_that("esb.test produced correct results with multilevel data", {
    skip_on_cran()
    df <- subset(df.OR.multi, factor == "Vitamin D")

    gen <- .quiet(esb.test(df, input = "dataframe", measure = "OR", seed = 4321, method.esb = "IT.binom", true_effect = "largest"))

    umb <- .quiet(umbrella(df, mult.level = TRUE, seed = 4321, method.esb = "IT.binom", true_effect = "largest"))

    expect_equal(.as_numeric(gen$statistic), .as_numeric(umb[[1]]$esb$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(gen$p.value), .as_numeric(umb[[1]]$esb$p.value), tolerance = tol_large)
})
# G/SMD input -----------------------
test_that("SMD/G/SMC", {

  df.test.smd = subset(df.SMD, factor == "Pharmacological")[, 1:9]

  df.test.smc = df.test.smd
  df.test.smc$measure = "SMC"
  tmp = .estimate_g_from_d(d=df.test.smc$value,
                           n_cases = df.test.smc$n_cases,
                           n_controls = df.test.smc$n_controls)
  df.test.smc$value = tmp$value
  df.test.smc$se = tmp$se

  df.test.g = df.test.smc
  df.test.g$measure = "G"

  ur_smc = umbrella(df.test.smc, method.esb = "TESSPSST", true_effect = "UWLS",
                    verbose=FALSE)
  ur_g = umbrella(df.test.g, method.esb = "TESSPSST", true_effect = "UWLS",
                  verbose=FALSE)
  ur_smd = umbrella(df.test.smd, method.esb = "TESSPSST", true_effect = "UWLS",
                    verbose=FALSE)
  tau2 = ur_smd[[1]]$heterogeneity$tau2
  esb_smc = esb.test(df.test.smc,
                     method.esb = "TESSPSST", true_effect = "UWLS",
                     input = "dataframe", tau2 = tau2)
  esb_g = esb.test(df.test.g,
                   method.esb = "TESSPSST", true_effect = "UWLS",
                   input = "dataframe", tau2 = tau2)
  esb_smd = esb.test(df.test.smd,
                     method.esb = "TESSPSST", true_effect = "UWLS",
                     input = "dataframe", tau2 = tau2)

  expect_equal(ur_smc[[1]]$esb$p.value, esb_smc$p.value, tolerance = tol_large)
  expect_equal(ur_g[[1]]$esb$p.value, esb_g$p.value, tolerance = tol_large)
  expect_equal(ur_smd[[1]]$esb$p.value, esb_smd$p.value, tolerance = tol_large)
  expect_equal(ur_smc[[1]]$esb$p.value, ur_g[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(ur_smc[[1]]$esb$p.value, ur_smd[[1]]$esb$p.value, tolerance = tol_large)
})

# TES metafor -----------------------------------------------------------------------------------------------------
test_that("esb.test produced correct results compared to metafor: SMD", {
    skip_on_cran()
    theta = 0.3
    df <- subset(df.SMD, factor == "Pharmacological")

    meta <- metafor::rma.uni(m1i = mean_cases, m2i = mean_controls,
                             sd1i = sd_cases, sd2i = sd_controls,
                             n1i = n_cases, n2i = n_controls,
                             data = df, method = "REML", measure = "SMD")

    df$value = .as_numeric(meta$yi)
    df$se = sqrt(.as_numeric(meta$vi))

    df <- subset(df, select = -c(ci_lo, ci_up, mean_cases, mean_controls))

    tes.chi <- .quiet(metafor::tes(x = df$value, sei = df$se, theta = theta,
                                   test = "chi2", alternative = "two.sided", tes.alternative = "greater"))
    tes.binom <- .quiet(metafor::tes(x = df$value, sei = df$se, theta = theta,
                                     test = "binom", alternative = "two.sided", tes.alternative = "greater"))


    esb.chi <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "SMD", input = "dataframe",
                        method.esb = "IT.chisq",
                        true_effect = theta, seed = 4321)))

    esb.bin <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "SMD", input = "dataframe",
                        method.esb = "IT.binom",
                        true_effect = theta, seed = 4321)))

    expect_equal(.as_numeric(esb.bin$power), tes.binom$power, tolerance = 0.005)
    expect_equal(.as_numeric(esb.bin$mean_power), mean(tes.binom$power), tolerance = 0.005)
    expect_equal(.as_numeric(esb.bin$SS), tes.binom$O)
    expect_equal(.as_numeric(esb.bin$Esig), tes.binom$E, tolerance = 0.005)
    expect_equal(.as_numeric(esb.bin$k), tes.binom$k)

    expect_equal(.as_numeric(esb.chi$power), tes.chi$power, tolerance = 0.005)
    expect_equal(.as_numeric(esb.chi$mean_power), mean(tes.chi$power), tolerance = 0.005)
    expect_equal(.as_numeric(esb.chi$SS), tes.chi$O)
    expect_equal(.as_numeric(esb.chi$Esig), tes.chi$E, tolerance = 0.005)
    expect_equal(.as_numeric(esb.chi$k), tes.chi$k)

    expect_equal(.as_numeric(binom.test(tes.binom$O, tes.binom$k, mean(tes.binom$power), alternative = "greater")$p.value),
                 .as_numeric(tes.binom$pval))

    expect_equal(.as_numeric(prop.test(tes.chi$O, tes.chi$k, p = mean(tes.chi$power), alternative = "greater", correct = FALSE)$p.value),
                 .as_numeric(tes.chi$pval), tolerance = tol_large)

    expect_equal(.as_numeric(esb.bin$p.value), .as_numeric(tes.binom$pval), tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$p.value), .as_numeric(tes.chi$pval), tolerance = 0.05)
})

test_that("esb.test produced correct results compared to metafor: OR", {
    skip_on_cran()
    theta = 0.5
    df <- subset(df.OR, factor == "ADHD")
    df$value <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp)$value)
    df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp)$se)
    df <- subset(df, select = -c(ci_lo, ci_up, n_cases_exp, n_controls_exp))

    tes.chi <- .quiet(metafor::tes(x = log(df$value), sei = df$se, theta = theta, test = "chi2",
                                   alternative = "two.sided", tes.alternative = "greater"))
    tes.binom <- .quiet(metafor::tes(x = log(df$value), sei = df$se,  theta = theta, test = "binom",
                                     alternative = "two.sided", tes.alternative = "greater"))


    esb.chi <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "OR", input = "dataframe",
                        method.esb = "IT.chisq",
                        true_effect = exp(theta), seed = 4321)))

    esb.bin <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "OR", input = "dataframe",
                        method.esb = "IT.binom",
                        true_effect = exp(theta), seed = 4321)))

    expect_equal(.as_numeric(esb.bin$power), tes.binom$power, tolerance = 0.05)
    expect_equal(.as_numeric(esb.bin$mean_power), mean(tes.binom$power), tolerance = 0.05)
    expect_equal(.as_numeric(esb.bin$SS), tes.binom$O)
    expect_equal(.as_numeric(esb.bin$Esig), tes.binom$E, tolerance = 0.05)
    expect_equal(.as_numeric(esb.bin$k), tes.binom$k)

    expect_equal(.as_numeric(esb.chi$power), tes.chi$power, tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$mean_power), mean(tes.chi$power), tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$SS), tes.chi$O)
    expect_equal(.as_numeric(esb.chi$Esig), tes.chi$E, tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$k), tes.chi$k)

    expect_equal(.as_numeric(binom.test(tes.binom$O, tes.binom$k, mean(tes.binom$power), alternative = "greater")$p.value),
                 .as_numeric(tes.binom$pval))

    expect_equal(.as_numeric(prop.test(tes.chi$O, tes.chi$k, p = mean(tes.chi$power), alternative = "greater", correct = FALSE)$p.value),
                 .as_numeric(tes.chi$pval))

    expect_equal(.as_numeric(esb.bin$p.value), .as_numeric(tes.binom$pval), tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$p.value), .as_numeric(tes.chi$pval), tolerance = 0.05)
})

######################
# test pess tess


test_that("TESS/PSST", {
    TESS_PESS = function(d, sed, tau2, n_cases, n_controls, measure ) {
        HetVar = tau2
        k = length(d)
        t = d/sed
        Precision=1/sed
        Precision_sq=1/sed^2
        reg = lm(t ~ 0 + Precision)
        UWLS = as.numeric(reg$coefficients)
        zz=((1.96*sed-UWLS)/(sed^2+HetVar)^.5)
        Esigtot = sum(1-pnorm(zz))

        #Get intermediate calculations for Eq. 5
        # SS = (t>1.96)*1
        if (measure == "SMD") {
            SS = .two_tail(pt(d / sed, n_cases + n_controls - 2)) < .05
        } else {
            SS = .two_tail(pnorm(d/sed)) < .05
        }
        SStot = sum(SS)
        Pss = SStot/k
        ESS=(SStot-Esigtot)/k
        Pe = Esigtot/k

        #Eq. 5, PSST
        PSST=(Pss-Pe)/(Pe*(1-Pe)/k)^.5

        #Eq. 6, TESS
        TESS= (ESS-.05)/(.0475/k)^.5

        #Logical of whether PSST and TESS are
        #statistically significant given one-tailed
        #test and alpha = 0.05
        PSST_Sig = 1 - pnorm(PSST)
        TESS_Sig = 1 - pnorm(TESS)

        return(list(SS = SStot,
                    Pss = Pss,
                    ESS = ESS,
                    Esig = Esigtot,
                    Pe = Pe,
                    UWLS = UWLS,
                    power = 1-pnorm(zz),
                    PSST = PSST,
                    TESS= TESS,
                    PSST_Sig = PSST_Sig,
                    TESS_Sig = TESS_Sig))
    }


    # R ---------------------------------------------
    df = subset(df.R, factor == "gestational_diabetes")
    df$measure = "Z"
    df$se = (1/(df$n_sample - 3))

    umb.PSST = umbrella(df, true_effect = "UWLS", method.esb = "PSST", verbose = FALSE)
    umb.TESS = umbrella(df, true_effect = "UWLS", method.esb = "TESS", verbose = FALSE)
    umb.TESSPSST = umbrella(df, true_effect = "UWLS", method.esb = "TESSPSST", verbose = FALSE)
    STAN = TESS_PESS(d = df$value, sed = df$se, tau2 = umb.PSST[[1]]$heterogeneity$tau, measure = "R")

    expect_equal(umb.PSST[[1]]$esb$p.value, STAN$PSST_Sig, tolerance = tol_large)
    expect_equal(umb.TESS[[1]]$esb$p.value, STAN$TESS_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value.TESS, STAN$TESS_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value.PSST, STAN$PSST_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value, min(STAN$TESS_Sig, STAN$PSST_Sig), tolerance = tol_large)

    # SMD ---------------------------------------------
    df = subset(df.SMD, factor == "Pharmacological")
    df$se = sqrt(1/df$n_cases + 1/df$n_controls)
    df2=df
    umb.PSST = umbrella(df2, true_effect = "UWLS", method.esb = "PSST", verbose = FALSE)
    umb.TESS = umbrella(df2, true_effect = "UWLS", method.esb = "TESS", verbose = FALSE)
    umb.TESSPSST = umbrella(df2, true_effect = "UWLS", method.esb = "TESSPSST", verbose = FALSE)
    STAN = TESS_PESS(d = df2$value, sed = df2$se,
                     n_cases = df2$n_cases, n_controls = df2$n_controls,
                     tau2 = umb.PSST[[1]]$heterogeneity$tau, measure = "SMD")

    expect_equal(umb.PSST[[1]]$esb$p.value, STAN$PSST_Sig, tolerance = tol_large)
    expect_equal(umb.TESS[[1]]$esb$p.value, STAN$TESS_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value.TESS, STAN$TESS_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value.PSST, STAN$PSST_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value, min(STAN$TESS_Sig, STAN$PSST_Sig), tolerance = tol_large)

    # OR ---------------------------------------------
    df = subset(df.OR, factor == "ADHD")
    df$se = with(df, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))
    df2=df
    umb.PSST = umbrella(df2, true_effect = "UWLS", method.esb = "PSST", verbose = FALSE)
    umb.TESS = umbrella(df2, true_effect = "UWLS", method.esb = "TESS", verbose = FALSE)
    umb.TESSPSST = umbrella(df2, true_effect = "UWLS", method.esb = "TESSPSST", verbose = FALSE)
    STAN = TESS_PESS(d = log(df2$value), sed = df2$se,
                     n_cases = df2$n_cases,
                     tau2 = umb.PSST[[1]]$heterogeneity$tau, measure = "OR")

    expect_equal(umb.PSST[[1]]$esb$p.value, STAN$PSST_Sig, tolerance = tol_large)
    expect_equal(umb.TESS[[1]]$esb$p.value, STAN$TESS_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value.TESS, STAN$TESS_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value.PSST, STAN$PSST_Sig, tolerance = tol_large)
    expect_equal(umb.TESSPSST[[1]]$esb$p.value, min(STAN$TESS_Sig, STAN$PSST_Sig), tolerance = tol_large)
})
