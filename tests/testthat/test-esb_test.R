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
                                data = df, method = "REML", measure = "SMD")

    esb.df.chisq <- suppressWarnings(.quiet(esb.test(df, input = "dataframe", measure = "SMD", method = "chisq.test", seed = 4321)))
    esb.df.binom <- .quiet(esb.test(df, input = "dataframe", measure = "SMD", method = "binom.test", seed = 4321))

    meta.df.chisq1 <- suppressWarnings(.quiet(esb.test(metasmd, input = "meta", method = "chisq.test", seed = 4321)))
    meta.df.binom1 <- .quiet(esb.test(metasmd, input = "meta", method = "binom.test", seed = 4321))

    meta.df.chisq2 <- suppressWarnings(.quiet(esb.test(metasmd2, input = "meta", method = "chisq.test", seed = 4321)))
    meta.df.binom2 <- .quiet(esb.test(metasmd2, input = "meta", method = "binom.test", seed = 4321))

    rma.df.chisq <- suppressWarnings(.quiet(esb.test(rmasmd, input = "rma", n_cases = df$n_cases, method = "chisq.test", seed = 4321)))
    rma.df.binom <- .quiet(esb.test(rmasmd, input = "rma", n_cases = df$n_cases, method = "binom.test", seed = 4321))

    umb <- .quiet(umbrella(df))
    umb2 <- .quiet(umbrella(subset(df, select = -c(mean_cases, mean_controls))))

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
    expect_equal(.as_numeric(esb.df.chisq$statistic), .as_numeric(rma.df.chisq$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.chisq$p.value), .as_numeric(rma.df.chisq$p.value), tolerance = tol_med)

    expect_equal(.as_numeric(esb.df.binom$statistic), .as_numeric(rma.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(esb.df.binom$p.value), .as_numeric(rma.df.binom$p.value), tolerance = tol_med)

    # umbrella
    expect_equal(.as_numeric(umb[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(umb[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_med)
    expect_equal(.as_numeric(umb2[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(umb2[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_med)

})

test_that("esb.test produced same results for generic inputs: SMD", {
    skip_on_cran()
    df <- df.SMD[df.SMD$factor == "Surgical", ]
    df$se <- with(df, .estimate_d_from_means(n_cases, n_controls, mean_cases, sd_cases, mean_controls, sd_controls)$se)
    j = with(df, .d_j(n_cases + n_controls - 2))
    metasmd1 <- meta::metagen(TE = value * j, seTE = se * j,
                              method.tau = "REML", data = df,
                              sm = "SMD")

    metasmd2 <- meta::metagen(TE = value * j, seTE = se * j, n.e = n_cases, n.c = n_controls,
                             method.tau = "REML", data = df,
                             sm = "SMD")

    rmasmd <- metafor::rma.uni(yi = value * j, sei = se * j, ni = n_cases + n_controls,
                               data = df, method = "REML", measure = "SMD")

    esb.df.chisq <- .quiet(esb.test(df, input = "dataframe", measure = "SMD", method = "chisq.test", seed = 4321))
    esb.df.binom <- .quiet(esb.test(df, input = "dataframe", measure = "SMD", method = "binom.test", seed = 4321))

    meta.df.chisq1 <- .quiet(esb.test(metasmd1, input = "meta", n_cases = df$n_cases, n_controls = df$n_controls, method = "chisq.test", seed = 4321))
    meta.df.binom1 <- .quiet(esb.test(metasmd1, input = "meta", n_cases = df$n_cases, n_controls = df$n_controls, method = "binom.test", seed = 4321))
    meta.df.chisq2 <- .quiet(esb.test(metasmd2, input = "meta", method = "chisq.test", seed = 4321))
    meta.df.binom2 <- .quiet(esb.test(metasmd2, input = "meta", method = "binom.test", seed = 4321))

    rma.df.chisq1 <- .quiet(esb.test(rmasmd, input = "rma", n_cases = df$n_cases, method = "chisq.test", seed = 4321))
    rma.df.chisq2 <- .quiet(esb.test(rmasmd, input = "rma", n_controls = df$n_controls, method = "chisq.test", seed = 4321))
    rma.df.binom1 <- .quiet(esb.test(rmasmd, input = "rma", n_cases = df$n_cases, method = "binom.test", seed = 4321))
    rma.df.binom2 <- .quiet(esb.test(rmasmd, input = "rma", n_controls = df$n_controls, method = "binom.test", seed = 4321))

    umb <- .quiet(umbrella(df))
    umb2 <- .quiet(umbrella(subset(df, select = -c(mean_cases, mean_controls))))

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
    expect_equal(.as_numeric(umb2[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(umb2[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_med)
})

test_that("esb.test produced same results for different inputs: OR", {
    skip_on_cran()
    df <- df.OR[df.OR$factor == "ASD", ]
    metaor <- meta::metabin(event.e = n_cases_exp, n.e = n_exp, event.c = n_cases_nexp, n.c = n_nexp,
                              data = df, sm = "OR")

    rmaor <- metafor::rma.uni(ai = n_cases_exp, bi = n_controls_exp,
                               ci = n_cases_nexp, di = n_controls_nexp,
                               n1i = n_cases, n2i = n_controls,
                               data = df, method = "REML", measure = "OR")

    esb.df.chisq <- suppressWarnings(.quiet(esb.test(df, input = "dataframe", measure = "OR", method = "chisq.test", seed = 4321)))
    esb.df.binom <- .quiet(esb.test(df, input = "dataframe", measure = "OR", method = "binom.test", seed = 4321))

    meta.df.chisq <- suppressWarnings(.quiet(esb.test(metaor, input = "meta", method = "chisq.test", seed = 4321)))
    meta.df.binom <- .quiet(esb.test(metaor, input = "meta", method = "binom.test", seed = 4321))

    rma.df.chisq <- suppressWarnings(.quiet(esb.test(rmaor, input = "rma", n_cases = df$n_cases, method = "chisq.test", seed = 4321)))
    rma.df.binom <- .quiet(esb.test(rmaor, input = "rma", n_cases = df$n_cases, method = "binom.test", seed = 4321))

    umb <- .quiet(umbrella(df, seed = 4321))
    umb2 <- .quiet(umbrella(subset(df, select = -c(n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp)), seed = 4321))

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
    expect_equal(.as_numeric(umb2[[1]]$esb$statistic), .as_numeric(esb.df.binom$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(umb2[[1]]$esb$p.value), .as_numeric(esb.df.binom$p.value), tolerance = tol_med)

})

test_that("esb.test produced same results for generic inputs: OR", {
    skip_on_cran()
    df <- df.OR[df.OR$factor == "ASD", ]
    df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp)$se)

    metaor <- meta::metagen(TE = log(df$value), seTE = df$se,
                            data = df, sm = "OR")

    rmaor <- metafor::rma.uni(yi = log(df$value), sei = df$se,
                              data = df, method = "REML", measure = "OR")

    esb.df.chisq <- suppressWarnings(.quiet(esb.test(df, input = "dataframe", measure = "OR", method = "chisq.test", seed = 4321)))
    esb.df.binom <- .quiet(esb.test(df, input = "dataframe", measure = "OR", method = "binom.test", seed = 4321))

    meta.df.chisq <- suppressWarnings(.quiet(esb.test(metaor, n_cases = df$n_cases, n_controls = df$n_controls, input = "meta", method = "chisq.test", seed = 4321)))
    meta.df.binom <- .quiet(esb.test(metaor, input = "meta", n_cases = df$n_cases, n_controls = df$n_controls, method = "binom.test", seed = 4321))

    rma.df.chisq <- suppressWarnings(.quiet(esb.test(rmaor, input = "rma", n_cases = df$n_cases, n_controls = df$n_controls, method = "chisq.test", seed = 4321)))
    rma.df.binom <- .quiet(esb.test(rmaor, input = "rma", n_cases = df$n_cases, n_controls = df$n_controls, method = "binom.test", seed = 4321))

    umb <- .quiet(umbrella(df, seed = 4321))
    umb2 <- .quiet(umbrella(subset(df, select = -c(n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp)), seed = 4321))

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

    gen <- .quiet(esb.test(df, input = "dataframe", measure = "SMD", seed = 4321))

    umb <- .quiet(umbrella(df, seed = 4321))

    expect_equal(.as_numeric(gen$statistic), .as_numeric(umb[[1]]$esb$statistic), tolerance = tol_med)
    expect_equal(.as_numeric(gen$p.value), .as_numeric(umb[[1]]$esb$p.value), tolerance = tol_med)
})

test_that("esb.test produced correct results with multilevel data", {
    skip_on_cran()
    df <- subset(df.OR.multi, factor == "Vitamin D")

    gen <- .quiet(esb.test(df, input = "dataframe", measure = "OR", seed = 4321))

    umb <- .quiet(umbrella(df, mult.level = TRUE, seed = 4321))

    expect_equal(.as_numeric(gen$statistic), .as_numeric(umb[[1]]$esb$statistic), tolerance = tol_large)
    expect_equal(.as_numeric(gen$p.value), .as_numeric(umb[[1]]$esb$p.value), tolerance = tol_large)
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
                        method = "chisq.test",
                        true_effect = theta, seed = 4321)))

    esb.bin <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "SMD", input = "dataframe",
                        method = "binom.test",
                        true_effect = theta, seed = 4321)))

    expect_equal(.as_numeric(esb.bin$power), tes.binom$power, tolerance = 0.005)
    expect_equal(.as_numeric(esb.bin$mean_power), mean(tes.binom$power), tolerance = 0.005)
    expect_equal(.as_numeric(esb.bin$O), tes.binom$O)
    expect_equal(.as_numeric(esb.bin$E), tes.binom$E, tolerance = 0.005)
    expect_equal(.as_numeric(esb.bin$k), tes.binom$k)

    expect_equal(.as_numeric(esb.chi$power), tes.chi$power, tolerance = 0.005)
    expect_equal(.as_numeric(esb.chi$mean_power), mean(tes.chi$power), tolerance = 0.005)
    expect_equal(.as_numeric(esb.chi$O), tes.chi$O)
    expect_equal(.as_numeric(esb.chi$E), tes.chi$E, tolerance = 0.005)
    expect_equal(.as_numeric(esb.chi$k), tes.chi$k)

    expect_equal(.as_numeric(binom.test(tes.binom$O, tes.binom$k, mean(tes.binom$power), alternative = "greater")$p.value),
                 .as_numeric(tes.binom$pval))

    expect_equal(.as_numeric(prop.test(tes.chi$O, tes.chi$k, p = mean(tes.chi$power), alternative = "greater")$p.value),
                 .as_numeric(tes.chi$pval), tolerance = 0.3)

    expect_equal(.as_numeric(esb.bin$p.value), .as_numeric(tes.binom$pval), tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$p.value), .as_numeric(tes.chi$pval), tolerance = 0.5)
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
                        method = "chisq.test",
                        true_effect = exp(theta), seed = 4321)))

    esb.bin <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "OR", input = "dataframe",
                        method = "binom.test",
                        true_effect = exp(theta), seed = 4321)))

    expect_equal(.as_numeric(esb.bin$power), tes.binom$power, tolerance = 0.05)
    expect_equal(.as_numeric(esb.bin$mean_power), mean(tes.binom$power), tolerance = 0.05)
    expect_equal(.as_numeric(esb.bin$O), tes.binom$O)
    expect_equal(.as_numeric(esb.bin$E), tes.binom$E, tolerance = 0.05)
    expect_equal(.as_numeric(esb.bin$k), tes.binom$k)

    expect_equal(.as_numeric(esb.chi$power), tes.chi$power, tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$mean_power), mean(tes.chi$power), tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$O), tes.chi$O)
    expect_equal(.as_numeric(esb.chi$E), tes.chi$E, tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$k), tes.chi$k)

    expect_equal(.as_numeric(binom.test(tes.binom$O, tes.binom$k, mean(tes.binom$power), alternative = "greater")$p.value),
                 .as_numeric(tes.binom$pval))

    expect_equal(.as_numeric(prop.test(tes.chi$O, tes.chi$k, p = mean(tes.chi$power), alternative = "greater")$p.value),
                 .as_numeric(tes.chi$pval), tolerance = 0.5)

    expect_equal(.as_numeric(esb.bin$p.value), .as_numeric(tes.binom$pval), tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$p.value), .as_numeric(tes.chi$pval), tolerance = 0.5)
})

test_that("esb.test produced correct results compared to metafor: HR", {
    skip_on_cran()
    theta = 0.5
    df <- subset(df.HR, factor == "Mindfulness")
    df$se <- (log(df$ci_up) - log(df$ci_lo)) / (2 * qnorm(0.975))
    df <- subset(df, select = -c(ci_lo, ci_up))

    tes.chi <- .quiet(metafor::tes(x = log(df$value), sei = df$se,
                                   theta = theta, test = "chi2",
                                   alternative = "two.sided", tes.alternative = "greater"))
    tes.binom <- .quiet(metafor::tes(x = log(df$value), sei = df$se,
                                     theta = theta, test = "binom",
                                     alternative = "two.sided", tes.alternative = "greater"))

    esb.chi <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "HR", input = "dataframe",
                        method = "chisq.test",
                        true_effect = exp(theta), seed = 4321)))

    esb.bin <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "HR", input = "dataframe",
                        method = "binom.test",
                        true_effect = exp(theta), seed = 4321)))

    expect_equal(.as_numeric(esb.bin$power), tes.binom$power, tolerance = 1)
    expect_equal(.as_numeric(esb.bin$mean_power), mean(tes.binom$power), tolerance = 0.7)
    expect_equal(.as_numeric(esb.bin$O), tes.binom$O)
    expect_equal(.as_numeric(esb.bin$E), tes.binom$E, tolerance = 1)
    expect_equal(.as_numeric(esb.bin$k), tes.binom$k)

    expect_equal(.as_numeric(esb.chi$power), tes.chi$power, tolerance = 1)
    expect_equal(.as_numeric(esb.chi$mean_power), mean(tes.chi$power), tolerance = 1)
    expect_equal(.as_numeric(esb.chi$O), tes.chi$O)
    expect_equal(.as_numeric(esb.chi$E), tes.chi$E, tolerance = 1)
    expect_equal(.as_numeric(esb.chi$k), tes.chi$k)

    expect_equal(.as_numeric(binom.test(tes.binom$O, tes.binom$k, mean(tes.binom$power), alternative = "greater")$p.value),
                 .as_numeric(tes.binom$pval))

    expect_equal(.as_numeric(prop.test(tes.chi$O, tes.chi$k, p = mean(tes.chi$power), alternative = "greater")$p.value),
                 .as_numeric(tes.chi$pval), tolerance = 0.5)

    expect_equal(.as_numeric(esb.bin$p.value), .as_numeric(tes.binom$pval), tolerance = 0.5)
    expect_equal(.as_numeric(esb.chi$p.value), .as_numeric(tes.chi$pval), tolerance = 0.5)
    expect_equal(esb.chi$mean_power, mean(tes.chi$power), tolerance = 0.6)
    expect_equal(esb.chi$O, tes.chi$O)
    expect_equal(esb.chi$E, tes.chi$E, tolerance = 1)
    expect_equal(esb.chi$k, tes.chi$k)

    expect_equal(.as_numeric(esb.chi$p.value), .as_numeric(tes.chi$pval), tolerance = 0.3)
    expect_equal(.as_numeric(esb.bin$p.value), .as_numeric(tes.binom$pval), tolerance = 0.5)
})

test_that("esb.test produced correct results compared to metafor: RR", {
    skip_on_cran()
    theta = 0.3
    df <- df.RR
    df$se <- with(df, .estimate_rr_from_n(n_cases_exp, n_exp, n_cases_nexp, n_nexp)$se)
    df$value <- with(df, .estimate_rr_from_n(n_cases_exp, n_exp, n_cases_nexp, n_nexp)$value)

    tes.chi <- .quiet(metafor::tes(x = log(df$value), sei = df$se,
                                   theta = theta, test = "chi2", alternative = "two.sided", tes.alternative = "greater"))
    tes.binom <- .quiet(metafor::tes(x = log(df$value), sei = df$se,
                                     theta = theta, test = "binom", alternative = "two.sided", tes.alternative = "greater"))

    esb.chi <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "RR", input = "dataframe",
                        method = "chisq.test",
                        true_effect = exp(theta), seed = 4321)))

    esb.bin <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "RR", input = "dataframe",
                        method = "binom.test",
                        true_effect = exp(theta), seed = 4321)))

    expect_equal(.as_numeric(esb.bin$power), tes.binom$power, tolerance = 0.5)
    expect_equal(.as_numeric(esb.bin$mean_power), mean(tes.binom$power), tolerance = 0.2)
    expect_equal(.as_numeric(esb.bin$O), tes.binom$O)
    expect_equal(.as_numeric(esb.bin$E), tes.binom$E, tolerance = 0.5)
    expect_equal(.as_numeric(esb.bin$k), tes.binom$k)

    expect_equal(.as_numeric(esb.chi$power), tes.chi$power, tolerance = 0.5)
    expect_equal(.as_numeric(esb.chi$mean_power), mean(tes.chi$power), tolerance = 0.2)
    expect_equal(.as_numeric(esb.chi$O), tes.chi$O)
    expect_equal(.as_numeric(esb.chi$E), tes.chi$E, tolerance = 0.5)
    expect_equal(.as_numeric(esb.chi$k), tes.chi$k)

    expect_equal(.as_numeric(binom.test(tes.binom$O, tes.binom$k, mean(tes.binom$power), alternative = "greater")$p.value),
                 .as_numeric(tes.binom$pval))

    expect_equal(.as_numeric(prop.test(tes.chi$O, tes.chi$k, p = mean(tes.chi$power), alternative = "greater")$p.value),
                 .as_numeric(tes.chi$pval), tolerance = 0.005)

    expect_equal(.as_numeric(esb.bin$p.value), .as_numeric(tes.binom$pval), tolerance = 0.005)
    expect_equal(.as_numeric(esb.chi$p.value), .as_numeric(tes.chi$pval), tolerance = 0.005)
})


test_that("esb.test produced correct results compared to metafor: IRR", {
    skip_on_cran()
    theta = 0.5
    df <- df.IRR
    df$se <- with(df, .estimate_irr_from_n(n_cases_exp, time_exp, n_cases_nexp, time_nexp)$se)
    df$value <- with(df, .estimate_irr_from_n(n_cases_exp, time_exp, n_cases_nexp, time_nexp)$value)

    tes.chi <- .quiet(metafor::tes(x = log(df$value), sei = df$se,
                                   theta = theta, test = "chi2", alternative = "two.sided", tes.alternative = "greater"))
    tes.binom <- .quiet(metafor::tes(x = log(df$value), sei = df$se,
                                     theta = theta, test = "binom", alternative = "two.sided", tes.alternative = "greater"))

    esb.chi <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "IRR", input = "dataframe",
                        method = "chisq.test",
                        true_effect = exp(theta), seed = 4321)))

    esb.bin <- suppressWarnings(
        .quiet(esb.test(df,
                        measure = "IRR", input = "dataframe",
                        method = "binom.test",
                        true_effect = exp(theta), seed = 4321)))

    expect_equal(.as_numeric(esb.bin$power), tes.binom$power, tolerance = 0.05)
    expect_equal(.as_numeric(esb.bin$mean_power), mean(tes.binom$power), tolerance = 0.01)
    expect_equal(.as_numeric(esb.bin$O), tes.binom$O)
    expect_equal(.as_numeric(esb.bin$E), tes.binom$E, tolerance = 0.5)
    expect_equal(.as_numeric(esb.bin$k), tes.binom$k)

    expect_equal(.as_numeric(esb.chi$power), tes.chi$power, tolerance = 0.05)
    expect_equal(.as_numeric(esb.chi$mean_power), mean(tes.chi$power), tolerance = 0.01)
    expect_equal(.as_numeric(esb.chi$O), tes.chi$O)
    expect_equal(.as_numeric(esb.chi$E), tes.chi$E, tolerance = 0.5)
    expect_equal(.as_numeric(esb.chi$k), tes.chi$k)

    expect_equal(.as_numeric(binom.test(tes.binom$O, tes.binom$k, mean(tes.binom$power), alternative = "greater")$p.value),
                 .as_numeric(tes.binom$pval))

    expect_equal(.as_numeric(prop.test(tes.chi$O, tes.chi$k, p = mean(tes.chi$power), alternative = "greater")$p.value),
                 .as_numeric(tes.chi$pval), tolerance = 0.1)

    expect_equal(.as_numeric(esb.bin$p.value), .as_numeric(tes.binom$pval), tolerance = 0.01)
    expect_equal(.as_numeric(esb.chi$p.value), .as_numeric(tes.chi$pval), tolerance = 0.5)
    })
