tol_large = 1e-10
tol_med = 1e-6
tol_ok = 1e-3
col_rand = c("value", "p.value", "ci_lo", "ci_up", "pi_lo", "pi_up")
col_rand_woPI = c("value", "p.value", "ci_lo", "ci_up")

test_that("mult.level creates message for multilevel studies", {
  skip_on_cran()
  df <- df.OR.multi
  expect_message(invisible(force(umbrella(df, mult.level = TRUE))), "contains multiple outcomes")
  expect_message(invisible(force(umbrella(df, mult.level = TRUE))), "contains multiple groups")
})

test_that("error: mult.level = FALSE", {
  skip_on_cran()
  expect_error(.quiet(umbrella(df.OR.multi)),
               "Please, check that it is not a repeated entry. If not, indicate that you have multivariate data by specfying 'mult.level = TRUE' as an argument of the 'umbrella' function.")
})

##########################
#### different inputs ####
##########################

##### SMD -----------------
test_that("different measures lead to similar results: value + SE vs means/SD", {
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
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})

test_that("different measures lead to similar results: SMD + var vs means/SD", {
  skip_on_cran()
  dfsmd <- df.SMD
  dfwomean <- subset(df.SMD, select = -c(se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  dfwomean$var <- with(dfwomean, 1 / n_cases + 1 / n_controls)
  umb1 <- .quiet(umbrella(dfsmd, seed = 4321))
  umb2 <- .quiet(umbrella(dfwomean, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})

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

  umb1 <- umbrella(dfsmd, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfwoVAL, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})

test_that("different measures lead to similar results: SMD w/o CI vs means/SD", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$measure <- "SMD"

  dfwoCI <- subset(df.SMD, select = -c(se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- umbrella(dfsmd, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfwoCI, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
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
  dfmd <- subset(dfmd, select = -c(value, se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  dfmd$value <- as.numeric(as.character(dfmd$yi))
  dfmd$var <- as.numeric(as.character(dfmd$vi))

  umb1 <-  umbrella(dfsmd, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfmd, seed = 4321, verbose = FALSE)

  expect_equal(umb1[[1]]$ma_results[, c("value", "p.value")], umb2[[1]]$ma_results[, c("value", "p.value")], tolerance = 3e-3)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = 5e-2)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_ok)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 5e-2)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 5e-2)
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
  dfmd$ci_lo = dfmd$value - sqrt(as.numeric(as.character(dfmd$vi))) * qt(0.975, dfmd$n_cases + dfmd$n_controls - 2)
  dfmd$ci_up = dfmd$value + sqrt(as.numeric(as.character(dfmd$vi)))  * qt(0.975, dfmd$n_cases + dfmd$n_controls - 2)

  umb1 <- umbrella(dfsmd, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfmd, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, c("value", "p.value")], umb2[[1]]$ma_results[, c("value", "p.value")], tolerance = 3e-3)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = 5e-2)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_ok)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 5e-2)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 5e-2)
})


test_that("different measures lead to similar results: means/SD v G+SE", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$measure <- "SMD"

  dfg <- dfsmd; dfg$measure <- "G"

  dfg$value = .estimate_g_from_d(d = dfsmd$value, n_cases = dfsmd$n_cases, n_controls = dfsmd$n_controls)$value
  dfg$se = .estimate_g_from_d(d = dfsmd$value, n_cases = dfsmd$n_cases, n_controls = dfsmd$n_controls)$se

  dfg <- subset(dfsmd, select = -c(ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- umbrella(dfsmd, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfg, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})

test_that("different measures lead to similar results: means/SD v value G", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$measure <- "SMD"

  dfg <- dfsmd; dfg$measure <- "G"

  dfg$value = .estimate_g_from_d(d = dfsmd$value, n_cases = dfsmd$n_cases, n_controls = dfsmd$n_controls)$value
  dfg$se = .estimate_g_from_d(d = dfsmd$value, n_cases = dfsmd$n_cases, n_controls = dfsmd$n_controls)$se

  dfg <- subset(dfsmd, select = -c(se, ci_lo, ci_up, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- umbrella(dfsmd, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfg, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
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
  dfg <- subset(dfg, select = -c(se, mean_cases, mean_controls, sd_cases, sd_controls))

  umb1 <- umbrella(dfg, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfg.comp, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})

### SMC ----------------------------------------------------------------
test_that("SMC: different measures lead to similar results: value + SE vs value + CI", {
  df1 <- subset(df.SMC, select = -c(value, se,
                                    #ci_lo, ci_up,
                                    mean_change_cases, mean_change_controls, sd_change_cases, sd_change_controls,
                                    mean_cases, mean_controls, sd_cases, sd_controls,
                                    mean_pre_cases, mean_pre_controls, sd_pre_cases, sd_pre_controls
                                    ))

  df2 <- subset(df.SMC, select = -c(#value, se
                                    ci_lo, ci_up,
                                    mean_change_cases, mean_change_controls, sd_change_cases, sd_change_controls,
                                    mean_cases, mean_controls, sd_cases, sd_controls,
                                    mean_pre_cases, mean_pre_controls, sd_pre_cases, sd_pre_controls
                                    ))

  umb1 <- .quiet(umbrella(df1, seed = 4321))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = 1e-6)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = 1e-6)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 1e-6)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 1e-6)
})
test_that("SMC: different measures lead to similar results: value + SE vs means + mean change", {
  df1 <- subset(df.SMC, select = -c(value, se,
                                    ci_lo, ci_up,
                                    #mean_change_cases, mean_change_controls, sd_change_cases, sd_change_controls,
                                    mean_cases, mean_controls, sd_cases, sd_controls,
                                    mean_pre_cases, mean_pre_controls, sd_pre_cases, sd_pre_controls
  ))

  df2 <- subset(df.SMC, select = -c(#value, se,
                                    ci_lo, ci_up,
                                    mean_change_cases, mean_change_controls, sd_change_cases, sd_change_controls,
                                    mean_cases, mean_controls, sd_cases, sd_controls,
                                    mean_pre_cases, mean_pre_controls, sd_pre_cases, sd_pre_controls
  ))

  umb1 <- .quiet(umbrella(df1, seed = 4321))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})

test_that("SMC: different measures lead to similar results: value + SE vs means pre/post", {
  df1 <- subset(df.SMC, select = -c(value, se,
                                    ci_lo, ci_up,
                                    mean_change_cases, mean_change_controls, sd_change_cases, sd_change_controls
                                    #mean_cases, mean_controls, sd_cases, sd_controls,
                                    #mean_pre_cases, mean_pre_controls, sd_pre_cases, sd_pre_controls
  ))

  df2 <- subset(df.SMC, select = -c(#value, se,
                                    ci_lo, ci_up,
                                    mean_change_cases, mean_change_controls, sd_change_cases, sd_change_controls,
                                    mean_cases, mean_controls, sd_cases, sd_controls,
                                    mean_pre_cases, mean_pre_controls, sd_pre_cases, sd_pre_controls
  ))
  df1$pre_post_cor = 0.8

  umb1 <- .quiet(umbrella(df1, seed = 4321, pre_post_cor = 0.8))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})

test_that("SMC: different measures lead to similar results: change + pre/post", {
  df1 <- subset(df.SMC, select = -c(value, se,
                                    ci_lo, ci_up,
                                    mean_change_cases, mean_change_controls, sd_change_cases, sd_change_controls
                                    #mean_cases, mean_controls, sd_cases, sd_controls,
                                    #mean_pre_cases, mean_pre_controls, sd_pre_cases, sd_pre_controls
  ))

  df2 <- subset(df.SMC, select = -c(value, se,
    ci_lo, ci_up,
    #mean_change_cases, mean_change_controls, sd_change_cases, sd_change_controls,
    mean_cases, mean_controls, sd_cases, sd_controls,
    mean_pre_cases, mean_pre_controls, sd_pre_cases, sd_pre_controls
  ))
  # df1$pre_post_cor = 0.8

  umb1 <- .quiet(umbrella(df1, seed = 4321, pre_post_cor = 0.8))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})
##### R ------------------
test_that("R: different measures lead to similar results: value + CI vs value", {
  df <- subset(df.R, factor == "Gestational_hypertension")

  df$se = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$se
  df$value = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$value
  df$ci_lo = (df$value - qnorm(0.975) * df$se)
  df$ci_up = (df$value + qnorm(0.975) * df$se)
  df$value = .z_to_r(df$value)
  df$ci_lo = .z_to_r(df$ci_lo)
  df$ci_up = .z_to_r(df$ci_up)
  df$se = with(df, (ci_up - ci_lo) / (2*qnorm(0.975)))


  df1 = subset(df, select = -c(se, ci_lo, ci_up))
  df2 = subset(df, select = -c(se))

  umb1 <- .quiet(umbrella(df1, seed = 4321))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = 5e-2)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = 1e-3)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 1e-3)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 5e-2)
})
test_that("R: different measures lead to similar results: value + SE vs value", {
  df <- subset(df.R, factor == "Gestational_hypertension")

  df$se = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$se
  df$value = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$value
  df$ci_lo = (df$value - qnorm(0.975) * df$se)
  df$ci_up = (df$value + qnorm(0.975) * df$se)
  df$value = .z_to_r(df$value)
  df$ci_lo = .z_to_r(df$ci_lo)
  df$ci_up = .z_to_r(df$ci_up)
  df$se = with(df, (ci_up - ci_lo) / (2*qnorm(0.975)))


  df1 = subset(df, select = -c(se, ci_lo, ci_up))
  df2 = subset(df, select = -c(ci_lo, ci_up))

  umb1 <- .quiet(umbrella(df1, seed = 4321))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = 5e-2)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = 1e-3)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 1e-3)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 5e-2)
})
test_that("Z: different measures lead to similar results: value + SE vs value", {
  df <- subset(df.R, factor == "Gestational_hypertension")

  df$se = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$se
  df$value = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$value
  df$ci_lo = (df$value - qnorm(0.975) * df$se)
  df$ci_up = (df$value + qnorm(0.975) * df$se)
  df$measure = "Z"


  df1 = subset(df, select = -c(se, ci_lo, ci_up))
  df2 = subset(df, select = -c(ci_lo, ci_up))

  umb1 <- .quiet(umbrella(df1, seed = 4321))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})
test_that("Z: different measures lead to similar results: value + CI vs value", {
  df <- subset(df.R, factor == "Gestational_hypertension")

  df$se = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$se
  df$value = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$value
  df$ci_lo = (df$value - qnorm(0.975) * df$se)
  df$ci_up = (df$value + qnorm(0.975) * df$se)
  df$measure = "Z"


  df1 = subset(df, select = -c(se, ci_lo, ci_up))
  df2 = subset(df, select = -c(se))

  umb1 <- .quiet(umbrella(df1, seed = 4321))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})
test_that("Z: different measures lead to similar results: value + CI vs value + se", {
  df <- subset(df.R, factor == "Gestational_hypertension")

  df$se = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$se
  df$value = .estimate_z_from_r(n_sample = df$n_sample, r = df$value)$value
  df$ci_lo = (df$value - qnorm(0.975) * df$se)
  df$ci_up = (df$value + qnorm(0.975) * df$se)
  df$measure = "Z"


  df1 = subset(df, select = -c(ci_lo, ci_up))
  df2 = subset(df, select = -c(se))

  umb1 <- .quiet(umbrella(df1, seed = 4321))
  umb2 <- .quiet(umbrella(df2, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})

##### OR -----------------

test_that("different measures lead to similar results: OR n_cases/controls vs. n_exp/n_nexp", {
  skip_on_cran()
  dfor1 <- subset(df.OR, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(n_cases, n_controls, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))

  umb1 <- umbrella(dfor1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfor2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})

test_that("different measures lead to similar results: OR + CI n_cases/controls vs. 2x2 table", {
  skip_on_cran()

  dfor1 <- subset(df.OR, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(value, ci_lo, ci_up, n_cases, n_controls, n_exp, n_nexp))
  se =   with(df.OR, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))
  dfor1$ci_lo = dfor1$value / exp(qnorm(0.975) * se)
  dfor1$ci_up = dfor1$value * exp(qnorm(0.975) * se)

  # approximation due to .improve_ci
  umb1 <- umbrella(dfor1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfor2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 5*tol_ok)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})

test_that("different measures lead to similar results: OR n_cases/controls SE vs. 2x2 table", {
  skip_on_cran()
  dfor1 <- subset(df.OR, select = -c(ci_lo, ci_up, n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(value, ci_lo, ci_up, n_cases, n_controls, n_exp, n_nexp))

  dfor1$se <- with(df.OR, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))

  umb1 <- umbrella(dfor1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfor2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})

test_that("different measures lead to similar results: OR n_exp/n_nexp vs. 2x2 table", {
  skip_on_cran()
  dfor1 <- subset(df.OR, select = -c(n_cases, n_controls, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(value, ci_lo, ci_up, n_cases, n_controls, n_exp, n_nexp))
  se = with(df.OR, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))
  dfor1$ci_lo = dfor1$value / exp(qnorm(0.975) * se)
  dfor1$ci_up = dfor1$value * exp(qnorm(0.975) * se)

  umb1 <- umbrella(dfor1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfor2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 5*tol_ok)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})

test_that("different measures lead to similar results: OR n_exp/n_nexp SE vs. 2x2 table", {
  skip_on_cran()
  dfor1 <- subset(df.OR, select = -c(ci_lo, ci_up, n_controls, n_cases, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(value, ci_lo, ci_up, n_cases, n_controls, n_exp, n_nexp))

  dfor1$se <- with(dfor2, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))
  umb1 <- umbrella(dfor1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfor2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
})

test_that("different measures lead to similar results: OR n_cases/controls with / without CI", {
  skip_on_cran()
  dfor1 <- subset(df.OR, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfor2 <- subset(df.OR, select = -c(ci_lo, ci_up, n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))

  se = with(df.OR, sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp))
  dfor1$ci_lo = dfor1$value / exp(qnorm(0.975) * se)
  dfor1$ci_up = dfor1$value * exp(qnorm(0.975) * se)

  umb1 <- umbrella(dfor1, seed = 4321, verbose = FALSE, method.esb = "IT.binom", true_effect = "largest")
  umb2 <- umbrella(dfor2, seed = 4321, verbose = FALSE, method.esb = "IT.binom", true_effect = "largest")

  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = 3e-1)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = 5e-2)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = 5e-2)
})

##### RR -----------------

test_that("different measures lead to similar results: RR + CI n_cases/controls vs. 2x2 table", {
  skip_on_cran()
  dfrr1 <- df.RR
  dfrr1$n_cases = dfrr1$n_cases_exp + dfrr1$n_cases_nexp
  dfrr1$n_controls = dfrr1$n_exp + dfrr1$n_nexp - dfrr1$n_cases
  dfrr1 <- subset(dfrr1, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp))
  se = with(df.RR, sqrt(1 / n_cases_exp - 1 / n_exp + 1 / n_cases_nexp - 1 / n_nexp))
  dfrr1$ci_lo = dfrr1$value / exp(qnorm(0.975) * se)
  dfrr1$ci_up = dfrr1$value * exp(qnorm(0.975) * se)

  dfrr2 <- df.RR

  umb1 <- umbrella(dfrr1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfrr2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})
test_that("different measures lead to similar results: RR + SE n_cases/controls vs. 2x2 table", {
  skip_on_cran()
  dfrr1 <- df.RR
  dfrr1$n_cases = dfrr1$n_cases_exp + dfrr1$n_cases_nexp
  dfrr1$n_controls = dfrr1$n_exp + dfrr1$n_nexp - dfrr1$n_cases
  dfrr1 <- subset(dfrr1, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp))
  dfrr1$se = with(df.RR, sqrt(1 / n_cases_exp - 1 / n_exp + 1 / n_cases_nexp - 1 / n_nexp))
  dfrr1$ci_lo = dfrr1$ci_up = NA

  dfrr2 <- df.RR

  umb1 <- umbrella(dfrr1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfrr2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
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
  dfrr3 <- subset(dfrr1, select = -c(ci_lo, ci_up, n_exp, n_nexp, n_cases_exp, n_cases_nexp))

  umb1 <- umbrella(dfrr2, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfrr3, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})

##### IRR -----------------

test_that("different measures lead to similar results: IRR + CI + n_cases/time vs. n_cases_exp-nexp/time-exp/nexp", {
  skip_on_cran()
  dfirr1 <- subset(df.IRR, select = -c(n_cases_exp, n_cases_nexp, time_exp, time_nexp))
  dfirr2 <- subset(df.IRR, select = -c(n_cases, time))

  se = with(df.IRR, sqrt(1 / n_cases_exp + 1 / n_cases_nexp))
  dfirr1$ci_lo = dfirr1$value / exp(qnorm(0.975) * se)
  dfirr1$ci_up = dfirr1$value * exp(qnorm(0.975) * se)


  umb1 <- umbrella(dfirr1, seed = 4321)
  umb2 <- umbrella(dfirr2, seed = 4321)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})
test_that("different measures lead to similar results: IRR + SE + n_cases/time vs. n_cases_exp-nexp/time-exp/nexp", {
  skip_on_cran()
  dfirr1 <- subset(df.IRR, select = -c(ci_lo, ci_up, n_cases_exp, n_cases_nexp, time_exp, time_nexp))
  dfirr2 <- subset(df.IRR, select = -c(n_cases, time))

  dfirr1$se = with(df.IRR, sqrt(1 / n_cases_exp + 1 / n_cases_nexp))

  umb1 <- umbrella(dfirr1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfirr2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})

test_that("different measures lead to similar results: IRR + SE + n_cases/time vs. IRR + CI + n_cases/time", {
  skip_on_cran()
  dfirr1 <- subset(df.IRR, select = -c(ci_lo, ci_up, n_cases_exp, n_cases_nexp, time_exp, time_nexp))
  dfirr2 <- subset(df.IRR, select = -c(ci_lo, ci_up, n_cases_exp, n_cases_nexp, time_exp, time_nexp))

  dfirr1$se = with(df.IRR, sqrt(1 / n_cases_exp + 1 / n_cases_nexp))
  dfirr2$ci_lo = dfirr1$value / exp(qnorm(0.975) * dfirr1$se)
  dfirr2$ci_up = dfirr1$value * exp(qnorm(0.975) * dfirr1$se)

  umb1 <- umbrella(dfirr1, seed = 4321, verbose = FALSE)
  umb2 <- umbrella(dfirr2, seed = 4321, verbose = FALSE)
  expect_equal(umb1[[1]]$ma_results[, col_rand], umb2[[1]]$ma_results[, col_rand], tolerance = tol_med)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_med)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_med)
})


### REVERSE ES

test_that("reverse ES leads to similar results: SMD", {
  skip_on_cran()
  dfsmd <- df.SMD; dfsmd$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfsmd))
  umb2 <- .quiet(umbrella(df.SMD))
  expect_equal(umb1[[1]]$ma_results[,1], -umb2[[1]]$ma_results[,1], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,3], umb2[[1]]$ma_results[,3], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,4], -umb2[[1]]$ma_results[,5], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,5], -umb2[[1]]$ma_results[,4], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,6], -umb2[[1]]$ma_results[,7], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,7], -umb2[[1]]$ma_results[,6], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
})

test_that("reverse ES leads to similar results: R", {
  skip_on_cran()
  dfR <- df.R; dfR$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfR))
  umb2 <- .quiet(umbrella(df.R))
  expect_equal(umb1[[1]]$ma_results[,1], -umb2[[1]]$ma_results[,1], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,3], umb2[[1]]$ma_results[,3], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,4], -umb2[[1]]$ma_results[,5], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,5], -umb2[[1]]$ma_results[,4], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,6], -umb2[[1]]$ma_results[,7], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,7], -umb2[[1]]$ma_results[,6], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
})

test_that("reverse ES leads to similar results: SMC", {
  skip_on_cran()
  dfsmcR <- df.SMC; dfsmcR$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfsmcR))
  umb2 <- .quiet(umbrella(df.SMC))
  expect_equal(umb1[[1]]$ma_results[,1], -umb2[[1]]$ma_results[,1], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,3], umb2[[1]]$ma_results[,3], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,4], -umb2[[1]]$ma_results[,5], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,5], -umb2[[1]]$ma_results[,4], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,6], -umb2[[1]]$ma_results[,7], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,7], -umb2[[1]]$ma_results[,6], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
})

test_that("reverse ES leads to similar results: OR", {
  skip_on_cran()
  dfor <- df.OR; dfor$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfor, seed = 4321))
  umb2 <- .quiet(umbrella(df.OR, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[,1], -umb2[[1]]$ma_results[,1], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,3], umb2[[1]]$ma_results[,3], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,4], -umb2[[1]]$ma_results[,5], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,5], -umb2[[1]]$ma_results[,4], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,6], -umb2[[1]]$ma_results[,7], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,7], -umb2[[1]]$ma_results[,6], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
})

test_that("reverse ES leads to similar results: RR", {
  skip_on_cran()
  dfrr <- df.RR; dfrr$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfrr, seed = 4321))
  umb2 <- .quiet(umbrella(df.RR, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[,1], -umb2[[1]]$ma_results[,1], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,3], umb2[[1]]$ma_results[,3], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,4], -umb2[[1]]$ma_results[,5], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,5], -umb2[[1]]$ma_results[,4], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,6], -umb2[[1]]$ma_results[,7], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,7], -umb2[[1]]$ma_results[,6], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_med)
})

test_that("reverse ES leads to similar results: HR", {
  skip_on_cran()
  dfhr <- df.HR; dfhr$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfhr, seed = 4321))
  umb2 <- .quiet(umbrella(df.HR, seed = 4321))
  expect_equal(umb1[[2]]$ma_results[,1], -umb2[[2]]$ma_results[,1], tolerance = tol_large)
  expect_equal(umb1[[2]]$ma_results[,3], umb2[[2]]$ma_results[,3], tolerance = tol_large)
  expect_equal(umb1[[2]]$ma_results[,4], -umb2[[2]]$ma_results[,5], tolerance = tol_large)
  expect_equal(umb1[[2]]$ma_results[,5], -umb2[[2]]$ma_results[,4], tolerance = tol_large)
  expect_equal(umb1[[2]]$ma_results[,6], -umb2[[2]]$ma_results[,7], tolerance = tol_large)
  expect_equal(umb1[[2]]$ma_results[,7], -umb2[[2]]$ma_results[,6], tolerance = tol_large)
  expect_equal(umb1[[2]]$heterogeneity$i2, umb2[[2]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[2]]$egger$p.value, umb2[[2]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb1[[2]]$esb$p.value, umb2[[2]]$esb$p.value, tolerance = 1e-6)
})

test_that("reverse ES leads to similar results: IRR", {
  skip_on_cran()
  dfirr <- df.IRR; dfirr$reverse_es <- "reverse"

  umb1 <- .quiet(umbrella(dfirr, seed = 4321))
  umb2 <- .quiet(umbrella(df.IRR, seed = 4321))
  expect_equal(umb1[[1]]$ma_results[,1], -umb2[[1]]$ma_results[,1], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,3], umb2[[1]]$ma_results[,3], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,4], -umb2[[1]]$ma_results[,5], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,5], -umb2[[1]]$ma_results[,4], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,6], -umb2[[1]]$ma_results[,7], tolerance = tol_large)
  expect_equal(umb1[[1]]$ma_results[,7], -umb2[[1]]$ma_results[,6], tolerance = tol_large)
  expect_equal(umb1[[1]]$heterogeneity$i2, umb2[[1]]$heterogeneity$i2, tolerance = tol_large)
  expect_equal(umb1[[1]]$egger$p.value, umb2[[1]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb1[[1]]$esb$p.value, umb2[[1]]$esb$p.value, tolerance = tol_large)
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

  # test that umbrella correctly estimate G from corrected sample size
  expect_equal(umb1[[1]]$x_multi$value, .estimate_g_from_d(d$value, df$n_cases, df$n_controls / 2)$value, tolerance = 1e-13)
  expect_equal(umb1[[1]]$x_multi$se, .estimate_g_from_d(d$value, df$n_cases, df$n_controls / 2)$se, tolerance = 1e-13)


  df.m <- df; df.m$n_controls = df.m$n_controls / 2

  ## metafor
  df.mfr <- metafor::escalc(m1i = mean_cases,
                                sd1i = sd_cases,
                                n1i = n_cases,
                                m2i = mean_controls,
                                sd2i = sd_controls,
                                n2i = n_controls, data = df.m, measure = "SMD", vtype = "UB")

  expect_equal(umb1[[1]]$x_multi$value, .as_numeric(df.mfr$yi), tolerance = 1e-13)
  expect_equal(umb1[[1]]$x_multi$se, sqrt(df.mfr$vi), tolerance = 1e-13)


  df.agg.mfr <- metafor::aggregate.escalc(df.mfr,
                                          cluster = author,
                                          struct = "CS",
                                          weighted = FALSE,
                                          rho = 0.5)

  expect_equal(umb1[[1]]$x$value, .as_numeric(df.agg.mfr$yi), tolerance = 1e-13)
  expect_equal(umb1[[1]]$x$se, sqrt(df.agg.mfr$vi), tolerance = 1e-13)
})

test_that("umbrella correctly handle multiple_es + shared_controls OR", {
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

  # test that umbrella correctly estimate OR from corrected sample size
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

test_that("umbrella correctly handle multiple_es + shared_nexp", {
  skip_on_cran()
  df <- df.OR[1:5,]
  df$shared_nexp <- c(rep(1, 4), NA)
  df$multiple_es <- c(rep("outcomes", 4), NA)
  df$author[c(2, 4)] <- df$author[c(1,3)]
  df$year[c(2, 4)] <- df$year[c(1,3)]

  umb1 <- .quiet(umbrella(df, mult.level = TRUE, seed = 4321))


  or = .estimate_or_from_n(df$n_cases_exp[1:4],
                           df$n_cases_nexp[1:4] / 2,
                           df$n_controls_exp[1:4],
                           df$n_controls_nexp[1:4] / 2)

  # test that umbrella correctly estimate OR from corrected sample size
  expect_equal(umb1[[1]]$x_multi$value[1:4], or$value, tolerance = 1e-13)
  expect_equal(umb1[[1]]$x_multi$se[1:4], or$se, tolerance = 1e-13)

  df.m <- df
  df.m$n_cases_nexp[1:4] = df.m$n_cases_nexp[1:4] / 2
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

