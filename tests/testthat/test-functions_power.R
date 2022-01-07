test_that("power for d is correctly estimated for individual studies", {
  df.save <- subset(df.SMD, factor == "Pharmacological")

  pow_umb <- pow_pwr <- NA

  for (i in 1:nrow(df.save)) {
    pow_umb[i] <- .power_d(df.save$n_cases[i], df.save$n_controls[i], 0.2)
    pow_pwr[i] <- pmin(pwr::pwr.t2n.test(n1 = df.save$n_cases[i],
                                         n2 = df.save$n_controls[i],
                                         d = 0.2, sig.level = 0.05)$power, 1)
  }

  tes.pwr = metafor::tes(x = df.save$value, sei = df.save$se, theta = 0.2,
                         alternative = "two.sided")

  umb.pwr = umbrella(df.save, true_effect = 0.2)[[1]]$esb$power
  expect_equal(pow_umb, pow_pwr)
  expect_equal(pow_umb, umb.pwr)
  expect_equal(pow_umb, tes.pwr$power, tolerance = 5e-3)
})

test_that("power for d is correctly estimated for pooled ES", {

  umb <- union.umbrella(union.umbrella(umbrella(df.SMD), umbrella(df.OR)), umbrella(df.HR))

  pow_meta_umb <- pow_meta_pwr <- NA
  n = 0
  for (i in names(umb)) {
    n = n + 1
    umb_i <- umb[[i]]
    se = sqrt(1 / umb_i$n$cases + 1 / umb_i$n$controls)
    pow_meta_umb[[n]] = .as_numeric(.power_d(umb_i$n$cases,
                            umb_i$n$controls,
                            0.2, se = se))

    pow_meta_pwr[[n]] <- pmin(pwr::pwr.t2n.test(n1 = umb_i$n$cases,
                                           n2 = umb_i$n$controls,
                                           d = 0.2, sig.level = 0.05)$power, 1)
  }
  expect_equal(pow_meta_umb, pow_meta_pwr, tolerance = 1e-6)
})


test_that("power for OR is correctly estimated", {
  df <- subset(df.OR, factor == "ID")
  df <- .quiet(.format_dataset(attr(.check_data(df), "data")))
  pow_umb <- pow_epi <- NA

  set.seed(4321)
  for (i in 1:nrow(df)) {

    pow_umb[i] <- .power_or(df[i, ], 2)
    pow_epi[i] <- epiR::epi.sscc(OR = 2,
                                 p1 = df$n_cases_exp[i] / df$n_cases[i],
                                 p0 = df$n_controls_exp[i] / df$n_controls[i],
                                 n = df$n_cases[i] + df$n_controls[i],
                                 power = NA, r = df$n_controls[i] / df$n_cases[i],
                                 sided.test = 2,
                                 conf.level = 0.95,
                                 method = "unmatched")$power
  }

  tes.pwr = metafor::tes(x = log(df$value), sei = df$se, theta = log(2),
                         alternative = "two.sided")

  umb.pwr = umbrella(subset(df.OR, factor == "ID"), true_effect = 2, seed = 4321)[[1]]$esb$power
  expect_equal(pow_umb, umb.pwr)
  expect_equal(pow_umb, pow_epi, tolerance = 7e-2)
  expect_equal(pow_umb, tes.pwr$power, tolerance = 5e-2)
  expect_equal(pow_epi, tes.pwr$power, tolerance = 7e-2)
})

test_that("power for IRR is correctly estimated", {
  df <- .quiet(.format_dataset(attr(.check_data(df.IRR), "data")))

  tes.pwr = metafor::tes(x = log(df$value), sei = df$se, theta = log(1.3),
                         alternative = "two.sided")

  umb.pwr = umbrella(df.IRR, true_effect = 1.3, seed = 4321)[[1]]$esb$power

  expect_equal(umb.pwr, tes.pwr$power, tolerance = 5e-2)
  expect_equal(mean(umb.pwr), mean(tes.pwr$power), tolerance = 5e-2)
})

test_that("power for RR is correctly estimated", {
  df <- .quiet(.format_dataset(attr(.check_data(df.RR), "data")))

  tes.pwr = metafor::tes(x = log(df$value), sei = df$se, theta = log(1.3),
                         alternative = "two.sided")

  umb.pwr = umbrella(df.RR, true_effect = 1.3, seed = 4321)[[1]]$esb$power

  expect_equal(umb.pwr, tes.pwr$power, tolerance = 5e-1)
  expect_equal(mean(umb.pwr), mean(tes.pwr$power), tolerance = 5e-1)
})

test_that("power for HR is correctly estimated", {
  df <- .quiet(.format_dataset(attr(.check_data(subset(df.HR, factor == "Yoga")), "data")))

  tes.pwr = metafor::tes(x = log(df$value), sei = df$se, theta = log(2),
                         alternative = "two.sided")

  umb.pwr = umbrella(subset(df.HR, factor == "Yoga"), true_effect = 2)[[1]]$esb$power

  expect_equal(umb.pwr, tes.pwr$power, tolerance = 1)
  expect_equal(mean(umb.pwr), mean(tes.pwr$power), tolerance = 1)
})

