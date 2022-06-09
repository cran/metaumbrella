test_that(".estimate_smc_change() appropriately calculates SMD and variance", {
  df <- df.SMC
  df$ze = 0

  smc_cases <- metafor::escalc(m1i = df$mean_change_cases,
                               sd1i = df$sd_change_cases,
                               ni = df$n_cases,
                               m2i = df$ze,
                               sd2i = df$ze,
                               ri = df$ze,
                               measure = "SMCC", vtype = "LS")
  smc_controls <- metafor::escalc(m1i = df$mean_change_controls,
                                sd1i = df$sd_change_controls,
                                ni = df$n_controls,
                                m2i = df$ze,
                                sd2i = df$ze,
                                ri = df$ze,
                                measure = "SMCC", vtype = "LS")

  smcc = smc_cases$yi - smc_controls$yi
  se = sqrt(smc_cases$vi + smc_controls$vi)

  smc_umb <- with(df, .estimate_smc_change(n_cases = n_cases, n_controls = n_controls,
                                             mean_change_cases = mean_change_cases,
                                             sd_change_cases = sd_change_cases,
                                             mean_change_controls = mean_change_controls,
                                             sd_change_controls = sd_change_controls))

  ## test ES
  expect_equal(smc_umb$value, as.numeric(as.character(smcc)), tolerance = 1e-14)
  expect_equal(smc_umb$se, as.numeric(as.character(se)), tolerance = 1e-14)
})


test_that(".estimate_smc_change() appropriately calculates SMD and variance", {
  df <- df.SMC

  df$pre_post_cor = 0.8

  smc_cases <- metafor::escalc(m1i = df$mean_cases,
                                sd1i = df$sd_cases,
                                ni = df$n_cases,
                                m2i = df$mean_pre_cases,
                                sd2i = df$sd_pre_cases,
                                ri = df$pre_post_cor,
                                measure = "SMCC", vtype = "LS")
  smc_controls <- metafor::escalc(m1i = df$mean_controls,
                                   sd1i = df$sd_controls,
                                   ni = df$n_controls,
                                   m2i = df$mean_pre_controls,
                                   sd2i = df$sd_pre_controls,
                                   ri = df$pre_post_cor,
                                   measure = "SMCC", vtype = "LS")

  smcc = smc_cases$yi - smc_controls$yi
  se = sqrt(smc_cases$vi + smc_controls$vi)

  smc_umb <- with(df, .estimate_smc_raw(n_cases, n_controls,
                                        mean_pre_cases, mean_cases, sd_pre_cases, sd_cases,
                                        mean_pre_controls, mean_controls, sd_pre_controls, sd_controls,
                                        pre_post_cor))

  ## test ES
  expect_equal(smc_umb$value, as.numeric(as.character(smcc)), tolerance = 1e-14)
  expect_equal(smc_umb$se, as.numeric(as.character(se)), tolerance = 1e-14)
})
