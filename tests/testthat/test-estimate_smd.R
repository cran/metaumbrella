test_that(".estimate_d_from_means() appropriately calculates SMD and variance", {

  df <- df.SMD
  meta.conv = with(df, meta::metacont(n.c = n_controls, mean.c = mean_controls, sd.c = sd_controls,
                                      n.e = n_cases, mean.e = mean_cases, sd.e = sd_cases, sm = "SMD",
                                      method.smd = "Cohen", method.tau = "REML"))

  se.umb = .estimate_se_from_d(n_cases = df$n_cases,
                               n_controls = df$n_controls,
                               d = df$value)

  expect_equal(df$value, meta.conv$TE, tolerance = 1e-16)
})


test_that(".estimate_g_from_d() appropriately estimates the Hedges' g and variance", {
  df <- df.SMD

  ## umbrella
  g.umb = .estimate_g_from_d(df$value, df$n_cases, df$n_controls)
  ## metafor
  g.conv.mfr <- metafor::escalc(m1i = df$mean_cases,
                                sd1i = df$sd_cases,
                                n1i = df$n_cases,
                                m2i = df$mean_controls,
                                sd2i = df$sd_controls,
                                n2i = df$n_controls, measure = "SMD", vtype="UB")

  ## test ES
  expect_equal(g.umb$value, as.numeric(as.character(g.conv.mfr$yi)), tolerance = 1e-14)

  ## test variance
  expect_equal(g.umb$se^2, as.numeric(as.character(g.conv.mfr$vi)), tolerance = 1e-14)
})

test_that(".estimate_g_from_d() appropriately estimates the Hedges' g and variance2", {
  df <- df.SMD
  df$se = with(df, sqrt(1/n_cases + 1/n_controls))
  ## umbrella
  g.umb = .estimate_g_from_d(d = df$value, n_cases = df$n_cases, n_controls = df$n_controls, se = df$se)
  ## metafor
  g.conv.mfr <- metafor::escalc(m1i = df$mean_cases,
                                sd1i = df$sd_cases,
                                n1i = df$n_cases,
                                m2i = df$mean_controls,
                                sd2i = df$sd_controls,
                                n2i = df$n_controls, measure = "SMD", vtype = "UB")

  ## test ES
  expect_equal(g.umb$value, as.numeric(as.character(g.conv.mfr$yi)), tolerance = 1e-14)

  ## test variance
  expect_equal(g.umb$se^2, as.numeric(as.character(g.conv.mfr$vi)), tolerance = 1e-14)
})
test_that(".estimate_d_from_md() appropriately converts MD, 95% CI and sample sizes to a SMD", {

  df <- df.SMD
  df_MD <- metafor::escalc(m1i = df$mean_cases,
                           sd1i = df$sd_cases,
                           n1i = df$n_cases,
                           m2i = df$mean_controls,
                           sd2i = df$sd_controls,
                           n2i = df$n_controls, measure = "MD")

  df_MD$yi <- as.numeric(as.character(df_MD$yi))
  df_MD$se <- sqrt(as.numeric(as.character(df_MD$vi)))
  md.umb = .estimate_d_from_md(md = df_MD$yi,
                               ci_lo = df_MD$yi - df_MD$se * qt(0.975, df$n_cases + df$n_controls - 2),
                               ci_up = df_MD$yi + df_MD$se * qt(0.975, df$n_cases + df$n_controls - 2),
                               n_cases = df$n_cases, n_controls = df$n_controls)
  md.umb = .estimate_d_from_md(md = df_MD$yi,
                               ci_lo = df_MD$yi - df_MD$se * qt(0.975, df$n_cases + df$n_controls - 2),
                               ci_up = df_MD$yi + df_MD$se * qt(0.975, df$n_cases + df$n_controls - 2),
                               n_cases = df$n_cases, n_controls = df$n_controls)

  expect_equal(md.umb$value, df$value, tolerance = 5e-2)
})
