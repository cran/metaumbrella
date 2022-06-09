test_that(".estimate_z_from_r() and .z_to_r() appropriately calculates z/r and variance", {

  df <- df.R

  z_umb = with(df, .estimate_z_from_r(r = value, n_sample = n_sample))

  df.Z.mfr <- metafor::escalc(measure = "ZCOR",
                              ri = value, ni = n_sample,
                              data = df)

  expect_equal(z_umb$value, .as_numeric(df.Z.mfr$yi), tolerance = 1e-14)
  expect_equal(df$value, .z_to_r(z_umb$value), tolerance = 1e-14)
  expect_equal(z_umb$se, sqrt(df.Z.mfr$vi), tolerance = 1e-16)
})

