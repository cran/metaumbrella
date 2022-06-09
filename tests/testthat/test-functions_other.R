tol_large = 1e-10

test_that(".meta_to_umbrella_x correctly extracts information and produces appropriate error message when being called with inappropriate inputs", {
  df <- df.SMD
  # standard smd
  metasmd <- meta::metacont(n_cases, mean_cases, sd_cases,
                             n_controls, mean_controls, sd_controls,
                             method.tau = "REML", data = df,
                             sm = "SMD", method.smd = "Hedges")

  res_metasmd <- .meta_to_umbrella_x(metasmd, n_cases = NULL, n_controls = NULL, measure = "SMD")
  expect_equal(df$value, res_metasmd$value, tolerance = 1e-6)
  expect_equal(df$se, res_metasmd$se, tolerance = tol_large)
  expect_equal(df$n_cases, res_metasmd$n_cases, tolerance = tol_large)
  expect_equal(df$n_controls, res_metasmd$n_controls, tolerance = tol_large)

  metasmd.C <- meta::metacont(n_cases, mean_cases, sd_cases,
                             n_controls, mean_controls, sd_controls,
                             method.tau = "REML", data = df,
                             sm = "SMD", method.smd = "Cohen")

  res_metasmd.C <- .meta_to_umbrella_x(metasmd.C, n_cases = NULL, n_controls = NULL, measure = "SMD")
  expect_equal(df$value, res_metasmd.C$value, tolerance = tol_large)
  expect_equal(df$se, res_metasmd.C$se, tolerance = tol_large)
  expect_equal(df$n_cases, res_metasmd.C$n_cases, tolerance = tol_large)
  expect_equal(df$n_controls, res_metasmd.C$n_controls, tolerance = tol_large)

  dfor <- df.OR
  dfor$value <- with(dfor, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                           n_controls_exp, n_controls_nexp)$value)
  dfor$se <- with(dfor, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                        n_controls_exp, n_controls_nexp)$se)

  # standard OR
  metaor <- meta::metabin(event.e = n_cases_exp, n.e = n_exp,
                          event.c = n_cases_nexp, n.c = n_nexp,
                          data = dfor, sm = "OR", method.tau = "REML")

  res_metaor <- .meta_to_umbrella_x(metaor, n_cases = NULL, n_controls = NULL, measure = "OR")

  expect_equal(dfor$value, res_metaor$value, tolerance = 1e-15)
  expect_equal(dfor$se, res_metaor$se, tolerance = 1e-15)
  expect_equal(dfor$n_cases, res_metaor$n_cases, tolerance = tol_large)
  expect_equal(dfor$n_controls, res_metaor$n_controls, tolerance = tol_large)

  # unsupported measure
  metarr <- meta::metabin(event.e = n_cases_exp, n.e = n_exp,
                          event.c = n_cases_nexp, n.c = n_nexp,
                          data = df.RR, sm = "RR", method.tau = "REML")
  expect_error(.meta_to_umbrella_x(metarr, n_cases = NULL, n_controls = NULL, measure = "RR"),
               "The esb.test function can be called from a 'meta' object only with 'G', 'SMD' or 'OR' as effect size measures.",
               fixed = TRUE)

  # missing measure
  metagenor <- meta::metagen(TE = log(value), seTE = se, data = dfor, method.tau = "REML")
  expect_error(.meta_to_umbrella_x(metagenor,
                                   n_cases = df$n_cases,
                                   n_controls = df$n_controls,
                                   measure = NULL
                                   ),
               "The effect size measure should be indicated when calling the esb.test function from a meta object (via the 'sm' argument in the meta function or via the 'measure' argument of the esb.test function)",
               fixed = TRUE)

  # missing sample sizes
  metagenor <- meta::metagen(TE = log(value), seTE = se, data = dfor, sm = "OR", method.tau = "REML")
  #n_controls
  expect_error(.meta_to_umbrella_x(metagenor,
                                   n_cases = df$n_cases,
                                   n_controls = NULL
                                   ), "The number of controls should be indicated. You can do it when calling the meta function or when calling the esb.test function (via 'n_controls' argument)", fixed = TRUE)
  #n_cases
  expect_error(.meta_to_umbrella_x(metagenor,
                                   n_cases = NULL,
                                   n_controls = df$n_controls
                                   ), "The number of cases should be indicated. You can do it when calling the meta function or when calling the esb.test function (via 'n_cases' argument)", fixed = TRUE)
})

test_that(".meta_to_umbrella_x correctly extracts information from generic inputs: SMD", {

  df <- df.SMD
  # generic SMD
  j = .d_j(df$n_cases + df$n_controls - 2)
  metagensmd <- meta::metagen(TE = value * j, seTE = se * j, data = df, method.tau = "REML")

  resgen <- .meta_to_umbrella_x(metagensmd,
                                n_cases = df$n_cases,
                                n_controls = df$n_controls,
                                measure = "SMD")

  expect_equal(df$value, resgen$value)
  expect_equal(df$se,resgen$se)
  expect_equal(df$n_cases,resgen$n_cases)
  expect_equal(df$n_controls,resgen$n_controls)
})

test_that(".meta_to_umbrella_x correctly extracts information from generic inputs: OR", {

  df <- subset(df.OR, factor == "ADHD")
  df$value <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                           n_controls_exp, n_controls_nexp)$value)
  df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                        n_controls_exp, n_controls_nexp)$se)

  metagenor <- meta::metagen(TE = log(value), seTE = se, data = df, method.tau = "REML")

  resgen <- .meta_to_umbrella_x(metagenor,
                                n_cases = df$n_cases,
                                n_controls = df$n_controls,
                                measure = "OR")

  expect_equal(df$value, resgen$value)
  expect_equal(df$se,resgen$se)
  expect_equal(df$n_cases,resgen$n_cases)
  expect_equal(df$n_controls,resgen$n_controls)
  expect_equal(df$n_cases_exp,resgen$n_cases_exp)
  expect_equal(df$n_cases_nexp,resgen$n_cases_nexp)
  expect_equal(df$n_controls_exp,resgen$n_controls_exp)
  expect_equal(df$n_controls_nexp,resgen$n_controls_nexp)
})


###########
### RMA ###
###########

test_that(".rma_to_umbrella_x correctly extracts information and produces appropriate error message when being called with inappropriate inputs: SMD", {

  df <- df.SMD
  # standard smd
  metasmd <- metafor::rma.uni(m1i = mean_cases, m2i = mean_controls,
                              sd1i = sd_cases, sd2i = sd_controls,
                              n1i = n_cases, n2i = n_controls,
                              data = df, method = "REML", measure = "SMD")

  expect_error(.rma_to_umbrella_x(metasmd, n_cases = NULL, n_controls = NULL), "The number of cases or the number of controls should be indicated when calling the esb.test function from an 'rma' object. You can do it via the 'n_cases' or 'n_controls' arguments of the esb.test function.", fixed = TRUE)

  res_metasmd_ca <- .rma_to_umbrella_x(metasmd, n_cases = df$n_cases, n_controls = NULL, measure = "SMD")
  res_metasmd_co <- .rma_to_umbrella_x(metasmd, n_cases = NULL, n_controls = df$n_controls, measure = "SMD")

  expect_equal(df$value, res_metasmd_ca$value, tolerance = tol_large)
  expect_equal(df$n_cases, res_metasmd_ca$n_cases, tolerance = tol_large)
  expect_equal(df$n_controls, res_metasmd_ca$n_controls, tolerance = tol_large)
  expect_equal(df$value, res_metasmd_co$value, tolerance = tol_large)
  expect_equal(df$n_cases, res_metasmd_co$n_cases, tolerance = tol_large)
  expect_equal(df$n_controls, res_metasmd_co$n_controls, tolerance = tol_large)
  expect_equal(df$se, res_metasmd_ca$se, tolerance = tol_large)
  expect_equal(df$se, res_metasmd_co$se, tolerance = tol_large)
})

test_that(".rma_to_umbrella_x correctly extracts information and produces appropriate error message when being called with inappropriate inputs: OR", {
  df <- df.OR
  df$value <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                           n_controls_exp, n_controls_nexp)$value)
  df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                        n_controls_exp, n_controls_nexp)$se)

  # standard OR
  metaor <- metafor::rma.uni(ai = n_cases_exp, bi = n_cases_nexp,
                             ci = n_controls_exp, di = n_controls_nexp,
                             data = df, method = "DL", measure = "OR")


  # unsupported measure
  metarr <- metafor::rma.uni(ai = n_cases_exp, bi = n_cases_nexp,
                             ci = n_controls_exp, di = n_controls_nexp,
                             data = df, method = "DL", measure = "RR")

  expect_error(.rma_to_umbrella_x(metarr, n_cases = df$n_cases, n_controls = df$n_controls, measure = "RR"),
               "The esb.test function can be called from an 'rma' or 'meta' object only with 'G', 'SMD' or 'OR' as effect size measure",
               fixed = TRUE)

  # missing measure
  metagenor <- metafor::rma.uni(yi = log(value), vi = se^2, data = df)
  expect_error(.rma_to_umbrella_x(metagenor,
                                   n_cases = df$n_cases,
                                   n_controls = df$n_controls,
                                  measure = NULL
  ), "The effect size measure should be indicated when calling the esb.test function from a rma object (via the 'measure' argument in the rma function or via the 'measure' argument of the esb.test function)",
  fixed = TRUE)

  # missing sample sizes
  metagenor <- metafor::rma.uni(yi = log(value), vi = se^2, data = df, measure = "OR")
  #n_controls
  expect_error(.meta_to_umbrella_x(metagenor,
                                   n_cases = df$n_cases,
                                   n_controls = NULL, measure = "OR"
  ), "The number of controls should be indicated. You can do it when calling the meta function or when calling the esb.test function (via 'n_controls' argument)", fixed = TRUE)
  #n_cases
  expect_error(.meta_to_umbrella_x(metagenor,
                                   n_cases = NULL,
                                   n_controls = df$n_controls, measure = "OR"
  ), "The number of cases should be indicated. You can do it when calling the meta function or when calling the esb.test function (via 'n_cases' argument)", fixed = TRUE)

  # missing sample sizes
  metagenor <- metafor::rma.uni(yi = log(value), vi = se^2, ni = df$n_cases + df$n_controls, data = df, measure = "OR")
  res_metaor_co <- .rma_to_umbrella_x(metaor,
                                   n_cases = NULL,
                                   n_controls = df$n_controls, measure = "OR")
  res_metaor_ca <- .rma_to_umbrella_x(metaor,
                                      n_controls = NULL,
                                      n_cases = df$n_cases, measure = "OR")
  res_metagenor_co <- .rma_to_umbrella_x(metagenor,
                                   n_cases = NULL,
                                   n_controls = df$n_controls, measure = "OR")
  res_metagenor_ca <- .rma_to_umbrella_x(metagenor,
                                   n_controls = NULL,
                                   n_cases = df$n_cases, measure = "OR")

  expect_equal(df$value, res_metaor_co$value, tolerance = tol_large)
  expect_equal(df$se, res_metaor_co$se, tolerance = tol_large)
  expect_equal(df$n_cases, res_metaor_co$n_cases, tolerance = tol_large)
  expect_equal(df$n_controls, res_metaor_co$n_controls, tolerance = tol_large)
  expect_equal(df$value, res_metaor_ca$value, tolerance = tol_large)
  expect_equal(df$se, res_metaor_ca$se, tolerance = tol_large)
  expect_equal(df$n_cases, res_metaor_ca$n_cases, tolerance = tol_large)
  expect_equal(df$n_controls, res_metaor_ca$n_controls, tolerance = tol_large)

  expect_equal(df$value, res_metagenor_co$value, tolerance = tol_large)
  expect_equal(df$se, res_metagenor_co$se, tolerance = tol_large)
  expect_equal(df$n_cases, res_metagenor_co$n_cases, tolerance = tol_large)
  expect_equal(df$n_controls, res_metagenor_co$n_controls, tolerance = tol_large)
  expect_equal(df$value, res_metagenor_ca$value, tolerance = tol_large)
  expect_equal(df$se, res_metagenor_ca$se, tolerance = tol_large)
  expect_equal(df$n_cases, res_metagenor_ca$n_cases, tolerance = tol_large)
  expect_equal(df$n_controls, res_metagenor_ca$n_controls, tolerance = tol_large)
})

test_that(".rma_to_umbrella_x correctly extracts information from generic inputs: SMD", {

  df <- df.SMD
  # generic SMD
  j = .d_j(df$n_cases + df$n_controls - 2)
  metagensmd <- metafor::rma(yi = value * j, sei = se * j, data = df)

  resgen <- .rma_to_umbrella_x(metagensmd,
                                n_cases = df$n_cases,
                                n_controls = df$n_controls,
                                measure = "SMD")

  expect_equal(df$value, resgen$value)
  expect_equal(df$se,resgen$se)
  expect_equal(df$n_cases,resgen$n_cases)
  expect_equal(df$n_controls,resgen$n_controls)
})

test_that(".rma_to_umbrella_x correctly extracts information from generic inputs: OR", {

  # generic OR
  df <- df.OR
  df$value <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                           n_controls_exp, n_controls_nexp)$value)
  df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                        n_controls_exp, n_controls_nexp)$se)

  metagenor <- metafor::rma(yi = log(value), sei = se, data = df)

  resgen <- .rma_to_umbrella_x(metagenor,
                                n_cases = df$n_cases,
                                n_controls = df$n_controls,
                                measure = "OR")

  expect_equal(df$value, resgen$value)
  expect_equal(df$se,resgen$se)
  expect_equal(df$n_cases,resgen$n_cases)
  expect_equal(df$n_controls,resgen$n_controls)
  expect_equal(df$n_cases_exp,resgen$n_cases_exp)
  expect_equal(df$n_cases_nexp,resgen$n_cases_nexp)
  expect_equal(df$n_controls_exp,resgen$n_controls_exp)
  expect_equal(df$n_controls_nexp,resgen$n_controls_nexp)
})

# select largest
test_that(".largest_smd selects the largest study", {

  df <- df.SMD[1:5,]
  df$n_cases[1] <- df$n_cases[4]
  df$n_controls[1] <- df$n_controls[4]
  df.largest <- .quiet(.format_dataset(attr(.check_data(df), "data")))
  largest <- .largest_smd(df.largest, return = "ci")

  expect_equal(df.largest$ci_lo[1], .as_numeric(largest[1]))
  expect_equal(df.largest$ci_up[1], .as_numeric(largest[2]))

  largest <- .largest_smd(df.largest, return = "value")

  expect_equal(df.largest$value[1], largest)
})

test_that(".largest_or_rr_hr selects the largest study", {

  df <- df.OR[1:5,]
  df$n_cases_exp[2] <- df$n_cases_exp[1]
  df$n_cases_nexp[2] <- df$n_cases_nexp[1]
  df$n_controls_exp[2] <- df$n_controls_exp[1]
  df$n_controls_nexp[2] <- df$n_controls_nexp[1]
  df.for <- subset(df, select = -c(n_cases, n_controls, n_nexp, n_exp))
  df.largest <- .quiet(.format_dataset(attr(.check_data(df.for), "data")))

  largest <- .largest_or_rr_hr(df.largest, return = "ci")

  expect_equal(df.largest$ci_lo[1], .as_numeric(largest[1]))
  expect_equal(df.largest$ci_up[1], .as_numeric(largest[2]))

  largest <- .largest_or_rr_hr(df.largest, return = "value")

  expect_equal(df.largest$value[1], largest)
})

test_that(".largest_irr selects the largest study", {

  df <- df.IRR[1:5, ]
  df$time_exp[1] <- df$time_exp[4]
  df$time_nexp[1] <- df$time_nexp[4]
  df.for <- subset(df, select = -c(n_cases, time))
  df.largest <- .quiet(.format_dataset(attr(.check_data(df.for), "data")))

  largest <- .largest_irr(df.largest, return = "ci")

  expect_equal(df.largest$ci_lo[4], .as_numeric(largest[1]))
  expect_equal(df.largest$ci_up[4], .as_numeric(largest[2]))

  largest <- .largest_irr(df.largest, return = "value")

  expect_equal(df.largest$value[4], largest)

})

#####################################################################################################

### CONVERSIONS BETWEEN MEASURES

# r to d
test_that(".r_to_d", {
  r_umb = .r_to_d(df.R$value)
  r_esc = esc::cohens_d(r = df.R$value)
  expect_equal(r_umb, r_esc, tolerance = tol_large)
})
test_that(".z_to_d", {
  z_umb = .z_to_d(.estimate_z_from_r(r=df.R$value, n_sample = df.R$n_sample))
  z_esc = esc::cohens_d(r = esc::convert_z2r(.estimate_z_from_r(r=df.R$value, n_sample = df.R$n_sample)))
  expect_equal(z_umb, z_esc, tolerance = tol_large)
})
test_that(".d_to_or", {
  or_umb = .d_to_or(df.SMD$value)
  or_esc = esc::odds_ratio(d = df.SMD$value)
  expect_equal(or_umb, or_esc, tolerance = tol_large)
})
test_that(".or_to_d", {
  d_umb = .or_to_d(df.OR$value)
  d_esc = esc::cohens_d(or = df.OR$value)
  expect_equal(d_umb, d_esc, tolerance = tol_large)
})

## R to SMD
test_that("correctly converts: R to SMD", {
  df.full.smd <- subset(df.SMD, factor == "Surgical")
  df.SMD.R <- subset(df.SMD, factor == "Surgical")
  df.5 <- df.full.smd[1:5,]

  df.SMD.R$value[1:5] = .d_to_r(df.5$value, n_cases = df.5$n_cases, n_controls = df.5$n_controls)
  df.SMD.R$se[1:5] = with(df.full.smd, (((n_cases[1:5]+n_controls[1:5])^2/(n_cases[1:5]*n_controls[1:5]))^2 * se[1:5]^2) / ((value[1:5]^2 + ((n_cases[1:5]+n_controls[1:5])^2/(n_cases[1:5]*n_controls[1:5])))^3))
  df.SMD.R$n_sample = NA
  df.SMD.R$n_sample[1:5] = with(df.full.smd, n_cases[1:5] + n_controls[1:5])

  df.SMD.R$measure[1:5] <- "R"

  df.5$value = .d_to_r(df.5$value, n_cases = df.5$n_cases, n_controls = df.5$n_controls)

  d_exp = .estimate_g_from_d(d = .r_to_d(.z_to_r(.estimate_z_from_r(r = df.SMD.R$value[1:5], n_sample = df.SMD.R$n_sample[1:5])$value)),
                             n_cases = round(df.5$n_cases + df.5$n_controls / 2),
                             n_controls = round(df.5$n_cases + df.5$n_controls / 2))

  df.SMD.R <- subset(df.SMD.R, select = -c(mean_cases, mean_controls, ci_lo, ci_up, sd_cases, sd_controls, se))

  umb.SMD.R <- .quiet(umbrella(df.SMD.R, seed = 4321))
  umb.full.smd <- .quiet(umbrella(df.full.smd, seed = 4321))
  expect_equal(umb.SMD.R[[1]]$x$value[1:5], umb.full.smd[[1]]$x$value[1:5], tolerance = 1e-1)
  expect_equal(umb.SMD.R[[1]]$x$se[1:5], umb.full.smd[[1]]$x$se[1:5], tolerance = 1e-1)
  expect_equal(umb.SMD.R[[1]]$ma_results, umb.full.smd[[1]]$ma_results, tolerance = 5e-2)
  expect_equal(umb.SMD.R[[1]]$egger$p.value, umb.full.smd[[1]]$egger$p.value, tolerance = 5e-2)
  expect_equal(umb.SMD.R[[1]]$esb$p.value, umb.full.smd[[1]]$esb$p.value, tolerance = 1e-5)
})

## RR to OR: full information
test_that("RR to OR: full information", {
  df.save <- subset(df.OR, factor == "ASD")
  df <- subset(df.OR, factor == "ASD")
  convert = .estimate_rr_from_n(n_cases_exp = df$n_cases_exp[1:5], n_exp = df$n_exp[1:5],
                                n_cases_nexp = df$n_cases_nexp[1:5], n_nexp = df$n_nexp[1:5])
  df$value[1:5] <- convert$value
  df$ci_lo[1:5] <- convert$value/exp(qnorm(0.975) * convert$se)
  df$ci_up[1:5] <- convert$value*exp(qnorm(0.975) * convert$se)
  df$measure[1:5] <- "RR"

  umb1 <- .quiet(umbrella(df, seed = 4321))
  umb2 <- .quiet(umbrella(df.save, seed = 4321))
  expect_equal(umb1[[1]]$x$value[1:5], umb2[[1]]$x$value[1:5], tolerance = tol_large)
  expect_equal(umb1$ASD$ma_results, umb2$ASD$ma_results, tolerance = tol_large)
  expect_equal(umb1$ASD$esb$p.value, umb2$ASD$esb$p.value, tolerance = tol_large)
})

## RR to OR: ES CI N_cases
test_that("RR to OR: ES CI N_cases", {
  df.full.OR <- subset(df.OR, factor == "ASD")
  df.OR.RR <- subset(df.OR, factor == "ASD")
  convert = .estimate_rr_from_n(n_cases_exp = df.OR.RR$n_cases_exp[1:5], n_exp = df.OR.RR$n_exp[1:5],
                                n_cases_nexp = df.OR.RR$n_cases_nexp[1:5], n_nexp = df.OR.RR$n_nexp[1:5])
  df.OR.RR$value[1:5] <- convert$value
  df.OR.RR$ci_lo[1:5] <- convert$value/exp(qnorm(0.975) * convert$se)
  df.OR.RR$ci_up[1:5] <- convert$value*exp(qnorm(0.975) * convert$se)
  df.OR.RR$measure[1:5] <- "RR"

  df.full.OR <- subset(df.full.OR, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  df.OR.RR <- subset(df.OR.RR, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))

  umb1 <- .quiet(umbrella(df.full.OR, seed = 4321))
  umb2 <- .quiet(umbrella(df.OR.RR, seed = 4321))
  expect_equal(umb1[[1]]$x$value[1:5], umb2[[1]]$x$value[1:5], tolerance = 5e-2)
  expect_equal(umb1$ASD$ma_results, umb2$ASD$ma_results, tolerance = 2e-1)
  expect_equal(umb1$ASD$esb$p.value, umb2$ASD$esb$p.value, tolerance = 5e-1)
})


## HR to OR
test_that("HR to OR", {
  df.save <- subset(df.OR, factor == "ASD")
  df <- subset(df.OR, factor == "ASD")
  df$measure[1:5] <- "HR"

  umb1 <- .quiet(umbrella(df, seed = 4321))
  umb2 <- .quiet(umbrella(df.save, seed = 4321))
  expect_equal(umb1[[1]]$x$value[1:5], umb2[[1]]$x$value[1:5], tolerance = tol_large)
  expect_equal(umb1$ASD$ma_results, umb2$ASD$ma_results, tolerance = tol_large)
  expect_equal(umb1$ASD$egger$p.value, umb2$ASD$egger$p.value, tolerance = tol_large)
  expect_equal(umb1$ASD$esb$p.value, umb2$ASD$esb$p.value, tolerance = tol_large)
})

## OR to SMD
test_that("OR to SMD correctly converted when value + CI", {
  df.full.smd <- subset(df.SMD, factor == "Surgical")
  df.mix.OR.SMD <- subset(df.SMD, factor == "Surgical")

  df.mix.OR.SMD$measure[1:5] <- "OR"
  df.mix.OR.SMD$value[1:5] <- .d_to_or(df.mix.OR.SMD$value[1:5])
  df.mix.OR.SMD$ci_lo[1:5] <- .d_to_or(df.mix.OR.SMD$ci_lo[1:5])
  df.mix.OR.SMD$ci_up[1:5] <- .d_to_or(df.mix.OR.SMD$ci_up[1:5])

  df.mix.OR.SMD <- subset(df.mix.OR.SMD, select = -c(se, mean_cases, sd_cases, mean_controls, sd_controls))
  df.full.smd <- subset(df.full.smd, select = -c(se, mean_cases, sd_cases, mean_controls, sd_controls))

  umb.mix.OR.SMD <- .quiet(umbrella(df.mix.OR.SMD, seed = 4321))
  umb.full.smd <- .quiet(umbrella(df.full.smd, seed = 4321))
  expect_equal(umb.mix.OR.SMD[[1]]$x$value[1:5], umb.full.smd[[1]]$x$value[1:5], tolerance = 1e-6)
  expect_equal(df.mix.OR.SMD$value[1:5], .d_to_or(umb.mix.OR.SMD[[1]]$x$value[1:5]), tolerance = 5e-3)
  expect_equal(umb.mix.OR.SMD[[1]]$ma_results, umb.full.smd[[1]]$ma_results, tolerance = 1e-6)
  expect_equal(umb.mix.OR.SMD[[1]]$egger$p.value, umb.full.smd[[1]]$egger$p.value, tolerance = 1e-6)
  expect_equal(umb.mix.OR.SMD[[1]]$esb$p.value, umb.full.smd[[1]]$esb$p.value, tolerance = 1e-6)
})

## OR to SMD
test_that("OR to SMD correctly converted when value + SE", {
  df.full.smd <- subset(df.SMD, factor == "Surgical")
  df.mix.OR.SMD <- subset(df.SMD, factor == "Surgical")

  df.mix.OR.SMD$measure[1:5] <- "OR"
  df.mix.OR.SMD$value[1:5] <- .d_to_or(df.mix.OR.SMD$value[1:5])
  df.mix.OR.SMD$ci_lo[1:5] <- .d_to_or(df.mix.OR.SMD$ci_lo[1:5])
  df.mix.OR.SMD$ci_up[1:5] <- .d_to_or(df.mix.OR.SMD$ci_up[1:5])

  df.mix.OR.SMD <- subset(df.mix.OR.SMD, select = -c(ci_lo, ci_up, mean_cases, sd_cases, mean_controls, sd_controls))
  df.full.smd <- subset(df.full.smd, select = -c(ci_lo, ci_up, mean_cases, sd_cases, mean_controls, sd_controls))

  umb.mix.OR.SMD <- .quiet(umbrella(df.mix.OR.SMD, seed = 4321))
  umb.full.smd <- .quiet(umbrella(df.full.smd, seed = 4321))
  expect_equal(umb.mix.OR.SMD[[1]]$x$value[1:5], umb.full.smd[[1]]$x$value[1:5], tolerance = tol_large)
  expect_equal(df.mix.OR.SMD$value[1:5], .d_to_or(umb.mix.OR.SMD[[1]]$x$value[1:5]), tolerance = 5e-3)
  expect_equal(umb.mix.OR.SMD[[1]]$ma_results, umb.full.smd[[1]]$ma_results, tolerance = tol_large)
  expect_equal(umb.mix.OR.SMD[[1]]$egger$p.value, umb.full.smd[[1]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb.mix.OR.SMD[[1]]$esb$p.value, umb.full.smd[[1]]$esb$p.value, tolerance = tol_large)
})

## SMD to OR
test_that("SMD to OR  correctly converted when value + CI", {
  df.full.OR <- subset(df.OR, factor == "ADHD")
  df.full.OR$se <- with(df.full.OR, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                                        n_controls_exp, n_controls_nexp)$se)

  df.mix.OR.SMD <- subset(df.full.OR, factor == "ADHD")

  df.mix.OR.SMD$measure[1:5] <- "SMD"
  df.mix.OR.SMD$value[1:5] <- .or_to_d(df.mix.OR.SMD$value[1:5])
  df.mix.OR.SMD$se[1:5] = sqrt(df.mix.OR.SMD$se[1:5]^2 * 3 / (pi^2))
  df.mix.OR.SMD <- subset(df.mix.OR.SMD, select = -c(ci_lo, ci_up, n_cases_exp, n_cases_nexp))
  df.full.OR <- subset(df.full.OR, select = -c(ci_lo, ci_up, n_cases_exp, n_cases_nexp))

  x_conv = .convert_SMD_to_OR(attr(.check_data(df.mix.OR.SMD), "data"))

  expect_equal(x_conv$value[1:5], df.full.OR$value[1:5], tolerance = tol_large)
  expect_equal(x_conv$se[1:5], df.full.OR$se[1:5], tolerance = tol_large)
})

## SMD to SMC
test_that("SMC to SMC correctly converts: smd raw", {
  df.full.smd <- subset(df.SMD, factor == "Surgical")
  df.mix.SMC.SMD <- subset(df.SMD, factor == "Surgical")

  df.mix.SMC.SMD$measure[1:5] <- "SMC"

  df.mix.SMC.SMD <- subset(df.mix.SMC.SMD, select = -c(mean_cases, mean_controls))

  umb.mix.SMC.SMD <- .quiet(umbrella(df.mix.SMC.SMD, seed = 4321))
  umb.full.smd <- .quiet(umbrella(df.full.smd, seed = 4321))
  expect_equal(umb.mix.SMC.SMD[[1]]$x$value[1:5], umb.full.smd[[1]]$x$value[1:5], tolerance = tol_large)
  expect_equal(umb.mix.SMC.SMD[[1]]$ma_results, umb.full.smd[[1]]$ma_results, tolerance = tol_large)
  expect_equal(umb.mix.SMC.SMD[[1]]$egger$p.value, umb.full.smd[[1]]$egger$p.value, tolerance = tol_large)
  expect_equal(umb.mix.SMC.SMD[[1]]$esb$p.value, umb.full.smd[[1]]$esb$p.value, tolerance = tol_large)
})

test_that("SMC to SMC correctly converts: smc raw", {
  df.full.smc <- subset(df.SMC, factor == "Prediabetes")
  df.mix.SMC.SMD <- subset(df.SMC, factor == "Prediabetes")

  df.mix.SMC.SMD$measure[1:5] <- "SMD"

  df.mix.SMC.SMD <- subset(df.mix.SMC.SMD, select = -c(mean_cases, mean_controls))

  df.full.smc$mean_pre_cases[1:5] = NA

  umb.mix.SMC.SMD <- umbrella(df.mix.SMC.SMD, seed = 4321, verbose = FALSE)
  umb.full.smc <- umbrella(df.full.smc, verbose = FALSE, seed = 4321)

  row.names(umb.mix.SMC.SMD[[1]]$ma_results) = "X"
  row.names(umb.full.smc[[1]]$ma_results) = "X"

  expect_equal(umb.mix.SMC.SMD[[1]]$x$mean_cases, umb.full.smc[[1]]$x$value, tolerance = tol_large)
  expect_equal(umb.mix.SMC.SMD[[1]]$ma_results, umb.full.smc[[1]]$ma_results, tolerance = 1e-1)
  expect_equal(umb.mix.SMC.SMD[[1]]$egger$p.value, umb.full.smc[[1]]$egger$p.value, tolerance = 3e-1)
  ## ESB
  expect_equal(umb.mix.SMC.SMD[[1]]$esb$p.value, umb.full.smc[[1]]$esb$p.value, tolerance = 1)
})

## ERREUR IRR
test_that("IRR to SMD", {
  df.save <- subset(df.SMD, factor == "Surgical")

  df.save$measure[1:5] <- "IRR"
  df.save$time <- NA; df.save$time[1:5] <- 1
  df.save$value[1:5] <- abs(df.save$value[1:5])
  df.save$ci_lo[1:5] <- 0.01
  df.save$ci_up[1:5] <- 5
  check <- grepl("Within a single factor, it is thus not possible to combine 'IRR' with another additional measure",
                 attr(.check_data(df.save), "message"), fixed = TRUE)
  expect_equal(check, TRUE)

})
