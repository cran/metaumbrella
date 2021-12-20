test_that(".meta_to_umbrella_x correctly extracts information and produces appropriate error message when being called with inappropriate inputs", {
  df <- df.SMD
  # standard smd
  metasmd <- meta::metacont(n_cases, mean_cases, sd_cases,
                             n_controls, mean_controls, sd_controls,
                             method.tau = "REML", data = df,
                             sm = "SMD", method.smd = "Hedges")

  res_metasmd <- .meta_to_umbrella_x(metasmd, n_cases = NULL, n_controls = NULL, measure = "SMD")
  expect_equal(df$value, res_metasmd$value, tolerance = 1e-6)
  expect_equal(df$se, res_metasmd$se, tolerance = 1e-16)
  expect_equal(df$n_cases, res_metasmd$n_cases, tolerance = 1e-16)
  expect_equal(df$n_controls, res_metasmd$n_controls, tolerance = 1e-16)

  metasmd.C <- meta::metacont(n_cases, mean_cases, sd_cases,
                             n_controls, mean_controls, sd_controls,
                             method.tau = "REML", data = df,
                             sm = "SMD", method.smd = "Cohen")

  res_metasmd.C <- .meta_to_umbrella_x(metasmd.C, n_cases = NULL, n_controls = NULL, measure = "SMD")
  expect_equal(df$value, res_metasmd.C$value, tolerance = 1e-16)
  expect_equal(df$se, res_metasmd.C$se, tolerance = 1e-16)
  expect_equal(df$n_cases, res_metasmd.C$n_cases, tolerance = 1e-16)
  expect_equal(df$n_controls, res_metasmd.C$n_controls, tolerance = 1e-16)

  dfor <- df.OR
  dfor$value <- with(dfor, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                           n_controls_exp, n_controls_nexp)$value)
  dfor$se <- with(dfor, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                        n_controls_exp, n_controls_nexp)$se)

  # standard OR
  metaor <- meta::metabin(event.e = n_cases_exp, n.e = n_exp,
                          event.c = n_cases_nexp, n.c = n_nexp,
                          data = dfor, sm = "OR")

  res_metaor <- .meta_to_umbrella_x(metaor, n_cases = NULL, n_controls = NULL, measure = "OR")

  expect_equal(dfor$value, res_metaor$value, tolerance = 1e-15)
  expect_equal(dfor$se, res_metaor$se, tolerance = 1e-15)
  expect_equal(dfor$n_cases, res_metaor$n_cases, tolerance = 1e-16)
  expect_equal(dfor$n_controls, res_metaor$n_controls, tolerance = 1e-16)

  # unsupported measure
  metarr <- meta::metabin(event.e = n_cases_exp, n.e = n_exp,
                          event.c = n_cases_nexp, n.c = n_nexp,
                          data = df.RR, sm = "RR")
  expect_error(.meta_to_umbrella_x(metarr, n_cases = NULL, n_controls = NULL, measure = "RR"),
               "The esb.test function can be called from a 'meta' object only with 'SMD' or 'OR' as effect size measures.",
               fixed = TRUE)

  # missing measure
  metagenor <- meta::metagen(TE = log(value), seTE = se, data = dfor)
  expect_error(.meta_to_umbrella_x(metagenor,
                                   n_cases = df$n_cases,
                                   n_controls = df$n_controls,
                                   measure = NULL
                                   ),
               "The effect size measure should be indicated when calling the esb.test function from a meta object (via the 'sm' argument in the meta function or via the 'measure' argument of the esb.test function)",
               fixed = TRUE)

  # missing sample sizes
  metagenor <- meta::metagen(TE = log(value), seTE = se, data = dfor, sm = "OR")
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
  metagensmd <- meta::metagen(TE = value * j, seTE = se * j, data = df)

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

  df <- df.OR
  df$value <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                           n_controls_exp, n_controls_nexp)$value)
  df$se <- with(df, .estimate_or_from_n(n_cases_exp, n_cases_nexp,
                                        n_controls_exp, n_controls_nexp)$se)

  metagenor <- meta::metagen(TE = log(value), seTE = se, data = df)

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

  expect_equal(df$value, res_metasmd_ca$value, tolerance = 1e-14)
  expect_equal(df$n_cases, res_metasmd_ca$n_cases, tolerance = 1e-16)
  expect_equal(df$n_controls, res_metasmd_ca$n_controls, tolerance = 1e-16)
  expect_equal(df$value, res_metasmd_co$value, tolerance = 1e-14)
  expect_equal(df$n_cases, res_metasmd_co$n_cases, tolerance = 1e-16)
  expect_equal(df$n_controls, res_metasmd_co$n_controls, tolerance = 1e-16)
  expect_equal(df$se, res_metasmd_ca$se, tolerance = 1e-16)
  expect_equal(df$se, res_metasmd_co$se, tolerance = 1e-16)
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
               "The esb.test function can be called from an 'rma' or 'meta' object only with 'SMD' or 'OR' as effect size measure",
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

  expect_equal(df$value, res_metaor_co$value, tolerance = 1e-14)
  expect_equal(df$se, res_metaor_co$se, tolerance = 1e-14)
  expect_equal(df$n_cases, res_metaor_co$n_cases, tolerance = 1e-16)
  expect_equal(df$n_controls, res_metaor_co$n_controls, tolerance = 1e-16)
  expect_equal(df$value, res_metaor_ca$value, tolerance = 1e-14)
  expect_equal(df$se, res_metaor_ca$se, tolerance = 1e-14)
  expect_equal(df$n_cases, res_metaor_ca$n_cases, tolerance = 1e-16)
  expect_equal(df$n_controls, res_metaor_ca$n_controls, tolerance = 1e-16)

  expect_equal(df$value, res_metagenor_co$value, tolerance = 1e-14)
  expect_equal(df$se, res_metagenor_co$se, tolerance = 1e-14)
  expect_equal(df$n_cases, res_metagenor_co$n_cases, tolerance = 1e-16)
  expect_equal(df$n_controls, res_metagenor_co$n_controls, tolerance = 1e-16)
  expect_equal(df$value, res_metagenor_ca$value, tolerance = 1e-14)
  expect_equal(df$se, res_metagenor_ca$se, tolerance = 1e-14)
  expect_equal(df$n_cases, res_metagenor_ca$n_cases, tolerance = 1e-16)
  expect_equal(df$n_controls, res_metagenor_ca$n_controls, tolerance = 1e-16)
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
  expect_equal(umb1[[1]]$x$value[1:5], umb2[[1]]$x$value[1:5], tolerance = 1e-14)
  expect_equal(umb1$ASD$random, umb2$ASD$random, tolerance = 1e-14)
  expect_equal(umb1$ASD$esb$p.value, umb2$ASD$esb$p.value, tolerance = 1e-14)
})

## RR to OR: ES CI N_cases
test_that("RR to OR: ES CI N_cases", {
  df.save <- subset(df.OR, factor == "ASD")
  df <- subset(df.OR, factor == "ASD")
  convert = .estimate_rr_from_n(n_cases_exp = df$n_cases_exp[1:5], n_exp = df$n_exp[1:5],
                                n_cases_nexp = df$n_cases_nexp[1:5], n_nexp = df$n_nexp[1:5])
  df$value[1:5] <- convert$value
  df$ci_lo[1:5] <- convert$value/exp(qnorm(0.975) * convert$se)
  df$ci_up[1:5] <- convert$value*exp(qnorm(0.975) * convert$se)
  df$measure[1:5] <- "RR"

  dfor <- subset(df.save, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  dfmix <- subset(df, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))

  umb1 <- .quiet(umbrella(dfor, seed = 4321))
  umb2 <- .quiet(umbrella(dfmix, seed = 4321))
  expect_equal(umb1[[1]]$x$value[1:5], umb2[[1]]$x$value[1:5], tolerance = 5e-2)
  expect_equal(umb1$ASD$random, umb2$ASD$random, tolerance = 2e-1)
  expect_equal(umb1$ASD$esb$p.value, umb2$ASD$esb$p.value, tolerance = 5e-1)
})


## HR to OR
test_that("HR to OR", {
  df.save <- subset(df.OR, factor == "ASD")
  df <- subset(df.OR, factor == "ASD")
  df$measure[1:5] <- "HR"

  umb1 <- .quiet(umbrella(df, seed = 4321))
  umb2 <- .quiet(umbrella(df.save, seed = 4321))
  expect_equal(umb1[[1]]$x$value[1:5], umb2[[1]]$x$value[1:5], tolerance = 1e-14)
  expect_equal(umb1$ASD$random, umb2$ASD$random, tolerance = 1e-14)
  expect_equal(umb1$ASD$esb$p.value, umb2$ASD$esb$p.value, tolerance = 1e-14)
})

## OR to SMD
test_that("OR to SMD", {
  df.save <- subset(df.SMD, factor == "Surgical")
  df <- subset(df.SMD, factor == "Surgical")

  df$measure[1:5] <- "OR"
  df$value[1:5] <- .d_to_or(df$value[1:5])
  df$ci_lo[1:5] <- .d_to_or(df$ci_lo[1:5])
  df$ci_up[1:5] <- .d_to_or(df$ci_up[1:5])

  dfsmd <- subset(df.save, select = -c(mean_cases, sd_cases, mean_controls, sd_controls))
  dfmix <- subset(df, select = -c(mean_cases, sd_cases, mean_controls, sd_controls))

  umb1 <- .quiet(umbrella(df, seed = 4321))
  umb2 <- .quiet(umbrella(df.save, seed = 4321))
  expect_equal(umb1[[1]]$x$value[1:5], umb2[[1]]$x$value[1:5], tolerance = 1e-8)
  expect_equal(umb1$Surgical$random, umb2$Surgical$random, tolerance = 1e-5)
  expect_equal(umb1$Surgical$esb$p.value, umb2$Surgical$esb$p.value, tolerance = 1e-5)
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
