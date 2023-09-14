# total_n = 6145
# power_low = 100
# rob = 33.4
# n_cases = 4337
# stud = 51
# I2 = 58.19
# p_value = 2.3e-19
# largest_CI = (0.77, 1.06)
# pi = (0.58, 1.06)
# esb_p = 7.3e-10
# egger_p =  0.00025
# amstar = NA


# RANKING
test_that("'Personalized' criteria produced correct ranking: 1", {
  umb <- .quiet(umbrella(df.RR))
  pers <- summary.umbrella(
    .quiet(add.evidence(umb, criteria = "Personalized",
                 class_I = c(n_studies = 50, total_n = 6146, imprecision = 100),
                 class_II = c(n_cases = 5000, p_value = .001, rob = 50, esb_p = 0.5, egger_p = 0.5),
                 class_III = c(pi = "notnull", largest_CI = "notnull", amstar = 4),
                 class_IV = c(I2 = 90))))
  expect_equal(as.character(pers$Class), "IV")
})

test_that("'Personalized' criteria produced correct ranking: 2", {
  umb <- .quiet(umbrella(df.RR))
  pers <- .quiet(summary.umbrella(
    add.evidence(umb, criteria = "Personalized",
                 class_I = c(n_studies = 30, n_cases = 3000, amstar = 4),
                 class_II = c(n_cases = 50),
                 class_III = c(I2 = 30),
                 class_IV = c(p_value = 1e-5))))
  expect_equal(as.character(pers$Class), "II")
})

test_that("'Personalized' criteria produced correct ranking: 3", {
  umb <- .quiet(umbrella(df.RR))
  pers <- .quiet(summary.umbrella(
    add.evidence(umb, criteria = "Personalized",
                 class_I = c(imprecision = 2, rob = 40),
                 class_II = c(amstar = 2),
                 class_III = c(I2 = 20),
                 class_IV = c(p_value = 1e-5))))
  expect_equal(as.character(pers$Class), "IV")
})

test_that("'Personalized' criteria produced correct ranking: 4", {
  umb <- .quiet(umbrella(df.RR, method.esb = "IT.binom", true_effect = "largest"))
  pers <- .quiet(summary.umbrella(
    add.evidence(umb, criteria = "Personalized",
                 class_I = c(egger_p = 0.05),
                 class_II = c(esb_p = 0.05))))
  expect_equal(as.character(pers$Class), "III")
})

test_that("'Personalized' criteria produced correct ranking: 5", {
  umb <- .quiet(umbrella(df.RR, method.esb = "IT.binom", true_effect = "largest"))
  pers <- .quiet(summary.umbrella(
    add.evidence(umb, criteria = "Personalized",
                 class_II = c(esb_p = 0.05))))

  expect_equal(as.character(pers$Class), "I")
})

 test_that("'Personalized' criteria produced correct ranking: 6", {
  umb <- .quiet(umbrella(df.RR))
  pers <- .quiet(summary.umbrella(
    add.evidence(umb, criteria = "Personalized",
                 class_I = c(pi = "notnull", n_studies = 2, n_cases = 2, imprecision = 0),
                 class_II = c(largest_CI = "notnull", p_value = 1, esb_p = 0, egger_p = 0))))
  expect_equal(as.character(pers$Class), "III")
})

# ERROR MESSAGES

 test_that("'Personalized criteria produced appropriate error messages: 1.A", {
   umb <- .quiet(umbrella(df.RR))
   expect_error(add.evidence(umb, criteria = "Personalized",
                             class_I = c(n_studies = 4, n_studies = 3),
                             class_I = c(n_studies = 3)),
   "matched by multiple actual arguments",
                fixed = TRUE)
 })
 test_that("'Personalized criteria produced appropriate error messages: 1.B", {
   umb <- .quiet(umbrella(df.RR))
   expect_error(add.evidence(umb, criteria = "Personalized",
                             class_I = c(n_studies = 4, n_studies = 3)),
                "There is a repeated entry of a criterion for a class. Check criteria used for each class.",
                fixed = TRUE)
 })

test_that("'Personalized criteria produced appropriate error messages: 2", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(n_studies = -2, imprecision = 2, rob = 40),
                            class_II = c(amstar = 2),
                            class_III = c(I2 = 20),
                            class_IV = c(p_value = 1e-5)),
               "The 'n_studies' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 3", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(n_cases = -2, imprecision = 2, rob = 40),
                            class_II = c(amstar = 2),
                            class_III = c(I2 = 20),
                            class_IV = c(p_value = 1e-5)),
               "The 'n_cases' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 4", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(p_value = 4, imprecision = 2, rob = 40),
                            class_II = c(amstar = 2),
                            class_III = c(I2 = 20),
                            class_IV = c(p_value = 1e-5)),
               "The 'p_value' inputs should be numbers within the [0, 1] range. See manual for more details on the formatting of the 'Personalized' criteria",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 5", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(p_value = -2, imprecision = 2, rob = 40),
                            class_II = c(amstar = 2),
                            class_III = c(I2 = 20),
                            class_IV = c(p_value = 1e-5)),
               "The 'p_value' inputs should be numbers within the [0, 1] range. See manual for more details on the formatting of the 'Personalized' criteria",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 6", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(imprecision = 2, rob = 40),
                            class_II = c(amstar = 2),
                            class_III = c(I2 = "-4"),
                            class_IV = c(p_value = 1e-5)),
               "The 'I2' inputs should be numbers within the [0, 100] range. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 7", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(imprecision = "A", rob = 40),
                            class_II = c(amstar = 2),
                            class_III = c(I2 = 4),
                            class_IV = c(p_value = 1e-5)),
               "The 'Personalized' criteria contain non-numeric characters while this is expected. Please check inputs.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 8", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(imprecision = 2, rob = 114),
                            class_II = c(amstar = 2),
                            class_III = c(I2 = 4),
                            class_IV = c(p_value = 1e-5)),
               "The 'rob' inputs should be numbers within the [0, 100] range. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 9", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(imprecision = 2, rob = 1),
                            class_II = c(amstar = -2),
                            class_III = c(I2 = 4),
                            class_IV = c(p_value = 1e-5)),
               "The 'amstar' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 11", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(imprecision = 2, rob = 1),
                            class_II = c(egger_p = 2),
                            class_III = c(I2 = 4),
                            class_IV = c(p_value = 1e-5)),
               "The 'egger_p' inputs should be p-values and shoud thus be numbers within the [0, 1] range. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 12", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(imprecision = 2, rob = 1),
                            class_II = c(esb_p = 2),
                            class_III = c(I2 = 4),
                            class_IV = c(p_value = 1e-5)),
               "The 'esb_p' inputs should be p-values and shoud thus be numbers within the [0, 1] range. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 13", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(imprecision = 2, rob = 1),
                            class_II = c(pi = "not nul"),
                            class_III = c(I2 = 4),
                            class_IV = c(p_value = 1e-5)),
               "The 'pi' inputs should be 'notnull'. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 13", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(imprecision = 2, rob = 1),
                            class_II = c(largest_CI = "not nul"),
                            class_III = c(I2 = 4),
                            class_IV = c(p_value = 1e-5)),
               "The 'largest_CI' inputs should be 'notnull'. See manual for more details on the formatting of the 'Personalized' criteria.",
               fixed = TRUE)
})

test_that("'Personalized criteria produced appropriate error messages: 14", {
  umb <- .quiet(umbrella(df.RR))
  expect_error(add.evidence(umb, criteria = "Personalized",
                            class_I = c(total_n = -4, rob = 1)),
               "The 'total_n' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria",
               fixed = TRUE)
})


test_that("'Personalized criteria produced appropriate error messages: 15", {
  umb <- .quiet(umbrella(df.RR[1:2,]))

  .quiet(expect_message(add.evidence(umb, criteria = "Personalized",
                            class_I = c(egger_p = 0.5)),
               "the 'egger_p' criteria is used but cannot be calculated due to the small number of studies",
               fixed = TRUE))

  expect_equal(as.character(.quiet(summary(add.evidence(umb, criteria = "Personalized",
                            class_I = c(egger_p = 0.5)))$Class)), "II")
})

test_that("'Personalized criteria produced appropriate error messages: 16", {
  umb <- .quiet(umbrella(df.RR[1:2,]))

  .quiet(expect_message(add.evidence(umb, criteria = "Personalized",
                            class_I = c(pi = "notnull")),
               "For 1 factors the 'pi' criteria is used but cannot be calculated due to the small number of studies (n < 3). In this situation, the 95% PI value is conservatively assumed to to include the null value. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function",
               fixed = TRUE))

  expect_equal(as.character(.quiet(summary(add.evidence(umb, criteria = "Personalized",
                                                        class_I = c(pi = "notnull")))$Class)), "II")
})

test_that("'Personalized criteria produced appropriate error messages: 17", {
  umb <- .quiet(umbrella(df.RR[1:2,]))

  .quiet(expect_message(add.evidence(umb, criteria = "Personalized",
                                     class_I = c(amstar = 4)),
                        "For 1 factors the 'amstar' criteria is used but cannot be calculated because this information is not indicated in the dataset. In this situation, the amstar value is conservatively assumed to be lower than the thresold requested. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.",
                        fixed = TRUE))

  expect_equal(as.character(.quiet(summary(add.evidence(umb, criteria = "Personalized",
                                                        class_I = c(amstar = 4)))$Class)), "II")
})

test_that("'Personalized criteria produced appropriate error messages: 18", {
  umb <- .quiet(umbrella(df.RR[1,]))
  .quiet(expect_message(add.evidence(umb, criteria = "Personalized",
                                     class_I = c(I2 = 4)),
                        "criteria is used but cannot be calculated due to the small number of studies (n = 1)",
                        fixed = TRUE))

  expect_equal(as.character(.quiet(summary(add.evidence(umb, criteria = "Personalized",
                                                        class_I = c(I2 = 4)))$Class)), "II")
})

test_that("df.R", {
  df.R$factor = "A"
  df.R$value = runif(nrow(df.R), 0.4, 0.6)
  res = summary(add.evidence(umbrella(df.R), criteria = "Ioannidis"))
  expect_true(res$Class=="IV")
  df.R$n_cases = 1000
  res2 = summary(add.evidence(umbrella(df.R), criteria = "Ioannidis"))
  expect_true(res2$Class=="I")

  expect_true(all(res2[-c(which(colnames(res2) %in% c("Class", "n_cases")))] ==
                    res[-c(which(colnames(res)  %in% c("Class", "n_cases")))]))


})

#
# x <- umb
# criteria = "Personalized"
# class_I = c(egger_p = 0.5)
# class_III = c(I2 = 4)
# class_IV = c(p_value = 1e-5)
# class_II = NA
# class_V = NA
# name = "SSRI"
