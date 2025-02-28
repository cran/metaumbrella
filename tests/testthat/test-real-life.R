##### SMD -----------------
test_that("1. EBIACT-CAM - analysis (SMD)", {
  skip_on_cran()
  # daf <- read.table("clipboard", header = TRUE, sep = "", dec = ",")
  # meta::metacont(
  #   mean.e =.as_numeric(daf$mean_cases), mean.c =.as_numeric(daf$mean_controls),
  #   sd.e =.as_numeric(daf$sd_cases), sd.c =.as_numeric(daf$sd_controls),
  #   n.e =.as_numeric(daf$n_cases), n.c = .as_numeric(daf$n_controls), sm = "SMD"
  # )
  #
  # load_all()
  dat = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/dat-ebi-asd-SMD.xlsx")
  )
  resv1.0.11 = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/res-ebi-asd-SMD.xlsx")
  )

  # xdat = dat[dat$factor == "Cheuk (2011)_ACUP_Social-communication", ]
  # xdat$multiple_es[c(1, 2, 3, 5)] <- NA
  # View(subset(dat, factor == "Ostinelli (2025)_TMS ([repetitive] transcranial magnetic stimulation)_Combined ADHD symptoms (inattentive + hyperactive/impulsive)_Self-rated_At study endpoint (closest to 12 weeks)"))
  umb = umbrella(dat,
                 mult.level = TRUE, method.var = "REML",
                 r = 0.8, pre_post_cor = 0.5)

  resv1.1.0 = summary(umb)

  expect_true(
    max(abs(resv1.0.11$value - resv1.1.0$value)) < 0.20)

  res = data.frame(F1=resv1.0.11$Factor,
                   F2=resv1.1.0$Factor,
                   F1F2 = resv1.0.11$Factor == resv1.1.0$Factor,
                   v1 = .as_numeric(resv1.0.11$value),
                   v2 = .as_numeric(resv1.1.0$value),
                   v12 = resv1.0.11$value_CI,
                   v22 = resv1.1.0$value_CI,
                   v1v2 = abs(.as_numeric(resv1.1.0$value) -
                                .as_numeric(resv1.0.11$value)))
  res_pb = res[abs(res$v1v2) > 0.2, ]

  expect_true(mean(abs(.as_numeric(resv1.1.0$value) -
                         .as_numeric(resv1.0.11$value))) < 0.008)
  expect_true(mean(abs(.as_numeric(resv1.1.0$p_value) -
                         .as_numeric(resv1.0.11$p_value))) < 0.009)

  # View(res)
  # View(res_pb)

})
test_that("2. EBIACT-CAM - analysis (OR)", {
  skip_on_cran() # daf <- read.table("clipboard", header = TRUE, sep = "", dec = ",")
  # meta::metacont(
  #   mean.e =.as_numeric(daf$mean_cases), mean.c =.as_numeric(daf$mean_controls),
  #   sd.e =.as_numeric(daf$sd_cases), sd.c =.as_numeric(daf$sd_controls),
  #   n.e =.as_numeric(daf$n_cases), n.c = .as_numeric(daf$n_controls), sm = "SMD"
  # )
  #
  # load_all()
  dat = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/dat-ebi-asd-OR.xlsx")
  )
  resv1.0.11 = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/res-ebi-asd-OR.xlsx")
  )

  # xdat = dat[dat$factor == "Cheuk (2011)_ACUP_Social-communication", ]
  # xdat$multiple_es[c(1, 2, 3, 5)] <- NA
  # View(subset(dat, factor == "Ostinelli (2025)_TMS ([repetitive] transcranial magnetic stimulation)_Combined ADHD symptoms (inattentive + hyperactive/impulsive)_Self-rated_At study endpoint (closest to 12 weeks)"))
  umb = umbrella(dat,
                 max_asymmetry = 50,
                 mult.level = TRUE, method.var = "PM",
                 r = 0.8, pre_post_cor = 0.5)

  resv1.1.0 = summary(umb)

  res = data.frame(F1=resv1.0.11$Factor,
                   F2=resv1.1.0$Factor,
                   F1F2 = resv1.0.11$Factor == resv1.1.0$Factor,
                   v1 = .as_numeric(resv1.0.11$value),
                   v2 = .as_numeric(resv1.1.0$value),
                   v12 = resv1.0.11$value_CI,
                   v22 = resv1.1.0$value_CI,
                   v1v2 = abs(.as_numeric(resv1.1.0$value) -
                                .as_numeric(resv1.0.11$value)))
  res_pb = res[abs(res$v1v2) > 0.05, ]
  # View(res)
  # View(res_pb)
  expect_true(mean(abs(.as_numeric(resv1.1.0$value) -
                         .as_numeric(resv1.0.11$value))) < 0.01)
  expect_true(mean(abs(.as_numeric(resv1.1.0$p_value) -
                         .as_numeric(resv1.0.11$p_value))) < 0.01)


})
test_that("3. EBIACT-CAM - GRADE (SMD)", {
  skip_on_cran()
  dat = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/dat-ebi-asd-SMD.xlsx")
  )
  dat$Factor = dat$factor
  res_GRADE = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/res-ebi-asd-GRADE.xlsx")
  )
  res_GRADE = subset(res_GRADE, !measure %in% c("OR", "RR"))

  dat_en = merge(dat, res_GRADE[, c("Factor", "indirectness")],
                 by = "Factor")

  dat_en$rob1_report = tolower(gsub(" risk", "", dat_en$RoB_Reporting))
  dat_en$rob1_report[dat_en$rob1_report == "unclear (not indicated)"] <- "unclear"

  # view.errors.umbrella(dat_en, "message")
  # View(view.errors.umbrella(dat_en))

  # dat_en2 = subset(dat_en, factor == "Siafis (child) (2022)_PUFA_Disruptive behaviors")
  # umb= umbrella(dat_en2,
  umb= umbrella(dat_en,
                mult.level = TRUE, method.var = "REML",
                r = 0.8, pre_post_cor = 0.5)

  res_grade = add.evidence(umb, criteria = "GRADE",
                           eq_range_or = c(0.8, 1.25),
                           eq_range_g = c(-0.10, 0.10))
  resv1.1.0 = summary(res_grade)

  resv1.1.0 = resv1.1.0[order(resv1.1.0$Factor), ]
  res_GRADE = res_GRADE[order(res_GRADE$Factor), ]
  res_GRADE$GRADE[res_GRADE$GRADE == "Very low"] <- "Very weak"
  res_GRADE$GRADE[res_GRADE$GRADE == "Low"] <- "Weak"

  resG = res_GRADE
  comp = data.frame( as.character(resv1.1.0$Factor) == as.character(resG$Factor),
                     as.character(resv1.1.0$Factor),
                     newV = as.character(resv1.1.0$Class),
                     as.character(resG$Factor),
                     Ancien = as.character(resG$GRADE),
                     comparaison = resv1.1.0$Class == resG$GRADE,
                     RoB = resG$down_rob,
                     Het = resG$down_het,
                     Indi = resG$down_ind,
                     Imp = resG$down_imp,
                     Pub = resG$down_pubbias,
                     Pub = resG$down_pubbias,
                     stud = resG$n_studies,
                     hetA = resG$down_hetA,
                     hetB = resG$down_hetB,
                     CI_v1 = resv1.1.0$eG_CI,
                     PI_v1 = resv1.1.0$PI_eG,
                     CI_v2 = resG$eG_CI,
                     PI_v2 = resG$PI_eG
  )

  # View(comp[which(!comp$comparaison), ])
  expect_true(nrow(comp[which(!comp$comparaison), ]) / nrow(comp) < 0.10)

})
test_that("4. EBIACT-CAM - GRADE (OR)", {
  skip_on_cran()
  dat = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/dat-ebi-asd-OR.xlsx")
  )
  dat$Factor = dat$factor
  res_GRADE = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/res-ebi-asd-GRADE.xlsx")
  )
  res_GRADE = subset(res_GRADE, measure %in% c("OR", "RR"))

  dat_en = merge(dat, res_GRADE[, c("Factor", "indirectness")],
                 by = "Factor")

  dat_en$rob1_report = tolower(gsub(" risk", "", dat_en$RoB_Reporting))
  dat_en$rob1_report[dat_en$rob1_report == "unclear (not indicated)"] <- "unclear"

  # view.errors.umbrella(dat_en, "message")
  # View(view.errors.umbrella(dat_en))

  # dat_en2 = subset(dat_en, factor == "Siafis (child) (2022)_NAC_Restricted/repetitive behaviors")
  # umb= umbrella(dat_en2,
  umb= umbrella(dat_en,
                max_asymmetry = 45,
                mult.level = TRUE, method.var = "PM",
                r = 0.8, pre_post_cor = 0.5)

  res_grade = add.evidence(umb, criteria = "GRADE",
                           eq_range_or = c(0.8, 1.25),
                           eq_range_g = c(-0.10, 0.10))
  resv1.1.0 = summary(res_grade)

  resv1.1.0 = resv1.1.0[order(resv1.1.0$Factor), ]
  res_GRADE = res_GRADE[order(res_GRADE$Factor), ]
  res_GRADE$GRADE[res_GRADE$GRADE == "Very low"] <- "Very weak"
  res_GRADE$GRADE[res_GRADE$GRADE == "Low"] <- "Weak"

  resG = res_GRADE
  comp = data.frame( as.character(resv1.1.0$Factor) == as.character(resG$Factor),
                     as.character(resv1.1.0$Factor),
                     newV = as.character(resv1.1.0$Class),
                     as.character(resG$Factor),
                     Ancien = as.character(resG$GRADE),
                     comparaison = resv1.1.0$Class == resG$GRADE,
                     RoB = resG$down_rob,
                     Het = resG$down_het,
                     Indi = resG$down_ind,
                     Imp = resG$down_imp,
                     Pub = resG$down_pubbias,
                     Pub = resG$down_pubbias,
                     stud = resG$n_studies,
                     hetA = resG$down_hetA,
                     hetB = resG$down_hetB,
                     CI_v1 = resv1.1.0$eG_CI,
                     PI_v1 = resv1.1.0$PI_eG,
                     CI_v2 = resG$eG_CI,
                     PI_v2 = resG$PI_eG,
                     resG$rob
  )


  # View(comp[which(!comp$comparaison), ])
  expect_true(nrow(comp[which(!comp$comparaison), ]) / nrow(comp) < 0.10)
})

test_that("EBI-ADHD-ERRORS", {
  skip_on_cran()
  dat = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/ebi-adhd-errors.xlsx")
  )
  res = umbrella(dat)

  expect_true(nrow(summary(res)) == 4)
})


test_that("3. EBI-ADHD  - analysis (SMD)", {
  skip_on_cran()
#
  # daf <- read.table("clipboard", header = TRUE, sep = "", dec = ",")
  # meta::metacont(
  #   mean.e =.as_numeric(daf$mean_cases), mean.c =.as_numeric(daf$mean_controls),
  #   sd.e =.as_numeric(daf$sd_cases), sd.c =.as_numeric(daf$sd_controls),
  #   n.e =.as_numeric(daf$n_cases), n.c = .as_numeric(daf$n_controls), sm = "SMD"
  # )

  # meta::metagen(TE = daf$value, lower = daf$ci_lo, upper = daf$ci_up)
  # meta::metacont(
  #   mean.e =daf$mean_exp, mean.c =daf$mean_controls,
  #   sd.e =daf$sd_exp, sd.c =daf$sd_controls,
  #   n.e =daf$n_cases, n.c = daf$n_controls, sm = "SMD"
  # )


  # load_all()
  dat = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/dat-ebi-adhd-SMD.xlsx")
  )
  resv1.0.11 = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/res-ebi-adhd-SMD.xlsx")
  )

  # datS = dat[dat$factor == "Storebo (2019)_Social skill training_Combined ADHD symptoms (inattentive + hyperactive/impulsive)_Teacher-rated_At follow-up (closest to 26 weeks)", ]
  # View(subset(dat, factor == "Ostinelli (2025)_TMS ([repetitive] transcranial magnetic stimulation)_Combined ADHD symptoms (inattentive + hyperactive/impulsive)_Self-rated_At study endpoint (closest to 12 weeks)"))
  umb = umbrella(dat,
           mult.level = TRUE, method.var = "REML",
           r = 0.8, pre_post_cor = 0.5)

  resv1.1.0 = summary(umb)


  res = data.frame(F1=resv1.0.11$Factor,
              F2=resv1.1.0$Factor,
              F1F2 = resv1.0.11$Factor == resv1.1.0$Factor,
              v1 = .as_numeric(resv1.0.11$value),
              v2 = .as_numeric(resv1.1.0$value),
              v12 = resv1.0.11$value_CI,
              v22 = resv1.1.0$value_CI,
              v1v2 = abs(.as_numeric(resv1.1.0$value) -
                    .as_numeric(resv1.0.11$value)))
  res_pb = res[abs(res$v1v2) > 0.1, ]
  # View(res)
  # View(res_pb)

  expect_true(mean(abs(.as_numeric(resv1.1.0$value) -
                         .as_numeric(resv1.0.11$value))) < 0.005)
  expect_true(mean(abs(.as_numeric(resv1.1.0$p_value) -
                         .as_numeric(resv1.0.11$p_value))) < 0.005)

  })

test_that("4. EBI-ADHD - analysis (OR)", {
  skip_on_cran()
  # daf <- read.table("clipboard", header = TRUE, sep = "", dec = ",")
  # meta::metacont(
  #   mean.e =.as_numeric(daf$mean_cases), mean.c =.as_numeric(daf$mean_controls),
  #   sd.e =.as_numeric(daf$sd_cases), sd.c =.as_numeric(daf$sd_controls),
  #   n.e =.as_numeric(daf$n_cases), n.c = .as_numeric(daf$n_controls), sm = "SMD"
  # )
  #
  # load_all()
  dat = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/dat-ebi-adhd-OR.xlsx")
  )
  resv1.0.11 = readxl::read_excel(
    paste0("D:/drive_gmail/Recherche/metaumbrella/",
           "data-raw/test-raw/res-ebi-adhd-OR.xlsx")
  )

  # xdat = dat[dat$factor == "Cheuk (2011)_ACUP_Social-communication", ]
  # xdat$multiple_es[c(1, 2, 3, 5)] <- NA
  # View(subset(dat, factor == "Ostinelli (2025)_TMS ([repetitive] transcranial magnetic stimulation)_Combined ADHD symptoms (inattentive + hyperactive/impulsive)_Self-rated_At study endpoint (closest to 12 weeks)"))
  umb = umbrella(dat,
                 max_asymmetry = 20,
                 mult.level = TRUE, method.var = "PM",
                 r = 0.8, pre_post_cor = 0.5)

  resv1.1.0 = summary(umb)

  res = data.frame(F1=resv1.0.11$Factor,
                   F2=resv1.1.0$Factor,
                   F1F2 = resv1.0.11$Factor == resv1.1.0$Factor,
                   v1 = .as_numeric(resv1.0.11$value),
                   v2 = .as_numeric(resv1.1.0$value),
                   v12 = resv1.0.11$value_CI,
                   v22 = resv1.1.0$value_CI,
                   v1v2 = abs(.as_numeric(resv1.1.0$value) -
                                .as_numeric(resv1.0.11$value)))
  res_pb = res[abs(res$v1v2) > 0.05, ]

  # View(res)
  # View(res_pb)

  expect_true(mean(abs(.as_numeric(resv1.1.0$value) -
                         .as_numeric(resv1.0.11$value))) < 0.01)
  expect_true(mean(abs(.as_numeric(resv1.1.0$p_value) -
                         .as_numeric(resv1.0.11$p_value))) < 0.01)

})

test_that("means alone wth / wthout n_cases", {
  skip_on_cran()
  dfwoN<- subset(df.SMD, select=-c(n_cases, n_controls,
                                   value,
                                   ci_lo, ci_up, se))
  expect_error(umbrella(dfwoN, verbose=FALSE, seed = 4321))
})
