# metaumbrella 1.0.11
- Correct some bugs, including an error that could appear in the Egger's test if 'outcome' instead of 'outcomes' was included in the mutiple_es colmun (github issue).

# metaumbrella 1.0.10
- Rework importation of the forest function from the meta package

# metaumbrella 1.0.9
- Redesign of the forest.umbrella function (now a wrapper of the amazing forest.meta function from the meta package). This also allows to plot R as effect size measures, and to correct some bugs that could appear when using "eOR" in v1.0.8.

# metaumbrella 1.0.8
- The add_evidence function can now stratify the evidence according to Ioannidis criteria for R and Z measures if users enter the number of cases

# metaumbrella 1.0.7
- Fixed an issue that could appear when a meta-analysis presented multiple outcomes with a correlation as effect size measure

# metaumbrella 1.0.6
- Added a groove() function to visualize overlap of primary studies (inspired by the work of Pérez-Bracchiglione et al. 2022)

# metaumbrella 1.0.5
- Fixed some bugs preventing to use all functions in the app and jamovi module

# metaumbrella 1.0.4
- Three new effect size measures are available (SMC, R, and Z)
- Three new ESB tests are available (from Stanley and colleagues, 2021: PSST, TESS, TESSPSST)
- Fixed some bugs when drawing a forest plot with colors
- The summary() function now allows to obtain more information on heterogeneity (via the 'het_max' argument)
- The umbrella() function now allows to request a fixed-effect meta-analysis (via the 'method.var' argument)
- The umbrella() function now allows to choose the excess of statistical significance test (via the 'method.esb' argument)
- The unrestricted weighted least squares weighted average can now be used to estimate the best approximation of the true effect (via the 'true_effect' argument of the umbrella() and esb.test() functions)

# metaumbrella 1.0.3
- The add.evidence() function now warns users when an issue occurred when estimating the ESB test (a warning message is thus printed when using the umbrella() + add.evidence() functions).
- New features are available in the forest.umbrella() function
- Test section: some tests have been updated

# metaumbrella 1.0.2
- umbrella() function: improvement of the meta-analysis selection process based on user input
- Test section: several tests have been added

# metaumbrella 1.0.1
- Test section: made a rounding correction and explicitly calls the estimator for the amount of heterogeneity (method.tau argument) when using the functions of the meta package
- Documentation: made some improvements (correction of typos and clarifications)

# metaumbrella 1.0.0
- First version released on CRAN
