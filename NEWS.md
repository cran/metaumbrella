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
