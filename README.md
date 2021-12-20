
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaumbrella

<!-- badges: start -->
<!-- badges: end -->

## Important

If you are not familiar with R, you can access the core features of this
package on your web browser on its dedicated
[website](https://www.metaumbrella.org/)

## Introduction

The `metaumbrella` package offers several facilities that assist in data
analysis when performing an umbrella review. This package is built
around three core functions which aim to facilitate (i) the completion
of the statistical analyses required for an umbrella review (via the
`umbrella()` function), (ii) the stratification of the evidence (via the
`add.evidence()` function) and (iii) the graphical presentation of the
results (via the `forest()` function).

First, the `umbrella()` function automatically performs meta-analyses
and additional calculations needed for an umbrella review. It outputs an
object of the class umbrella. The advantage of this function over
standard R packages only designed for fitting a single meta-analysis
lies, for example, in the possibility of automatically fitting several
meta-analyses when input information differs, automatically extracting
the necessary information to stratify the evidence, and automatically
performing the additional tests needed (a test for excess significance
bias, a test for publication bias and a jackknife leave-one-out
analysis).

Second, the `add.evidence()` function stratifies the evidence generated
by the umbrella() function according to a set of pre-specified criteria
(those proposed by Prof. Ioannidis or an algorithmic version of GRADE
classification), or according to personalized criteria that the users
may specify manually. This feature allows users to rely on already
developed criteria or to develop new ones that match the specific needs
of their umbrella review.

Third, the `forest()` function creates graphical representations of the
results of the umbrella review, including a forest plot along with
information on the stratification of evidence.

## Installation

You can install the released version of metaumbrella from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("metaumbrella")
```

## Citation

The metaumbrella package was written by Corentin J Gosling, Aleix
Solanes, Paolo Fusar-Poli and Joaquim Radua. It is licensed under the
GNU General Public License. <br>You can use metaumbrella for free… but
please cite our work ;-) <br> <b>Citation:</b> <em>Corentin J Gosling,
Aleix Solanes, Paolo Fusar-Poli and Joaquim Radua (2021). metaumbrella:
An R Package for Conducting Umbrella Reviews. R package version
0.0.0.9000.</em><br> To obtain a bibtex version of this citation in R,
type

``` r
citation(package = "metaumbrella")
```
