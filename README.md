
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaumbrella

<!-- badges: start -->

[![License: GPL
(\>=3)](https://img.shields.io/badge/license-GPL-red)](https://www.gnu.org/licenses/gpl-3.0.html)
[![CRAN
Version](https://www.r-pkg.org/badges/version/metaumbrella)](https://cran.r-project.org/package=metaumbrella)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/metaumbrella)](https://cranlogs.r-pkg.org/badges/grand-total/metaumbrella)
<!-- badges: end -->

## Important

If you are not familiar with R, you can access the core features of this
package on your web browser on its dedicated
[website](https://www.metaumbrella.org/).

## Introduction

The `metaumbrella` package offers several facilities to assist in data
analysis when performing an umbrella review. This package is built
around three core functions which aim to facilitate (i) the completion
of the statistical analyses required for an umbrella review (via the
`umbrella()` function), (ii) the stratification of the evidence (via the
`add.evidence()` function) and (iii) the graphical presentation of the
results (via the `forest()` function).

First, the `umbrella()` function automatically performs meta-analyses
and additional calculations needed for an umbrella review. It outputs an
object of class umbrella. The advantage of this function over standard R
packages only designed for fitting a single meta-analysis lies, for
example, in the possibility of automatically fitting several
meta-analyses when input information differs, automatically extracting
the necessary information to stratify the evidence, and automatically
performing the additional tests needed (a test for excess significance
bias, a test for publication bias and a jackknife leave-one-out
analysis).

Second, the `add.evidence()` function stratifies the evidence generated
by the umbrella() function according to a set of pre-specified
classifications (the classification proposed by Prof. Ioannidis or an
algorithmic version of GRADE classification), or according to a
personalized classification that the users may specify manually. This
feature allows users to rely on already developed criteria or to develop
new ones that match the specific needs of an umbrella review.

Third, the `forest()` function creates graphical representations of the
results of an umbrella review, including a forest plot along with
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
please cite our work ;-) <br><br> <b>Citation AMA:</b> Gosling CJ,
Solanes A, Fusar-Poli P, Radua J. metaumbrella: the first comprehensive
suite to perform data analysis in umbrella reviews with stratification
of the evidence. BMJ Ment Health. 2023;26(1):e300534.<br><br>
<b>Citation APA:</b> Gosling, C.J., Solanes, A., Fusar-Poli, P., &
Radua, J. (2023). metaumbrella: the first comprehensive suite to perform
data analysis in umbrella reviews with stratification of the evidence.
BMJ mental health, 26(1), e300534.<br><br>

To obtain a bibtex version of this citation in R, type

``` r
citation(package = "metaumbrella")
```
