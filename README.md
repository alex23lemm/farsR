
<!-- README.md is generated from README.Rmd. Please edit that file -->
farsR
=====

Overview
--------

[![Travis-CI Build Status](https://travis-ci.org/alex23lemm/farsR.svg?branch=master)](https://travis-ci.org/alex23lemm/farsR)

The goal of farsR is to help you *process* and *plot* data downloaded from the US National Highway Traffic Safety Administration's [Fatality Analysis Reporting System](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars) about fatal injuries suffered in motor vehicle traffic crashes in the US.

Installation
------------

You can install farsR from github with:

``` r
# install.packages("devtools")
devtools::install_github("alex23lemm/farsR")
```

Getting started
---------------

``` r
library(farsR)
```

There are two main functions to process and plot FARS data:

-   `fars_summarize_years()` summarizes FARS report accidents by month and year in a tibble

-   `fars_map_state()` plots all accident locations for a given year on a US federal state map

*Note*: Both functions assume that the respective FARS data was downloaded before and that it resides in the current working directory.

To get started, read the farsR vignette (`vignette("farsR-introduction")`).
