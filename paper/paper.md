---
title: "validateHOT - an R package for the analysis of holdout/validation 
tasks and other choice modeling tools"
tags:
- R
- MaxDiff
- Conjoint Analysis
- Market Simulations
- Predictive Validity
authors:
- name: Joshua Benjamin Schramm
  orcid: 0000-0001-5602-4632
  corresponding: true
  affiliation: 1
- name: Marcel Lichters
  orcid: 0000-0002-3710-2292
  corresponding: false
  affiliation: 1
affiliations:
- name: Otto von Guericke University of Magdeburg, Germany
  index: 1
date: 25 January 2025
bibliography: paper.bib
output: pdf_document
---

# Summary

validateHOT is an R package that provides functions for preference measurement techniques such as (adaptive) choice-based conjoint analyses (hereafter CBC and ACBC, respectively) and maximum difference scaling (hereafter MaxDiff). Specifically, the package allows users to analyze validation tasks, perform market simulations, and rescale raw utility scores. It is compatible with data obtained using, for example, the ChoiceModelR package [@ChoiceModelR] or Sawtooth Software's Lighthouse Studio [@sawtooth2024].[^1]

[^1]: We refer to both validation and holdout tasks interchangeably.

# Statement of need

Researchers and practitioners use preference measurement techniques for various research purposes, such as simulating markets or determining the importance of attributes when making choices [@steiner2018]. The ultimate goal is to predict future behavior [@green1990]. To make accurate predictions and informed decisions, it is crucial to ensure that the collected data is valid. A prominent way to test a model's validity is the analysis of validation tasks [e.g., @Orme2015], which are typically fixed tasks (i.e., same across participants) and excluded for utility estimation.

The validateHOT package provides the necessary tools for the aforementioned applications: (1) assessing the model's validity, (2) performing market simulations, (3) converting raw utility scores. Finally, as an open-source tool, it helps academics to report analysis scripts in scientific journal articles, fostering reproducibility.

# State of the field

Other R packages offer functions to calculate validation metrics. However, these are not always tailored to the individual raw utilities extracted from preference measurement techniques. The Metrics package [@Metrics], for example, provides functions to run validation metrics such as mean absolute error (MAE) or metrics of the confusion matrix. However, converting outputs such as those from estimations using Sawtooth Software's Lighthouse Studio [@sawtooth2024] or the ChoiceModelR package [@ChoiceModelR] into the proper format requires some data wrangling. The conjoint package [@conjoint] offers functions that are similar to those in validateHOT but it lacks validation functions and focuses primarily on classical conjoint analysis. Thus, it is limited when applied to more common conjoint methods. The logitr package [@logitr] offers market simulation tools but does not include validation metrics such as mean hit probability or hit rate. \autoref{comparison} compares validateHOT's functions with those of other R packages. To the best of our knowledge, no package converts raw utility scores into validation metrics or running a variety of marketing simulations (especially Total Unduplicated Reach and Frequency (TURF) and TURF ladder).

\begin{figure}[ht]
  \includegraphics{figures/functioncomparison.png}
  \caption{Comparison of the functions of validateHOT with those of existing R packages}
  \label{comparison}
\end{figure}

We introduce validateHOT drawing on data estimated using Sawtooth Software Lighthouse Studio [@sawtooth2024]. However, it can also be used on results stemming from packages such as ChoiceModelR [@ChoiceModelR], bayesm [@bayesm], or STAN [@rstan], if used with similar settings.

# Key functions

validateHOT's functions can be categorized into four main components (see \autoref{tab:table1}). To convert the data in the correct format for most functions, we created the `create_hot()` function, which calculates each alternative's total utility in conjoint studies by applying the additive utility model.

| Validation metrics | Confusion matrix | Market simulations | Rescaling scores |
|:-----------------:|:----------------:|:-----------------:|:----------------:|
|     hitrate()      |    accuracy()    |    freqassort()    |    att_imp()     |
|        kl()        |       f1()       |     marksim()      |  prob_scores()   |
|       mae()        |   precision()    |      reach()       |    zc_diffs()    |
|      medae()       |     recall()     |       turf()       | zero_anchored()  |
|       mhp()        |  specificity()   |   turf_ladder()    |                  |
|       rmse()       |                  |                    |                  |

: Overview of validateHOT's functions \label{tab:table1}

# Typical workflow

We present the workflow for a MaxDiff [@schramm2024] and a CBC study with a linear-coded price attribute (the vignette provides further examples; @sablotny-wackershauser2024).

## MaxDiff

### Creating validation task/market scenario

Following hierarchical Bayes estimation, the raw utilities must be imported into an R `data.frame` object. The first example assumes a validation task with seven alternatives plus a no-buy alternative. We define the data set (`data`), and the column names of the unique identifier (`id`) and the no-buy alternative (`none`). Next, we specify the attribute levels of each alternative (`prod.levels`), the `method`, the variables that should be kept in the new data frame (`varskeep`), and finally, the actual `choice` in the validation task.

``` r
hot_mxd <- create_hot(
  data = maxdiff,
  id = "id",
  none = "none",
  prod.levels = list(2, 9, 10, 14, 15, 16, 17),
  method = "maxdiff",
  varskeep = "group",
  choice = "hot"
)
```

### Validation metrics

To calculate the hit rate using the `hitrate()` function, we define the arguments: `data`, `opts` (the alternatives in the validation task), and `choice`.

``` r
hitrate(
  data = hot_mxd,
  opts = c(option_1:none),
  choice = choice
)
```

### Market simulation

`turf()` is a tool helping to find the optimal assortment generating the highest reach. This method is particularly useful for MaxDiff studies [@chrzan2019, p. 108]. Users can specify the arguments `fixed` (i.e., alternatives that are mandatory in an assortment) and `prohib` (i.e., forbid specific combinations).

Below we assume the user conducted an anchored MaxDiff analysis with 10 items (`opts`) and wants to find the best assortment with a size of 3 items (`size`). The anchor (no-buy alternative) is the threshold that must be exceeded (`none`). Finally, `option_01` (`fixed`) is fixed meaning that it must be part of each combination.

``` r
turf(
  data = maxdiff,
  opts = c(option_01:option_10),
  none = none,
  size = 3L,
  fixed = "option_01"
  approach = "thres"
) %>%
  head(n = 5)
```

## CBC

### Creating validation task/market scenario

The CBC setup of `create_hot()` is nearly the same as for the MaxDiff example. Besides the arguments defined above, we must define the linear-coded variable (`lin.p`), the coding of the attributes (`coding`), and the values that were used when estimating the utilities for the linear-coded variable (`interpolate.levels`).

``` r
hot_cbc_linear <- create_hot(
  data = cbc_linear,
  id = "id",
  none = "none",
  prod.levels = list(
    c(3, 6, 10, 13, 16, 20, 24, 32, 248.55),
    c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
    c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
    c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
    c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
    c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
  ),
  coding = c(rep(0, times = 8), 1),
  lin.p = "price",
  interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
  method = "cbc",
  choice = "hot"
)
```

### Rescaling scores

The function `att_imp()` calculates the attributes' relative importance. `attrib` defines the attribute levels. The other arguments are the same as above.

``` r
att_imp(
  data = cbc_linear,
  attrib = list(
    paste0("att1_lev", c(1:3)),
    paste0("att2_lev", c(1:2)),
    paste0("att3_lev", c(1:4)),
    paste0("att4_lev", c(1:4)),
    paste0("att5_lev", c(1:2)),
    paste0("att6_lev", c(1:4)),
    paste0("att7_lev", c(1:6)),
    paste0("att8_lev", c(1:6)),
    "price"
  ),
  coding = c(rep(0, times = 8), 1),
  interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
  res = "agg"
)
```

# Availability

The package validateHOT is available on [GitHub](https://github.com/JoshSchramm94/validateHOT).

# Acknowledgments

We would like to thank Sawtooth Software [@sawtooth2024] for their transparent documentation.

# References
