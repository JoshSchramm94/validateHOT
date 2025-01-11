---
title: "validateHOT - an R package for validating validation tasks and choice modelling
  tools"
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
date: 11 January 2025
bibliography: paper.bib
output: pdf_document
---

# Summary

validateHOT is an R package that provides functions for preference measurement techniques like (adaptive) choice-based conjoint analyses (hereafter (A)CBC) and maximum difference scaling (hereafter MaxDiff). More specifically with the package, users can validate validation tasks, perform market simulations, and rescale raw utility scores. The package works with data obtained using, for example, the ChoiceModelR package [@ChoiceModelR] or Sawtooth's Lighthouse Studio [@sawtooth2024].[^1]

[^1]: We refer to both validation and holdout tasks interchangeably.

# Statement of need

Researchers and practitioners use preference measurement techniques for many reasons, for example, simulating markets or to determine the importance of attributes [@steiner2018]. Their ultimate goal is to predict future behavior [@green1990]. In order to predict accurately and make the right decisions, it is essential to ensure that the collected data is valid. One way to test the model's validity is by including validation tasks [e.g., @Orme2015], which are usually fixed tasks (i.e., same across participants) and excluded for utility estimation in hierarchical Bayes (HB) estimation.

The validateHOT provides the relevant tools for these steps: (1) it assesses the model's validity, (2) runs relevant market simulations, (3) converts raw utilities scores into scores that are easy to interpret. Finally, it is an open source tool helping researchers reporting accompanied scripts for their research papers.

# State of the field in R

Other packages provide functions to calculate validation metrics, however, these are not always specified for individual raw utilities extracted from preference measurement techniques. The Metrics package [@Metrics], for example, provide functions to run validation metrics such as *mean absolute error* or the five metrics of the confusion matrix. However, converting the output of, for example, estimations using Sawtooth Software [@sawtooth2024] or the ChoiceModelR package [@ChoiceModelR] into the right format, requires some data wrangling. The conjoint package [@conjoint] provides functions most similar to the ones of validateHOT, but no validation functions are included and the package focuses on classical conjoint analysis. Thus, it is limited when applying more common conjoint methods. The logitr package [@logitr] provides market simulations tools, however, no validation metrics such as mean hit probability or hit rate. \autoref{comparison} shows a comparison of validateHOT's functions with current R packages. To the best of our knowledge, a package that converts raw utility scores into validation metrics or running a variety of marketing simulations (especially TURF and TURF ladder) is missing.

\begin{figure}[ht]
  \includegraphics{figures/functioncomparison.png}
  \caption{Comparison of validateHOT's functions to existing R packages}
  \label{comparison}
\end{figure}

validateHOT is introduced with data estimated with Lighthouse Studio. It, however, can be used with data estimated with the packages ChoiceModelR [@ChoiceModelR], bayesm [@bayesm], or STAN [@rstan], if used with similar settings.

# Key functions

validateHOT's functions can be categorized into four main components, see \autoref{tab:table1}. To bring the data into the right format for most functions, we created the `create_hot()` function, which creates each alternatives' total utility by applying the additive utility model.

| Validation metrics | Confusion matrix | Market simulations | Rescaling scores |
|:----------------:|:----------------:|:----------------:|:----------------:|
|     hitrate()      |    accuracy()    |    freqassort()    |    att_imp()     |
|        kl()        |       f1()       |     marksim()      |  prob_scores()   |
|       mae()        |   precision()    |      reach()       |    zc_diffs()    |
|      medae()       |     recall()     |       turf()       | zero_anchored()  |
|       mhp()        |  specificity()   |   turf_ladder()    |                  |
|       rmse()       |                  |                    |                  |

: Overview of validateHOT's main components and their corresponding functions \label{tab:table1}

# Typical workflow

We provide the workflow for a MaxDiff study [@schramm2024] and a CBC study with a linear-coded price attribute (the vignette provides further examples; @sablotny-wackershauser2024). To run the following code chunks, please install and load the magrittr package [@magrittr].

## MaxDiff

### Creating validation task / market scenario

After running the HB estimation, the raw utilities must be read into R. For the first example, we assume a validation task with seven alternatives plus the no-buy alternative.

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

To get, for example, the hit rate (`hitrate()`), we provide the data, the alternatives in the validation task (`opts`), and the actual choice (`choice`).

``` r
hitrate(
  data = hot_mxd,
  opts = c(option_1:none),
  choice = choice
)
```

### Market simulations

We also introduce two functions for market simulations, namely `marksim()` and `turf()`. In the following example, we simulated market shares according to the multinomial logit model [@McFadden1974].

``` r
marksim(
  data = hot_mxd,
  opts = c(option_1:none),
  method = "sop",
  res = "agg"
)
```

Next, `turf()`, a "product line extension model" [@miaoulis1990, p. 29], is a tool to find the perfect assortment that creates the highest reach. This method is useful for MaxDiff studies [@chrzan2019, p. 108]. Users can specify the arguments `fixed` (i.e., alternatives that must be part of the assortment) and `prohib` (i.e., forbid specific combinations).

Assuming the user conducted an anchored MaxDiff analysis with ten items (`opts`) and now wants to find the best assortment with a size of three items (`size`). As a threshold that needs to be exceeded (`none`), the user uses the anchor (no-buy alternative).

``` r
turf(
  data = maxdiff,
  opts = c(option_01:option_10),
  none = none,
  size = 3L,
  approach = "thres"
) %>%
  head(n = 5)
```

## CBC

### Creating validation task / market scenario

For a CBC, the setup of `create_hot()` is almost the same, only the arguments `prod.levels`, `lin.p`, `coding`, and `method` are new.

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

We can also display the attributes importance scores (`att_imp()`). Therefore, we need to define the attribute levels (`attrib`) and again the coding of the attributes (`coding`).

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

We would like to thank [Sawtooth Software](https://sawtoothsoftware.com/) for their great transparent documentation.

# References
