---
title: 'validateHOT - an R package for holdout task validation and market simulations'
tags:
  - R
  - MaxDiff
  - Conjoint Analysis
  - Market Simulations
  - Predictive Validity
authors:
  - name: Joshua Benjamin Schramm
    orcid: 0000-0001-5602-4632
    corresponding: True
    affiliation: 1
  - name: Marcel Lichters
    orcid: 0000-0002-3710-2292
    corresponding: FALSE
    affiliation: 1    
affiliations:
 - name: Otto von Guericke University of Magdeburg, Germany
   index: 1
citation_author: Schramm & Lichters
date: 07 July
year: 2024
bibliography: paper.bib
link-citations: true
output: rticles::joss_article
journal: JOSS
---

\newcommand{\colcod}[1]{\texttt{\color{purple}#1}}



# Summary

validateHOT is an R package that provides functions to both validate a validation/holdout task and run market simulations for results obtained in a (adaptive) choice-based conjoint analysis (hereafter ACBC and CBC, respectively) and maximum difference scaling (hereafter MaxDiff) using, for example, ChoiceModelR [@ChoiceModelR] or Sawtooth's Lighthouse Studio.

# Statement of need

Preference measurement techniques' (e.g., (A)CBC or MaxDiff) aim is to predict behavior [@green1990]. Hence, it is essential to ensure that the collected data is valid and predicts outside tasks (i.e., the model has external validity) well. The easiest way for testing validity is by including validation tasks [e.g., @Orme2015], which are fixed tasks (i.e., same across participants) and not used for estimating the part-worth utilities (raw logit utilities) in hierarchical Bayes (HB) estimation. Despite their importance, practitioners don't always include them [@yang2018]. This is unsatisfactory given the fact that the model is used to estimate market shares which poses the basis for relevant marketing decisions.

validateHOT combines both validation and market simulation in one package and has three key advantages, it (1) helps opting for the best model and (2) runs relevant market simulations that help finding the right product combinations or assortments, and (3) is an open source tool which helps especially researchers reporting accompanied scripts for their research papers.

# State of the field in R

Other packages provide functions to calculate validation metrics, however, these are not always specified for individual part-worth utilities. Metrics [@Metrics], for example, provide functions to run validation metrics such as *mean absolute error*, *root mean squared error*, or the five metrics of the confusion matrix. However, to get the output of, for example, Sawtooth Software or ChoiceModelR [@ChoiceModelR] into the right format, the user needs some data wrangling. The package conjoint [@conjoint] provides functions that are most similar to validateHOT's ones, but no validation functions are included and the package focuses on classical conjoint analysis, thus it is limited when applying more common conjoint methods. logitr [@logitr] provides market simulations tools, however, no validation metrics such as mean hit probability or hit rate. \autoref{comparison} shows a comparison of validateHOT's functions with current R packages. To the best of our knowledge, a package that converts raw utility scores into validation metrics or running a variety of marketing simulations (especially TURF) is missing.

```{=tex}
\begin{figure}[h]
  \includegraphics{figures/FunctionComparison.png}
  \caption{Comparison of validateHOT's function to existing R packages}
  \label{comparison}
\end{figure}
```
validateHOT is introduced with data estimated with Lighthouse Studio using effects-coding for creating the design matrix. It, however, can easily be used with data estimated with ChoiceModelR [@ChoiceModelR], bayesm [@bayesm], or STAN [@rstan], if used with similar settings (ChoiceModelR, for example, automatically implements effects-coding).

# Key functions

validateHOT's functions can be categorized into four main components, see \autoref{tab:table1}. To bring the data into the right format for some functions, the \texttt{\color{purple}createHOT()} function can be applied, which creates each alternatives' total utility by applying the additive utility model.

| Validation metrics | Confusion matrix | Market simulations | Rescaling scores |
|:----------------:|:----------------:|:----------------:|:----------------:|
|     hitrate()      |    accuracy()    |    freqassort()    |    att_imp()     |
|        kl()        |       f1()       |     marksim()      |  prob_scores()   |
|       mae()        |   precision()    |      reach()       |    zc_diffs()    |
|      medae()       |     recall()     |       turf()       | zero_anchored()  |
|       mhp()        |  specificity()   |                    |                  |
|       rmse()       |                  |                    |                  |

: Overview of validateHOT's main components and their corresponding functions \label{tab:table1}

# Typical workflow

We provide the workflow for a MaxDiff study and a CBC study with only part-worth coded attributes (the vignette provides detailed examples for other CBCs and an ACBC). To run the following code chunks, make sure to also install the dplyr package [@dplyr] since it is a dependency of validateHOT.

## MaxDiff

### Creating Holdout Task / Market Scenario

After running the HB estimation [@allenby1995], the **raw** utility scores have to be exported and read into an *R* data frame. Assuming you included a validation task with seven alternatives plus the no-buy alternative (\texttt{\color{purple}none}). To create this validation task in *R*, we use the \texttt{\color{purple}createHOT()} function.


``` r
HOT <- createHOT(
  data = MaxDiff,
  id = "ID",
  none = "none",
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  method = "MaxDiff",
  choice = "HOT",
  varskeep = "Group"
)
```

### Validating Holdout Task

To get the relevant validation metrics that are reported in conjoint studies, for example, hit rate or mean hit probability, we provide the data, the alternatives in the validation task (\texttt{\color{purple}opts}), and the actual choice (\texttt{\color{purple}choice}). The function can be implemented using the tidyverse [@tidyverse] logic.


``` r
hitrate(
  data = HOT,
  opts = c(Option_1:None),
  choice = choice
) %>%
  round(2)
```

```
## # A tibble: 1 x 5
##      HR    se chance   cor     n
##   <dbl> <dbl>  <dbl> <dbl> <dbl>
## 1  55.7  5.98   12.5    39    70
```

### Market Simulations

We also introduce two functions for market simulations, namely \texttt{\color{purple}marksim()} and \texttt{\color{purple}turf()}. In the following example, the market share is calculated according to the multinomial logit model [@McFadden1974].


``` r
marksim(
  data = HOT,
  opts = c(Option_1:None),
  method = "sop"
) %>%
  mutate_if(is.numeric, round, 2)
```

```
## # A tibble: 8 x 5
##   Option      mw    se lo.ci up.ci
##   <chr>    <dbl> <dbl> <dbl> <dbl>
## 1 Option_1 18.3   4.12 10.2  26.4 
## 2 Option_2 11.3   2.69  6.05 16.6 
## 3 Option_3  4.08  1.49  1.16  6.99
## 4 Option_4 32.5   4.45 23.8  41.2 
## 5 Option_5  1.93  0.92  0.13  3.72
## 6 Option_6 10.4   2.68  5.12 15.6 
## 7 Option_7  5.58  1.75  2.15  9.01
## 8 None     16.0   3.29  9.53 22.4
```

Next, \texttt{\color{purple}turf()}, a "product line extension model" [@miaoulis1990, p. 29], is a tool to find the perfect assortment that creates the highest reach and is especially powerful for MaxDiff studies [@chrzan2019, p. 108]. To optimize the search for the optimal assortment, we also include the arguments \texttt{\color{purple}fixed}, to define alternatives that have to be part of the assortment, and \texttt{\color{purple}prohib}, to prohibit certain item combinations in the assortment (see the vignette for more details and how to apply \texttt{\color{purple}turf()} with data obtained using a likert scale).

For the following example, let's assume that the user conducted an anchored MaxDiff analysis with 10 items (\texttt{\color{purple}opts}) and now wants to find the best assortment with a size of 3 items. The user uses the anchor (no-buy alternative) as a threshold.


``` r
turf(
  data = MaxDiff,
  opts = c(Option_01:Option_10),
  none = none,
  size = 3,
  approach = "thres"
) %>%
  head(., n = 5) %>%
  mutate_if(is.numeric, round, 2) %>%
  t() %>%
  as.data.frame() %>%
  slice(-1) %>%
  rename_all(., ~ paste0("Combo ", c(1:5)))
```

```
##           Combo 1 Combo 2 Combo 3 Combo 4 Combo 5
## reach       82.86   81.43   81.43   81.43   80.00
## freq         1.46    1.57    1.43    1.41    1.44
## Option_01       1       1       1       1       1
## Option_02       0       0       1       0       0
## Option_03       0       1       0       0       0
## Option_04       1       0       1       1       0
## Option_05       0       0       0       0       0
## Option_06       1       1       0       0       1
## Option_07       0       0       0       0       0
## Option_08       0       0       0       0       1
## Option_09       0       0       0       0       0
## Option_10       0       0       0       1       0
```

## CBC

### Creating Holdout Task / Market Scenario

The setup is almost the same, only the arguments \texttt{\color{purple}prod.levels}, \texttt{\color{purple}coding}, and \texttt{\color{purple}method} are different or new, respectively.


``` r
HOT_CBC <- createHOT(
  data = CBC,
  id = "ID",
  none = "none",
  prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
  coding = c(0, 0, 0),
  method = "CBC",
  choice = "HOT"
)
```

### Rescaling Scores

We can also display the attributes importance scores. Therefore, we need to define the attribute levels as well as the coding of the attributes.


``` r
att_imp(
  data = CBC,
  attrib = list(
    c(4:8),
    c(9:13),
    c(14:20)
  ),
  coding = c(rep(0, 3)),
  res = "agg"
) %>%
  mutate_if(is.numeric, round, 2)
```

```
## # A tibble: 3 x 3
##   Option       mw   std
##   <chr>     <dbl> <dbl>
## 1 att_imp_1  35.7 11.3 
## 2 att_imp_2  27.7 10.0 
## 3 att_imp_3  36.6  9.32
```

# Availability

validateHOT is available on [Github](https://github.com/JoshSchramm94/validateHOT).

# Acknowledgments

We would like to thank [Sawtooth Software](https://sawtoothsoftware.com/) for their great transparent documentation.

# References
