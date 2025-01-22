
<!-- README.md is generated from README.Rmd. Please edit that file -->

# validateHOT 🎯

<!-- badges: start -->

[![R-CMD-check](https://github.com/JoshSchramm94/validateHOT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JoshSchramm94/validateHOT/actions/workflows/R-CMD-check.yaml)

[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

<!-- badges: end -->

validateHOT is a package for preference measurement techniques. It
provides functions to evaluate validation tasks, perform market
simulations, and convert raw utility estimates into scores that are
easier to interpret. All three components are key functions for
preference measurement techniques such as choice-based conjoint (CBC),
adaptive choice-based conjoint (ACBC), or Maximum Difference Scaling
(MaxDiff). This package is particularly relevant for the [Sawtooth
Software](https://sawtoothsoftware.com/) community who would like to
report their analysis in *R* for open science purposes. In addition, it
is compatible with other packages, for example, the ChoiceModelR package
(Sermas 2022). Further, the validateHOT package is valuable for
practitioners, who would like to conduct the analyses using an
open-source software.

Researchers and practitioners use preference measurement techniques for
various purposes, such as calculating the importance of specific
attributes or simulating markets (Gilbride, Lenk, and Brazell 2008;
Steiner and Meißner 2018). The ultimate goal is to predict future
behavior (Green and Srinivasan 1990). To ensure valid and reliable
results, it is crucial that the collected data is valid and can predict
outcomes for tasks that were not included in the estimation of the
utility scores. Including validation tasks is highly recommended (Orme
2015; Rao 2014). They do not only verify data validity but can also help
to test different models. The validateHOT package offers helpful tools,
to facilitate these functions (i.e., validate a validation task, perform
market simulations, and communicate results of preference measurement
techniques) - all within an open-source tool.

> The validateHOT package was primarily developed for use with Sawtooth
> Software (Sawtooth Software Inc. 2024) and the ChoiceModelR package
> (Sermas 2022). Please be cautious about using it with other platforms
> (especially for linear and piecewise-coded variables).

👉🏾 <u>**What you need to provide**</u>: <br> After collecting your data
and running your initial hierarchical Bayes model, you need to import
your raw utility scores. If you plan to validate a validation task you
also need to provide the actual choice made in this task. We provide a
short tutorial in this markdown. For a more comprehensive tutorial,
please see the vignette provided with the validateHOT package
(`vignette("validateHOT", package = "validateHOT")`).

👈🏾 <u>**What you get**</u>:<br> The validateHOT package currently
provides functions for four key components:

- validation metrics

- metrics commonly reported in machine learning (i.e., confusion matrix)

- simulation methods, such as determining optimal product combinations

- converting raw logit utilities into more interpretable scores

For the first three components, the `create_hot()` function is
essential. This function calculates the total utilities for each
alternative in the validation task or the market scenario to be tested.
The `create_hot()` function computes the total utility of each
alternative based on the additive utility model (Rao 2014, 82).

### Classical validation metrics

- `hitrate()`: calculates the hit rate (correctly predicted choices) of
  the validation task.
- `kl()`: Kullback-Leibler-Divergence calculates the divergence between
  the actual choice distribution and the predicted choice distribution
  (Ding et al. 2011; HG 2018). Due to the asymmetry of the
  Kullback-Leibler divergence, the output includes divergence both from
  predicted to observed and from observed to predicted. The validateHOT
  package currently provides two logarithm bases: $log$ and $log_2$.
- `mae()`: calculates the mean absolute error, i.e., the deviation
  between predicted and stated choice shares
- `medae()`: calculates the median absolute error
- `mhp()`: calculates the averaged hit probability of participant’s
  actual choice in the validation task
- `rmse()`: calculates the root mean square error of deviation between
  predicted and stated choice shares

All functions can be extended with the `group` argument to get the
output split by group(s).

### Confusion Matrix

The validateHOT package includes metrics from machine learning, i.e.,
the confusion matrix (e.g., Burger (March 2018)). For all of the 5
provided functions, a **none** alternative has to be included in the
validation task. The logic of the implemented confusion matrix is to
test, for example, whether a buy or no-buy was correctly predicted.
Information could be used to get a sense of overestimation and
underestimation of general product demand. In the table below `TP`
stands for true positives, `FP` for false positives, `TN` for true
negatives, and `FN` for false negatives (Burger March 2018; Kuhn 2008).
To translate this to the logic of the validateHOT package, imagine you
have a validation task with five alternatives plus the alternative of
not buying. The validateHOT package now calculates whether a buy
(participant opts for one of the five alternatives) or a no-buy
(participant opts for the none alternative), respectively, is correctly
predicted.

Please be aware that the validateHOT package applies the following
coding of the *buy* and *no-buy* alternatives. Rows refer to the
observed decisions while columns refer to the predicted ones.

|        | Buy | No-buy |
|--------|:---:|:------:|
| Buy    | TP  |   FN   |
| No-buy | FP  |   TN   |

- `accuracy()`: calculates the number of correctly predicted choices
  (buy or no-buy); $\frac{TP + TN}{TP + TN + FP + FN}$ (Burger March
  2018)

- `f1()`: defined as $\frac{2 * precision * recall}{precision + recall}$
  or stated differently by Burger (March 2018)
  $\frac{2TP}{2TP + FP + FN}$

- `precision()`: defined as $\frac{TP}{TP + FP}$ (Burger March 2018)

- `recall()`: defined as $\frac{TP}{TP + FN}$ (Burger March 2018)

- `specificity()`: defined as $\frac{TN}{TN + FP}$ (Burger March 2018)

Again, all functions can be extended with the `group` argument to get
the output split by group(s).

### Simulation Methods

- `turf()`: **T**(otal) **U**(nduplicated) **R**(each) and
  **F**(requency) is a “product line extension model” \[Miaoulis,
  Parsons, and Free (1990); p. 29\] that helps to find the perfect
  product bundle based on the reach (e.g., how many participants
  consider buying at least one product of that assortment) and the
  frequency (how many products on average are a purchase option).
  `turf()` currently provides both the *threshold* approach
  (`approach ='thres'`; all products that exceed a threshold are
  considered, e.g., as purchase option; Chrzan and Orme (2019), p. 112)
  and the *first choice* approach (`approach = 'fc'`; only product with
  highest utility is considered as purchase option; Chrzan and Orme
  (2019), p. 111).
- `turf_ladder()`: starts with one product and then subsequently adds
  one new product, which adds the maximum possible reach. Alternatively,
  it is also possible to define fixed products (i.e., products that must
  be part of the assortment).
- `freqassort()`: Similar to `turf()`, `freqassort()` will give you the
  average frequency, representing how many products the participants
  will choose from a potential assortment. Again, you have to define a
  `none` alternative. `freqassort()` uses the *threshold* approach (see
  above). While `turf()` calculates the reach and frequency for **all**
  combinations, you specify the combination you are interested in
  `freqassort()`.
- `reach()`: Similar to `turf()`, `reach()` will give you the average
  percentage of how many participants you can reach (at least one of the
  products resembles a purchase option) with your defined assortment.
  `reach()` also uses the *threshold* approach (see above). While
  `turf()` calculates the reach and frequency for **all** combinations,
  you specify the combination you are interested in `reach()`.
- `marksim()`: Runs market simulations (either the share of preference,
  `sop` or first choice rule, `fc`), including the standard error, and
  the lower and upper confidence intervals (see also Orme 2020, 94).

### Converting raw utilities

The validateHOT package also includes four functions designed to make
the scores from both (A)CBC and MaxDiff analyses more interpretable,
namely:

- `att_imp()`: Converts the raw utilities of either an ACBC or CBC into
  importance scores for each attribute (see Orme 2020, 79–81)

- `prob_scores()`: Converts the raw utilities of a MaxDiff to choice
  probabilities by applying the following procedures:

- For unanchored MaxDiff: First, the scores are zero-centered, and then
  they are transformed by the following formula
  $\frac{exp^{U_i}}{(exp^{U_i} + a - 1)}$ (Chrzan and Orme 2019, 56),
  where $U_i$ is the raw utility of item *i* and `a` is the number of
  items shown per MaxDiff task.

- For anchored MaxDiff the following formula is applied:
  $\frac{exp^{U_i}}{(exp^{U_i} + a - 1)} * 100 * \frac{1}{a}$ (Chrzan
  and Orme 2019, 59).

- `zc_diffs()`: Rescales the raw logit utilities to make them comparable
  across participants (Sawtooth Software Inc. 2024, 330).

- `zero_anchored()`: Rescales the raw logits of a MaxDiff to
  zero-centered diffs (Chrzan and Orme 2019, 64).

### Data Frames provided by the validateHOT package

The package includes five data sets that help to better explain the
functions as well as the structure of the input, especially for the
`create_hot()` function.

- `acbc`: Example data set with raw utilities of an ACBC study conducted
  in Sawtooth Software (Sawtooth Software Inc. 2024). The price was
  linear-coded while the other attributes were coded as part-worths
  (Sablotny-Wackershauser et al. 2024; Sawtooth Software Inc. 2024).

- `acbc_interpolate`: Example data set with raw utilities of an ACBC
  study conducted in Sawtooth Software (Sawtooth Software Inc. 2024).
  Price was piecewise-coded, another attribute was linear-coded while
  the other attributes were coded as part-worth (Sablotny-Wackershauser
  et al. 2024; Sawtooth Software Inc. 2024).

- `cbc`: Example data set with raw utilities of a CBC study conducted in
  Sawtooth Software (Sawtooth Software Inc. 2024). All attributes were
  coded as part-worth (Sablotny-Wackershauser et al. 2024; Sawtooth
  Software Inc. 2024).

- `cbc_linear`: Example data set with raw utilities of a CBC study
  conducted in Sawtooth Software (Sawtooth Software Inc. 2024) One
  attribute was linear-coded while the other attributes were part-worth
  coded (Sablotny-Wackershauser et al. 2024; Sawtooth Software Inc.
  2024).

- `maxdiff`: Example data set with raw utilities of a MaxDiff study
  conducted in Sawtooth Software (Sawtooth Software Inc. 2024; Schramm
  and Lichters 2024).

## The story behind the validateHOT package

The validateHOT package was born out of teaching preference measurement
seminars to students, many of whom have little to no prior experience
with R. One of the chapters in this class is about model validation by
checking holdout tasks and we teach this, of course, in *R* 😍. We
emphasize open science, and providing tools to run the analyses and
share the code afterward. The validateHOT package makes this process
look easy 🤹‍♀️. Of course, there are other great packages (i.e., the
Metrics package by Hamner and Frasco (2018)), however, these packages
need some more data wrangling to use the appropriate functions with the
raw utilities, which might be a burden or barrier for some users.

Moreover, as Yang, Toubia, and Jong (2018) report, commercial studies
often do not use any validation tasks. Again, the lack of experience in
*R* could be one explanation. Since these functions are not always
implemented in other software, not knowing how to apply it correctly
might be the reason of not including it in the first instance. Having a
package to evaluate the validation task can be very beneficial from this
perspective.

## Installation

You can install the development version of the validateHOT package from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("JoshSchramm94/validateHOT", dependencies = T, build_vignettes = T)
```

## Example

First, we load the package:

``` r
library("validateHOT")
```

### Example I - CBC

Since *CBC’s* are applied more commonly compared to *ACBC* and
*MaxDiff*, we will provide an example with a *CBC*. Let us load the
`cbc` data frame for this example (Sablotny-Wackershauser et al. 2024).

``` r
data(cbc)
```

Now imagine you included a validation task with six alternatives plus a
no-buy alternative. We specify the `data` argument and the `id`. Since
we also have a *no-buy* alternative in our validation task, we specify
the `none` argument. Afterwards, we define each alternative using the
argument `prod.levels`. If we look back at the data frame, we can see
that the first alternative in the holdout task
(`c(3, 6, 10, 13, 16, 20, 24, 32, 35)`) is composed of the following
attribute levels att1_lev2, att2_lev2, att3_lev4, att4_lev3, att5_lev2,
att6_lev4, att7_lev4, att8_lev6, and price_3.

As mentioned above, all the attributes are part-worth coded and the
alternatives have the same price as one of the levels shown (i.e., no
interpolation). Thus, we set `coding = c(rep(0, times = 9))`. Finally,
we specify the method, which is `method = "cbc"` in our case, and define
the column of the actual participant’s choice (`choice`). If you run the
code, a data frame called `hot_cbc` will be returned to the global
environment.

> ❗ `create_hot()` takes both column names and column indexes. However,
> please be aware, if you include linear-coded or piecewise-coded, you
> **have** to provide the column indexes for the input in `prod.levels`.

``` r
hot_cbc <- create_hot(
  data = cbc,
  id = "id",
  none = "none",
  prod.levels = list(
    c(3, 6, 10, 13, 16, 20, 24, 32, 35),
    c(3, 5, 10, 14, 16, 18, 22, 27, 35),
    c(4, 6, 9, 14, 15, 20, 25, 30, 36),
    c(4, 5, 10, 11, 16, 19, 26, 32, 34),
    c(2, 6, 8, 14, 16, 17, 26, 31, 36),
    c(2, 5, 7, 12, 16, 20, 26, 29, 33)
  ),
  coding = c(rep(0, times = 9)),
  method = "cbc",
  choice = "hot"
)
```

> In case you just need to create a market scenario, you can also leave
> the `choice` argument empty.

Sometimes you estimate a part-worth coded attribute but want to treat
this attribute as continuous in the validation task or market
simulations (i.e., interpolate values). Please use the code `2` for this
variable in the `coding` argument.

Let us take a glimpse at the output, which shows the participants’ total
raw utilities for each of the six alternatives that were included in the
validation task.

``` r
head(hot_cbc)
#>   id   option_1  option_2  option_3  option_4    option_5  option_6       none
#> 1  1  1.2803044 -4.357278  4.601286 -5.200059 -2.72984853 -3.096481  0.7079119
#> 2  2  2.4718906 -4.760604 -1.064563 -3.350765 -0.03149749 -3.220985 -2.5741281
#> 3  3 -0.3429725  1.765604 -3.612479  2.724073 -4.17425884  2.583086  0.1110601
#> 4  4 -1.4464198 -3.585124  3.728081 -8.535421 -2.15061169 -8.390555 -0.7168751
#> 5  5 -1.1148066 -4.411730  1.462518 -3.948949 -1.96740869 -3.304241  2.4743820
#> 6  6  0.1308527 -4.620229  2.006385 -4.629630 -1.38634308 -6.505516 -1.0676113
#>   choice
#> 1      3
#> 2      3
#> 3      6
#> 4      7
#> 5      2
#> 6      7
```

In the next step, we would like to see how well our model (from which we
took the raw utilities) predicts the actual choices in the validation
task. First, we will run the `hitrate()` function. We specify the
`data`, the column names of the alternatives (`opts`; remember there are
six alternatives + the *no-buy* alternative), and finally the actual
choice (`choice`).

``` r
hitrate(
  data = hot_cbc, # data frame
  opts = c(option_1:none), # column names of alternatives
  choice = choice # column name of choice
)
#> # A tibble: 1 × 5
#>      hr    se chance   cor     n
#>   <dbl> <dbl>  <dbl> <int> <int>
#> 1  26.7  4.34   14.3    28   105
```

Next, we look at the magnitude of the mean absolute error by running the
`mae()` function. The arguments are the same as for the `hitrate()`
function.

``` r
mae(
  data = hot_cbc, # data frame
  opts = c(option_1:none), # column names of alternatives
  choice = choice # column name of choice
)
#> # A tibble: 1 × 1
#>     mae
#>   <dbl>
#> 1  5.68
```

Finally, let us test, how many participants would buy at least one of
three products, assuming that this is one potential assortment we would
like to offer to the consumers. We will use the `reach()` function. To
specify the bundles we are offering we use the `opts` argument in the
`reach()` function.

``` r
reach(
  data = hot_cbc, # data frame
  opts = c(option_1:option_3), # products that should be considered
  none = none # column name of none alternative
)
#> # A tibble: 1 × 1
#>   reach
#>   <dbl>
#> 1  72.4
```

### Example II - CBC with linear-coded attribute(s)

In the second example, we again use a *CBC*, however, this time we show
how to use the `create_hot()` function, if one of the variables is
linear-coded. All other examples are provided in the accompanied
vignette.

We are using the data frame `cbc_linear` (Sablotny-Wackershauser et al.
2024). Again, we first load the data frame.

``` r
data(cbc_linear)
```

Next, we create the validation task to evaluate it in the next step. We
use the same validation task as defined above (i.e., six alternatives
plus the *no-buy* alternative). The only difference to the previous
example is that the last attribute (`price`) was linear-coded and this
time, we want to interpolate values.

Again, we first define the `data` argument, the `id` as well as the
`none` alternative. Next, we define the `prod.levels` for each
alternative. Since we have one linear coded attribute, we need to
specify the column indexes instead of the column names in `prod.levels`.
We tell `create_hot()` that the last attribute needs to be interpolated
by specifying the `coding` argument accordingly. This tells us that the
first eight attributes are part-worth coded (`0`) while the last
attribute is linear-coded (`1`).

To interpolate the value, we need to provide `create_hot()` the
`interpolate.levels`. These **need** to be the same levels as provided
to Sawtooth Software (Sawtooth Software Inc. 2024) or `ChoiceModelR`.
Extrapolation is allowed, however, `create_hot()` will give a warning in
case extrapolation is applied.

Next, we define the column name of the linear coded variable (`lin.p`).
Again, we are running a CBC specified by the `method` argument. This
time, we would like to keep some of the variables in the data frame,
which we specify by using the `varskeep` argument. We only keep one
further variable, however, you can specify as many as you want. This
could be relevant if you would like to display the results, for example,
split by group. Finally, we define the actual choice (`choice`) in the
validation task and we are all set.

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
  choice = "hot",
  varskeep = "group"
)
```

The next steps are the same as above. However, let us take a look at
some examples in which we display the results per group. Let us again
begin with the `hitrate()` function.

``` r
hitrate(
  data = hot_cbc_linear, # data frame
  opts = c(option_1:none), # column names of alternatives
  choice = choice, # column name of choice
  group = group # column name of Grouping variable
)
#> # A tibble: 3 × 6
#>   group    hr    se chance   cor     n
#>   <int> <dbl> <dbl>  <dbl> <int> <int>
#> 1     1  30    8.51   14.3     9    30
#> 2     2  28.6  7.06   14.3    12    42
#> 3     3  33.3  8.33   14.3    11    33
```

Lastly, this time we also want to use a rescaling function, namely
`att_imp()` which gives us the importance of each attribute included
(Orme 2020). We need the data set with the raw logit coefficients
(`cbc_linear`; Sablotny-Wackershauser et al. (2024)). Next, we define
the `attrib` argument. Here, we need to specify each attribute level for
the corresponding level. Afterwards, we specify the coding again, and
since we have one linear coded attribute, we need to define the
`interpolate.levels` argument again, as we did for the `create_hot()`
function above. Finally, we set `res` to `agg`, which tells the
`att_imp()` function to display the aggregated results (to get results
for each individual set argument `res` to `ind`).

``` r
att_imp(
  data = cbc_linear,
  attrib = list(
    c(paste0("att1_lev", c(1:3))),
    c(paste0("att2_lev", c(1:2))),
    c(paste0("att3_lev", c(1:4))),
    c(paste0("att4_lev", c(1:4))),
    c(paste0("att5_lev", c(1:2))),
    c(paste0("att6_lev", c(1:4))),
    c(paste0("att7_lev", c(1:6))),
    c(paste0("att8_lev", c(1:6))),
    "price"
  ),
  coding = c(rep(0, times = 8), 1),
  interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
  res = "agg"
)
#> # A tibble: 9 × 3
#>   alternative    mw   std
#>   <chr>       <dbl> <dbl>
#> 1 att_imp_1    8.83  6.38
#> 2 att_imp_2    5.89  4.84
#> 3 att_imp_3   13.5   6.44
#> 4 att_imp_4   10.7   4.76
#> 5 att_imp_5    4.64  2.98
#> 6 att_imp_6    9.48  3.71
#> 7 att_imp_7   15.6   6.24
#> 8 att_imp_8    9.69  4.09
#> 9 att_imp_9   21.7  16.2
```

For more examples, please see the accompanied vignette.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-burger2018" class="csl-entry">

Burger, Scott V. March 2018. *Introduction to Machine Learning with r:
Rigorous Mathematical Analysis*. First edition. Beijing; Boston;
Farnham; Sebastopol; Tokyo: O’Reilly.

</div>

<div id="ref-chrzan2019" class="csl-entry">

Chrzan, Keith, and Bryan K. Orme. 2019. *Applied MaxDiff: A
Practitioner’s Guide to Best-Worst Scaling*. Provo, UT: Sawtooth
Software.

</div>

<div id="ref-ding2011" class="csl-entry">

Ding, Min, John R. Hauser, Songting Dong, Daria Dzyabura, Zhilin Yang,
SU Chenting, and Steven P. Gaskin. 2011. “Unstructured Direct
Elicitation of Decision Rules.” *Journal of Marketing Research* 48 (1):
116–27. <https://doi.org/10.1509/jmkr.48.1.116>.

</div>

<div id="ref-gilbride2008" class="csl-entry">

Gilbride, Timothy J., Peter J. Lenk, and Jeff D. Brazell. 2008. “Market
Share Constraints and the Loss Function in Choice-Based Conjoint
Analysis.” *Marketing Science* 27 (6): 995–1011.
<https://doi.org/10.1287/mksc.1080.0369>.

</div>

<div id="ref-green1990" class="csl-entry">

Green, Paul E., and V. Srinivasan. 1990. “Conjoint Analysis in
Marketing: New Developments with Implications for Research and
Practice.” *Journal of Marketing* 54 (4): 3–19.
<https://doi.org/10.1177/002224299005400402>.

</div>

<div id="ref-Metrics" class="csl-entry">

Hamner, Ben, and Michael Frasco. 2018. “Metrics: Evaluation Metrics for
Machine Learning.” <https://CRAN.R-project.org/package=Metrics>.

</div>

<div id="ref-philentropy" class="csl-entry">

HG, Drost. 2018. “Philentropy: Information Theory and Distance
Quantification with r” 3: 765.
<https://joss.theoj.org/papers/10.21105/joss.00765>.

</div>

<div id="ref-kuhn2008" class="csl-entry">

Kuhn, Max. 2008. “Building Predictive Models in*R*Using
the**caret**Package.” *Journal of Statistical Software* 28 (5).
<https://doi.org/10.18637/jss.v028.i05>.

</div>

<div id="ref-miaoulis1990" class="csl-entry">

Miaoulis, George, Henry Parsons, and Valerie Free. 1990. “Turf: A New
Planning Approach for Product Line Extensions.” *Marketing Research* 2
(1): 28–40.

</div>

<div id="ref-Orme2015" class="csl-entry">

Orme, Bryan K. 2015. “Including Holdout Choice Tasks in Conjoint
Studies.”

</div>

<div id="ref-orme2020" class="csl-entry">

———. 2020. *Getting Started with Conjoint Analysis: Strategies for
Product Design and Pricing Research*. 4th ed. Manhattan Beach, CA:
Research Publishers LLC.

</div>

<div id="ref-rao2014" class="csl-entry">

Rao, Vithala R. 2014. *Applied Conjoint Analysis*. Springer Berlin
Heidelberg. <https://doi.org/10.1007/978-3-540-87753-0>.

</div>

<div id="ref-sablotny-wackershauser2024" class="csl-entry">

Sablotny-Wackershauser, Verena, Marcel Lichters, Daniel Guhl, Paul
Bengart, and Bodo Vogt. 2024. “Crossing Incentive Alignment and Adaptive
Designs in Choice-Based Conjoint: A Fruitful Endeavor.” *Journal of the
Academy of Marketing Science*, January.
<https://doi.org/10.1007/s11747-023-00997-5>.

</div>

<div id="ref-sawtooth2024" class="csl-entry">

Sawtooth Software Inc. 2024. “Lighthouse Studio 9.” Sequim, WA.:
Sawtooth Software Inc.
<https://www.sawtoothsoftware.com/products/online-surveys>.

</div>

<div id="ref-schramm2024" class="csl-entry">

Schramm, Joshua Benjamin, and Marcel Lichters. 2024. “Incentive
Alignment in Anchored MaxDiff Yields Superior Predictive Validity.”
*Marketing Letters*, January.
<https://doi.org/10.1007/s11002-023-09714-2>.

</div>

<div id="ref-ChoiceModelR" class="csl-entry">

Sermas, Ryan. 2022. “ChoiceModelR: Choice Modeling in r.”
<https://CRAN.R-project.org/package=ChoiceModelR>.

</div>

<div id="ref-steiner2018" class="csl-entry">

Steiner, Michael, and Martin Meißner. 2018. “A User’s Guide to the
Galaxy of Conjoint Analysis and Compositional Preference Measurement.”
*Marketing ZFP* 40 (2): 3–25.
<https://doi.org/10.15358/0344-1369-2018-2-3>.

</div>

<div id="ref-yang2018" class="csl-entry">

Yang, Liu (Cathy), Olivier Toubia, and Martijn G. de Jong. 2018.
“Attention, Information Processing, and Choice in Incentive-Aligned
Choice Experiments.” *Journal of Marketing Research* 55 (6): 783–800.
<https://doi.org/10.1177/0022243718817004>.

</div>

</div>
