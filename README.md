
<!-- README.md is generated from README.Rmd. Please edit that file -->

# validateHOT 🎯

<!-- badges: start -->

[![R-CMD-check](https://github.com/JoshSchramm94/validateHOT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JoshSchramm94/validateHOT/actions/workflows/R-CMD-check.yaml)

[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

<!-- badges: end -->

The goal of the validateHOT package is to provide a tool to validate the
results of a validation task (also known as holdout task; short form
**HOT**), simulating markets with the results of a MaxDiff, CBC, and
ACBC, and lastly converting the raw logit scores into scores that are
easier to communicate. This package is especially relevant for the
[Sawtooth Software](https://sawtoothsoftware.com/) community who would
like to report their analysis in *R* for open science purposes. However,
it also works with other packages, for example, the ChoiceModelR package
(Sermas, 2022). Further, the validateHOT package is useful for
practitioners, who would like to run the analyses in an open source
software.

The goal of preference measurement techniques is to predict future
behavior (Green & Srinivasan, 1990). Therefore, it is essential for both
academics and practitioners to ensure that the collected data is valid
and can also predict outside tasks that were not included in the
estimation of the utility scores. Including validation tasks is highly
recommended (Orme, 2015; Rao, 2014). They do not only check whether your
data is valid but can also help to test different models. The validatHOT
package provides some of the relevant metrics to test the performance of
the data in predicting a validation task.

The results (i.e., raw logits) of preference measurement techniques are
often used to simulate or predict markets (Gilbride, Lenk, & Brazell,
2008). The validateHOT package provides some useful functions for an
easy implementation to simulate markets for both (A)CBC and MaxDiff.

> The validateHOT package was primarily developed to work with Sawtooth
> Software and the ChoiceModelR package. Please be cautious about using
> it with different platforms (especially for linear and piecewise-coded
> variables).

👉🏾 <u>**What you need to provide**</u>: <br> After collecting your data
and running your initial Hierarchical Bayes model, you use the
validateHOT package and test how good your model predicts choices in the
validation task, run market simulations, or convert your data into
scores which are easier to interpret. To use the validateHOT package,
you need to read in your raw utility scores. If you plan to validate a
validation task you also need to provide the actual choice in this task.
We provide a short tutorial in this markdown. For a more detailed
tutorial, please see the vignette that comes along with the validateHOT
package (`vignette("validateHOT", package = "validateHOT")`).

👈🏾 <u>**What you get**</u>:<br> At the moment, the validateHOT package
provides functions for 4 key components:

- validation metrics

- metrics that are usually reported in machine learning (i.e., confusion
  matrix)

- simulation methods, for example, to determine optimal product
  combinations

- converting raw logit utilities in scores that are easier to interpret

For the first 3 components, the `create_hot()` function is essential.
This function creates the total utilities for each alternative in the
validation task and in the market simulation, respectively.
`create_hot()` will calculate the total utility of each alternative
according to the additive utility model (Rao, 2014, p. 82).

### Classical validation metrics

- `hitrate()`: creates the hit rate (correctly predicted choices) of the
  validation task.

- `kl()`: Kullback-Leibler-Divergence measures the divergence between
  the actual choice distribution and the predicted choice distribution
  (Ding et al., 2011; Drost, 2018). The output provides both divergence
  between predicted from observed and observed from predicted due to the
  asymmetry of the Kullback-Leibler divergence. The validateHOT package
  currently provides two logarithm bases: $log$ and $log_2$.

- `mae()`: calculates the mean absolute error, i.e., deviation between
  predicted and stated choice share.

- `medae()`: calculates the median absolute error, which is, compared to
  the mean absolute error, less affected by outliers.

- `mhp()`: calculates the averaged hit probability of participant’s
  actual choice in the validation task.

- `rmse()`: provides the root mean square error of deviation between
  predicted and stated choice share.

All functions can be extended with the `group` argument to get output
split by group(s).

### Confusion Matrix

We also include metrics from machine learning, i.e., the confusion
matrix (e.g., Burger, 2018). For all of the five provided functions, a
**none** alternative has to be included in the validation task. The
logic of the implemented confusion matrix is to test, for example,
whether a buy or no-buy was correctly predicted. Information could be
used to get a sense of overestimation and underestimation of general
product demand. In the table below `TP` stands for true positives, `FP`
for false positives, `TN` for true negatives, and `FN` for false
negatives (Burger, 2018; Kuhn, 2008). To translate this to the logic of
the validateHOT package, imagine you have a validation task with five
alternatives plus the alternative not to buy any of those alternatives.
The validateHOT package now measures whether or not a buy (participant
opts for one of the five alternatives) or a no-buy (participant opts for
the none alternative), respectively, is correctly predicted.

Please be aware that the validateHOT package applies the following
coding of the *buy* and *no-buy* alternatives. Rows refer to the
observed decisions while columns refer to the predicted ones.

|        | Buy | No-buy |
|--------|:---:|:------:|
| Buy    | TP  |   FN   |
| No-buy | FP  |   TN   |

- `accuracy()`: calculates the number of correctly predicted choices
  (buy or no-buy); $\frac{TP + TN}{TP + TN + FP + FN}$ (Burger, 2018).

- `f1()`: defined as $\frac{2 * precision * recall}{precision + recall}$
  or stated differently by Burger (2018) $\frac{2TP}{2TP + FP + FN}$.

- `precision()`: defined as $\frac{TP}{TP + FP}$ (Burger, 2018).

- `recall()`: defined as $\frac{TP}{TP + FN}$ (Burger, 2018).

- `specificity()`: defined as $\frac{TN}{TN + FP}$ (Burger, 2018).

Again, all functions can be extended with the `group` argument to get
the output split by group(s).

### Simulation Methods

- `turf()`: **T**(otal) **U**(nduplicated) **R**(each) and
  **F**(requency) is a “product line extension model” (Miaoulis et al.,
  1990, p. 29) that helps to find the perfect product bundle based on
  the reach (e.g., how many participants consider buying at least one
  product of that assortment) and the frequency (how many products on
  average are a purchase option). `turf()` currently provides both the
  *threshold* approach (`approach ='thres'`; all products that exceed a
  threshold are considered, e.g., as purchase option; Chrzan & Orme,
  2019, p. 112) and the *first choice* approach (`approach = 'fc'`; only
  product with highest utility is considered as purchase option; Chrzan
  & Orme, 2019, p. 111).

- `freqassort()`: Similar to `turf()`, `freqassort()` will give you the
  averaged frequency, how many products the participants will choose
  from a potential assortment. Again, you have to define a `none`
  alternative, because `freqassort()` uses the *threshold* approach,
  meaning if the utility of one product is above the utility of `none`,
  it is marked as potential purchase option. While `turf()` calculates
  the reach and frequency for **all** combinations, you specify the
  combination you are interested in `freqassort()`.

- `reach()`: Similar to `turf()`, `reach()` will give you the averaged
  percentage of how many participants you can reach (at least one of the
  products resemble a purchase option) with your in the function
  determined potential assortment. `reach()` also uses the *threshold*
  approach (see above). While `turf()` calculates the reach and
  frequency for **all** combinations, you specify the combination you
  are interested in `reach()`.

- `marksim()`: Runs market simulations (either share of preference,
  `sop` or first choice rule, `fc`), including the standard error, and
  the lower and upper confidence interval, which is calculated according
  to the following formula $mean +/- 1.96 x \frac{sd}{\sqrt(n)}$ (Orme,
  2020, p. 94).

> **Important**: For both `reach()` and `freqassort()`, `none` does not
> necessarily have to be the no-buy alternative but can be another
> alternative that should be exceeded.

### Converting raw utilities

The validateHOT package also provides four functions to better interpret
the scores of both (A)CBC and MaxDiff, namely:

- `att_imp()`: Converts the raw utilities of either an ACBC or CBC into
  importance scores for each attribute (see, Orme, 2020, pp. 79-81)

- `prob_scores()`: Converts the raw utilities of a MaxDiff to choice
  probabilities by applying the following procedures:

- For unanchored MaxDiff: First, the scores are zero-centered,
  afterwards they are transformed by the following formula
  $\frac{exp^{U_i}}{(exp^{U_i} + a - 1)}$ (Chrzan & Orme, 2019, p. 56),
  where $U_i$ is the raw utility of item *i* and `a` is the number of
  items shown simultaneously per MaxDiff task. Finally, the scores are
  normalized.

- For anchored MaxDiff a slightly different formula is applied:
  $\frac{exp^{U_i}}{(exp^{U_i} + a - 1)} * 100 * \frac{1}{a}$ (Chrzan &
  Orme, 2019, p. 59).

- `zc_diffs()`: Rescales the raw logit utilities to make them comparable
  across participants (Sawtooth Software Inc, 2023, p. 343).

- `zero_anchored()`: Rescales the raw logits of a MaxDiff to
  zero-centered diffs (Chrzan & Orme, 2019, p. 64).

### Data Frames provided by the validateHOT package

The package provides five data sets that help to better explain the
functions as well as the structure of the input, especially for the
`create_hot()` function.

- `acbc`: Example data set with raw utilities of an ACBC study conducted
  in Sawtooth. Price was linear-coded while the other attributes were
  coded as part-worths.

- `acbc_interpolate`: Example data set with raw utilities of an ACBC
  study conducted in Sawtooth. Price was piecewise-coded, another
  attribute was linear-coded while the other attributes were coded as
  part-worths.

- `cbc`: Example data set with raw utilities of a CBC study conducted in
  Sawtooth. All attributes were coded as part-worth.

- `cbc_linear`: Example data set with raw utilities of a CBC study
  conducted in Sawtooth. One attribute was linear-coded while the other
  attributes are part-worth coded.

- `maxdiff`: Example data set with raw utilities of a MaxDiff study
  conducted in Sawtooth.

## The story behind the validateHOT package

We are teaching a preference measurement seminar for students. Often
these students did not have any prior experience (or only sparsely) with
*R*. One of the chapters in this class is about model validation by
checking holdout task and we teach this, of course, in *R* 😍. We want
to put a strong emphasis on open science and providing tools to run the
analyses and provide the code afterwards is essential. The validateHOT
package makes this process look easy 🤹‍♀️. Of course, there are other
great packages which are faster in terms of running time (i.e.,
`Metrics` by Hamner & Frasco, 2018), however, these packages need some
more data wrangling to use the appropriate functions with the raw
utilities, which might be a burden or barrier for the one or the other.

Moreover, as Yang et al. (2018) report, commercial studies often do not
use any validation task. Again, the missing experience in *R* could be
one explanation. Since these functions are not always implemented in
other software, this might be one reason why they do not include one
simply because they do not know how to use it correctly. Having a
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
*MaxDiff*, we will provide an example with a *CBC*. Let us begin with a
*CBC* where all of the attributes are part-worth coded and we do not use
any interpolation. Let us load the `cbc` data frame for this example.

``` r
data("cbc")
```

The data frame has a total of 105 participants and 41 columns.

Now imagine you included a validation task with six alternatives plus a
no-buy alternative. We specify the `data` argument and as well as the
`id`. Since we also have a *no-buy* alternative in our validation task,
we specify the `none` argument, otherwise we would have left it empty.
Afterwards, we define each alternative with the argument `prod.levels`.
If we look back at the data frame, we can see that the first alternative
in the holdout task (`c(3, 6, 10, 13, 16, 20, 24, 32, 35)`) is composed
of the following attribute levels att1_lev2, att2_lev2, NA, att4_lev3,
att5_lev2, att6_lev4, att7_lev4, att8_lev6, and price_3.

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
#> 1  25.7  4.29   14.3    27   105
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

To also cover an example on how to use the metrics of the confusion
matrix, we could test whether our model overestimates the purchase
behavior (our model predicts *buy* although participant opts for
*no-buy*) or underestimates it (i.e., model predicts *no-buy* but
participant opts for *buy*). We will test the accuracy of the model by
running the `accuracy()` function.

``` r
accuracy(
  data = hot_cbc, # data frame
  opts = c(option_1:none), # column names of alternatives
  choice = choice, # column name of choice
  none = none # column name of none alternative
)
#> # A tibble: 1 × 1
#>   accuracy
#>      <dbl>
#> 1     69.5
```

Finally, let us test, how many participants would at least buy one of
three products, assuming that this is one potential assortment we would
like to offer to our consumers. We will use the `reach()` function. To
specify the bundles we are offering we use the `opts` argument in our
function.

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

In a second example, we again use a *CBC*, however, this time we show
how to use the package if one of the variables is linear-coded. All
other examples are provided in the accompanied vignette.

We are using the data frame `cbc_linear`. Again, we first load the data
frame.

``` r
data("cbc_linear")
```

Next, we create the validation task to evaluate it in the next step. We
use the same validation task as defined above (i.e., 6 alternatives plus
the *no-buy* alternative). The only difference to the previous example
is that the the last attribute (`price`) was linear-coded and this time,
we want to interpolate values.

Again, we first define data, namely the `id` as well as the `none`
alternative. Next, we define the `prod.levels` for each alternative.
Since we have one linear coded attribute, we have to specify the column
indexes instead of the column names in `prod.levels`. We tell
`create_hot()` that the last attribute needs to be interpolated by
specifying the `coding` argument accordingly. This tells us that the
first eight attributes are part-worth coded (`0`) while the last
attribute is linear-coded (`1`).

To interpolate the value, we have to provide `create_hot()` the
`interpolate.levels`. These **need** to be the same levels as provided
to Sawtooth or `ChoiceModelR`. Extrapolation is allowed, however,
`create_hot()` will give a warning in case extrapolation is applied.

Next, we define the column of the linear coded variable (`lin.p`).
Again, we are running a *CBC* specified by the `method` argument. This
time, we would like to keep some of the variables in the data frame,
which we specify by using the `varskeep` argument. We only keep one
further variable, however, you can specify as many as you want. This
could be relevant if you would like to display results per group.
Finally, we define the actual choice (`choice`) in the validation task
and we are all set.

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
  lin.p = 33,
  interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
  method = "cbc",
  choice = "hot",
  varskeep = "group"
)
```

The next steps are the same as above. However, let us take a look at
some examples in which we display the results per group. Let us again
begin with the `hitrate()` function. To do so, we specify the column
name of the grouping variable in the `group` argument.

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

In this case, the group assignment is stored in form of integers.
However, the output is the same if it is a factor or labelled data. To
proof this we just quickly change `group` into a factor by using the
`factor()` function provided by the *R* Core Team (2023).

``` r
hot_cbc_linear$group <- base::factor(
  hot_cbc_linear$group,
  levels = c(1:3),
  labels = paste0("group_", c(1:3))
)
```

Afterward, we display the *mean hit probability* by running the `mhp()`
function.

``` r
mhp(
  data = hot_cbc_linear, # data frame
  opts = c(option_1:none), # column names of alternatives
  choice = choice, # column name of choice
  group = group # column name of Grouping variable
)
#> # A tibble: 3 × 3
#>   group     MHP    se
#>   <fct>   <dbl> <dbl>
#> 1 group_1  29.1  6.85
#> 2 group_2  27.8  5.28
#> 3 group_3  36.1  6.96
```

Lastly, this time we also want to use a rescaling function, namely
`att_imp()` which basically tells us the importance of each attribute
included (Orme, 2020). We need the data set with the raw logit
coefficients (`cbc_linear`). Next, we define the `attrib` argument.
Here, we need to specify each attribute level for the corresponding
level. Afterwards, we specify the coding again, and since we have one
linear coded attribute, we need to define the `interpolate.levels`
argument again, as we did for the `create_hot()` function above.
Finally, we set `res` to `agg`, which tells `att_imp()` to display the
aggregated results (for individuals results set `res` to `ind`).

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

Burger, Scott V. 2018. <em>Introduction to Machine Learning with R:
Rigorous Mathematical Analysis</em>. O’Reilly.

Chrzan, K., and Orme, B. K. 2019. <em>Applied MaxDiff: A Practitioner’s
Guide to Best-Worst Scaling</em> Provo, UT: Sawtooth Software.

Ding, Min, John R. Hauser, Songting Dong, Daria Dzyabura, Zhilin Yang,
SU Chenting, and Steven P. Gaskin. 2011. “Unstructured Direct
Elicitation of Decision Rules.” <em>Journal of Marketing Research
48</em>(1): 116-27. <https://doi.org/10.1509/jmkr.48.1.116>

Drost, Hajk-Georg. 2018. “Philentropy: Information Theory and Distance
Quantification with R” <em>Journal of Open Source Software 3</em>(26),
765, <https://joss.theoj.org/papers/10.21105/joss.00765>.

Gilbride, Timothy J., Lenk, Peter J., and Brazell, Jeff D. 2008. “Market
Share Constraints and the Loss Function in Choice-Based Conjoint
Analysis.” <em>Marketing Science 27</em>(6):
995-1011.<https://doi.org/10.1287/mksc.1080.0369>

Green, Paul E., and Srinivasan, V. 1990. “Conjoint Analysis in
Marketing: New Developments with Implications for Research and
Practice.” <em>Journal of Marketing 54</em>(4):
3-19.<https://doi.org/10.1177/002224299005400402>.

Kuhn, Max. 2008. “Building Predictive Models in R Using the caret
Package.” <em>Journal of Statistical Software 28</em>(5), 1-26.
<https://doi.org/10.18637/jss.v028.i05>.

Sawtooth Software Inc. 2023. <em>Lighthouse Studio 9. Version
9.15.2.</em> Sequim, WA.: Sawtooth Software Inc.

Hamner, Ben, and Michael Frasco. 2018. “Metrics: Evaluation Metrics for
Machine Learning.” <https://CRAN.R-project.org/package=Metrics>.

Miaoulis, G., Parsons, H., and Free, V. 1990. Turf: A New Planning
Approach for Product Line Extensions. <em>Marketing Research 2</em>(1):
28-40.

Orme, Bryan K. 2015. “Including Holdout Choice Tasks in Conjoint
Studies.”
<https://sawtoothsoftware.com/resources/technical-papers/including-holdout-choice-tasks-in-conjoint-studies>.

Orme, B. K. 2020. <em>Getting Started with Conjoint Analysis: Strategies
for Product Design and Pricing Research</em>. 4th edition. Manhattan
Beach, CA: Research Publishers LLC.

R Core Team. 2023. “R: A Language and Environment for Statistical
Computing.” <https://www.R-project.org/>.

Rao, Vithala R. 2014. <em>Applied Conjoint Analysis.</em> Springer
Berlin Heidelberg. <https://doi.org/10.1007/978-3-540-87753-0>.

Sermas, R. 2022. *ChoiceModelR: Choice Modeling in R*. R package version
1.3.0, <https://CRAN.R-project.org/package=ChoiceModelR>.

Yang, Liu (Cathy), Olivier Toubia, and Martijn G. de Jong. 2018.
“Attention, Information Processing, and Choice in Incentive-Aligned
Choice Experiments.” <em>Journal of Marketing Research 55</em>(6):
783–800. <https://doi.org/10.1177/0022243718817004>.
