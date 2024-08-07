
<!-- README.md is generated from README.Rmd. Please edit that file -->

# validateHOT 🎯

<!-- badges: start -->

[![R-CMD-check](https://github.com/JoshSchramm94/validateHOT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JoshSchramm94/validateHOT/actions/workflows/R-CMD-check.yaml)

[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

<!-- badges: end -->

The goal of `validateHOT` is to provide a tool to validate the results
of a validation task (also known as holdout task; short form **HOT**),
simulating markets with the results of your MaxDiff, CBC, and ACBC, and
lastly converting the raw logit scores of your MaxDiff, CBC, and ACBC
into scores that are easier to communicate. This package is especially
relevant for the [Sawtooth Software](https://sawtoothsoftware.com/)
community who would like to report their analysis in *R* for open
science purposes. However, it also works perfectly with the package
`ChoiceModelR`(Sermas, 2022). This package is extremely useful for
practitioners, who would like to run the analyses in an open source
software. The ultimate goal of these preference measurement techniques
is to predict future behavior (Green & Srinivasan, 1990). Therefore, it
is essential for both academics and practitioners to ensure that the
collected data is valid and can also predict outside tasks that were not
included in the estimation of the utility scores.

Although commercial studies often do not include a validation/holdout
task (Yang et al., 2018), it is highly recommended to do so (Orme, 2015;
Rao, 2014). Validation/holdout tasks do not only check whether your data
is valid but can also help to test different models. `validatHOT`
provides some of the relevant metrics to test the performance of the
data in predicting a validation/holdout task.

Moreover, another key goal of preference measurement techniques is to
simulate or predict markets (Gilbride, Lenk, & Brazell, 2008).
`validateHOT` provides key functions for an easy implementation to
simulate markets for both (A)CBC and MaxDiff.

> `validateHOT` was developed to work with Sawtooth Software and
> `ChoiceModelR`. Please be cautious about using it with different
> platforms (especially for linear and piecewise-coded variables).

👉🏾 <u>**What you need to provide**</u>: <br> After collecting your data
and running your initial Hierarchical Bayes models, you can turn to
`validateHOT` and test how good your model predicts choices in the
validation/holdout task, run market simulations, or convert your data
into scores which are easier to interpret. To enjoy `validateHOT` you
only have to read in your raw utility scores. If you plan to validate
your validation/holdout tasks you also need to provide the actual choice
of your validation/holdout task. You can use the `merge()` function
provided by the *R* base package to do so (R Core Team, 2023). We
provide a short tutorial in the markdown, for a more detailed tutorial,
please see the vignette that comes along with `validateHOT`
(`vignette("validateHOT", package = "validateHOT")`).

👈🏾 <u>**What you get**</u>:<br> At the moment, `validateHOT` provides
functions for 4 key components:

<ul>
<li>
validation metrics
</li>
<li>
metrics that are usually reported in machine learning (i.e., confusion
matrix)
</li>
<li>
simulation methods, for example, to determine optimal product
combinations
</li>
<li>
converting raw logit utilities in scores that are easier to interpret
</li>
</ul>

For the first 3 components, the `createHOT()` function is essential.
This function creates the total utilities for each alternative in the
validation/holdout task and also in the simulation method, respectively.
Rao (2014, p. 82) mentions the additive utility model stating that the
total utility of a profile or conjoint model is the sum of its attribute
levels. `createHOT()` will calculate the total utility of each
alternative you specify.

### Classical validation metrics

<ul>
<li>
<code>hitrate()</code>: creates the Hit Rate (correctly predicted
choices) of your validation/holdout task. The output will contain the
percentage of how many choices were correctly predicted including the
standard error, the chance level in your validation task
($\frac{1}{alternatives}$) in percentage, the number of correctly
predicted participants’ choices as well as the number of observations.
</li>
<li>
<code>kl()</code>: Kullback-Leibler-Divergence measures the divergence
between the actual choice distribution and the predicted choice
distribution (Ding et al., 2011; Drost, 2018). Output provides both
divergence between predicted from observed and observed from predicted
due to the asymmetry of the Kullback-Leibler divergence.
<code>validateHOT</code> currently provides two logarithm bases: $log$
and $log_2$. The default is set to $log$.
</li>
<li>
<code>mae()</code>: calculates the mean absolute error, i.e., deviation
between predicted and stated choice share.
</li>
<li>
<code>medae()</code>: calculates the median absolute error, which is,
compared to the mean absolute error, less affected by outliers.
</li>
<li>
<code>mhp()</code>: calculates the averaged hit probability of
participant’s actual choice in the validation/holdout task.
</li>
<li>
<code>rmse()</code>: provides the root-mean-square error of deviation
between predicted and stated choice share.
</li>
</ul>

All functions can be extended with the `group` argument to get output
split by group(s).

### Confusion Matrix

We also include metrics from machine learning, i.e., the confusion
matrix (e.g., Burger, 2018). For all of the five provided functions, a
**none** alternative has to be included in the validation/holdout task.
The logic of the implemented confusion matrix is to test, for example,
whether a buy or no-buy was correctly predicted. Information could be
used to get a sense of overestimation and underestimation, respectively,
of general product demand. In the table below <code>TP</code> stands for
true positives, <code>FP</code> for false positives, <code>TN</code> for
true negatives, and <code>FN</code> for false negatives (Burger, 2018;
Kuhn, 2008). To translate this to the `validateHOT` logic, imagine you
have a validation/holdout task with five alternatives plus the
alternative not to buy any of those alternatives. `validateHOT` now
measures whether or not a buy (participant opts for one of the five
alternatives) or a no-buy (participant opts for the none alternative),
respectively, is correctly predicted.

Please be aware that `validateHOT` applies the following coding of the
*buy* and *no-buy* alternatives. Rows refer to the observed decisions
while columns refer to the predicted ones.

|        | Buy | No-buy |
|--------|:---:|:------:|
| Buy    | TP  |   FN   |
| No-buy | FP  |   TN   |

<ul>
<li>
<code>accuracy()</code>: calculates the number of correctly predicted
choices (buy or no-buy); $\frac{TP + TN}{TP + TN + FP + FN}$ (Burger,
2018).
</li>
<li>
<code>f1()</code>: defined as
$\frac{2 * precision * recall}{precision + recall}$ or stated
differently by Burger (2018) $\frac{2TP}{2TP + FP + FN}$.
</li>
<li>
<code>precision()</code>: defined as $\frac{TP}{TP + FP}$ (Burger,
2018).
</li>
<li>
<code>recall()</code>: defined as $\frac{TP}{TP + FN}$ (Burger, 2018).
</li>
<li>
<code>specificity()</code>: defined as $\frac{TN}{TN + FP}$ (Burger,
2018).
</li>
</ul>

Again, all functions can be extended with the `group` argument to get
the output split by group(s).

### Simulation Methods

<ul>
<li>
<code>turf()</code>: <b>T</b>(otal) <b>U</b>(nduplicated) <b>R</b>(each)
and <b>F</b>(requency) is a “product line extension model” (Miaoulis et
al., 1990, p. 29) that helps to find the perfect product bundle based on
the reach (e.g., how many participants consider buying at least one
product of that assortment) and the frequency (how many products on
average are a purchase option). <code>turf()</code> currently provides
both the *threshold* approach (<code>approach = ‘thres’</code>; all
products that exceed a threshold are considered, e.g., as purchase
option; Chrzan & Orme, 2019, p. 112) and the *first choice* approach
(<code>approach = ‘thres’</code>; only product with highest utility is
considered as purchase option; Chrzan & Orme, 2019, p. 111).
</li>
<li>
<code>freqassort()</code>: Similar to <code>turf()</code>,
<code>freqassort()</code> will give you the averaged frequency, how many
products the participants will choose from a potential assortment.
Again, you have to define a <code>none</code> alternative, because
<code>freqassort()</code> uses the <i>threshold</i> approach, meaning if
the utility of one product is above the utility of <code>none</code>, it
is marked as potential purchase option. While <code>turf()</code>
calculates the reach and frequency for <b>all</b> combinations, you
specify the combination you are interested in <code>freqassort()</code>.
</li>
<li>
<code>reach()</code>: Similar to <code>turf()</code>,
<code>reach()</code> will give you the averaged percentage of how many
participants you can reach (at least one of the products resemble a
purchase option) with your in the function determined potential
assortment. <code>reach()</code> also uses the <i>threshold</i> approach
(see above). While <code>turf()</code> calculates the reach and
frequency for <b>all</b> combinations, you specify the combination you
are interested in <code>reach()</code>.
</li>
<li>
<code>marksim()</code>: Runs market simulations (either share of
preference or first choice rule), including the standard error, as well
as the lower and upper confidence interval, which is calculated
according to the following formula $mean +/- 1.96 x \frac{sd}{\sqrt(n)}$
(Orme, 2020, p. 94).
</li>
</ul>

> <b>Important</b>: For both <code>reach()</code> and
> <code>freqassort()</code>, <code>none</code> does not necessarily have
> to be the no-buy alternative but can be another alternative that
> should be exceeded.

### Converting raw utilities

<code>validateHOT</code> also provides four functions to better
interpret the scores of both (A)CBC and MaxDiff, namely:

<ul>
<li>
<code>att_imp()</code>: Converts the raw utilities of either an ACBC or
CBC into importance scores for each attribute (see, Orme, 2020, pp.
79-81)
</li>
<li>

<code>prob_scores()</code>: Converts the raw utilities of a MaxDiff to
choice probabilities by applying the following procedures:

<ul>
<li>
For unanchored MaxDiff: First, the scores are zero-centered, afterwards
they are transformed by the following formula
$\frac{exp^{U_i}}{(exp^{U_i} + a - 1)}$ (Chrzan & Orme, 2019, p. 56),
where $U_i$ is the raw utility of item *i* and <code>a</code> is the
number of items shown simultaneously per MaxDiff task. Finally, the
scores are normalized.
</li>
<li>
For anchored MaxDiff a slightly different formula is applied:
$\frac{exp^{U_i}}{(exp^{U_i} + a - 1)} * 100 * \frac{1}{a}$ (Chrzan &
Orme, 2019, p. 59).
</li>
</ul>
</li>
<li>
<code>zc_diffs()</code>: Rescales the raw logit utilities to make them
comparable across participants (Sawtooth Software Inc, 2023, p. 343).
</li>
<li>
<code>zero_anchored()</code>: Rescales the raw logits of a MaxDiff to
zero-centered diffs (Chrzan & Orme, 2019, p. 64).
</li>
</ul>

### Data Frames provided by <code>validateHOT</code>

`validateHOT` provides five data sets that help to better explain the
functions as well as the structure of the input, especially for the
`createHOT()` function.

<ul>
<li>
<code>ACBC</code>: Example data set with raw utilities of an ACBC study
conducted in Sawtooth. Price was linear-coded while the other attributes
were coded as part-worths.
</li>
<li>
<code>ACBC_interpolate</code>: Example data set with raw utilities of an
ACBC study conducted in Sawtooth. Price was piecewise-coded, another
attribute was linear-coded while the other attributes were coded as
part-worths.
</li>
<li>
<code>CBC</code>: Example data set with raw utilities of a CBC study
conducted in Sawtooth. All attributes were coded as part-worth.
</li>
<li>
<code>CBC_lin</code>: Example data set with raw utilities of a CBC study
conducted in Sawtooth. One attribute was linear-coded while the other
attributes are part-worth coded.
</li>
<li>
<code>MaxDiff</code>: Example data set with raw utilities of a MaxDiff
study conducted in Sawtooth.
</li>
</ul>

## The story behind `validateHOT`

We are teaching a preference measurement seminar for students. Often
these students did not have any prior experience (or only sparsely) with
*R*. One of the chapters in this class is about model validation by
checking holdout task and we teach this, of course, in *R* 😍. We want
to put a strong emphasis on open science and providing tools to run the
analyses and provide the code afterwards is essential. `validateHOT`
makes this process look easy 🤹‍♀️. Of course, there are other great
packages which are faster in terms of running time (i.e., `Metrics` by
Hamner & Frasco, 2018), however, these packages need some more data
wrangling in order to use the appropriate functions with the raw
utilities, which might be a burden or barrier for the one or the other.

Moreover, as Yang et al. (2018) report, commercial studies often do not
use any validation task. Again, the missing experience in *R* could be
one explanation. Since these functions are not always implemented in
other software, this might be one reason why they do not include one
simply because they do not know how to use it correctly. Having a
package to evaluate the validation/holdout task can be very beneficial
from this perspective.

## Installation

You can install the development version of `validateHOT` from
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
*CBC* where all of the attributes are part-worth coded, so we need no
interpolation. Let us load the `CBC` data frame for this example.

``` r
data("CBC")
```

The data frame has a total of 79 participants and 23 columns.

Now imagine you included a validation/holdout task with three
alternatives plus a no-buy alternative. We specify the `data` argument
and as well as the `id`. Since we also have a *no-buy* alternative in
our validation task, we specify the `none` argument, otherwise we would
have left it empty. Afterwards, we define each alternative with the
argument `prod.levels`. If we look back at the data frame, we can see
that the first alternative in the holdout task (`c(4, 9, 18)`) is
composed of the following attribute levels `Att1_Lev1`, `Att2_Lev1`, and
`Att3_Lev5`.

As mentioned above, all the attributes are part-worth coded, therefore,
we set `coding = c(0, 0, 0)`. Finally, we specify the method, which is
`method = "CBC"` in our case, and define the column index of the actual
participant’s choice (`choice`). If you run the code, a data frame
called `HOT` (short form of **H**old**o**ut **t**ask) will be returned
to the global environment.

> ❗ `createHOT()` takes both column names and column indexes. However,
> please be aware, if you include linear-coded or piecewise-coded, you
> **have** to provide the column indexes for the input in `prod.levels`.
> However, you can easily find out the index by either using the
> `names()` function to see all names of the data frame, or if you
> already know the name of the variable, you can use `which()` and
> `colnames()`, both functions are provided by the base package (R Core
> Team, 2023). For example, if you want to find out the column index of
> `ID`, you could run `which(colnames(CBC) == "ID")`.

``` r
HOT <- createHOT(
  data = CBC,
  id = "ID",
  none = 21,
  prod.levels = list(c("Att1_Lev1", "Att2_Lev1", "Att3_Lev5"), c(8, 12, 17), c(5, 10, 17)),
  coding = c(0, 0, 0),
  method = "CBC",
  choice = "HOT"
)
```

> In case you just need to create a market scenario, you can also leave
> the `choice` argument empty.

Let us take a glimpse at the output, which shows the participants’ total
raw utilities for each of the three alternatives that were included in
the validation/holdout task.

``` r
head(HOT)
#>   ID   Option_1   Option_2   Option_3       None choice
#> 1  1  0.5613133  0.5851903  1.3231991 -3.2921817      1
#> 2  2 -1.8143533 -0.7186898 -1.7759660 -0.9123018      1
#> 3  3 -0.1217458 -0.4783988 -2.2596407 -0.7447178      1
#> 4  4 -0.6333181 -1.8028929 -0.9638149  2.5995588      4
#> 5  5  1.3698645 -0.6541611 -0.9273235 -2.8965076      2
#> 6  6 -0.6231678 -0.9250507 -1.3261888 -1.0404554      2
```

In the next step, we would like to see how well our model (from which we
took the raw utilities) predicts the actual choices in the
validation/holdout task. First, we will run the `hitrate()` function. We
specify the `data`, the column names of the alternatives (`opts`;
remember there are three alternatives + the *no-buy* alternative), and
finally the actual choice (`choice`).

``` r
hitrate(
  data = HOT, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice # column name of choice
)
#> # A tibble: 1 × 5
#>      HR    se chance   cor     n
#>   <dbl> <dbl>  <dbl> <int> <int>
#> 1  46.8  5.65     25    37    79
```

Next, we look at the magnitude of the mean absolute error by running the
`mae()` function. The arguments are the same as for the `hitrate()`
function.

``` r
mae(
  data = HOT, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice # column name of choice
)
#> # A tibble: 1 × 1
#>     mae
#>   <dbl>
#> 1  10.1
```

To cover also one example on how to use the metrics of the confusion
matrix, we could test whether our model overestimates the purchase
behavior (our model predicts *buy* although participant opts for
*no-buy*) or underestimates it (i.e., model predicts *no-buy* but
participant opts for *buy*). We will test the accuracy of the model by
running the `accuracy()` function.

``` r
accuracy(
  data = HOT, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice, # column name of choice
  none = None # column name of none alternative
)
#> # A tibble: 1 × 1
#>   accuracy
#>      <dbl>
#> 1     93.7
```

Finally, let us test, how many participants would at least buy one of
the three products, assuming that this is one potential assortment we
would like to offer to our consumers. We will use the `reach()`
function. To specify the bundles we are offering we use the `opts`
argument in our function.

``` r
reach(
  data = HOT, # data frame
  opts = c(Option_1:Option_3), # products that should be considered
  none = None # column name of none alternative
)
#> # A tibble: 1 × 1
#>   reach
#>   <dbl>
#> 1  86.1
```

### Example II - CBC with linear-coded attribute(s)

In a second example, we again use a *CBC*, however, this time we show
how to use `validateHOT` if one of the variables is linear-coded. All
other examples are provided in the accompanied vignette.

We are using the data frame `CBC_lin`. Again, we first load the data
frame.

``` r
data("CBC_lin")
```

Next, we create the validation/holdout task to evaluate it in the next
step. We use a validation/holdout task with 3 alternatives plus the
*no-buy* alternative, just as we did in the previous example. The only
difference to the previous example is that the third attribute
(`Att3_Lin`) was linear-coded.

Again, we first define data, namely the `id` as well as the `none`
alternative. Next, we define the `prod.levels` for each alternative.
Since we have one linear coded attribute, we have to specify the column
indexes instead of the column names in `prod.levels`. For example, for
the second alternative which is composed of `Att1_Lev5`, `Att2_Lev4`,
and `60` which is the value that should be interpolated. We tell
`createHOT()` that this value needs to be interpolated by specifying the
`coding` argument accordingly. This tells us that the first two
attributes (`Att1_Lev5`, `Att2_Lev4` in the case of alternative 2) are
part-worth coded (`0`) while the third attribute is linear-coded (`1`).

To interpolate the value, we have to provide `createHOT()` the
`interpolate.levels`. These **need** to be the same levels as provided
to Sawtooth or `ChoiceModelR`. Moreover, it is important that the value
that should be interpolated needs to lie within the lower and upper
bound of `interpolate.levels`. In our case, we had 7 levels that range
from 10 to 70.

Next, we define the column index of the linear coded variable (`lin.p`).
Again, we are running a *CBC* specified by the `method` argument. This
time, we would like to keep some of the variables in the data frame,
which we specify by using the `varskeep` argument. We only keep one
further variable, however, you can specify as many as you want. This
could be relevant if you would like to display results per group.
Finally, we define the actual choice (`choice`) in the
holdout/validation task and we are all set.

``` r
CBC <- createHOT(
  data = CBC_lin, # data frame
  id = 1,
  none = 15,
  prod.levels = list(c(4, 9, 60), c(8, 12, 40), c(5, 10, 45)),
  coding = c(0, 0, 1),
  interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
  lin.p = 14,
  method = "CBC",
  varskeep = "Group",
  choice = "HOT"
)
```

The next steps are the same as above. However, let us take a look at
some examples in which we display the results per group. Let us again
begin with the `hitrate()` function. To do so, we specify the column
name of the grouping variable in the `group` argument.

``` r
hitrate(
  data = CBC, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice, # column name of choice
  group = Group # column name of Grouping variable
)
#> # A tibble: 3 × 6
#>   Group    HR    se chance   cor     n
#>   <int> <dbl> <dbl>  <dbl> <int> <int>
#> 1     1  59.1 10.7      25    13    22
#> 2     2  37.0  9.47     25    10    27
#> 3     3  56.7  9.20     25    17    30
```

In this case, the group assignment is stored in form of integers.
However, the output is the same if it is a factor or labelled data. To
proof this we just quickly change `group` into a factor by using the
`factor()` function provided by the *R* Core Team (2023).

``` r
CBC$Group <- base::factor(
  CBC$Group,
  levels = c(1:3),
  labels = paste0("Group_", c(1:3))
)
```

Afterward, we display the *mean hit probability* by running the `mhp()`
function.

``` r
mhp(
  data = CBC, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice, # column name of choice
  group = Group # column name of Grouping variable
)
#> # A tibble: 3 × 3
#>   Group     MHP    se
#>   <fct>   <dbl> <dbl>
#> 1 Group_1  41.7  6.11
#> 2 Group_2  39.0  5.17
#> 3 Group_3  47.8  5.00
```

Lastly, this time we also want to use a rescaling function, namely
`att_imp()` which basically tells us the importance of each attribute
included (Orme, 2020). We need the data set with the raw logit
coefficients (`CBC_lin`). Next, we define the `attrib` argument. Here,
we need to specify each attribute level for the corresponding level.
Afterwards, we specify the coding again, and since we have one linear
coded attribute, we need to define the `interpolate.levels` argument
again, as we did for the `createHOT()` function above. Finally, we set
`res` to `agg`, which tells `att_imp()` to display the aggregated
results (for individuals results set `agg` to `ind`).

``` r
att_imp(
  data = CBC_lin,
  attrib = list(
    c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
    c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
    c("Att3_Lin")
  ),
  coding = c(0, 0, 1),
  interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
  res = "agg"
)
#> # A tibble: 3 × 3
#>   Option       mw   std
#>   <chr>     <dbl> <dbl>
#> 1 att_imp_1  36.9  12.4
#> 2 att_imp_2  31.9  13.4
#> 3 att_imp_3  31.3  16.4
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
