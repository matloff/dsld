
# DSLD: Data Science Looks at Discrimination 

[![CRAN Status](https://www.r-pkg.org/badges/version/dsld)](https://cran.r-project.org/package=dsld)

> Statistical and graphical tools for detecting and measuring discrimination and bias in data

This is an R package with Python interfaces available.

## Overview

Discrimination is a key social issue in the United States and in a 
number of other countries. There is lots of available data with which 
one might investigate possible discrimination. But how might such 
investigations be conducted?

Our **DSLD** package provides statistical and graphical tools for
detecting and measuring discrimination and bias; be it racial, gender, 
age or other. It is widely applicable, here are just a few possible use 
cases:

- Quantitative analysis in instruction and research in the social sciences.
- Corporate HR analysis and research.
- Litigation involving discrimination and related issues.
- Concerned citizenry.

This package is broadly aimed at users ranging from instructors of 
statistics classes to legal professionals, as it offers a powerful yet 
intuitive approach to discrimination analysis. It also includes an 80 page 
**Quarto book** to serve as a guide of the key statistical principles and 
their applications.

- **Quarto Book**: [Paper](https://htmlpreview.github.io/?https://github.com/matloff/dsldBook/blob/main/_book/index.html) - Important statistical principles and applications.
- **Research Paper**: [Paper](https://arxiv.org/abs/2411.04228) - Package implementation details.

## Installation:

From CRAN :

```r
install.packages("dsld")
```

<!-- From GitHub (latest Version):

```r
library(devtools)
install_github("matloff/dsld", force = TRUE)
``` -->

## Analysis categories:

DSLD addresses two main types of bias analysis:

**Estimation Analysis**: Investigates possible discrimination by 
estimating effects while accounting for confounders. Confounders are
variables that may affect the outcome variable other than through 
the sensitive variable. DSLD provides both analytical and graphical functions 
for this purpose.

**Prediction Analysis**: Addresses algorithmic bias in machine learning 
by excluding sensitive variables while controlling proxy effects. 
Proxies are variables strongly related to the sensitive variable that 
could indirectly introduce bias.

The first case examines *societal* or *institutional bias*. The second case 
focuses on *algorithmic bias*.

<table border="1">
   <tr>
   <th>Statistical Analysis</th>
   <th>Fair Machine Learning</th>
   </tr>
   <tr>
   <td>Estimate an effect</td>
   <td>Predict an outcome</td>
   </tr>
   <tr>
   <td>Harm comes from society</td>
   <td>Harm comes from algorithm</td>
   </tr>
   <tr>
   <td>Include sensitive variables</td>
   <td>Exclude sensitive variables</td>
   </tr>
   <tr>
   <td>Adjust for covariates</td>
   <td>Limit proxy impact</td>
   </tr>
</table>

We will tour of a small subset of dsld's features using the **svcensus** data 
included in the package.

### The data

The **svcensus** dataset consists of recorded income across 6 different 
engineering occupations. It consists of the features: 'age', 'education level', 
'occupation', 'wage income', 'number of weeks worked', 'gender'.

```R
> data(svcensus)
> head(svcensus)
       age     educ occ wageinc wkswrkd gender
1 50.30082 zzzOther 102   75000      52 female
2 41.10139 zzzOther 101   12300      20   male
3 24.67374 zzzOther 102   15400      52 female
4 50.19951 zzzOther 100       0      52   male
5 51.18112 zzzOther 100     160       1 female
6 57.70413 zzzOther 100       0       0   male
```

We will use only a few features to keep things simple. The *Quarto Book* 
provides a more extensive analysis of examples shown below.

## Part One: Adjustment for Confounders 

We want to estimate the impact of a sensitive variable [S] 
on an outcome variable [Y], while accounting for confounders [C]. Let's 
call such analysis "confounder adjustment."

### Estimation Example

We are investigating a possible gender pay gap between men and women. 
Here, [Y] is wage and [S] is gender. We will treat age as a confounder [C], 
using a linear model. For simplicity, no other confounders (such as occupation) 
or any other predictors [X] are included in this example. 

**No interactions**

```r
> data(svcensus)
> svcensus <- svcensus[,c(1,4,6)]      # subset columns: age, wage, gender
> z <- dsldLinear(svcensus,'wageinc','gender', interactions = FALSE)
> summary(z)                              # show coefficients of linear model

$`Summary Coefficients`
    Covariate   Estimate StandardError PValue
1 (Intercept) 31079.9174    1378.08158      0
2         age   489.5728      30.26461      0
3  gendermale 13098.2091     790.44515      0

$`Sensitive Factor Level Comparisons`
         Factors Compared Estimates Standard Errors P-Value
Estimate    male - female  13098.21        790.4451       0
```
Our linear model can be written as: 

> E(W) = $\beta_0$ + $\beta_1$ A + $\beta_2$ M

Consider the case without any interaction: Here *W* indicates wage 
income, *A* is age and *M* denotes an indicator variable, with M = 1 for men and 
M = 0 for women.

Where W is wage income, A is age, and M is male indicator (M = 1 for men, M = 0 for women).

$\beta_2$ represents the gender wage gap at any age. The linear model shows men earn 
$13,000 more than women across *all* ages. However, the wage gap might also vary by age. 
We test for such interactions by fitting separate models for men and women, for example comparing ages 36 and 43:

**Interactions**
```R
newData <- data.frame(age=c(36,43))
z <- dsldLinear(svcensus,'wageinc','gender',interactions=TRUE, newData)
summary(z)

$female
    Covariate   Estimate StandardError PValue
1 (Intercept) 30551.4302    2123.44361      0
2         age   502.9624      52.07742      0

$male
    Covariate  Estimate StandardError PValue
1 (Intercept) 44313.159    1484.82216      0
2         age   486.161      36.02116      0

$`Sensitive Factor Level Comparisons`
  Factors Compared New Data Row Estimates Standard Errors
1    female - male            1 -13156.88        710.9696
2    female - male            2 -13039.27        710.7782
```

The gender pay gap is -$13,157 at age 36 and -$13,039 at age 43, differing by only $118. 
This suggests minimal age-gender interaction. We only focused on age as the confounder, 
but other variables like occupation could be included depending on the analysis goals.

## Part Two: Discovering/Mitigating Bias in Machine Learning

We are predicting [Y] from a feature set [X] and a sensitive variable [S]. 
We want to minimize the effect of [S], along with any proxies [O] in [X] that may 
be correlated with [S]. The inherent trade-off of increasing fairness (minimizing [S] and [O]) 
is reduced utility. The package provides wrappers for several functions. 

### Prediction Example

**Goal**: Predict wage income while minimizing gender bias by limiting the 
impact of occupation as a proxy variable. 

**Setup**: 
- **Outcome [Y]**: Wage income
- **Sensitive Variable [S]**: Gender  
- **Proxy Variable [O]**: Occupation (deweighted to 0.2)
- **Method**: Fair K-Nearest Neighbors using `dsldQeFairKNN()`

<table border="1">

   <tr>
   <th>Fairness/Utility Tradeoff</th>
   <th>Fairness</th>
   <th>Accuracy</th>
   </tr>

   <tr>
   <td>K-Nearest Neighbors</td>
   <td>0.1943313</td>
   <td>25452.08</td>
   </tr>

   <tr>
   <td>Fair K-NN (via EDFFair)</td>
   <td>0.0814919</td>
   <td>26291.38</td>
   </tr>
</table>

The base K-NN model shows 0.194 correlation between predicted wage and gender, with $25,452 prediction error. Using `dsldQeFairKNN`, the correlation drops to 0.081, but test error increases by $839. This shows the fairness-utility trade-off. Users can test parameter combinations to find their optimal balance. The `dsldFairUtils` function facilitates this search.

## Function List

1. Graphical Functions

<table border="1">
   <tr>
   <th>Function</th>
   <th>Description</th>
   <th>Use Case</th>
   </tr>
   <tr>
   <td><code>dsldFreqPCoord</code></td>
   <td>Frequency-based parallel coordinates</td>
   <td>Visualizing multivariate relationships</td>
   </tr>
   <tr>
   <td><code>dsldScatterPlot3D</code></td>
   <td>3D scatter plots with color coding</td>
   <td>Exploring 3D data relationships</td>
   </tr>
   <tr>
   <td><code>dsldConditDisparity</code></td>
   <td>Conditional disparity plots</td>
   <td>Detecting Simpson's Paradox</td>
   </tr>
   <tr>
   <td><code>dsldDensityByS</code></td>
   <td>Density plots by sensitive variable</td>
   <td>Comparing distributions across groups</td>
   </tr>
   <tr>
   <td><code>dsldConfounders</code></td>
   <td>Confounder analysis</td>
   <td>Identifying confounding variables</td>
   </tr>
   <tr>
   <td><code>dsldIamb</code></td>
   <td>Constraint-based structure learning algorithms</td>
   <td>Fits a causal model to data</td>
   </tr>
</table>

2. Analytical Functions

<table border="1">
   <tr>
   <th>Function</th>
   <th>Description</th>
   <th>Use Case</th>
   </tr>
   <tr>
   <td><code>dsldLinear</code></td>
   <td>Linear regression with sensitive group comparisons</td>
   <td>Regression outcome analysis</td>
   </tr>
   <tr>
   <td><code>dsldLogit</code></td>
   <td>Logistic regression with sensitive group comparisons</td>
   <td>Binary outcome analysis</td>
   </tr>
   <tr>
   <td><code>dsldML</code></td>
   <td>Machine learning with sensitive group comparisons</td>
   <td>Analysis via non-parametric models (KNN, RF)</td>
   </tr>
   <tr>
   <td><code>dsldTakeALookAround</code></td>
   <td>Feature set evaluation</td>
   <td>Assessing prediction fairness</td>
   </tr>
   <tr>
   <td><code>dsldCHunting</code></td>
   <td>Confounder hunting</td>
   <td>Finding variables that predict both Y and S</td>
   </tr>
   <tr>
   <td><code>dsldOHunting</code></td>
   <td>Proxy hunting</td>
   <td>Finding variables that predict S</td>
   </tr>
   <tr>
   <td><code>dsldMatchedAte</code></td>
   <td>Causal inference via matching</td>
   <td>Estimating treatment effects</td>
   </tr>
</table>

3. Fair Machine Learning Functions

<table border="1">
   <tr>
   <th>Function</th>
   <th>Description</th>
   <th>Package</th>
   </tr>
   <tr>
   <td><code>dsldFairML</code></td>
   <td>FairML algorithm wrappers</td>
   <td>FairML</td>
   </tr>
   <tr>
   <td><code>dsldQeFairML</code></td>
   <td>EDFFair algorithm wrappers</td>
   <td>EDFFair</td>
   </tr>
   <tr>
   <td><code>dsldFairUtils</code></td>
   <td>Grid search and parameter optimization for fair ML</td>
   <td>  </td>
   </tr>
</table>

**Available Algorithms**:
- **FairML**: <code>dsldFrrm</code>, <code>dsldFgrrm</code>, <code>dsldZlm</code>, <code>dsldNclm</code>, <code>dsldZlrm</code>
- **EDFFair**: <code>dsldQeFairKNN</code>, <code>dsldQeFairRf</code>, <code>dsldQeFairRidgeLin</code>, <code>dsldQeFairRidgeLog</code>

## Authors

- **Norm Matloff** 
- **Aditya Mittal** 
- **Taha Abdullah** 
- **Arjun Ashok** 
- **Shubhada Martha** 
- **Billy Ouattara**
- **Jonathan Tran** 
- **Brandon Zarate** 

For issues, contact **Aditya Mittal** at mittalaa@uci.edu