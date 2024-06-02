
# DSLD: Data Science Looks at Discrimination (An R Package)

Authors: 
- Norm Matloff
- Aditya Mittal
- Taha Abdullah
- Arjun Ashok
- Shubhada Martha
- Billy Ouattara
- Jonathan Tran
- Brandon Zarate

## Overview

Discrimination is a key social issue in the United States and in a number of other
countries. There is lots of available data with which one might
investigate possible discrimination. But how might such investigations
be conducted?

Our **DSLD** package provides statistical and graphical tools for detecting and 
measuring discrimination and bias; be it racial, gender, age or other. 
This is an R package. It is widely applicable, here are just a few possible use cases:

- Quantitative analysis in instruction and research in the social sciences.
- Corporate HR analysis and research.
- Litigation involving discrimination and related issues.
- Concerned citizenry.

This package is broadly aimed at users ranging from instructors of statistics classes to legal professionals, 
as it offers a powerful yet intuitive approach to discrimination analysis. It also includes an 80 page **Quarto book** 
to serve as a guide of the key statistical principles and their applications.

## Installation:

As of now, the package may be installed using the **devtools** package:

```R
library(devtools)
install_github("matloff/dsld", force = TRUE)
```

[WAITING TO PUT ON CRAN]

## Analysis categories:

Here are the main categories:

- In the estimation realm, say investigating a possible gender pay gap.
In doing so, we must be careful to account for *confounders*, variables
that may affect wages other than through gender.

- In a prediction context, with concern that an ML algorithm has built-in
bias against some racial group.  We want to eliminate race from the
analysis, while also controlling the effect of *proxies* – other variables
that may be strongly related to race.

In the first case, we are checking for *societal* or *institutional*
bias. In the second, the issue is *algorithmic* bias.

To distinguish between a "fair ML" and a "statistics" dataset. Here is a side-by-side comparison:

<table border="1">

   <tr>
   <th>statistics</th>
   <th>fair ML</th>
   </tr>

   <tr>
   <td>estimate an effect</td>
   <td>predict an outcome</td>
   </tr>

   <tr>
   <td>harm comes from society</td>
   <td>harm comes from an algorithm</td>
   </tr>

   <tr>
   <td>include sensitive variables</td>
   <td>exclude sensitive variables</td>
   </tr>

   <tr>
   <td>adjust for covariates</td>
   <td>use proxies but limit their impact</td>
   </tr>

</table>

Here we will take a quick tour of a subset of dsld's features, using the **svcensus** data included in the package.

### The data

The **svcensus** dataset consists of recorded income across 6 different engineering occupations. It consists of the columns 'age',
'education level', 'occupation','wage income', 'number of weeks worked', 'gender'.

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

We will use only a few features, to keep things simple. Note that the *Quarto Book* provides an extensive 
analysis of examples shown below.

## Part One: Adjustment for Confounders 

We wish to *estimate the impact* of a sensitive variable [S] on an outcome variable [Y], while *accounting for confounders* [C]. 
Let's call such analysis "confounder adjustment." The package provides several graphical and analytical tools for this purpose.

### Estimation Example

We are investigating a possible gender pay gap between men and women. Here, [Y] is wage and [S] is gender. 
We will treat age as a confounder [C], using a linear model. For simplicity, no other confounders (such as occupation) or any other predictors [X]
are included in this example. 

```R
> data(svcensus)
> svcensus <- svcensus[,c(1,4,6)]  # subset columns: age, wage, gender
> z <- dsldLinear(svcensus,'wageinc','gender')
> coef(z) # show coefficients of linear model

$gender
(Intercept)         age  gendermale 
 31079.9174    489.5728  13098.2091 
```
Our linear model can be written as: 

> E(W) = $\beta_0$ + $\beta_1$ A + $\beta_2$ M

Consider the case without any interaction: Here *W* indicates wage income, *A* is age and *M* denotes an indicator variable, with M = 1 for men and M = 0 for women.

We can speak of $\beta_2$ as *the* gender wage gap, at any age. According to the model, younger men earn an estimated $13,000 more than
younger women, with the *same-sized* gap between older men and older women. Furthermore, it may be, for instance that the gender gap is small at younger ages but much larger for older people. Thus, we can account for an interaction by fitting two linear models, one for men and one for women. Suppose we are comparing this difference between ages 36 and 43:

```R
newData <- data.frame(age=c(36,43))
z <- dsldLinear(svcensus,'wageinc','gender',interactions=T,
                newData)
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

The gender pay gap is estimated to be -13156.88 at age 36, and -13039.27 at age 43, differing by only about $100. The estimated gap between ages
36 and 53, not shown, is larger, close to $300, but it seems there is not much interaction here. Note that we chose only one [C] variable here, age, for our analysis. We might also choose "occupation", or any other combination depending on the application and dataset which can affect our results. The package also provides several graphical and analytical tools to aid users further. 

## Part Two: Discovering/Mitigating Bias in Machine Learning

Our goal is to predict [Y] from [X] and [O], omitting the sensitive variable [S]. Though, we are concerned that we may be indirectly using [S] via proxies [O] 
and want to limit their usage. The inherent tradeoff of increasing fairness is reduced utility (reduced predictive power/accuracy). 
The package provides wrappers for several functions for this purpose.

### Prediction Example

We are predicting the wage [Y], the sensitive variable [S] is gender, with the proxy [O] as occupation. The proxy [O] "occupation" will be deweighted
to 0.2 using the *dsldQeFairKNN()* function to limit its predictive power.

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

In the base K-NN model, the correlation between predicted wage and gender was 0.1943, with mean prediction error of $25,500. Using *dsldQQeFairKNN*, we see that the correlation between predicted wage and gender has decreased significantly. On the other hand, test accuracy increased by about \$700 dollars. Hence, we see an increase in fairness at some expense of accuracy. 

## Function List

- **DsldLinear**/**DsldLogit**: Comparison of conditions for sensitive groups via linear & logistic models
  
- **DsldML**: Comparison of conditions for sensitive groups via ML algorithms
  
- **DsldTakeLookAround**: Evaluates feature sets for predicting Y while considering correlation with sensitive variable S
  
- **DsldCHunting**: Confounder hunting--searches for variables C that predict both Y and S

- **DsldOHunting**: Proxy hunting--searches for variables O that predict S

- **DsldScatterPlot3D**: Plots a dataset on 3 axes, with the color of the point depending on a 4th variable
  
- **DsldConditsDisparity**: Plots mean Y against X for each level of S, revealing potential Simpson's Paradox-like differences under specified conditions
  
- **DsldConfounders**: Analyzes confounding variables in a dataframe
  
- **DsldFreqPCoord**:  Wrapper for the freqparcoord function from the freqparcoord package

- **DsldDensityByS**: Graphs densities of a response variable, grouped by a sensitive variable

- **DsldFrequencyByS**: Assess possible confounding relationship between a sensitive variable and a categorical variable via graphical means
  
- **DsldFairML**: Wrappers for several fair machine learning algorithms functions provided via the FairML package
  
- **DsldEDFFair**: Wrappers for several EDFFair functions provided via the EDFFair package
