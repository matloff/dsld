
# DSLD: Data Science Looks at Discrimination (An R Package)

Authors: 
- Norm Matloff (UC Davis)
- Taha Abdulla
- Aditya Mittal
- Arjun Ashok
- Shubhada Martha
- Billy Ouattara
- Jonathan Tran
- Brandon Zarate

## Overview

Discrimination is a key social issue in the US and in a number of other
countries. There is lots of available data with which one might
investigate possible discrimination. But how might such investigations
be conducted?

Our **dsld** Statistical and graphical tools for tools for detecting and 
measuring discrimination and bias, be it racial, gender, age or other. 
This is an R package. It is widely applicable; here are just a few use cases:

- Quantitative analysis in instruction and research in the social sciences.
- Corporate HR analysis and research.
- Litigation involving discrimination and related issues.
- Concerned citizenry.

## Installation:

The package can be installed using the devtools package:

```R
library(devtools)
install_github("matloff/dsld", force = TRUE)
```
[WAITING TO PUT ON CRAN]

## Analysis categories:

- In the estimation realm, say investigating a possible gender pay gap.
In doing so, we must be careful to account for *confounders*, variables
that may affect wages other than through gender.

- In a prediction context, with concern that an AI algorithm has built-in
bias against some racial group.  We want to eliminate race from the
analysis, and to also limit the effect of *proxies*, other variables
that may be strongly related to race.

In the first case, we are checking for *societal* or *institutional*
bias. In the second, the issue is *algorithmic* bias.

To distinguished between a "fair ML" dataset and a "statistics" one. Here is a side-by-side comparison:

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

## Part One: Adjustment for Confounders 

We wish to *estimate the impact* of a sensitive variable S on an outcome variable Y, but *accounting for confounders* C. Let's call such analysis "confounder adjustment." The package provides graphical and analytical tools for this purpose.

### Example

We are investigating a possible gender pay gap using svcensus data. [Y] is wage and [S] is gender. We will treat age as a confounder [C], using a linear model.

```R
data(svcensus)
svcensus <- svcensus[,c(1,4,6)]  # subset: age, wage, gender
z <- dsldLinear(svcensus,'wageinc','gender')
coef(z)  # print the estimated coefficients b_i 
```
Our linear model would thus be

> mean W = $\beta$~0~ + $\beta$~1~ A + $\beta$~2~ M

where W is wage, A is age and M is an indicator variable, with M = 1 for men and M = 0 for women.

Thus, we can speak of $\beta$~2~ as *the* gender wage gap, at any age. According to the model, younger men earn an estimated $13,000 more than
younger women, with the *same-sized* gap between older men and older
women.

## Discovering/Mitigating Bias in Machine Learning
Our goal is to predict Y from X and O, omitting S. We are concerned that we may be indirectly using S via O and want to limit the usage of proxies.
The inherent tradeoff of increasing fairness is reduced utility (reduced predictive power over the dataset).

### Example

UNDER CONSTRUCTION

## Function List
- DsldLinear/DsldLogit: Comparison of conditions for sensitive groups via linear/logistic models
- DsldML: Comparison of conditions for sensitive groups via ML algorithm
- DsldTakeLookAround: Evaluates feature sets for predicting Y while considering correlation with sensitive variable S
- DsldScatterPlot3D: Plots a dataset on 3 axes, with the color of the point depending on a 4th variable.
- DsldCHunting: Confounder hunting--searches for variables C that predict both Y and S  
- DsldOHunting: Proxy hunting--searches for variables O that predict S.
- DsldConditsDisparity: Plots mean Y against X for each level of S, revealing potential Simpson's Paradox-like differences under specified conditions
- DsldConfounders: Analyzes confounding variables in a dataframe 
- DsldFreqPCoord:  wrapper for the freqparcoord function from the freqparcoord package
- FairML wrappers: wrappers for FairML functions from FairML package
- EDFFair Wrappers: wrappers for EDFFair functions from EDFFair package
