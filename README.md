
# DSLD: Data Science Looks at Discrimination (An R Package)

Authors: Norm Matloff (UC Davis), Taha Abdulla, Aditya Mittal, Arjun Ashok

## Overview

Statistical and graphical tools for detecting and measuring
discrimination and bias, be it racial, gender, age or other. 
This is an R package, with Python interfaces available.

## Analysis categories:

* In the estimation realm, say investigating a possible gender pay gap.
In doing so, we must be careful to account for *covariates*, variables
that may affect wages other than through gender.

* In a prediction context, with concern that an AI algorithm has built-in
bias against some racial group.  We want to eliminate race from the
analysis, and to also limit the effect of *proxies*, other variables
that may be strongly related to race.

In the first case, we are checking for *societal* or *institutional*
bias.  In the second, the issue is *algorithmic* bias.

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

## Adjustment for Confounders 

We wish to *estimate the impact* of a sensitive variable S on an outcome variable Y, but *accounting for confounders* C. Let's call such analysis "confounder adjustment."

### Example

Investigating a possible gender pay gap using svcensus data. [Y] is wage and [S] is gender. We will treat age as a confounder [C] using a linear model.

```R
library(dsld)
data(svcensus)
z <- dsldLinear(svcensus,'wageinc','gender')
coef(z)
```

## Discovering/Mitigating Bias in Machine Learning
Our goal is to predict Y from X and O, omitting S. We are concerned that we may be indirectly using S via O and want to limit the usage of proxies.
The inherent tradeoff of increasing fairness is reduced utility (reduced predictive power over the dataset).

### Example

- Concerning racial differences: Two very similar people (same quality law school, undergraduate/law school grades, bar passage
status) will have LSAT scores differing on average Y by almost 6 points if one person is Black and
the other is white.
Exploratory Data Analysis
Fig. 2:Distribution of LSAT Scores by Race
• Distribution of LSAT scores for white students
appears to be higher than others, particularly than the black students
Fig. 3:Distribution of Family Income by Race
• White students tend to fall under higher family
income group as opposed to other races
• The inherent tradeoff of increasing fairness is reduced utility (reduced predictive power over the dataset)

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
