
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

To expand on what I said today about the suitability of datasets for our Term Project, I distinguished between a "fair ML" dataset and a "statistics" one. Here is a side-by-side comparison:

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

Under Construction

## Discovering/Mitigating Bias in Machine Learning
Under Construction

## Function List
- DsldLinear/DsldLogit/DsldML:
- DsldTakeLookAround: 
- DsldScatterPlot3D: 
- DsldCHunting:
- DsldOHunting:
- DsldConditsDisparity:
- DsldConfounders:
- DsldFreqPCoord:
- FairML wrappers:
- EDFFair Wrappers:
