interactionR
============

[![Build
Status](https://travis-ci.com/epi-zen/interactionR.svg?branch=master)](https://travis-ci.com/epi-zen/interactionR)

Produces a publication-ready table that includes all effect estimates
necessary for full reporting effect modification and interaction
analysis. It also estimates confidence interval for the trio of additive
interaction measures using the delta method, variance recovery method,
or bootstrapping.

Installation
------------

interactionR is on
[CRAN](https://cran.r-project.org/package=interactionR), install using
`install.packages("interactionR")`.

You can also install the development version from
[GitHub](https://github.com/epi-zen/interactionR) with:

``` r
# install.packages("devtools")
devtools::install_github("epi-zen/interactionR")
```

Example
-------

This is an example showing the main functions of the package:

``` r
library(interactionR)
data (OCdata) ## Case-control data from Rothman and Keller (1972) evaluating the joint effect of alcohol and smoking on oral cancer risk is included in the package (cited in Hosmer and Lemeshow (1992) and Zou (2008))

## fit the interaction model
model.glm <- glm(oc ~ alc*smk, family = binomial(link = "logit"), data = OCdata)

## Then pass the fitted model to the function which generates a list object of class 'interactionR'
value = interactionR(model.glm, exposure_names = c("alc", "smk"), ci.type = "delta", ci.level = 0.95, em = F, recode = F)
```

If you want, you could use the variance recovery method (Zou (2008)) to
estimate the CI for additive interaction measures by specifying “mover”
for the ‘ci.type’ argument

``` r
value = interactionR(model.glm, exposure_names = c("alc", "smk"), ci.type = "mover", ci.level = 0.95, em = T, recode = F)
```

To generate a publication-ready table, we’ll call the tabling function

``` r
interactionR_table(value)
 
#>                      *        smk absent        smk present
#> 1                 <NA>              <NA>               <NA>
#> 2                 <NA>       OR [95% CI]        OR [95% CI]
#> 3           alc absent     1 [Reference] 2.96 [0.68, 12.91]
#> 4          alc present 3.33 [0.7, 15.86] 9.04 [2.64, 30.91]
#> 5 Multiplicative scale 0.91 [0.15, 5.42]               <NA>
#>   Effect of smk within the strata of alc
#> 1                                   <NA>
#> 2                            OR [95% CI]
#> 3                     2.96 [0.68, 12.91]
#> 4                         2.71 [1, 7.37]
#> 5                                   <NA>
```
