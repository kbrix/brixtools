
<!-- README.md is generated from README.Rmd. Please edit that file -->
brixtools
=========

The package and site are still under construction...

The goal of brixtools is to make it easier to model non-life insurance data with deductibles using maximum likelihood methods.

Installation
------------

You can currently install brixtools from github with:

``` r
# install.packages("devtools")
devtools::install_github("kbrix/brixtools")
```

Example
-------

Included in the package is a simulated data set of 10000 policies. See the documentation for `normal_poisson_policy`. Below is one of the policies with two losses, but only one (reported) claim:

``` r
library(brixtools)
occurrence <- lapply(normal_poisson_policy, function(x) unlist(x["occurrence"]))
reported <- lapply(normal_poisson_policy, function(x) unlist(x["reported"]))
set <- intersect(which(occurrence == 2), which(reported == 1))[1]
normal_poisson_policy[[set]]
#> $deductible
#> [1] 0.7735717
#> 
#> $occurrence
#> [1] 2
#> 
#> $loss
#> [1] 0.1178939 1.9849283
#> 
#> $claim
#> [1] 1.984928
#> 
#> $reported
#> [1] 1
```

Since the losses (and therefore also claims) within each policy are assumed i.i.d., we can simplify the indexing of the data to ease numerical procedures, see `normal_poisson_x` and `normal_poisson_n`.

The Normal-Poisson log-likelihood is called using `normal_poisson_log_likelihood` and can be used with `brix_simple` to find the optimum yielding the maximum likelihood estimates of the parameters:

``` r
model <- brix_simple(optimizer = optim,
                     fn = normal_poisson_log_likelihood,
                     X = normal_poisson_x, N = normal_poisson_n,
                     par = rep(1, 3),
                     control = list(fnscale = -1),
                     method = "Nelder-Mead",
                     hessian = TRUE)
summary(model)
#> Call used with brix_simple:
#> optim(fn = normal_poisson_log_likelihood, X = normal_poisson_x, 
#>     N = normal_poisson_n, par = rep(1, 3), control = list(fnscale = -1), 
#>     method = "Nelder-Mead", hessian = TRUE)
#> 
#> Coefficients:
#>         Estimate Std. Error z value  Pr(>|z|)    
#> mean   2.0719865  0.0774775  26.743 < 2.2e-16 ***
#> sd     1.4162340  0.0590935  23.966 < 2.2e-16 ***
#> lambda 0.1347086  0.0054767  24.597 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood value: -4188.422  AIC: 8382.845
```

The true parameter values are 1.5 for the mean and standard deviation for the normal distribution and the mean for the non-thinned Poisson is 0.1. The possiblities for extensions are very large.
