---
title: "MATH 3080 Lab Project 4"
author: "Harrison Webb"
date: "2/3/2022"
output:
  html_document:
    toc: TRUE
    keep_md: TRUE
---


# Problem 1

*The `cats` data set (**MASS**) contains the heart and body weight of a sample
of male and female cats. Use the data set to estimate a 95% prediction interval
for the body weight of a male cat. Assume that the body weight of cats is
Normally distributed.*

#### Solution

First, lets look at a boxplot of cat weights by sex, just to get an idea of the data we are working with.


```r
boxplot(Bwt~Sex, data = cats, col = c("#ce2d54", "#44135c"))
```

<img src="3080Project4_StatisticalIntervals_files/figure-html/unnamed-chunk-2-1.png" width="75%" style="display: block; margin: auto;" />
</br>
Here we see that the weight of male cats is roughly centered around 2.8 or so, and that the values range from roughly 2 to 3.8.
</br>

Now we can set up the prediction interval to predict the weight of a "new" male cat.

```r
pi_norm <- function(x, conf.level = 0.95, alternative = "two.sided") {
  alpha <- 1 - conf.level
  n <- length(x)
  xbar <- mean(x)
  err <- sd(x) * sqrt(1 + 1 / n)
  crit <- switch(alternative,
                 "two.sided" = qt(alpha / 2, df = n - 1, lower.tail = FALSE),
                 "less" = -qt(alpha, df = n - 1, lower.tail = FALSE),
                 "greater" = qt(alpha, df = n - 1, lower.tail = FALSE),
                 # Below is the "default" switch, triggered if none of the above
                 stop("alternative must be one of two.sided, less, greater"))
  interval <- switch(alternative,
                     "two.sided" = c(xbar - crit * err, xbar + crit * err),
                     "less" = c(xbar + crit * err, Inf),
                     "greater" = c(-Inf, xbar + crit * err),
                     stop("How did I get here?"))
  attr(interval, "conf.level") <- conf.level
  interval
}

data = subset(cats, subset = Sex == 'M')$Bwt

pi_norm(data)
```

```
## [1] 1.96728 3.83272
## attr(,"conf.level")
## [1] 0.95
```


# Problem 2

*The data set `SP500` (**MASS**) contains the returns of the S&P 500 stock index
for the 1990s; that is, it's the ratio of the change of the index's price
divided by the preceding day price. In principle, when predicting the direction
of the stock market with the intention of buying stock, we are willing to be
wrong in one direction but not another; we are okay with predicting the market
grows too little and be pleasantly surprised than to predict the market grows
more than it actually does. So compute a 99% lower prediction bound, assuming
that stock returns are Normally distributed. (You should not trust this number.
First the Normality assumption, despite being assumed a lot in finance, is not
true. Second, stock returns are* not *an independent and identically distributed
sample.)*

#### Solution


```r
# Your code here
```

# Problem 3

*The data set `abbey` (**MASS**) contains determinations of nickel content (ppm)
in a Canadian syenite rock. The assumption of a Normal distribution clearly is
inappropriate for this data set. Construct a 90% prediction interval for the
next measurement from the data set. Use a nonparametric procedure.*


```r
# Your code here
```

# Problem 4

*Use the data from Problem 1 to construct a 95% tolerance interval for 99% of
cats' body weight.*


```r
# Your code here
```

# Problem 5

*The data set `geyser` (**MASS**) contains both wait time between and duration
of eruptions of the Old Faithful geyser in Yellowstone National Park. Use the
data set to construct a nonparametric tolerance interval containing 90% of
geyser eruptions with 99% confidence.*


```r
# Your code here
```

# Problem 6

*The data set `accdeaths` (**MASS**) contains a count of accidental deaths in
the United States between 1973 and 1978. What was the mean count of accidental
deaths per month? Use this data set to construct a statistical interval for the
mean number of accidental deaths over the next five years. (Bonus points if you
can compare your interval to the observed mean over those years and assess how
well it did.)*


```r
# Your code here
```
