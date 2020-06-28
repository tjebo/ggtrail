
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtrail

<!-- badges: start -->

<!-- badges: end -->

### geom\_trail

A base plot type = “b” equivalent for ggplot. Works also with text\!
\#\# Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjebo/ggtrail")
```

## Example

## geom\_trail

A base plot type = “b” equivalent for ggplot. Works also with text\!

This readme is not meant to dive into detail how to use `ggplot2`, an
amazing package which allows for very creative data visualization in a
fairly user-friendly manner. A good place to start learning about
ggplot2 is <http://www.cookbook-r.com/Graphs/>

The following it an example of how to use `geom_trail` in the context of
the inbuilt data set `amd`.

First, the data needs to be prepared. I often find that especially
rather inexperienced users hesitate to manipulate / shape their data
before doing any visualization or analysis.

However, there is really no need to fret. It is an often very important
and necessary step. Actually, it can make your life much easier. And if
you follow simple precautions, you can not cause much damage to your
actual raw data.

(**Those simple precautions are**: Do not use functions in your script
or report which write a file on your disk. This may easily overwrite
your raw data\! E.g. `write.csv` and friends should be used only with
utmost care.)

Any wrong assignments where you may need to rerun your scripts can be
annoying, but are in itself not detrimental (provided your scripts are
sound).

On this note, I recommend to always start in a fresh session and to make
sure that your script does not rely on objects that have not been
correctly created. This is another advantage of using rmarkdown:
Knitting your script to a report makes you aware of inconsistencies in
your script.

<details>

<summary>Prepare AMD data for plot (click to unfold) </summary>

``` r
library(tidyverse)
library(eye)

amd_aggr <-
  amd %>%
  group_by(
    age_cut10 = cut_width(BaselineAge, 10),
    days_cut90 = cut_width(FollowupDays, 90, labels = seq(0, 810, 90))
  ) %>%
  summarise(mean_va = mean(VA_ETDRS_Letters)) %>%
  filter(as.integer(days_cut90) <= 9)
```

</details>

``` r
p <-
  ggplot(amd_aggr, aes(days_cut90, mean_va, color = age_cut10)) + 
  scale_color_brewer(palette = "Set1") +
  theme_classic() +
  labs(
    x = "Follow up time [binned by 90 days]", y = "Mean VA [ETDRS letters]",
    color = "Age strata"
  )
```

    p + geom_trail(aes(group = age_cut10))
    
    p + geom_trail(aes(group = age_cut10), size = 0) +
        geom_text(aes(label = round(mean_va, 0)), show.legend = FALSE)

<img src="man/figures/README-unnamed-chunk-2-1.png" width="45%" /><img src="man/figures/README-unnamed-chunk-2-2.png" width="45%" />

## An extended brewer scale for binned scales

``` r
library(ggplot2)
ggplot(mtcars, aes(mpg, disp, fill = hp)) +
  geom_point(shape = 21) +
  labs(title = "Craft brewer - an extended brewer scale") +
  scale_fill_craftfermenter(
    breaks = seq(0,520,40),
    limits = c(0,520),
    palette = "Spectral",
    guide =  guide_colorsteps(even.steps = FALSE, # workaround for issues #4019/#4100
                             barheight = 15) 
  )
#> Warning: 13 colours used, but Spectral has only 11 - New palette generated based
#> on all colors of Spectral
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
