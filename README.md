
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtrail

<!-- badges: start -->
<!-- badges: end -->

## Features

-   **geom\_colorpath**: Lines with alternating colors.
-   **scale\_aes\_craftfermenter**: Extended brewer scales for binned
    scales (for color and fill aesthetic).
-   **long guide ticks** for color scales (doesn’t really work yet)
-   \*\*convenience scale calls that modify both x and y scales at the
    same time

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjebo/ggtrail")
```

### Examples

#### geom\_colorpath

``` r
air_df <- data.frame(x = 1: length(AirPassengers), y = c(AirPassengers))

ggplot(air_df, aes(x, y)) +
  geom_colorpath(cols = c("red", "blue", "green"))
```

<img src="man/figures/README-geom_colorpath-1.png" width="100%" />

``` r
dat <- data.frame(x = seq(2,10, 2), y = seq(4,20, 4))

p1 <- ggplot(dat, aes(x = x, y = y)) +
  geom_colorpath()

p2 <- ggplot(dat, aes(x, y)) +
  geom_colorpath(cols = c("red", "blue"))

p3 <- ggplot(dat, aes(x, y)) +
  geom_colorpath(cols = c("red", "blue", "green"))

p4 <- ggplot(dat, aes(x, y)) +
  geom_colorpath(cols = c("red", "blue", "green", "white"))

patchwork::wrap_plots(mget(ls(pattern = "p[1-9]")))
```

<img src="man/figures/README-geom_colorpath-2.png" width="100%" />

#### An extended brewer scale for binned scales

``` r
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

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

#### Long ticks for color bars

``` r
ggplot(iris, aes(Sepal.Length, y = Sepal.Width, fill = Petal.Length))+
  geom_point(shape = 21) +
  scale_fill_fermenter(breaks = c(1:3,5,7), palette = "Reds") +
  guides(fill = guide_longticks(
    ticks = TRUE,
    even.steps = FALSE,
    frame.colour = "black",
    ticks.colour = "black")) +
  theme(legend.position = "bottom")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

#### Common scale for both x and y in one call

``` r
x <- y <- 0:10
mydat <- data.frame(x, y)

ggplot(mydat, aes(x, y)) +
  geom_point() +
  scale_axis_continuous(expand = c(0, NA), breaks = 2:10) +
  theme_classic()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## About reproducible scripts, and using ggplot2

This readme is not meant to dive into detail how to use `ggplot2`, an
amazing package which allows for very creative data visualization in a
fairly user-friendly manner. A good place to start learning about
ggplot2 is <http://www.cookbook-r.com/Graphs/>

First, the data needs to be prepared. I often find that especially
rather inexperienced users hesitate to manipulate / shape their data
before doing any visualization or analysis.

However, there is really no need to fret. It is an often very important
and necessary step. Actually, it can make your life much easier. And if
you follow simple precautions, you can not cause much damage to your
actual raw data.

(**Those simple precautions are**: Do not use functions in your script
or report which write a file on your disk. This may easily overwrite
your raw data! E.g. `write.csv` and friends should be used only with
utmost care.)

Any wrong assignments where you may need to rerun your scripts can be
annoying, but are in itself not detrimental (provided your scripts are
sound).

On this note, I recommend to always start in a fresh session and to make
sure that your script does not rely on objects that have not been
correctly created. This is another advantage of using rmarkdown:
Knitting your script to a report makes you aware of inconsistencies in
your script.
