Faktoren mit factors
================

## Faktoren mit factors

``` r
install.packages("tidyverse")
```

    ## Installing package into '/home/skybreakjohnson/R/x86_64-pc-linux-gnu-library/4.0'
    ## (as 'lib' is unspecified)

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

-   kreiren von Faktoren

-   nominale variablen sind Faktoren

``` r
string = c("Dez", "Feb", "Jan", "Apr")
sort(string)
```

    ## [1] "Apr" "Dez" "Feb" "Jan"

``` r
monate_level = c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")

# factor
faktor = factor(string, levels = monate_level)
sort(faktor)
```

    ## [1] Jan Feb Apr Dez
    ## Levels: Jan Feb Mär Apr Mai Jun Jul Aug Sep Okt Nov Dez

``` r
# Social Survey
gss_cat %>%
  count(race)
```

    ## # A tibble: 3 × 2
    ##   race      n
    ##   <fct> <int>
    ## 1 Other  1959
    ## 2 Black  3129
    ## 3 White 16395

``` r
ggplot(gss_cat, aes(race)) +
  geom_bar()
```

![](faktoren_mit_factors_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

-   Reihenfolge von Faktoren

``` r
relig = gss_cat %>%
  group_by(relig) %>%
  summarize(avg_age = mean(age, na.rm = T),
            avg_tvhours = mean(tvhours, na.rm = T),
            n = n())
relig
```

    ## # A tibble: 15 × 4
    ##    relig                   avg_age avg_tvhours     n
    ##    <fct>                     <dbl>       <dbl> <int>
    ##  1 No answer                  49.5        2.72    93
    ##  2 Don't know                 35.9        4.62    15
    ##  3 Inter-nondenominational    40.0        2.87   109
    ##  4 Native american            38.9        3.46    23
    ##  5 Christian                  40.1        2.79   689
    ##  6 Orthodox-christian         50.4        2.42    95
    ##  7 Moslem/islam               37.6        2.44   104
    ##  8 Other eastern              45.9        1.67    32
    ##  9 Hinduism                   37.7        1.89    71
    ## 10 Buddhism                   44.7        2.38   147
    ## 11 Other                      41.0        2.73   224
    ## 12 None                       41.2        2.71  3523
    ## 13 Jewish                     52.4        2.52   388
    ## 14 Catholic                   46.9        2.96  5124
    ## 15 Protestant                 49.9        3.15 10846

``` r
ggplot(relig, aes(avg_tvhours, relig)) +
  geom_point()
```

![](faktoren_mit_factors_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(relig, aes(avg_tvhours, fct_reorder(relig, avg_tvhours))) +
  geom_point()
```

![](faktoren_mit_factors_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

-   Neu kodieren von Faktoren

``` r
gss_cat %>%
  count(partyid)
```

    ## # A tibble: 10 × 2
    ##    partyid                n
    ##    <fct>              <int>
    ##  1 No answer            154
    ##  2 Don't know             1
    ##  3 Other party          393
    ##  4 Strong republican   2314
    ##  5 Not str republican  3032
    ##  6 Ind,near rep        1791
    ##  7 Independent         4119
    ##  8 Ind,near dem        2499
    ##  9 Not str democrat    3690
    ## 10 Strong democrat     3490

``` r
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republikaner, stark" = "Strong republican",
                              "Republikaner, schwach" = "Not str republican",
                              "Unhabhängig, nahe Rep." = "Ind,near rep",
                              "Unhabhängig, nahe Dem." = "Ind,near dem",
                              "Demokrat, schwach" = "Not strong democrat",
                              "Demokrat, stark" = "Strong democrat")) %>%
  count(partyid)
```

    ## Warning: Unknown levels in `f`: Not strong democrat

    ## # A tibble: 10 × 2
    ##    partyid                    n
    ##    <fct>                  <int>
    ##  1 No answer                154
    ##  2 Don't know                 1
    ##  3 Other party              393
    ##  4 Republikaner, stark     2314
    ##  5 Republikaner, schwach   3032
    ##  6 Unhabhängig, nahe Rep.  1791
    ##  7 Independent             4119
    ##  8 Unhabhängig, nahe Dem.  2499
    ##  9 Not str democrat        3690
    ## 10 Demokrat, stark         3490

-   Zusammenfassen von Faktoren

``` r
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat"))) %>%
  count(partyid)
```

    ## # A tibble: 4 × 2
    ##   partyid     n
    ##   <fct>   <int>
    ## 1 other     548
    ## 2 rep      5346
    ## 3 ind      8409
    ## 4 dem      7180

-   Zusammenballen von Faktoren

``` r
gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>% # ballt alle 15 Religionen zu 10 zusammen, fügt die Religionen mit den wenigsten Anhängern zu "Other" hinzu
  count(relig, sort = TRUE)
```

    ## # A tibble: 10 × 2
    ##    relig                       n
    ##    <fct>                   <int>
    ##  1 Protestant              10846
    ##  2 Catholic                 5124
    ##  3 None                     3523
    ##  4 Christian                 689
    ##  5 Other                     458
    ##  6 Jewish                    388
    ##  7 Buddhism                  147
    ##  8 Inter-nondenominational   109
    ##  9 Moslem/islam              104
    ## 10 Orthodox-christian         95

``` r
gss_cat %>%
  count(relig, sort = TRUE) 
```

    ## # A tibble: 15 × 2
    ##    relig                       n
    ##    <fct>                   <int>
    ##  1 Protestant              10846
    ##  2 Catholic                 5124
    ##  3 None                     3523
    ##  4 Christian                 689
    ##  5 Jewish                    388
    ##  6 Other                     224
    ##  7 Buddhism                  147
    ##  8 Inter-nondenominational   109
    ##  9 Moslem/islam              104
    ## 10 Orthodox-christian         95
    ## 11 No answer                  93
    ## 12 Hinduism                   71
    ## 13 Other eastern              32
    ## 14 Native american            23
    ## 15 Don't know                 15
