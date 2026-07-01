
# HLCtools

`HLCtools` calculates Herd Lying Concordance (HLC) metrics from
individual animal lying-behaviour data.

HLC is a framework for quantifying group-level behavioural cohesion. It
uses the distribution of individual lying behaviour within each
observation interval to estimate how similarly animals are behaving as a
group.

## Concept

Traditional threshold-based synchrony metrics classify an interval as
synchronous when a fixed proportion of animals are lying, for example
70%. HLC takes a different approach: it calculates group cohesion
continuously from between-animal dispersion.

The package currently supports:

- `HLC_SD`: standard-deviation-based HLC
- `HLC_MAD`: mean-absolute-deviation-based HLC
- `HLC_IQR`: interquartile-range-based HLC
- `HLC_ENT`: entropy-based HLC

The package also calculates lying-weighted HLC, which combines group
cohesion with the proportion of the interval spent lying.

## Installation

``` r

# Development version
# install.packages("remotes")
# remotes::install_github("your-github-username/HLCtools")
```

## Example data

``` r
data(example_lies)

example_lies
#>    group day week  time  cow lying
#> 1      A   1    1 00:00 cow1    15
#> 2      A   1    1 00:15 cow2    15
#> 3      A   1    1 00:00 cow3    15
#> 4      A   1    1 00:15 cow4    15
#> 5      A   1    1 00:00 cow5    15
#> 6      A   1    1 00:15 cow1     0
#> 7      A   1    1 00:00 cow2     0
#> 8      A   1    1 00:15 cow3     0
#> 9      A   1    1 00:00 cow4     0
#> 10     A   1    1 00:15 cow5     0
#> 11     A   2    1 00:00 cow1    15
#> 12     A   2    1 00:15 cow2    15
#> 13     A   2    1 00:00 cow3    15
#> 14     A   2    1 00:15 cow4     0
#> 15     A   2    1 00:00 cow5     0
#> 16     A   2    1 00:15 cow1    15
#> 17     A   2    1 00:00 cow2     0
#> 18     A   2    1 00:15 cow3    15
#> 19     A   2    1 00:00 cow4     0
#> 20     A   2    1 00:15 cow5    15
#> 21     B   1    1 00:00 cow1    15
#> 22     B   1    1 00:15 cow2    15
#> 23     B   1    1 00:00 cow3    10
#> 24     B   1    1 00:15 cow4    10
#> 25     B   1    1 00:00 cow5    15
#> 26     B   1    1 00:15 cow1    15
#> 27     B   1    1 00:00 cow2    12
#> 28     B   1    1 00:15 cow3    15
#> 29     B   1    1 00:00 cow4     8
#> 30     B   1    1 00:15 cow5    15
#> 31     B   2    1 00:00 cow1     0
#> 32     B   2    1 00:15 cow2     0
#> 33     B   2    1 00:00 cow3     5
#> 34     B   2    1 00:15 cow4     0
#> 35     B   2    1 00:00 cow5     0
#> 36     B   2    1 00:15 cow1     0
#> 37     B   2    1 00:00 cow2    15
#> 38     B   2    1 00:15 cow3     0
#> 39     B   2    1 00:00 cow4    15
#> 40     B   2    1 00:15 cow5     0
```

## Calculate interval-level HLC

``` r
hlc_intervals <- calculate_hlc(
  data = example_lies,
  group = group,
  animal = cow,
  day = day,
  period = week,
  interval = time,
  lying = lying,
  interval_min = 15,
  methods = c("sd", "mad", "iqr", "entropy"),
  sync_thresholds = c(0.6, 0.7, 0.8, 0.9),
  add_lying_weighted = TRUE
)

hlc_intervals
#> # A tibble: 8 × 24
#>   group   day  week time  n_animals mean_lying lying_prop sd_lying mad_lying
#>   <chr> <int> <dbl> <chr>     <int>      <dbl>      <dbl>    <dbl>     <dbl>
#> 1 A         1     1 00:00         5          9      0.6       7.35       7.2
#> 2 A         1     1 00:15         5          6      0.4       7.35       7.2
#> 3 A         2     1 00:00         5          6      0.4       7.35       7.2
#> 4 A         2     1 00:15         5         12      0.8       6          4.8
#> 5 B         1     1 00:00         5         12      0.8       2.76       2.4
#> 6 B         1     1 00:15         5         14      0.933     2          1.6
#> 7 B         2     1 00:00         5          7      0.467     6.78       6.4
#> 8 B         2     1 00:15         5          0      0         0          0  
#> # ℹ 15 more variables: iqr_lying <dbl>, p_lying <dbl>, HLC_SD <dbl>,
#> #   HLC_MAD <dbl>, HLC_IQR <dbl>, entropy_raw <dbl>, HLC_ENT <dbl>,
#> #   sync60 <int>, sync70 <int>, sync80 <int>, sync90 <int>, HLC_SD_LYING <dbl>,
#> #   HLC_MAD_LYING <dbl>, HLC_IQR_LYING <dbl>, HLC_ENT_LYING <dbl>
```

## Summarise HLC by day

``` r
hlc_daily <- summarise_hlc_daily(
  data = hlc_intervals,
  group = group,
  day = day,
  period = week,
  interval_min = 15
)

hlc_daily
#> # A tibble: 4 × 19
#>   group   day  week mean_HLC_SD mean_HLC_MAD mean_HLC_IQR mean_HLC_ENT
#>   <chr> <int> <dbl>       <dbl>        <dbl>        <dbl>        <dbl>
#> 1 A         1     1      0.0202       0.0400        0           0.0290
#> 2 A         2     1      0.110        0.2           0.5         0.154 
#> 3 B         1     1      0.683        0.733         0.833       1     
#> 4 B         2     1      0.548        0.573         0.5         0.515 
#> # ℹ 12 more variables: mean_HLC_SD_LYING <dbl>, mean_HLC_MAD_LYING <dbl>,
#> #   mean_HLC_IQR_LYING <dbl>, mean_HLC_ENT_LYING <dbl>, sync60_time_min <dbl>,
#> #   sync70_time_min <dbl>, sync80_time_min <dbl>, sync90_time_min <dbl>,
#> #   mean_lying_min_interval <dbl>, mean_lying_prop <dbl>, n_intervals <int>,
#> #   mean_n_animals <dbl>
```

## Available methods

``` r
hlc_methods()
#> [1] "sd"      "mad"     "iqr"     "entropy"
```

## Interpretation

Unweighted HLC describes group-level behavioural cohesion. It can be
high when animals are uniformly lying or uniformly standing.

Lying-weighted HLC describes cohesive lying specifically. It is
calculated by multiplying interval-level HLC by the mean lying
proportion in that interval.

Thus:

- high HLC + high lying-weighted HLC = cohesive collective lying;
- high HLC + low lying-weighted HLC = cohesive behaviour, but not mainly
  lying;
- low HLC = fragmented group behaviour.

## Current scope

`HLCtools` computes HLC metrics. It does not automatically decide which
dispersion basis is universally best. The choice of SD, MAD, IQR,
entropy, or another implementation should depend on the dataset,
biological question, group size, sampling interval, and intended
application.
