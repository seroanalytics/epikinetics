# Align longitudinal measurements to a reference date

Convert calendar-date observations into numeric days relative to either
a participant-specific reference column (for example, each participant's
vaccination date) or one shared reference date. The original date
columns are retained and a new numeric column is added.

## Usage

``` r
align_time_to_reference(
  data,
  measurement_date,
  reference,
  id = NULL,
  time = "time_since_reference",
  missing = c("error", "keep")
)
```

## Arguments

- data:

  A data frame containing longitudinal observations.

- measurement_date:

  Name of the `Date` column containing measurement or sample-collection
  dates.

- reference:

  Either the name of a `Date` column in `data`, normally repeated within
  participant, or one shared `Date` value.

- id:

  Optional participant-id column. When `reference` names a column,
  supplying `id` checks that its non-missing values agree within each
  participant.

- time:

  Name of the numeric output column to add.

- missing:

  How missing measurement or reference dates are handled. `"error"` (the
  default) reports them; `"keep"` retains the rows and writes `NA_real_`
  to `time`.

## Value

`data` with its original columns unchanged and an additional numeric
time column measured in days relative to `reference`.

## Details

`measurement_date` and a reference column must contain base R
[Date](https://rdrr.io/r/base/Dates.html) values. `POSIXct` is
deliberately not converted implicitly because a hidden timezone or
time-of-day conversion can shift calendar dates; convert it explicitly
with [`as.Date()`](https://rdrr.io/r/base/as.Date.html) first. Negative
values are retained, making pre-reference observations visible during
data checking. The current single-exposure model itself accepts
observations on or after time zero.

## Examples

``` r
observations <- data.frame(
  participant = c("A", "A", "B", "B"),
  exposure_date = as.Date(c(
    "2024-01-10", "2024-01-10", "2024-02-01", "2024-02-01"
  )),
  sample_date = as.Date(c(
    "2024-01-08", "2024-01-17", "2024-01-30", "2024-02-15"
  ))
)

align_time_to_reference(
  observations,
  measurement_date = "sample_date",
  reference = "exposure_date",
  id = "participant",
  time = "time_since_exposure"
)
#>   participant exposure_date sample_date time_since_exposure
#> 1           A    2024-01-10  2024-01-08                  -2
#> 2           A    2024-01-10  2024-01-17                   7
#> 3           B    2024-02-01  2024-01-30                  -2
#> 4           B    2024-02-01  2024-02-15                  14

align_time_to_reference(
  observations,
  measurement_date = "sample_date",
  reference = as.Date("2024-01-01"),
  time = "time_since_study_start"
)
#>   participant exposure_date sample_date time_since_study_start
#> 1           A    2024-01-10  2024-01-08                      7
#> 2           A    2024-01-10  2024-01-17                     16
#> 3           B    2024-02-01  2024-01-30                     29
#> 4           B    2024-02-01  2024-02-15                     45
```
