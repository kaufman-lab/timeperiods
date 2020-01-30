timperiods
================

This repository is a collection of functions designed to deal with
values measured over an interval, typically as a time-integrated
average.

Some context: One approach to measuring particulate matter such as PM2.5
would be to actively collect PM2.5 on a filter (at a constant flow rate)
over a two-week period. Information about instantaneous PM2.5
concentration over that two-week period is not retreivable from the
filter, but a very accurate measurement of the filter’s mass can be used
to calculated an accurate 2-week time-integrated average. At a single
location, we might measure PM2.5 over several repeated two-week periods.
This measurement approach is much more accurate than non-gravimetric
approaches, but has the obvious limitation that the measurement is over
an extended time period. If the measurement period (in this case, 2
weeks) is small relative to the period over which we’re interested in
PM2.5 concentrations (typically, months to years), then the
time-integrated average issue becomes negligible.

However, if filters at different locations are measuring concentration
for intervals of variable length or on non-aligned schedules, the
resulting dataset would need to be processed to align measurements into
a consistent set of intervals (e.g., adjacent, non-overlapping,
recurring intervals of fixed length).

The following R code is designed to address this data processing step.
It is generalizable to any measures taken over periods (not just air
pollution data).

## functions

### interval\_weighted\_avg\_f

**interval\_weighted\_avg\_f** is a function which takes values recorded
over non-overlapping intervals and averages them to defined intervals,
possibly within individuals/monitors/locations.

this function uses the data.table package.

Arguments:

  - x: a data.table object containing measuresments over intervals which
    must be completely non-overlapping (i.e periods can’t even be
    partially overlapping) within groups defined by group\_vars. if
    group\_vars is specified (non-NULL), BOTH x and y must contain
    columns specified in group\_vars.
  - y: a data.table object containing intervals over which you’d like
    averages of x-measures computed. y intervals, unlike x intervals,
    may be overlapping. if group\_vars is specified (non-NULL), y must
    contains those group\_vars column names, and this would allow
    different averaging period for each subject/monitor/location.
  - interval\_vars: a length-2 character vector of column names in both
    x and y. these columns in x and y should be all numeric or all
    Dates.
  - value\_vars: a character vector of column names in x, to be averaged
  - group\_vars: a character vector of colum names in x and in y (note
    that this is a change from a previous version) specifying
    subjects/monitors/locations within which to take averages. can by
    NULL, in which case averages are taken over the entire x dataset for
    each y period.
  - required\_percentage: the percentage of non-missing, measured
    x-observations in periods defined by y for the resulting measure in
    the return to be nonmissing. by default, 100 (any missing
    observations will result in an NA).
  - skip\_overlap\_check: by default, FALSE. setting this to TRUE will
    skip internal checks to make sure x intervals are non-overlapping
    within groups defined by group\_vars. intervals in x must be
    non-overlapping, but you may want to skip this check if you’ve
    already checked this because it is computationally intensive for
    large datasets.

returns a data.table object. Rows correspond to intervals from y,
separately for groups defined by group\_vars. *nrow of the return will
be nrow of y because the function is designed to behave like a right
inner join on intervals (and optional additional grouping variables)
with an average*. Rows of y where an average cannot be calculated
(either due to the measurements being present in X but NA or the
measurements not being in x at all) are still returned with value
variable columns set to NA (see the required\_percentage argument for
more details). Columns of the returned data.table are as follows

  - grouping variables as specified in group\_vars
  - interval columns corresponding to intervals in y. columns are named
    the same as they were in x and y.
  - value variable columns from x, averaged to periods in y. named the
    same as they were in x
  - yduration: the length of the interval (ie as a count) specified in y
  - xduration: the total length of the intervals (ie as a count) from x
    that fall into this interval from y. this will be equal to yduration
    if x is comprehensive for (ie, fully covers) this interval from y.
  - nobs\_<value_vars>: for each value\_var specified, this is the count
    of non-missing values from x that fall into this interval from y.
    this will be equal to xduration if the value\_var contains no NA
    values over the y interval. If there are NAs in value variables,
    then nobs\_<value_vars> will be different from xduration and won’t
    necessarily be all the same for each value\_var.
  - xminstart: the minimum of the start intervals in x used in averaging
    returned y intervals, within groups. If the start of the earliest x
    interval is less than the start of the y interval, the minumum of
    the y interval is returned. Note, this is the minimum start time
    whether or not value\_vars were missing or not for that start time.
    If you really need non-missing minimum start times, you can remove
    missing intervals from x prior to calling interval\_weighted\_avg\_f
    (calling this separately for each value\_var).
  - xmaxend: the maximum of the end intervals in x used in averaging
    returned y intervals, within groups. Again, like for xminstart, this
    does not pay attention to whether the interval in x had non-missing
    value\_vars.

Additional notes:

  - When required\_percentage is less than 100, xminstart and xmaxend
    may be useful to determine whether an average meets specified
    coverage requirements in terms of span (range) of the y interval.
  - Warning, this function will reorder the rows of x and y. This occurs
    because data.table objects to funtions are pass by reference for
    efficiency (R is normally pass by value). If this is a concern, a
    couple of quick edits would maintain the order (by reordering at
    function completion).

#### interval\_weighted\_avg\_f Examples

``` r
library(data.table)
source("timeperiod_functions.R")
```

First let’s start with some measurement data collected on a regular
schedule where each interval is 5 units in length. Let’s say we want to
average this measurement data to a long interval containing measured
intervals (0-30),as well as to a specific set of intervals corresponding
to a schedule where each interval is 7 units in
length.

``` r
(x <- data.table(value1=c(1,2,3,2,1),start=c(1L,6L,11L,16L,21L),end=c(5L,10L,15L,20L,25L)))
```

    ##    value1 start end
    ## 1:      1     1   5
    ## 2:      2     6  10
    ## 3:      3    11  15
    ## 4:      2    16  20
    ## 5:      1    21  25

``` r
(y <- data.table(start=c(0L,0L,7L,14L,21L),end=c(30L,6L,13L,20L,27L)))
```

    ##    start end
    ## 1:     0  30
    ## 2:     0   6
    ## 3:     7  13
    ## 4:    14  20
    ## 5:    21  27

``` r
interval_weighted_avg_f(x,y,interval_vars=c("start","end"),value_vars=c("value1"))
```

    ## [1] "2020-01-30 13:38:17 passed errorcheck: x is non-overlapping."

    ##    start end   value1 yduration xduration nobs_value1 xminstart xmaxend
    ## 1:     0   6       NA         7         6           6         1       6
    ## 2:     0  30       NA        31        25          25         1      25
    ## 3:     7  13 2.428571         7         7           7         7      13
    ## 4:    14  20 2.285714         7         7           7        14      20
    ## 5:    21  27       NA         7         5           5        21      25

Note that average value is missing (NA) for periods which are not
completely observed in x. By using looser competeness requirements we
can get values (although note that these are not truly the average if we
consider value1 to be an unmeasured random variable at non-measured
timepoints):

``` r
interval_weighted_avg_f(x,y,interval_vars=c("start","end"),value_vars=c("value1"),required_percentage=.8)
```

    ## [1] "2020-01-30 13:38:18 passed errorcheck: x is non-overlapping."

    ##    start end   value1 yduration xduration nobs_value1 xminstart xmaxend
    ## 1:     0   6 1.166667         7         6           6         1       6
    ## 2:     0  30 1.800000        31        25          25         1      25
    ## 3:     7  13 2.428571         7         7           7         7      13
    ## 4:    14  20 2.285714         7         7           7        14      20
    ## 5:    21  27 1.000000         7         5           5        21      25

It would also be possible to calculate multiple averages simulataneously
or calculate averages for different y-intervals for different sets of
observations defined by a group\_var.

See tests.R for more examples.

#### interval\_weighted\_avg\_f Technical overview

This function merges periods in y to periods in x by interval variables
and possible grouping variables. Since the function is designed to
return a row for each row in y, non-matches in y are identified by
non-join merges and added in as rows with missing value variables. The
value variables in the resulting data.table are averaged in a group-by
statement (by periods defined in y as well by optional grouping
variables). Specifically, the average is a weighted average that takes
into account the duration of an observed measurement in a desired
averaging period (y interval). The function is written to make use of
data.table’s Gforce capabilities. data.table’s gforce efficeintly does
group-by operations directly in C rather than in R. Since data.table’s
ability to detect an optimized group-by function call is limited, using
gforce for a weighted mean (and even just for an abritrary number of
value\_vars) requires use of some nonstandard evaluation. In this
context, the non-standard evaluation is constructing a data.table call
via paste and calling it with eval(parse(text=\<\>)).

An additional consideration is that since data.table objects are
pass-by-reference in functions (unless you explicitly make a copy which
would be memory intensive), care needs to be taken not add or alter
existing columns when using data.table’s column assignment by reference
because this would alter the data.table object outside of the function
environment. This is why there’s a bunch of code to deal with variable
names so that temporary variables don’t clash with existing columns.

### interval\_weighted\_avg\_slow\_f

This is a more algorithmicly straightforward but more memory intensive
version of interval\_weighted\_avg\_f. Specifically, it expands all
individual measurements over an interval into repeated observations of
that measurement for each discrete time increment in that interval.
Since the resulting data is now observed on time-increments which are
equal in length, weighted averaging is no longer necessary–a simple
average can be used. Since this approach expands the data, it is not
appropriate for replacing sql-pipilines for large data in R and will
result in out-of-memory errors. Due to the mission-critical work that
the interval\_weighted\_avg\_f function will be performing, I wrote
interval\_weighted\_avg\_slow\_f as a secondary testing algorithm.
Numerous tests invovling different data scenarios comparing
interval\_weighted\_avg\_f and interval\_weighted\_avg\_slow\_f return
identical results.

### remove\_overlaps

The remove\_overlaps function takes measures over intervals which may be
partially-overlapping and breaks these intervals into non-overlapping
intervals and exactly overlapping intervals. This would allow you to
then average values over exactly-overlapping intervals. useful if you
have overlapping monitoring data at a single site.

## Files

  - timeperiod\_functions.R contains the source for the functions.
  - The example file generates some example data and demonstrates how to
    use the interval\_weighted\_avg\_f function.
  - the tests file contains testing to ensure the
    interval\_weighted\_avg\_f function is working and contains some
    more complicated examples, particularly when dealing with recorded
    data that is overlapping.
