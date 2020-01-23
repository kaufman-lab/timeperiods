# timeperiods

## functions

**interval_weighted_avg_f** is a function which takes values recorded over non-overlapping intervals and averages them to defined intervals, possibly within individuals/monitors/locations. 

this function uses the data.table package.

Arguments: 
- x: a data.table object containing measuresments over intervals which must be completely non-overlapping (i.e periods can't even be partially overlapping) within groups defined by group_vars. if group_vars is specified (non-NULL), BOTH x and y must contain columns specified in group_vars.
- y: a data.table object containing intervals over which you'd like averages of x-measures computed. y intervals, unlike x intervals, may be overlapping. if group_vars is specified (non-NULL) and y contains those group_vars column names, this would allow different averaging period for each subject/monitor/location. if group_vars is non-NULL but columns specified in group_vars are not in y, then all intervals in y will be used to average measurements for every individual in x.
- interval_vars: a length-2 character vector of column names in both x and y. these columns in x and y should be all numeric or all Dates.
- value_vars: a character vector of column names in x, to be averaged
- group_vars: a character vector of colum names in x and in y (note that this is a change from a previous version) specifying subjects/monitors/locations within which to take averages. can by NULL, in which case averages are taken over the entire x dataset for each y period.
- required_percentage: the percentage of non-missing, measured x-observations in periods defined by y for the resulting measure in the return to be nonmissing. by default, 100 (any missing observations will result in an NA).
- skip_overlap_check: by default, FALSE. setting this to TRUE will skip internal checks to make sure x intervals are non-overlapping within groups defined by group_vars. intervals in x must be non-overlapping, but you may want to skip this check if you've already checked this because it is computationally intensive for large datasets.

returns a data.table object. Rows correspond to intervals from y, separately for groups defined by group_vars. *nrow of the return will be nrow of y because the function is designed to behave like a right inner join on intervals (and optional additional grouping variables) with an average*. Rows of y where an average cannot be calculated (either due to the measurements being present in X but NA or the measurements not being in x at all) are still returned with value variable columns set to NA (see the required_percentage argument for more details). Columns of the returned data.table are as follows
- grouping variables as specified in group_vars
- interval columns corresponding to intervals in y. columns are named the same as they were in x and y.
- value variable columns from x, averaged to periods in y. named the same as they were in x
- yduration: the length of the interval (ie as a count) specified in y
- xduration: the total length of the intervals (ie as a count) from x that fall into this interval from y. this will be equal to yduration if x is comprehensive for (ie, fully covers)  this interval from y.
- nobs_<value_vars>: for each value_var specified, this is the count of non-missing values from x that fall into this interval from y. this will be equal to xduration if the value_var contains no NA values over the y interval. If there are NAs in value variables, then nobs_<value_vars> will be different from xduration and won't necessarily be all the same for each value_var.
- xminstart: the minimum of the start intervals in x used in averaging returned y intervals, within groups. If the start of the earliest x interval is less than the start of the y interval, the minumum of the y interval is returned. Note, this is the minimum start time whether or not value_vars were missing or not for that start time. If you really need non-missing minimum start times, you can remove missing intervals from x prior to calling interval_weighted_avg_f (calling this separately for each value_var).
- xmaxend: the maximum of the end intervals in x used in averaging returned y intervals, within groups. Again, like for xminstart, this does not pay attention to whether the interval in x had non-missing value_vars.

when required_percentage is less than 100, xminstart and xmaxend may be useful to determine whether an average meets specified coverage requirements in terms of span (range) of the y interval. 

*Technical overview*: This function merges periods in y to periods in x by interval variables and possible grouping variables. Since the function is designed to return a row for each row in y, non-matches in y are identified by non-join merges and added in as rows with missing value variables.  The value variables in the resulting data.table are averaged in a group-by statement (by periods defined in y as well by optional grouping variables). Specifically, the average is a weighted average that takes into account the duration of an observed measurement in a desired averaging period (y interval). The function is written to make use of data.table's Gforce capabilities. data.table's gforce  efficeintly does group-by operations directly in C rather than in R. Since data.table's ability to detect an optimized group-by function call is limited, using gforce for a weighted mean (and even just for an abritrary number of value_vars) requires use of some nonstandard evaluation. In this context, the non-standard evaluation is constructing a data.table call via paste and calling it with eval(parse(text=<>)). 

**interval_weighted_avg_slow_f** is a more algorithmicly straightforward but more memory intensive version of the function. Specifically, it expands all individual measurements over an interval into repeated observations of that measurement for each discrete time increment in that interval. Since the resulting data is now observed on time-increments which are equal in length, weighted averaging is no longer necessary--a simple average can be used. Since this approach expands the data, it is not appropriate for replacing sql-pipilines for large data in R and will result in out-of-memory errors. Due to the mission-critical work that the  interval_weighted_avg_f function will be performing, I wrote interval_weighted_avg_slow_f as a secondary testing algorithm. Numerous tests invovling different data scenarios comparing interval_weighted_avg_f and interval_weighted_avg_slow_f return identical results.

**remove_overlaps** takes measures over intervals which  may be partially-overlapping and breaks these intervals into non-overlapping intervals and exactly overlapping intervals. This would allow you to then average values over exactly-overlapping intervals. useful if you have overlapping monitoring data at a single site.

## Files

- timeperiod_functions.R contains the source for the functions. 
- The example file generates some example data and demonstrates how to use the interval_weighted_avg_f function.
- the tests file contains testing to ensure the interval_weighted_avg_f function is working and contains some more complicated examples, particularly when dealing with recorded data that is overlapping.



