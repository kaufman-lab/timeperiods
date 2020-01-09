# timeperiods

## functions

**interval_weighted_avg_f** is a function which takes values recorded over non-overlapping intervals and averages them to defined intervals, possibly within individuals/monitors/locations. 

this function uses the data.table package.

Arguments: 
- x: a data.table object containing measuresments over intervals which must be non-overlapping within groups defined by group_vars. if group_vars is specified (non-NULL), x must contain columns specified in group_vars.
- y: a data.table object containing intervals over which you'd like averages of x-measures computed. y intervals, unlike x intervals, may be overlapping. if group_vars is specified (non-NULL) and y contains those group_vars column names, this would allow different averaging period for each subject/monitor/location. if group_vars is non-NULL but columns specified in group_vars are not in y, then all intervals in y will be used to average measurements for every individual in x.
- interval_vars: a length-2 character vector of column names in both x and y. these columns in x and y should be all numeric or all Dates.
- value_vars: a character vector of column names in x, to be averaged
- group_vars: a character vector of colum names in x (and possibly in y) specifying subjects/monitors/locations within which to take averages. can by NULL, in which case averages are taken over the entire x dataset for each y period.
- required_percentage: the percentage of non-missing, measured x-observations in periods defined by y for the resulting measure in the return to be nonmissing. by default, 100 (any missing observations will result in an NA).
- skip_overlap_check: by default, FALSE. setting this to TRUE will skip internal checks to make sure x intervals are non-overlapping within groups defined by group_vars. intervals in x must be non-overlapping, but you may want to skip this check if you've already checked this because it is computationally intensive for large datasets.

returns a data.table object. rows correspond to intervals from y, seperately for groups defined by group_vars. columns of the returned data.table:
- grouping variables as specified in group_vars
- interval columns corresponding to intervals in y. columns are named the same as they were in x and y.
- value variable columns from x, averaged to periods in y. named the same as they were in x
- yduration: the length of the interval (ie as a count) specified in y
- xduration: the total length of the intervals (ie as a count) from x that fall into this interval from y. this will be equal to yduration if x is comprehensive for (ie, fully covers)  this interval from y.
- nobs_<value_vars>: for each value_var specified, this is the count of non-missing values from x that fall into this interval from y. this will be equal to xduration if the value_var contains no NA values over the y interval. If there are NAs in value variables, then nobs_<value_vars> will be different from xduration and won't necessarily be all the same.

**interval_weighted_avg_slow_f** is a more algorithmicly straightforward but more memory intensive version of the function. Not appropriate for replacing sql-processes for large data but used extensively in testing.

## Files

- timeperiod_functions.R contains the source for the functions. 
- The example file generates some example data and demonstrates how to use the interval_weighted_avg_f function.
- the tests file contains testing to ensure the interval_weighted_avg_f function is working and contains some more complicated examples, particularly when dealing with recorded data that is overlapping.

