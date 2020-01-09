# timeperiods


interval_weighted_avg_f is a function which takes values recorded over non-overlapping intervals and averages them to defined intervals, possibly within individuals/monitors/locations. 

this function uses the data.table package.

Arguments: 
- x: a data.table object containing measuresments over intervals which must be non-overlapping within groups defined by group_vars
- y: a data.table object containing intervals over which you'd like averages of x-measures computed. y intervals, unlike x intervals, may be overlapping.
- interval_vars: a length-2 character vector of column names in both x and y
- value_vars: a character vector of column names in x, to be averaged
- group_vars: a character vector of colum names in x (and possibly in y) specifying subjects/monitors/locations within which to take averages. 
- required_percentage: the percentage of non-missing, measured x-observations in periods defined by y for the resulting measure in the return to be nonmissing. by default, 100 (any missing observations will result in an NA).
- skip_overlap_check: by default, FALSE. setting this to TRUE will skip internal checks to make sure x intervals are non-overlapping within groups defined by group_vars. intervals in x must be non-overlapping, but you may want to skip this check if you've already checked this because it is computationally intensive for large datasets.


Note that interval_weighted_avg_slow_f is a more algorithmicly straightforward but more memory intensive version of the function. Not appropriate for replacing sql-processes but used extensively in testing.

## Files

- timeperiod_functions.R contains the source for the functions. 
- The example file generates some example data and demonstrates how to use the interval_weighted_avg_f function.
- the tests file contains testing to ensure the interval_weighted_avg_f function is working and contains some more complicated examples, particularly when dealing with recorded data that is overlapping.

