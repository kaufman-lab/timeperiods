# timeperiods


interval_weighted_avg_f is a function which takes values recorded over non-overlapping intervals and averages them to defined intervals, possibly within individuals/monitors/locations. 

this funciton uses the data.table package.

Arguments: 
x: a data.table object containing measuresments over intervals
y: a data.table object containing ##interval vars is a length-2 character vector of column names in both x and y
interval_vars: a length-2 character vector of column names in both x and y
value_vars: a character vector of column names in x, to be averaged
group_vars: a character vector of colum names in x (and possibly in y) specifying subjects/monitors/locations within which to take averages. 


Note that interval_weighted_avg_slow_f is a more algorithmicly straightforward but more memory intensive version of the function. Not appropriate for replacing sql-processes but used extensively in testing.

## Files

- timeperiod_functions.R contains the source for the functions. 
- The example file generates some example data and demonstrates how to use the interval_weighted_avg_f function.
- the tests file contains testing to ensure the interval_weighted_avg_f function is working and contains some more complicated examples, particularly when dealing with recorded data that is overlapping.

