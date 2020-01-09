library(data.table)
source("timeperiod_functions.R")

####generate example prediction data ####
s <- seq(as.Date("1998-12-30"),as.Date("2017-06-28"),by=7) #start_dates
n_id <- 20
id <- paste0("id_",sprintf("%04d",1:n_id))
x <- CJ(id,start_date=s)
x[, end_date:=start_date+6]
x[, c("value1","value2"):=list(rnorm(nrow(x)),rnorm(nrow(x)))]


#### generate example averaging period data ####
#averaging period data doesn't need to be on any sort of intelligble schedule
 #you just rows with starts and ends

#let's say we wanted to average the predictions to calendar year
jan1 <- as.Date(paste0(1999:2017, "-01-01"))
y <- data.table(start_date=jan1)
y[,end_date:=as.Date(paste0(year(start_date),"-12-31"))]


#### take averages of x on a schedule defined in y ####
##interval vars is a length-2 character vector of column names in both x and y
#value_vars is a character vector of column names in x, to be averaged
##group_vars is a character vector of colum names in x (and possibly in y)
out <- interval_weighted_avg_f(x=x,y=y,interval_vars=c("start_date","end_date"),
                        value_vars=c("value1","value2"),
                        group_vars="id")
out 

#the return value is a data.table that has columns of start_date and end_date from y,
 #and averages of value_vars from x, averaged over these periods.

#calendar year average for each location
  #note that the final year (2017) average is returned as NA since predictions 
   #are incomplete in that year
   #you can see this in the xduration column which counts number of days present as records in x
     #nobs_value1 which counts number of nonmissing days in value present as records in x
     #nobs_value1 and xduration are the same here since there's no missingness in "value"
     #(as should be the case with our prediction dataset)
     
    #the only case you should encounter weirdness with xduration/counts of days is if you try to 
      #average a period that's outside or partially-outside our prediction dates

##note that the above function has been tested pretty extensively using different examples, 
 #but it's worth comparing it to the slower, more robust version of the function 
  #this isn't recommended for large datasets since it expands every day into a row 
   #rather than doing weighted averages
  #but for your dataset there won't be a noticable difference
out_slow<- interval_weighted_avg_slow_f(x=x,y=y,interval_vars=c("start_date","end_date"),
                               value_vars=c("value1","value2"),
                               group_vars="id")

all.equal(out,out_slow) #here we see the two algorithms return the same averages,
 #at least to the level of machine precision tested with all.equal.

#definitely let me know if you see an example where these two functions don't
 #return results that are all.equal



#### generate example averaging period data: different averaging periods for each id ####
#note that in the above example the averaging period was the same (calendar year) for each site
#now I'll specify different averaging periods for each monitor

#generate complete overlapping fifteen day periods, not aligned with the above prediction schedule

s <- seq(as.Date("1999-01-01"),as.Date("2017-06-28"),by=14) #start_dates
id <- paste0("id_",sprintf("%04d",1:n_id))
y2 <- CJ(id,start_date=s)
y2[, end_date:=start_date+14]

##randomly subsample 3 periods per monitor
y2 <- y2[,.SD[sample(1:.N,3)],by=id]


out2 <- interval_weighted_avg_f(x=x,y=y2,interval_vars=c("start_date","end_date"),
                               value_vars=c("value1","value2"),
                               group_vars="id")


#compare...
out2 <- interval_weighted_avg_slow_f(x=x,y=y2,interval_vars=c("start_date","end_date"),
                                     value_vars=c("value1","value2"),
                                     group_vars="id")

all.equal(out,out2)


out2_slow<- interval_weighted_avg_slow_f(x=x,y=y2,interval_vars=c("start_date","end_date"),
                                        value_vars=c("value1","value2"),
                                        group_vars="id")

all.equal(out2,out2_slow)


