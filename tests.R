library(data.table)
source("timeperiod_functions.R")

set.seed(180)

#averaging intervals longer than observed period
a_start_date <- seq(structure(10590, class = "Date"),
                    structure(17163, class = "Date"),by=7)


a_end_date <- seq(structure(10596, class = "Date"),
                    structure(17169, class = "Date"),by=7)

a <- CJ(id1=1:3,id2=1:100, start_date=a_start_date)
a <- merge(data.table(start_date=a_start_date,end_date=a_end_date),a, by="start_date")
a[, value1:=rnorm(nrow(a))]
a[, value2:=rnorm(nrow(a))]

b <- data.table(start_date=as.Date(paste0(1999:2017,"-01-01")),
                  end_date=as.Date(paste0(1999:2017,"-12-31")))


interval_weighted_avg_f(x=a,y=b,interval_vars=c("start_date","end_date"),
                        value_vars=c("value1","value2"),
                        group_vars=c("id1","id2"))

#misspecified order of interval_vars should return an error:
stopifnot(tryCatch(
  interval_weighted_avg_f(a,b,interval_vars=c("end_date","start_date"),
                          value_vars=c("value1","value2"),
                          group_vars=c("id1","id2")),
  error=function(x){TRUE}))

#no groups

#1 group

#2 groups

#averaging intervals shorter than observed period

#2 groups, 3 groups (in x AND y (e.g., different desired averaging period for each group))

#irregular length observation periods that are not necessarily adjoining 

#periods that have overlaps--this should return an error