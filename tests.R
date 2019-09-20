library(data.table)
source("timeperiod_functions.R")

set.seed(180)

#averaging intervals longer than observed period
a_start_date <- seq(structure(10590, class = "Date"),
                    structure(17163, class = "Date"),by=7)

a <- CJ(id1=1:3,id2=1:100, start_date=a_start_date)
a[, end_date:=start_date+6]
a[, value1:=rnorm(nrow(a))]
a[, value2:=rnorm(nrow(a))]

b <- data.table(start_date=as.Date(paste0(1999:2017,"-01-01")),
                  end_date=as.Date(paste0(1999:2017,"-12-31")))


###two groups in x,two values####

q <- interval_weighted_avg_f(x=a,y=b,interval_vars=c("start_date","end_date"),
                        value_vars=c("value1","value2"),
                        group_vars=c("id1","id2"))

  #simple example:
a <- CJ(id=1:2,id2=1:2,start=seq(1,50,by=7))
a[, end:=start+6]
a[, value:=rbinom(nrow(a),5,prob=.5)]
b <- data.table(start=seq(0,42,by=14))
b[,end:=start+13]


q1 <- interval_weighted_avg_f(x=a,
                             y=b,interval_vars=c("start","end"),
                             group_vars=c("id","id2"),
                             value_vars=c("value"))


q2 <- interval_weighted_avg_slow_f(x=a,
                             y=b,interval_vars=c("start","end"),
                             group_vars=c("id","id2"),
                             value_vars=c("value"))

#no group, only 1 value ###

q <- interval_weighted_avg_f(x=data.table(start_date=a_start_date,
                                          end_date=a_end_date,
                                          value1=rnorm(length(a_start_date))),
                             y=b,interval_vars=c("start_date","end_date"),
                             value_vars=c("value1"))




###averaging intervals shorter than observed period ####
b2 <- data.table(start_date=seq(as.Date(paste0(1999,"-01-01")),
                                as.Date(paste0(2017,"-12-31")),by=1),
                end_date=seq(as.Date(paste0(1999,"-01-01")),
                             as.Date(paste0(2018,"-01-31")),by=1))

##averaging intervals shorter than observed period includes dates not in observed period at all ####
 #
b2 <- data.table(start_date=seq(as.Date(paste0(1999,"-01-01")),
                                as.Date(paste0(2018,"-1-31")),by=1),
                 end_date=seq(as.Date(paste0(1999,"-01-01")),
                              as.Date(paste0(2018,"-01-31")),by=1))

q <- interval_weighted_avg_f(x=data.table(start_date=a_start_date,
                                          end_date=a_end_date,
                                          value1=rnorm(length(a_start_date))),
                             y=b2,interval_vars=c("start_date","end_date"),
                             group_vars=NULL,
                             value_vars=c("value1"))



#1 groups in both x AND y with different desired averagin periods for each group
 #(e.g., different desired averaging period for each group))

#two groups in x and one group in y

#irregular length observation periods that are not necessarily adjoining 

#periods that have overlaps--this should return an error

#misspecified order of interval_vars should return an error:
stopifnot(tryCatch(
  interval_weighted_avg_f(a,b,interval_vars=c("end_date","start_date"),
                          value_vars=c("value1","value2"),
                          group_vars=c("id1","id2")),
  error=function(x){TRUE}))