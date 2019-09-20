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

q1 <- interval_weighted_avg_f(x=a,
                             y=b,
                             interval_vars=c("start_date","end_date"),
                             value_vars=c("value1","value2"),
                             group_vars=c("id1","id2"))


q2 <- interval_weighted_avg_slow_f(x=a,
                                   y=b,
                                   interval_vars=c("start_date","end_date"),
                                   value_vars=c("value1","value2"),
                                   group_vars=c("id1","id2"))


all.equal(q1,q2)

  #simple example:
#in this example, b is a regular schedule over which we'd like to compute averages of values from a
#a is also on a regular schedule that does not fit neatly into b's schedule 
#(some intervals in a partially overlap with two intervals in b )
#additionally, the first interval in a doesn't overlap with 
 #intervals in b at all and is entirely non-adjoining. since we're looking for averages over intervals in b, 
 #we don't care about this interval and it should be left out of the results

#b contains an interval that is observed in a for almost all but not the entire period 
 #(partial overlap, we probably want an average for this period)
#b also contains an interval that is barely observed except for a small overlap
  #(partial overlap, we probably want NA for the average of this period)



#b finishes with an interval which contains no overlap with any intervals in a
#we want averges for all intervals in b--
 #so the result should have a row for intervals in b whether there's a complete, partial, or no overlap with a:
 #intervals in a that don't overlap at all with intervals in b should not be returned--
  #because we have no interest in intervals not specified in b

a <- CJ(id=1:2,id2=1:2,start=c(-13L,seq(1L,36L,by=7L)))
a[, end:=start+6L]
a[, value:=rbinom(nrow(a),5,prob=.5)]
#randomly insert some missing values:
a[as.logical(rbinom(nrow(a),1,prob=.3)), value:=NA]
b <- data.table(start=seq(0,56,by=14))
b[,end:=start+13L]


q1 <- interval_weighted_avg_f(x=a,
                             y=b,
                             interval_vars=c("start","end"),
                             group_vars=c("id","id2"),
                             value_vars=c("value")
                             )


q2 <- interval_weighted_avg_slow_f(x=a,
                             y=b,
                             interval_vars=c("start","end"),
                             group_vars=c("id","id2"),
                             value_vars=c("value"))

all.equal(q1,q2)


##test missingness when required_percentage is not 100


q1 <- interval_weighted_avg_f(x=a,
                              y=b,
                              interval_vars=c("start","end"),
                              group_vars=c("id","id2"),
                              value_vars=c("value"),
                              required_percentage = 70
)


q2 <- interval_weighted_avg_slow_f(x=a,
                                   y=b,
                                   interval_vars=c("start","end"),
                                   group_vars=c("id","id2"),
                                   value_vars=c("value"),
                                   required_percentage=70)


###next steps: deal with missing values in x correctly such that the percentage function works in slow function
#actually test missing values
##add duration columns to slow
##y can be overlapping
##y can have groups that match groups in x
##y can have groups that match groups in x: partial overlap (ie more grouping vars in x than in y)
#compare to sql functions!

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
