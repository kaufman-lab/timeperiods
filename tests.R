
##next steps:
##y and x two matching groups. different averging periods in y depending on groups
#two groups in x and one group in y
##y can be overlapping
#compare to sql functions!


library(data.table)
source("timeperiod_functions.R")

set.seed(180)

#averaging intervals longer than observed period
a_start_date <- seq(structure(10590, class = "Date"),
                    structure(17163, class = "Date"),by=7)

a0 <- CJ(id1=1:3,id2=1:100, start_date=a_start_date)
a0[, end_date:=start_date+6]
a0[, value1:=rnorm(nrow(a0))]
a0[, value2:=rnorm(nrow(a0))]

b0 <- data.table(start_date=as.Date(paste0(1999:2017,"-01-01")),
                  end_date=as.Date(paste0(1999:2017,"-12-31")))


###two groups in x,two values####

q0_1 <- interval_weighted_avg_f(x=a0,
                             y=b0,
                             interval_vars=c("start_date","end_date"),
                             value_vars=c("value1","value2"),
                             group_vars=c("id1","id2"))


q0_2 <- interval_weighted_avg_slow_f(x=a0,
                                   y=b0,
                                   interval_vars=c("start_date","end_date"),
                                   value_vars=c("value1","value2"),
                                   group_vars=c("id1","id2"))


all.equal(q0_1,q0_2)

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
a[, value2:=rbinom(nrow(a),5,prob=.5)]
#randomly insert some missing values in one column:
a[as.logical(rbinom(nrow(a),1,prob=.3)), value:=NA]

b <- data.table(start=seq(0L,56L,by=14L))
b[,end:=start+13L]


q1 <- interval_weighted_avg_f(x=a,
                             y=b,
                             interval_vars=c("start","end"),
                             group_vars=c("id","id2"),
                             value_vars=c("value","value2")
                             )


q2 <- interval_weighted_avg_slow_f(x=a,
                             y=b,
                             interval_vars=c("start","end"),
                             group_vars=c("id","id2"),
                             value_vars=c("value","value2"))

all.equal(q1,q2)


##test missingness when required_percentage is not 100


qm1 <- interval_weighted_avg_f(x=a,
                              y=b,
                              interval_vars=c("start","end"),
                              group_vars=c("id","id2"),
                              value_vars=c("value","value2"),
                              required_percentage = 50
)


qm2 <- interval_weighted_avg_slow_f(x=a,
                                   y=b,
                                   interval_vars=c("start","end"),
                                   group_vars=c("id","id2"),
                                   value_vars=c("value","value2"),
                                   required_percentage=50)
all.equal(q1,q2)



##averaging intervals shorter than observed period includes dates 
#not in observed period at all ####
b2 <- data.table(start=seq(0L,56L,by=3L))
b2[,end:=start+2L]


q2_1 <- interval_weighted_avg_f(x=a,
                              y=b2,
                              interval_vars=c("start","end"),
                              group_vars=c("id","id2"),
                              value_vars=c("value","value2"),
                              required_percentage = 50
)


q2_2 <- interval_weighted_avg_slow_f(x=a,
                                y=b2,
                                interval_vars=c("start","end"),
                                group_vars=c("id","id2"),
                                value_vars=c("value","value2"),
                                required_percentage = 50
)







#1 groups in both x AND y with different desired averagin periods for each group
 #(e.g., different desired averaging period for each group))


b3_11 <- data.table(id=1L,id2=1L,start=seq(0L,56L,by=14L))
b3_11[,end:=start+13L]
b3_12 <- data.table(id=1L,id2=2L,start=seq(0L,54L,by=3L))
b3_12[,end:=start+2L]
b3_21 <- data.table(id=2L,id2=1L,start=seq(3L,43L,by=20L))
b3_21[,end:=start+19L]
b3_22 <- data.table(id=2L,id2=2L,start=5L,end=12L)

b3 <- rbind(b3_11,b3_12,b3_21,b3_22) 
#note that for id=1,id2=1 and id=1,id2=2 these should be the same as qm1 and q2



q3_1 <- interval_weighted_avg_f(x=a,
                                y=b3,
                                interval_vars=c("start","end"),
                                group_vars=c("id","id2"),
                                value_vars=c("value","value2"),
                                required_percentage = 50
)


q3_2 <- interval_weighted_avg_slow_f(x=a2,
                                     y=b2,
                                     interval_vars=c("start","end"),
                                     group_vars=c("id","id2"),
                                     value_vars=c("value","value2"),
                                     required_percentage = 50
)



#irregular length observation periods that are not necessarily adjoining 

#periods that have overlaps--this should return an error

#misspecified order of interval_vars should return an error:
stopifnot(tryCatch(
  interval_weighted_avg_f(a,b,interval_vars=c("end_date","start_date"),
                          value_vars=c("value1","value2"),
                          group_vars=c("id1","id2")),
  error=function(x){TRUE}))
