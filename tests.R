##next steps:
#compare to sql functions

rm(list=ls())
library(data.table)
source("timeperiod_functions.R")

set.seed(180)


### test remove_overlaps####

x <- data.table(start=c(1,5,5),end=c(5,5,10),id1="1",id2="1")
remove_overlaps(x,interval_vars=c("start","end"),group_vars=c("id1","id2"))



n <- 1000
x <- matrix(round(runif(n=n*2, 0,1000)),ncol=2)
x <- as.data.table(t(apply(x,1,sort)))
setnames(x,names(x),c("start","end"))
x[, id1:=rbinom(n,3,prob=.3)]
x[, id2:=rbinom(n,7,prob=.5)]
setkey(x, id1,id2,start,end)

xd <- remove_overlaps(x,interval_vars=c("start","end"),group_vars=c("id1","id2"))


##check that the new intervals: the union of the new intervals need to span exactly the original intervals:
xd[,{
  x <- .SD[,list(v1=seq(start,end)),by=1:nrow(.SD)]$v1
  x <- unique(sort(x))
  stopifnot(min(x)==o.start)
  stopifnot(max(x)==o.end)
  stopifnot(all(diff(x)==1))
  stopifnot(sum(duplicated(.SD))==0) #within original intervals and grouping variables, no duplicated new intervals
},by=c("id1","id2", "o.start","o.end"),.SDcols=c("start","end")]


x_l <- melt(x,id.vars=c("id1","id2"))
x_l[,value2:=value]
setkey(x_l, id1,id2,value,value2)
setkey(xd, id1,id2,start,end)

#make sure that *every* original start or end point in x is either a start or end point for the new start/end variables in xd, 
 #if those original start/end dates overlap with the new start dates at all (within groups)
#if value is between start and end but not exactly start or end, this means new start/end is missing a cut
vr <- foverlaps(x_l,xd)

vr[, stopifnot(value %in% c(start,end)),by=1:nrow(vr)]  





#####test CJ.dt for data.tables ####

X <- data.table(x1=1:2,x2=2:3)
Y <- data.table(y1=4:6,y2=5:7)
stopifnot(all.equal(CJ.dt(X,Y), 
                    data.table(x1=rep(1:2,times=3),
                               x2=rep(2:3,times=3),
                               y1=rep(4:6,each=2),
                               y2=rep(5:7,each=2)
                    )))


X <- data.table(
  x1 = 1:8,
  x2 = 2:9,
  id1 = rep(1:2, each = 2),
  id2 = rep(1:2, times = 2)
)
Y <- data.table(
  y1 = 4:11,
  y2 = 5:12,
  id1 = rep(1:2, each = 2),
  id2 = rep(1:2, times = 2)
)


##CJ.dt with groups should be like iterating through each combination
#of groups and doing a cartesian merge/grid expand within that:
templ <- list()
counter <- 1
for(i in unique(X$id1)){
  for(j in unique(X$id2)){
    templ[[counter]] <- CJ.dt(X[id1==i&id2==j],Y[id1==i&id2==j,list(y1,y2)])
    counter <- counter +1
  }
}
stopifnot(identical(
  CJ.dt(X,Y,groups=c("id1","id2")), 
  rbindlist(templ)
))

X <- data.table(
  x1 = 1,
  x2 = 2,
  id1 = 1,
  id2 = 1
)
Y <- data.table(
  y1 = 4:11,
  y2 = 5:12,
  id1 = rep(1:2, each = 2),
  id2 = rep(1:2, times = 2)
)


templ <- list()
counter <- 1
for(i in unique(X$id1)){
  for(j in unique(X$id2)){
    templ[[counter]] <- CJ.dt(X[id1==i&id2==j],Y[id1==i&id2==j,list(y1,y2)])
    counter <- counter +1
  }
}
stopifnot(identical(
  CJ.dt(X,Y,groups=c("id1","id2")), 
  rbindlist(templ)
))

####test averaging function #########




##averaging intervals where groups=NULL
a0.0 <- data.table(start=seq(1L,100L,by=10L),value1=rnorm(10))
a0.0[,end:=start+9L]
b0.0 <- data.table(start=1L,end=25L)
q0.01 <- interval_weighted_avg_f(x=a0.0,
                                y=b0.0,
                                interval_vars=c("start","end"),
                                value_vars=c("value1"))


q0.02 <- interval_weighted_avg_slow_f(x=a0.0,
                                     y=b0.0,
                                     interval_vars=c("start","end"),
                                     value_vars=c("value1"))


stopifnot(all.equal(q0.01,q0.02))


#averaging intervals longer than observed period
a_start_date <- seq(structure(10590, class = "Date"),
                    structure(17163, class = "Date"),by=7)

a0 <- CJ(id1=1:3,id2=1:100, start_date=a_start_date)
a0[, end_date:=start_date+6]
a0[, value1:=rnorm(.N)] 
a0[, value2:=rnorm(.N)]

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


stopifnot(all.equal(q0_1,q0_2))

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
a[, value:=rbinom(.N,5,prob=.5)]
a[, value2:=rbinom(.N,5,prob=.5)]
#randomly insert some missing values in one column:
a[as.logical(rbinom(.N,1,prob=.3)), value:=NA]

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

stopifnot(all.equal(q1,q2))


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
stopifnot(all.equal(q1,q2))



##averaging intervals shorter than observed period & includes dates not in observed period at all ####
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
b3_22 <- data.table(id=c(2L),id2=c(2L),start=c(5L,100L),
                    end=c(12L,101L))

b3 <- rbind(b3_11,b3_12,b3_21,b3_22) 
#note that for id=1,id2=1 and id=1,id2=2 these should be the same as 
#subsets of qm1 and q2_1 respectively



q3_1 <- interval_weighted_avg_f(x=a,
                                y=b3,
                                interval_vars=c("start","end"),
                                group_vars=c("id","id2"),
                                value_vars=c("value","value2"),
                                required_percentage = 50
)


q3_2 <- interval_weighted_avg_slow_f(x=a,
                                     y=b3,
                                     interval_vars=c("start","end"),
                                     group_vars=c("id","id2"),
                                     value_vars=c("value","value2"),
                                     required_percentage = 50
)
stopifnot(all.equal(q3_1[id==1&id2==1],qm1[id==1&id2==1]))
stopifnot(all.equal(q3_1[id==1&id2==2],q2_1[id==1&id2==2]))

stopifnot(all.equal(q3_1,q3_2,check.attributes=FALSE))


#more groups in x than in y
b4 <- copy(b3)
b4[,id2:=NULL]


q4_1 <- interval_weighted_avg_f(x=a,
                                y=b4,
                                interval_vars=c("start","end"),
                                group_vars=c("id","id2"),
                                value_vars=c("value","value2"),
                                required_percentage = 50
)


q4_2 <- interval_weighted_avg_slow_f(x=a,
                                     y=b4,
                                     interval_vars=c("start","end"),
                                     group_vars=c("id","id2"),
                                     value_vars=c("value","value2"),
                                     required_percentage = 50
)

stopifnot(all.equal(q4_1,q4_2))


#periods that have overlaps in y--this should work
b_overlap <- rbind(b2,data.table(start=3L,end=18L))
q_overlap_y1 <- interval_weighted_avg_f(x=a,
                                y=b_overlap,
                                interval_vars=c("start","end"),
                                group_vars=c("id","id2"),
                                value_vars=c("value","value2"),
                                required_percentage = 50
)


q_overlap_y2 <- interval_weighted_avg_slow_f(x=a,
                                     y=b_overlap,
                                     interval_vars=c("start","end"),
                                     group_vars=c("id","id2"),
                                     value_vars=c("value","value2"),
                                     required_percentage = 50
)


all.equal(q_overlap_y1,q_overlap_y2)

#periods that have partial overlaps in x--this should return an error

a_overlap <- CJ(id=1:2,id2=1:2,start=c(-13L,seq(1L,36L,by=7L),2L))
a_overlap[, end:=start+6L]
a_overlap[, value:=rbinom(.N,5,prob=.5)]

stopifnot(tryCatch(
  interval_weighted_avg_f(a_overlap,b,interval_vars=c("start","end"),
                          value_vars=c("value"),
                          group_vars=c("id","id2")),
  error=function(x){TRUE}))




##periods that have exact overlaps but no partial overlaps--this return error##
a_exact_overlap <- data.table(start_date=c(1L,1L,4L),end_date=c(3L,3L,10L),value1=c(0,5,10))
b_exact_overlap <- data.table(start_date=2L:3L,end_date=8L:9L)

stopifnot(
  tryCatch(interval_weighted_avg_f(a_exact_overlap,b_exact_overlap,interval_vars=c("start_date","end_date"),
                                   value_vars=c("value1")),error=function(x){TRUE})
)
     
stopifnot(
  tryCatch(interval_weighted_avg_slow_f(a_exact_overlap,b_exact_overlap,interval_vars=c("start_date","end_date"),
                                   value_vars=c("value1")),error=function(x){TRUE})
)


#misspecified order of interval_vars should return an error:
stopifnot(tryCatch(
  interval_weighted_avg_f(a,b,interval_vars=c("end_date","start_date"),
                          value_vars=c("value1","value2"),
                          group_vars=c("id1","id2")),
  error=function(x){TRUE}))



##make sure remove_overlaps throws an error when "i." variables are provided
a_overlap1 <- CJ(id1=1:3,id2=1:100, start_date=a_start_date)
a_overlap1[, end_date:=start_date+10]
a_overlap1[, i.start_date:=TRUE]
stopifnot(tryCatch(
  remove_overlaps(
    x=a_overlap1,
    interval_vars=c("start_date","end_date"),
    group_vars=c("id1","id2")
  ),
  error=function(x){TRUE}))


####realistic example with overlaping values: deoverlap them then average to a period:
a_overlap1 <- CJ(id1=1:3,id2=1:100, start_date=a_start_date)
a_overlap1[, end_date:=start_date+10]
a_overlap1[, value1:=rnorm(.N)] 
a_overlap1[, value2:=rnorm(.N)]
a_overlap1_removed <- remove_overlaps(
  x=a_overlap1,
  interval_vars=c("start_date","end_date"),
  group_vars=c("id1","id2")
)

#average duplicate intervals together
a_overlap1_removed <- 
  a_overlap1_removed[,list(value1=mean(value1),value2=mean(value2)),by=list(id1,id2,start_date,end_date)]

#insert missingness
a_overlap1_removed[sample(1:nrow(a_overlap1_removed),size=floor(.2*nrow(a_overlap1_removed))),`:=`(value1=NA,value2=NA)]


b_overlap1 <- CJ(id1=1:3,id2=1:100)[,
                                    rep(TRUE,sample(1:10,size=1)),
                                    by=list(id1,id2)]
b_overlap1[, start_date:=sample(a_start_date,nrow(b_overlap1),replace=TRUE)+sample(-5L:5L,nrow(b_overlap1),replace=TRUE)]
b_overlap1[, end_date:=start_date+sample(0:10,nrow(b_overlap1),replace=TRUE)]




system.time(realistic1 <-   interval_weighted_avg_f(x=a_overlap1_removed,b_overlap1,interval_vars=c("start_date","end_date"),
                          value_vars=c("value1","value2"),
                          group_vars=c("id1","id2"),
                          skip_overlap_check=FALSE))
  
system.time(realistic2 <-   interval_weighted_avg_slow_f(a_overlap1_removed,b_overlap1,interval_vars=c("start_date","end_date"),
                          value_vars=c("value1","value2"),
                          group_vars=c("id1","id2"),
                          skip_overlap_check=FALSE))
  
stopifnot(all.equal(realistic1,realistic2))




##more realism
n <- 1e5
x <- matrix(as.integer(round(runif(n=n*2, 0,1000))),ncol=2)
x <- as.data.table(t(apply(x,1,sort)))
setnames(x,names(x),c("start","end"))
x[, id1:=rbinom(n,3,prob=.3)]
x[, id2:=rbinom(n,7,prob=.5)]
x[,value1:=rnorm(n)]
x[,value2:=rnorm(n)]
setkey(x, id1,id2,start,end)

a <- remove_overlaps(x,interval_vars=c("start","end"),group_vars=c("id1","id2"))
#average duplicate intervals together
a <- 
  a[,list(value1=mean(value1),value2=mean(value2)),by=list(id1,id2,start,end)]

#insert missingness
a[sample(1:nrow(a),size=floor(.2*nrow(a))),`:=`(value1=NA,value2=NA)]

b <- matrix(as.integer(round(runif(n=n*2, 0L,1000L))),ncol=2)
b <- as.data.table(t(apply(b,1,sort)))
setnames(b,names(b),c("start","end"))
b[, id1:=rbinom(n,3,prob=.3)]
b[, id2:=rbinom(n,7,prob=.5)]



zzz1 <- interval_weighted_avg_f(a,b,interval_vars=c("start","end"),value_vars=c("value1","value2"),
                                           group_vars=c("id1","id2"),
                                           skip_overlap_check=FALSE)

zzz2 <- interval_weighted_avg_f(a,b,interval_vars=c("start","end"),value_vars=c("value1","value2"),
                                            group_vars=c("id1","id2"),
                                            skip_overlap_check=FALSE)

stopifnot(all.equal(zzz1,zzz2))




####large dataset that's non-overlapping
az_start_date <- seq(structure(0, class = "Date"),
                    structure(1e5, class = "Date"),by=14)

az <- CJ(id1=1:100, start_date=az_start_date)
az[, end_date:=start_date+13]
az[, value1:=rnorm(.N)] 
az[, value2:=rnorm(.N)]

bz_start_date <- seq(structure(2, class = "Date"),
                    structure(1e5, class = "Date"),by=7)

bz <- CJ(id1=1:100, start_date=bz_start_date)
bz[, end_date:=start_date+6]

zzbig1 <- interval_weighted_avg_f(az,bz,interval_vars=c("start_date","end_date"),
                                  value_vars=c("value1","value2"),
                                group_vars="id1",
                                skip_overlap_check=TRUE)

zzbig2 <- interval_weighted_avg_slow_f(az,bz,interval_vars=c("start_date","end_date"),
                                  value_vars=c("value1","value2"),
                                  group_vars="id1",
                                  skip_overlap_check=TRUE)

all.equal(zzbig1,zzbig2)
