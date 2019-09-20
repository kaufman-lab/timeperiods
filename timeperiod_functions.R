library(data.table)


#a function to grid expand two data.tables
#largely based on https://github.com/lockedata/optiRum/blob/master/R/CJ.dt.R
#groups is a character vector
CJ.dt <- function(X, Y,groups=NULL) {
  kvar <- "k"
  while(kvar%in% names(X)|kvar%in% names(Y)){
    kvar <- paste0("i.",kvar)
  }
  X <- copy(X)
  Y <- copy(Y)
  X[,(kvar):=1]
  Y[,(kvar):=1]
  setkeyv(X, c(kvar,groups))
  setkeyv(Y, c(kvar,groups))
  X[Y, allow.cartesian = TRUE][, `:=`(k, NULL)][]
}



#function to take non-overlapping time-integrated averages observed over a defined period
#and average those observations up (or down) to a specified schedule
#this schedule does not necessarily need to align to the periods

#for example, take observations on the two-week scale over decades and 
#calculate averages for calendar year

#or, take observations over periods of variable lengths between 6 and 14 days 
#and average them to a one-week fixed schedule 

#do this within an arbitrary number of groups
#and allow specification of what percentage of time within the specified schedule needs to be 
#observed in order to not return NA. default is 100

#interval_vars is a length-2 character vector 
#corresponding to columns in both x and y which are either integers or dates 
#representing the start and end of each interval. 
#intervals must be non-overlapping within groups that are combinations of the group_vars columns
#a unit difference in these interval variables is assumed to be the smallest observable increment

#group_vars is a character vector corresponding to columns in x that represent groups

##value_vars are character vectores corresponding to columns in x which represent average values over the 
 #intervals specified in the columns corresponding to interval_vars


#by default, the function ensures that periods are non-overlapping in x and y
#this is slow, but is a necessary condition of this function
#if you're sure your intervals are non-overlapping you can skip this
#check by specifying skip_overlap_check=TRUE
#at a later time, this may work for overlapping values of y.


#Value
#a data.table containing columns: interval_vars (from y) group_vars, value_vars (averaged to y intervals)
#one row for every interval in y, regardless of whether there were overlapping intervals in x
#xduration: the length of overlap between x and y for the specified y interval
#yduration: the length of the specified y interval
#nobs_variables:  one for each value_var. the number of nonmissing observations from x in the specified y interval.
 #missingness could be either from missing values in the original data or due to portions 
  #of that y interval not overlapping with any interval in x
#note that for periods in y not overlapping with any periods in x, no rows will be returned


interval_weighted_avg_f <- function(x, y,interval_vars,value_vars, group_vars=NULL,
                                    required_percentage=100,skip_overlap_check=FALSE){
  EVAL <- function(...)eval(parse(text=paste0(...)))
  
  ###error checks:
  
  if(x[,any(sapply(.SD,function(m){any(is.na(m))})) ,.SDcols=interval_vars]){
    stop("columns corresponding to interval_vars cannot be missing in x")
  }
  
  if(y[,any(sapply(.SD,function(m){any(is.na(m))})) ,.SDcols=interval_vars]){
    stop("columns corresponding to interval_vars cannot be missing in y")
  }
  
  if(x[,!all(sapply(.SD,is.integer)|sapply(.SD,function(x){class(x)=="Date"})),.SDcols=interval_vars]){
    stop("interval_vars must correspond to columns in x of class integer or Date")
  }
  if(x[,class(.SD[[1]])!=class(.SD[[2]]),.SDcols=interval_vars]){
    stop("interval_vars must correspond to columns in x of the same class")
  }
  
  if(y[,!all(sapply(.SD,is.integer)|sapply(.SD,function(x){class(x)=="Date"})),.SDcols=interval_vars]){
    stop("interval_vars must correspond to columns in y of class integer or Date")
  }
  if(y[,class(.SD[[1]])!=class(.SD[[2]]),.SDcols=interval_vars]){
    stop("interval_vars must correspond to columns in y of the same class")
  }
  
  #stop if there are variables specified in both groups and interval_vars
  if(!length(intersect(interval_vars,group_vars))==0){
    stop("interval_vars and group_vars cannot refer to the same column(s)")
  }
  
  if(!is.null(group_vars)){
    if(!all(group_vars %in% names(x))){
      stop("group_vars are not found in columns of x")
    }
  }
  
  print("test")
  
  #stop if start_dates are before end dats
  if(x[, sum(.SD[[2]]-.SD[[1]] <0)!=0,.SDcols=interval_vars]){
    stop("there exist values in x[[interval_vars[1] ]] that are
         less than corresponding values in  x[[interval_vars[2] ]]. 
         interval_vars must specify columns corresponding to increasing intervals")
  }
  
  #set keys for testing if there are overlaps
  #groups do not need to be in x but they do need to be in y
  if(!skip_overlap_check){
    setkeyv(x,c(group_vars,interval_vars))
    
    #stop if there are overlapping periods within groups: 
    stopifnot(nrow(foverlaps(x,x))==nrow(x))  
    print(paste(Sys.time(),"passed test to determine whether data is non-overlapping"))
  }else{
    print("Caution: skipping test to determine whether data is non-overlapping")
  }
  
  
  #create column names for variables that will be created in this function
  #create a character vector representing the 
  #second set of column names for start and end that will be created  
  #once x and y are merged via foverlaps
  #the ones prefixed with i. are from x 
  xinterval_vars <- paste0("i.",interval_vars)
  
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  dur <- "duration"
  while(dur%in% names(x)|dur%in% names(y)){
    dur <- paste0(".",dur)
  }
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  xdur <- "xduration"
  while(xdur%in% names(x)|xdur%in% names(y)){
    xdur <- paste0("i.",xdur)
  }
  
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  ydur <- "yduration"
  while(ydur%in% names(x)|ydur%in% names(y)){
    ydur <- paste0("i.",ydur)
  }
  
  #note that dur and xdur are used for calculating time-weighted averging
  
  #but for each value variable, the number of non-missing days also needs to counted separately:
  #create a character vector (the length of value_vars) that will be column names in x,y, and z that are not in use already

  #temp nobs vars will be the count (number) of non-missing obs for each row resulting from the foverlaps merge
  temp_nobs_vars <- paste("temp_nobs",value_vars, sep="_")
  for(i in 1:length(temp_nobs_vars)){
    while(temp_nobs_vars[i]%in% names(x)|temp_nobs_vars[i]%in% names(y)){
      ydur <- paste0("i.",temp_nobs_vars[i])
    }
  }
  
  #nobs vars will be the count of non-missing obs for each time-period in y (ie, summed from temp nobs)
  nobs_vars <- paste("nobs",value_vars, sep="_")
  for(i in 1:length(nobs_vars)){
    while(nobs_vars[i]%in% names(x)|nobs_vars[i]%in% names(y)){
      ydur <- paste0("i.",nobs_vars[i])
    }
  }
  
  
  #make a copy of y so a new column doesn't get created in the user's data.table
  y <- copy(y)
  
  #count length of each interval,
  #this will be used to count percentage of observed time x has in intervals of y
  
  y[,(ydur):=as.numeric( (get(interval_vars[2])-get(interval_vars[1])) + 1)]
  
  ### merge x and y ####
  #group variables in x AND y. (group_vars are known to be in x from an error check above)
  overlapping_group_vars <- intersect(group_vars,names(y)) 
  setkeyv(y,c(overlapping_group_vars, interval_vars))
  setkeyv(x,c(overlapping_group_vars, interval_vars))
  z <- foverlaps(x,y,nomatch=NULL)  
  
   #nomatch=NULL here means intervals in x that don't match to y are dropped
   #we dont' care about them because we only want averages over intervals specified in y

    #HOWEVER we *do* want intervals in y that aren't in x and these are not created 
    #afaik, foverlaps has no way of producing these *for every category defined by groups*

  #get intervals in y that were not joined to x
  setkeyv(y,c(overlapping_group_vars, interval_vars))
  setkeyv(z,c(overlapping_group_vars, interval_vars))
  non_joins <- y[!z] #data.table idiomatically: return y subsetted to rows of "not z"
        #that is, return rows of y that don't match to z
  if(nrow(non_joins)>0){
    #get all possible combinations of group variables and expand them:
    unique_id_list <- lapply(group_vars,function(r){
      x[,logical(1),keyby=r][[r]]
    })
    names(unique_id_list) <- group_vars
    q <- do.call(CJ,unique_id_list)
    
    #if there are no group_vars in y just grid expand the two data.tables together

      non_joins <- CJ.dt(non_joins,q,overlapping_group_vars)

    ##add in intervals in y that weren't matched to intervals in x 
    #separately for each combination of groups
    z <- rbindlist(list(z,non_joins),fill=TRUE)
  } 
  
  #calucate the dur of the overlap:
  #the first of the two end dates minus the second of the two start dates plus one
  z[, (dur):=as.numeric(pmin( get(xinterval_vars[2]), get(interval_vars[2]))-
                          pmax(get(xinterval_vars[1]),get(interval_vars[1]))) + 1] 
  
  #dur being missing corresponds to xinterval_vars being missing, from merging in nonmathes above
  #the duration of overlap is zero, not missing.
  EVAL("z[is.na(",dur,"),",dur,":=0]")
  
  #if value_var is missing for a row, then number of observations in this period is 0
  #otherwise, nobs is the length of the period
  for(i in 1:length(temp_nobs_vars)){
    tmp <- EVAL("z[,is.na(",value_vars[i],")]")
    EVAL("z[tmp,",temp_nobs_vars[i],":=0]")
    EVAL("z[!tmp,",temp_nobs_vars[i],":=",dur,",]")
  }
  
  
  #define xdur as the sum of durations over y intervals
  #keying by interval_vars should be the same as keying by interval_vars[1]
  #since there are assumed to be no duplicates in intervals in y within categories of group_vars
  setkeyv(z,c(group_vars,interval_vars)) 
  z[,(xdur):= sum(get(dur)),by=c(group_vars,interval_vars) ]
  
  #define nobs as the number of nobs over y intervals
  for(i in 1:length(temp_nobs_vars)){
    z[,(nobs_vars[i]):= sum(get(temp_nobs_vars[i])),by=c(group_vars,interval_vars) ]  
  }
  
  #time weighted averaging:
  #for all value vars, calculate time-weighted mean
  
  
  #time-weighted average:
  #ie sum(value1*duration)/xduration, sum(value2*duration)/xduration
  tw_avg <- paste0(value_vars,"=sum(",value_vars,"*",dur, ",na.rm=TRUE)/",nobs_vars,collapse=",")
  #xdur and ydur, and nobs_vars  provide no additional grouping here 
  #since they were created as a single value within cateogires of interval_vars and group_vars
  #but including them in the by argument ensures they get passed to the resulting q object
  q <- EVAL("z[,list(",tw_avg,"),by=c(group_vars,interval_vars,xdur,ydur,nobs_vars)]")
  
  stopifnot(q[,all(xdur<=ydur)])
  print("test8")
  #remove averages with too few observations in period
    #e.g. q[100*nobs_value/yduration < required_percentage, nobs_value:=NA]
  for(i in 1:length(value_vars)){
    EVAL("q[100*",nobs_vars[i],"/",ydur,"< required_percentage,",value_vars[i],":=NA]")  
  }
  
  
  #reorder rows and set column order
  q <- EVAL("q[order(",group_vars,",",interval_vars,")]")
  setcolorder(q, c(group_vars,interval_vars,value_vars,ydur,xdur,nobs_vars))
  q[]
}


#slow but more likely to be right since it expands the data
interval_weighted_avg_slow_f <- function(x, y,interval_vars,value_vars, group_vars=NULL,
                                         required_percentage=100,skip_overlap_check=FALSE){
  
  EVAL <- function(...)eval(parse(text=paste0(...)))
  
  overlapping_group_vars <- intersect(group_vars,names(y)) 
  
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  t <- "time"
  while(t %in% names(x)|t%in%names(y)){
    t <- paste0("i.",t)
  }
  
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  # measurement occured on this day but it might be measured as missing
  measurement <- "meas"
  while(measurement%in% names(x)|measurement%in% names(y)){
    measurement <- paste0("i.",measurement)
  }
  
  
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  xdur <- "xduration"
  while(xdur%in% names(x)|xdur%in% names(y)){
    xdur <- paste0("i.",xdur)
  }
  
  ydur <- "yduration"
  while(ydur%in% names(x)|ydur%in% names(y)){
    ydur <- paste0("i.",ydur)
  }
  
  
  #nobs vars will be the count of non-missing obs for each time-period in y 
   #(ie, summed from temp nobs)
  nobs_vars <- paste("nobs",value_vars, sep="_")
  for(i in 1:length(nobs_vars)){
    while(nobs_vars[i]%in% names(x)|nobs_vars[i]%in% names(y)){
      ydur <- paste0("i.",nobs_vars[i])
    }
  }
  

  #expand x 
  #in x values_vars represent an average over an interval
  #in expand_x, intervals are expanded such that there is now a separate row for each smallest observable 
    #increment in each interval (represented as variable t). 
    #values are repeated over rows that correspond to those increments,
    #and interval_vars columns specifying the original increments are retained
  
  x_expanded <- EVAL("x[,list(",t,"=",interval_vars[1],":",interval_vars[2],"),
       by=c(group_vars,interval_vars,value_vars)]")
  
  x_expanded[,(interval_vars):=NULL]
  x_expanded[, (measurement):=1L]
  
  y_expanded <-  EVAL("y[,list(",t,"=",interval_vars[1],":",interval_vars[2],"),
       by=c(interval_vars,overlapping_group_vars)]")
  
  #y is expanded but if grouping variables are not in y (ie if overlapping_group_vars is NULL) 
   #then there is only one set of intervals
  #in order to "complete" y, make a copy of y for groups correspnding to every combination of grouping variables
  #this will represent the desired output structure: 
   #for every combination of group_vars, have every time-interval in y
  
  #if grouping variables *are* in y...?
  
  #take unique values of every group_var
  #https://stackoverflow.com/questions/36953026/what-is-the-fastest-way-to-get-a-vector-of-sorted-unique-values-from-a-data-tabl
  unique_id_list <- lapply(group_vars,function(r){
    x[,logical(1),keyby=r][[r]]
  })
  names(unique_id_list) <- group_vars
  unique_time_list <- list(t=y_expanded[[t]])
  
  y_expanded_complete <- do.call("CJ", c(unique_time_list, unique_id_list ))
  setnames(y_expanded_complete,"t",t)
  
  #merge back in start and end times
  setkeyv(y_expanded_complete,t)
  setkeyv(y_expanded,t)
  y_expanded_complete <- y_expanded[y_expanded_complete]
  
  setkeyv(x_expanded,c(t, group_vars))
  setkeyv(y_expanded_complete,c(t, group_vars))
  
  z <- x_expanded[y_expanded_complete]

  avg_call <- paste0(value_vars,"=mean(",value_vars,",na.rm=TRUE)",collapse=",")
    paste0()
  nobs_call <- paste0(nobs_vars,"=sum(!is.na(",value_vars,"))",collapse=",")
  ydur_call <- paste0(ydur,"=.N")
  xdur_call <- paste0(xdur,"=sum(",measurement,",na.rm=TRUE)")
  
  out <- EVAL("z[,","list(",avg_call,",",ydur_call,",",xdur_call,",",nobs_call,")" ,
              ",by=c(interval_vars,group_vars)]")
  
  for(i in 1:length(value_vars)){
    EVAL("out[100*",nobs_vars[i],"/",ydur,"< required_percentage,",value_vars[i],":=NA]")  
  }
  
  
  out <- EVAL("out[order(",group_vars,",",interval_vars,")]")
  setcolorder(out, c(group_vars,interval_vars,value_vars,ydur,xdur,nobs_vars))
  out[]
  
  
}

remove_overlaps <- function(x){
  x <- copy(x)
  x[, g := c(0L, cumsum(shift(start, -1L) > cummax(end))[-.N]), by=.(id)]
  
  #cut those intervals into non-overlapping ones
  itvl <- x[, {
    s <- sort(c(start - 1L, start, end, end + 1L))
    as.data.table(matrix(s[s %between% c(min(start), max(end))], ncol=2L, byrow=TRUE))
  }, by=.(id, g)]
  
  #get OP's desired output using non-equi join
  x[itvl, on=.(id, start<=V1, end>=V1),
    .(id1=id, start1=V1, end1=V2, i.start1=x.start, i.end1=x.end, obs),
    allow.cartesian=TRUE]
  
}



