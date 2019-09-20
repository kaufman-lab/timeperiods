library(data.table)



#function to take non-overlapping time-integrated averages observed over a defined period
#and average those observations up (or down) to a specified schedule
#this schedule does not necessarily need to align to the periods

#for example, take observations on the two-week scale over decades and 
 #calculate averages for calendar year

#alternatively, take observations over periods of variable lengths between 6 and 14 days 
 #and average them down to the one-week schedule

#do this within an arbitrary number of groups
#and allow specification of what percentage of time within the specified schedule needs to be 
 #observed in order to not return NA. default is 100

#note that for periods in y not overlapping with any periods in x, no rows will be returned

#interaval_vars is a length-2 vector of shared column names
 #in x an y corresponding to the start and end dates
#group_vars is a character vector of column names in x 
 #that designate groups


#by default, the function ensures that periods are non-overlapping in x and y
#this is slow, but is a necessary condition of this function
#if you're sure your intervals are non-overlapping you can skip this
 #check by specifying skip_overlap_check=TRUE

interval_weighted_avg_f <- function(x, y,interval_vars,value_vars, group_vars=NULL,
                                    required_percentage=100,skip_overlap_check=FALSE){
  EVAL <- function(...)eval(parse(text=paste0(...)))
  
  ###error checks:
  #stop if there are variables specified in both groups and interval_vars
  stopifnot(length(intersect(interval_vars,group_vars))==0)
  
  if(!is.null(group_vars)){
    #stop if group vars are not columns in x
    stopifnot(all(group_vars %in% names(x)))
    
    #stop if group vars exist in y
    if(any(group_vars %in% names(y))){stop("grouping vars in y not implemented")}
  }
  
  
  
  #stop if start_dates are before end dats
  stopifnot(x[, sum(.SD[[2]]-.SD[[1]] <0)==0,.SDcols=interval_vars])
  print(paste(Sys.time(),"passed test to ensure interval starts are less than interval ends"))
  
  #set keys for testing if there are overlaps
  #groups do not need to be in x but they do need to be in y
  if(!skip_overlap_check){
    setkeyv(y,interval_vars)
    setkeyv(x,c(group_vars,interval_vars))
    
    #stop if there are overlapping periods within groups: 
    stopifnot(nrow(foverlaps(y,y))==nrow(y))
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
    
    #find a name for a new  "duration" variable that's not already taken
    dur <- "duration"
    while(dur%in% names(x)|dur%in% names(y)){
      dur <- paste0(".",dur)
    }
    #find a name for a new  "xduration" and "yduration" variables that's not already taken
    xdur <- "xduration"
    while(xdur%in% names(x)|xdur%in% names(y)){
      xdur <- paste0("i.",xdur)
    }
    ydur <- "yduration"
    while(ydur%in% names(x)|ydur%in% names(y)){
      ydur <- paste0("i.",ydur)
    }
    

  #make a copy of y so a new column doesn't get created in the user's data.table
  y <- copy(y)
  
  #count length of each interval,
  #this will be used to count percentage of observed time x has in intervals of y
  
  y[,(ydur):=as.numeric( (get(interval_vars[2])-get(interval_vars[1])) + 1)]
  
  setkeyv(y,interval_vars)
  setkeyv(x,interval_vars)
  z <- foverlaps(x,y) 
  
  #calucate the dur of the overlap:
  #the first of the two end dates minus the second of the two start dates plus one
  z[, (dur):=as.numeric(pmin( get(xinterval_vars[2]), get(interval_vars[2]))-
                               pmax(get(xinterval_vars[1]),get(interval_vars[1]))) + 1] 

  
  #keying by interval_vars should be the same as keying by interval_vars[1]
  #since there are assumed to be no duplicates in intervals in y within categories of group_vars
  setkeyv(z,c(group_vars,interval_vars)) 
  z[,(xdur):= sum(get(dur)),by=c(group_vars,interval_vars) ]
  
  
  
  #time weighted averaging:
  #for all value vars, calculate time-weighted mean

  #xdur and ydur provide no additional grouping here 
   #since they were created as a single value within cateogires of interval_vars and group_vars
  q <- z[,lapply(.SD,function(x){
    sum(x*get(dur))/.BY[[xdur]]
  }),
         by=c(group_vars,interval_vars,xdur,ydur),
         .SDcols=value_vars]
  
  stopifnot(q[,all(xdur<=ydur)])

  #remove averages with too few observations in period
  q[q[,100*get(xdur)/get(ydur) <required_percentage],
    (value_vars) :=NA]
  
  q <- EVAL("q[order(",group_vars,",",interval_vars,")]")
  setcolorder(q, c(group_vars,interval_vars,value_vars))
  q[]
}


#slow but more likely to be right since it expands the data
interval_weighted_avg_slow_f <- function(x, y,interval_vars,value_vars, group_vars=NULL,
                                    required_percentage=100,skip_overlap_check=FALSE){
  
  t <- "time"
  
  while(t %in% names(x)|t%in%names(y))
    t <- paste0("i.",t)
  
  EVAL <- function(...)eval(parse(text=paste0(...)))
  x_expanded <- EVAL("x[,list(",t,"=",interval_vars[1],":",interval_vars[2],"),
       by=c(group_vars,interval_vars,value_vars)]")
  x_expanded[,(interval_vars):=NULL]
  
  y_expanded <-  EVAL("y[,list(",t,"=",interval_vars[1],":",interval_vars[2],"),
       by=interval_vars]")
  
  #for every combination of group vars, create a row for each time in y
  
  
  unique_id_list <- as.list(x[,lapply(.SD,unique),.SDcols=group_vars])
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
  as.data.frame(z[id==1&id2==1])
  
  avg_call <-
    paste0("list(",paste0(value_vars,"=mean(",value_vars,")",collapse=","),")")
  
  out <- EVAL("z[,",avg_call ,",by=c(interval_vars,group_vars)]")
  
  
  out <- EVAL("out[order(",group_vars,",",interval_vars,")]")
  setcolorder(out, c(group_vars,interval_vars,value_vars))
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

  
 
  