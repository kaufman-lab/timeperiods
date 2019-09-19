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

#interaval_vars is a length-2 vector of shared column names
 #in x an y corresponding to the start and end dates
#group_vars is a character vector of column names in x (and possibly shared in y) 
 #that designate groups
interval_weighted_avg_f <- function(x, y,interval_vars,value_vars, group_vars=NULL,
                                    required_percentage=100,skip_overlap_check=FALSE){
  y <- copy(y)
  #stop if there are variables specified in both groups and interval_vars
  stopifnot(length(intersect(interval_vars,group_vars))==0)
  
  #stop if group vars are not columns in y
  if(!is.null(group_vars)){
    stopifnot(all(group_vars %in% names(x)))
  }
  
  #stop if start_dates are before end dats
  stopifnot(x[, sum(.SD[[2]]-.SD[[1]] <0)==0,.SDcols=interval_vars])
  print(paste(Sys.time(),"passed test to ensure interval starts are less than interval ends"))
  
  #set keys for testing if there are overlaps
  #groups do not need to be in x but they do need to be in y
  if(!skip_overlap_check){
    setkeyv(y,c(intersect(group_vars,names(y)), interval_vars))
    setkeyv(x,c(group_vars,interval_vars))
    
    #stop if there are overlapping periods within groups: 
    stopifnot(nrow(foverlaps(y,y))==nrow(y))
    stopifnot(nrow(foverlaps(x,x))==nrow(x))  
    print(paste(Sys.time(),"passed test to determine whether data is non-overlapping"))
  }else{
    print("skipping test to determine whether data is non-overlapping")
  }
  
  print(paste(Sys.time(),"begin setting final y keys"))
  if(any(group_vars %in% names(y))){
    setkeyv(y,c(intersect(group_vars,names(y)), interval_vars))
  }else{
    setkeyv(y,interval_vars)
  }
  print(paste(Sys.time(),"setting final y keys complete"))
  
  
  print(paste(Sys.time(),"begin setting final x keys"))
  if(any(group_vars %in% names(y))){
    setkeyv(x,c(intersect(group_vars,names(y)), interval_vars))
  }else{
    setkeyv(x,interval_vars)
  }
  print(paste(Sys.time(),"setting final x keys complete"))
  
  
  
  stopifnot(!"yduration"%in% c(names(y),names(x)))
  
  group_vars_y <- intersect(group_vars,names(y))
  if(length(group_vars_y)==0) group_vars_y <- NULL
  
  print(paste(Sys.time(),"begin foverlaps"))
  
  #make sure the names start and end aren't already in y (other than interval_vars)
  stopifnot(!any(setdiff(c("start","end"),interval_vars) %in%names(y)))
  stopifnot(!any(setdiff(c("start","end"),interval_vars) %in%names(x)))
  setnames(y, interval_vars, c("start","end"))
  
  #count length of each interval, within groups of y if y has groups
  #this will be used to count percentage of observed time x has in intervals of y
  y[,yduration:=as.numeric((end-start)+1),by=group_vars_y]
  
  z <- foverlaps(x,y) #this will result in a data.table with two sets of start and end date columns
   #the ones prefixed with i. are from x 
  
  setnames(z, interval_vars, c("i.start","i.end"))
  
  print(paste(Sys.time(),"foverlaps complete"))
  
  stopifnot(!"zzziii_max_start"%in% names(z))
  stopifnot(!"zzziii_min_end"%in% names(z))
  
  print(paste(Sys.time(),"begin pmin/pmax"))
  z[, zzziii_max_start:=pmax(start, i.start)]
  z[, zzziii_min_end:=pmin(end,i.end)]
  print(paste(Sys.time(),"pmin/pmax complete"))
  
  stopifnot(!"duration"%in% c(names(y),names(x)))
  stopifnot(!"xduration"%in% c(names(y),names(x)))
  
  z[, duration:=as.numeric((zzziii_min_end-zzziii_max_start)+1)] #length of each interval
  
  print(paste(Sys.time(),"begin set z key"))
  setkeyv(z,c(group_vars,c("start","end")))
  print(paste(Sys.time(),"set z key complete"))
  
  print(paste(Sys.time(),"begin xduration"))
  z[,xduration:= sum(duration),by=c(group_vars,"start") ]
  print(paste(Sys.time(),"xduration complete"))
  
  #time weighted average call:
  print(paste(Sys.time(),"begin time-weighted averaging"))
  
  #for all value vars, calculate time-weighted mean
  tw <- paste0("list(",paste0(value_vars,"=sum(",value_vars,"*duration)/xduration[1]",collapse=","),")")
  q <- z[,eval(parse(text=tw)),
         by=c(group_vars,"start","end","xduration","yduration")]
  print(paste(Sys.time(),"time-weighted averaging complete"))
  
  
  print(paste(Sys.time(),"begin duration check"))
  stopifnot(q[,all(xduration<=yduration)])
  print(paste(Sys.time(),"duration check complete"))
  
  print(paste(Sys.time(),"begin removing durations below required_percentage"))
  set(q,q[,which(100*xduration/yduration <required_percentage)],
      j=value_vars,
      value=NA)
  print(paste(Sys.time(),"complete"))
  
  setnames(q, c("start","end"),interval_vars)
  q
  
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

  
 
  