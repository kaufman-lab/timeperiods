

library(data.table)

create_unused_name <- function(x,reserved_cols){
  for(i in 1:length(x)){
    while(x[i] %in% reserved_cols){
      x[i] <- paste0("i.",x[i])
    }  
  }
  x
}


#a function to grid expand an arbitrary number of data.tables
#largely based on https://github.com/lockedata/optiRum/blob/master/R/CJ.dt.R
#groups is a character vector corresponding to column names of grouping vars  
#in all of the data.tables
CJ.dt <- function(...,groups=NULL) {
  l = list(...)
  EVAL <- function(...)eval(parse(text=paste0(...)))
  if(any(sapply(l,nrow)==0)){stop("one or more data.tables have no rows")}
  
  
  #while kvar is in names of any of the data.tables, keep prepending i. to it until it's not
  kvar <- create_unused_name("k",unlist(lapply(l,names)))
  
  invars <- create_unused_name(paste0("in",1:length(l)),
                             unlist(lapply(l,names)))
  
  for(i in 1:length(l)){
    l[[i]][,(kvar):=1]
    l[[i]][,(invars[i]):=TRUE]
    setkeyv(l[[i]],c(kvar,groups))
  }

  mymerge = function(x,y) x[y, allow.cartesian = TRUE]
  out <- Reduce(mymerge,l)
  out[,(kvar):=NULL]
  
  for(i in 1:length(l)){
    l[[i]][,(kvar):=NULL]
    l[[i]][,(invars[i]):=NULL]
  }
  
  out <- EVAL("out[",paste0(paste0("!is.na(",invars,")"),collapse="&"),"]")
  out[,(invars):=NULL]
  out[]
}


cummax.Date <- function(x) as.Date(cummax(as.integer(x)),'1970-01-01')



interval_weighted_avg <- function(x, y,value_vars,
                                    required_percentage=100,skip_overlap_check=FALSE,
                                    verbose=FALSE
){
  
  
}

#function to take values over defined periods which are non-overlapping within individuals specified by group_vars
#average those values up (or down) to a specified schedule
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
#intervals in x must be non-overlapping within groups that are combinations of the group_vars columns
#a unit difference in these interval variables is assumed to be the smallest observable increment

#group_vars is a character vector corresponding to columns in x and y that represent groups within which to take weighted averages

##value_vars are character vectores corresponding to columns in x which represent average values over the 
#intervals specified in the columns corresponding to interval_vars


#by default, the function ensures that periods are non-overlapping in x and y
#this is slow, but is a necessary condition of this function 
#if you're sure your intervals are non-overlapping you can skip this
#check by specifying skip_overlap_check=TRUE
#WARNING, it's possible skipping this check may result in a completely wrong, meaningless return value without error

#partially overlapping intervals in y are allowed but repeated identical intervals within groups will be deduplicated (with warning)


#required_percentage=100 basically means na.rm=TRUE
#if that percentage is less than 100, then this will be the percentage of nonmissing observations 
 #that need to exist in a period of y. if the percentage of nonmissing observations is less than required_percentage 
 #then the value returned for that period is NA


#Value
#a data.table containing columns: interval_vars (from y) group_vars, value_vars (averaged to y intervals)
#one row for every interval in y, regardless of whether there were overlapping intervals in x
#xduration: the length of overlap between x and y for the specified y interval
#yduration: the length of the specified y interval
#nobs_variables:  one for each value_var. the number of nonmissing observations from x in the specified y interval.
#missingness could be either from missing values in the original data or due to portions 
#of that y interval not overlapping with any interval in x
#note that for periods in y not overlapping with any periods in x, no rows will be returned

#KEYS of x AND Y WILL BE ALTERED.

interval_weighted_avg_f <- function(x, y,interval_vars,value_vars, group_vars=NULL,
                                    required_percentage=100,skip_overlap_check=FALSE,
                                    verbose=FALSE
                                    ){
  EVAL <- function(...)eval(parse(text=paste0(...)))
  setkeyv(x,c(group_vars,interval_vars))
  setkeyv(y,c(group_vars,interval_vars))
  
  
  if( any(c("yduration","xduration","xminstart","xmaxend")%in% c(interval_vars,value_vars,group_vars))){
    stop(paste0("column(s) named 'yduration', 'xduration', 'xminstart', or 'xmaxend' has been detected in interval_vars,",
                " value_vars, or group_vars.         These column names ('yduration', 'xduration', 'xminstart', or 'xmaxend')",
                " are reserved for the output. please rename this (or these) column(s) in x/y/interval_vars/value_vars/group_vars."))
  }
  
  if(!is.null(group_vars)){
    if(!all(group_vars %in% names(x))){
      stop("every value in group_vars must be a columname in x")
    }
    if(!all(group_vars %in% names(y))){
      stop("every value in group_vars must be a columname in y")
    }
  }
  
  
   if(!all(interval_vars %in% names(x))){
     stop("every value in interval_vars must be a columname in x")
   }
   if(!all(interval_vars %in% names(y))){
     stop("every value in interval_vars must be a columname in y")
   }
  
  if(!all(value_vars %in% names(x))){
    stop("every value in value_vars must be a columname in x")
  }
  
  
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
  
  if(y[,!all(sapply(.SD,is.integer)|sapply(.SD,function(x){any(class(x)%in%c("Date"))})),.SDcols=interval_vars]){
    stop("interval_vars must correspond to columns in y of class integer or Date")
  }
  if(y[,class(.SD[[1]])!=class(.SD[[2]]),.SDcols=interval_vars]){
    stop("interval_vars must correspond to columns in y of the same class")
  }
  
  #stop if there are variables specified in both groups and interval_vars
  if(!length(intersect(interval_vars,group_vars))==0){
    stop("interval_vars and group_vars cannot refer to the same column(s)")
  }
  
  if(!length(intersect(value_vars,group_vars))==0){
    stop("value_vars and group_vars cannot refer to the same column(s)")
  }
  
  if(!length(intersect(value_vars,interval_vars))==0){
    stop("value_vars and interval_vars cannot refer to the same column(s)")
  }
  
  
  #stop if start_dates are before end dates
  if(x[, sum(.SD[[2]]-.SD[[1]] <0)!=0,.SDcols=interval_vars]){
    stop("there exist values in x[[interval_vars[1] ]] that are
         less than corresponding values in  x[[interval_vars[2] ]]. 
         interval_vars must specify columns corresponding to increasing intervals")
  }
  
  #check for exact overlaps in x
  if(sum(duplicated(x[,c(..group_vars,..interval_vars)]))!=0){
    stop("sum(duplicated(x[,c(..group_vars,..interval_vars)]))!=0 is not TRUE. 
         there are replicate/duplicate intervals within groups. 
         If you wish to average these together, then do this first")
  }
  
  
  ydups <- duplicated(y[,c(..group_vars,..interval_vars)])
  if(sum(ydups)!=0){
    warning("sum(duplicated(y[,c(..group_vars,..interval_vars)]))!=0 is not TRUE. 
         there are replicate/duplicate intervals within groups of y. 
         removing these duplicated rows automatically")
    y <- y[!ydups]
  }
  
  if(!skip_overlap_check){

    #stop if there are overlapping periods within groups: 
    stopifnot(nrow(foverlaps(x,x))==nrow(x))  
    print(paste(Sys.time(),"passed errorcheck: x is non-overlapping."))
  }else{
    message("skipping errorcheck. if intervals in x are  overlapping, incorrect results may be returned without error.")
  }
  
  
  ### merge x and y ####

  #in order to guarantee that there aren't column name conflicts
     #between the temporary variables such as interval_start 
    #and the user-provided column names such as group_vars, value_vars, interval_vars
  #create temporary names for user-provided variables
  gvars <- if(!is.null(group_vars)){paste0("g",1:length(group_vars))}else{NULL}
  vvars <- paste0("v",1:length(value_vars))
  ivars <- paste0("i",1:2)
  i.ivars <- paste0("i.",ivars)
  
  xx <- proc.time()
  
  ### non-equi join of x and y (right join to y) and do an immediate group by EACHI####
  #subset x and y to only variables subsequently. 
   #this avoids  naming conflicts with extraneous variables which don't need to be part of the join.
  z <- foverlaps(x=y[,c(group_vars,interval_vars),with=FALSE],
                 y=x[,c(group_vars,interval_vars,value_vars),with=FALSE]) 
  #i.ivars are from x=y, ivars are from y=x
  
  if(verbose){
  print("foverlaps duration:")
  print(proc.time()-xx)
  xx <- proc.time()}
  
  
 ##rename all user-supplied columns to the column names pre-specified above. 
  #this avoids naming conflicts with temp variables such as interval_end, interval_start, dur, and the other temporary variables
  #columns will be renamed back to their original names at the end of the function
  setnames(z, 
           c(group_vars,interval_vars,paste0("i.",interval_vars),value_vars),
           c(gvars,ivars,i.ivars,vvars)
  )
  
  EVAL(paste0("z[,interval_end:=pmin(",i.ivars[2],", ",ivars[2],")]"))
  EVAL(paste0("z[,interval_start:=pmax(",i.ivars[1],",",ivars[1],")] "))
  
  if(verbose){
  print("pmin/pmax duration:")
  print(proc.time()-xx)}
  xx <- proc.time()
  
  z[,dur := as.integer(interval_end-interval_start+1L)]
  
  #temp_nobs_vars is the count of nonmissing observations
   #this is 0 when the value_var is missing and dur otherwise. 
   #calculated as !is.na(value_var)*dur
  temp_nobs_vars <- paste0("vnm",1:length(value_vars))
   for(i in 1:length(vvars)){
      set(z, 
          j=temp_nobs_vars[i], 
          value=as.integer(!is.na(z[[vvars[i]]]))*z[["dur"]]
      )
   }
  
  
  prod_vars <- paste0("p",1:length(value_vars))
  for(i in 1:length(vvars)){
    set(z, 
        j=prod_vars[i], 
        value=z[[vvars[i]]]*z[["dur"]]
    )
  }
  
  if(verbose){
    print("pre-grouping var calculation:")
  print(proc.time()-xx)
  xx <- proc.time()}

  #nobs vars will be the count of non-missing obs for each time-period in y (ie, summed from temp nobs)
  nobs_vars <- paste("nobs",value_vars, sep="_")
  sumprod_vars <- paste("sumproduct",value_vars, sep="_")
  
  ###gforce data.table statement: sum, min, max are optimized for use in by statement.
    #but syntactically this is limited. 
      # x[, list(range=max(x)-min(x))] is NOT optimized 
      #but x[, list(min=min(x), max=max(x))] is optimized
  q <- EVAL(
    paste0(
      "z[,list(",
      "xduration=sum(dur),",
      paste0(nobs_vars,"=sum(",temp_nobs_vars,",na.rm=TRUE)",collapse=","),
      ",",
      paste0(sumprod_vars,"=sum(",prod_vars,",na.rm=TRUE)",collapse=","),
      ",xminstart=min(interval_start),",
      "xmaxend=max(interval_end)",
      "),",
      "keyby=c(gvars,i.ivars)]"
    )
  )
  
  if(verbose){
  print("gforce:")
  print(proc.time()-xx)
  xx <- proc.time()}
  
  #note that weighted mean isn't directly calculated in the by
    #because a complex formula that data.table gforce isn't capable of (yet)
    
  
  #calculate the mean as value_vars=sum(value_vars*dur)/sum(temp_nobs_vars) by group_vars and interval_vars
  #note that I could just as use temp_nobs_vars instead of dur as the product
   #but they're only different when value_vars is missing (by definition)
     #so there's no point in bringing al the temp_nobs_vars columns into the j statement when one column can accomplish the same thing
  EVAL(paste0("q[,`:=`(",paste0(value_vars,"=",sumprod_vars,"/",nobs_vars,collapse=","),")]"))

  #remove temporary column sum_prod_vars
  EVAL(paste0("q[,`:=`(",paste0(sumprod_vars,"=NULL",collapse=","),")]"))
  
  #count length of each interval,
  #this will be used to count percentage of observed time x has in intervals of y
  q[,yduration:=as.numeric( .SD[[2]]-.SD[[1]] + 1),.SDcols=c(i.ivars)]
  
  q[is.na(xduration), c("xduration",nobs_vars):=0]
  stopifnot(q[,all(xduration<=yduration)])

    #remove averages with too few observations in period
  #e.g. q[100*nobs_value/yduration < required_percentage, nobs_value:=NA]
  for(i in 1:length(value_vars)){
    EVAL(paste0("q[100*",nobs_vars[i],"/yduration < required_percentage, ", value_vars[i],":=as.numeric(NA)]"))
  }
  
  setnames(q, c(i.ivars,gvars),c(interval_vars,group_vars))
  setcolorder(q, c(group_vars,interval_vars,value_vars,"yduration","xduration",nobs_vars,
                   "xminstart","xmaxend"))
  
  if(verbose){
  print("everything else:")
  print(proc.time()-xx)}
  
  q[]
  }



#slower algorithm. used for error checking since this is the simpler approach and less likely to have errors.
#Not recommended for large datasets since this expands the data into a row for every increment.
interval_weighted_avg_slow_f <- function(x, y,interval_vars,value_vars, group_vars=NULL,
                                         required_percentage=100,skip_overlap_check=FALSE){
  
  EVAL <- function(...)eval(parse(text=paste0(...)))
  
  
  if(!is.null(group_vars)){
    if(!all(group_vars %in% names(x))){
      stop("every value in group_vars must be a columname in x")
    }
    if(!all(group_vars %in% names(y))){
      stop("every value in group_vars must be a columname in y")
    }
  }
  
  #check for exact overlaps
  if(sum(duplicated(x[,c(..group_vars,..interval_vars)]))!=0){
    stop("sum(duplicated(x[,c(..group_vars,..interval_vars)]))!=0 is not TRUE. 
         there are replicate/duplicate intervals within groups. 
         If you wish to average these together, then do this first")
  }
  
  
  ydups <- duplicated(y[,c(..group_vars,..interval_vars)])
  if(sum(ydups)!=0){
    warning("sum(duplicated(y[,c(..group_vars,..interval_vars)]))!=0 is not TRUE. 
         there are replicate/duplicate intervals within groups of y. 
         removing these duplicated rows automatically")
    y <- y[!ydups]
  }
  
  
  
  
  #set keys for testing if there are overlaps
  #groups do not need to be in x but they do need to be in y
  if(!skip_overlap_check){
    
    setkeyv(x,c(group_vars,interval_vars))
    
    #stop if there are overlapping periods within groups: 
    stopifnot(nrow(foverlaps(x,x))==nrow(x))  
    print(paste(Sys.time(),"passed errorcheck: x is non-overlapping."))
  }else{
    warning("skipping errorcheck. if intervals in x are  overlapping, incorrect results may be returned without error.")
  }
  
  
  
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  t <- create_unused_name("time",c(names(x),names(y)))
  
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  # measurement occured on this day but it might be measured as missing
  measurement <- create_unused_name("meas",c(names(x),names(y)))
  
  
  
  if("yduration"%in%names(x)|"yduration"%in%names(y)){
    stop("names(x) or names(y) contains the column name 'yduration'. A column named 'yduration' cannot be present in x or y 
         because it will be a special column in the return value. please rename this column.")
  }
  ydur <- "yduration"
  
  if("xduration"%in%names(x)|"xduration"%in%names(y)){
    stop("names(x) or names(y) contains the column name 'xduration'. A column named 'xduration' cannot be present in x or y 
         because it will be a special column in the return value. please rename this column.")
  }
  xdur <- "xduration"
  
  
  if("xminstart"%in%names(x)|"xminstart"%in%names(y)){
    stop("names(x) or names(y) contains the column name 'xduration'. A column named 'xminstart' cannot be present in x or y 
         because it will be a special column in the return value. please rename this column.")
  }
  
  if("xmaxend"%in%names(x)|"xmaxend"%in%names(y)){
    stop("names(x) or names(y) contains the column name 'xduration'. A column named 'xmaxend' cannot be present in x or y 
         because it will be a special column in the return value. please rename this column.")
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
                      by=c(interval_vars,group_vars)]")
  


  setkeyv(x_expanded,c(t, group_vars))
  setkeyv(y_expanded,c(t, group_vars))
  
  z <- x_expanded[y_expanded]
  
  avg_call <- paste0(value_vars,"=mean(",value_vars,",na.rm=TRUE)",collapse=",")
  paste0()
  nobs_call <- paste0(nobs_vars,"=sum(!is.na(",value_vars,"))",collapse=",")
  ydur_call <- paste0(ydur,"=length(unique(",t,"))")
  xdur_call <- paste0(xdur,"=sum(",measurement,",na.rm=TRUE)")
  
  #get the min start and max end dates for intervals that were actually provided as inputs in x
  #as of 1/12/2020 I think gforce does not work with :=, 
    #so this is written to create a separate table that is then merged
  minmaxtable <- 
    EVAL(
      paste0(
       "z[!is.na(",measurement,"),list(xminstart=min(time),xmaxend=max(time)),by=c(interval_vars,group_vars)]"    
      )
    )
  
  if(class(x[[interval_vars[1]]])=="Date"){
    minmaxtable[, xminstart:=as.Date(xminstart,origin="1970-01-01")]  
    minmaxtable[, xmaxend:=as.Date(xmaxend,origin="1970-01-01")]
  }
  
    
  
  out <- EVAL(
    paste0(
    "z[,","list(",avg_call,",",ydur_call,",",xdur_call,",",nobs_call,")" ,
              ",by=c(interval_vars,group_vars)]"
    )
    )
  
  stopifnot(EVAL("out[,all(",xdur,"<=",ydur,")]"))
  
  ##merge in minmaxtable
  setkeyv(minmaxtable,c(group_vars,interval_vars))
  setkeyv(out,c(group_vars,interval_vars))
  
  out <- minmaxtable[out]
  
  
  for(i in 1:length(value_vars)){
    EVAL("out[100*",nobs_vars[i],"/",ydur,"< required_percentage,",value_vars[i],":=NA]")  
  }

  setcolorder(out, c(group_vars,interval_vars,value_vars,ydur,xdur,nobs_vars,
                     "xminstart","xmaxend"))
  
  setkeyv(out,c(group_vars,interval_vars))
  out[]
}


#value, returns the new intervals along with the old intervals with column names paste("o.",interval_vars) o is for "original"
remove_overlaps <- function(x,interval_vars,group_vars=NULL){
  EVAL <- function(...)eval(parse(text=paste0(...)))
  
  
  xkey <- key(x)
  
  variable_var <- create_unused_name("variable",names(x))
  i.interval_vars <- paste0("i.", interval_vars)
    if(any(i.interval_vars%in% names(x))){
      stop("names(x) contains columns ",paste0(interval_vars,collapse=" "),
      " and at least one column named ",paste0(i.interval_vars,collapse=" "),". Columns named ",paste0(i.interval_vars,collapse=" "),
      " cannot be present in x because they are reserved for use by foverlaps")
      }
  
  value_var <- create_unused_name("value",names(x))
  open_var <- create_unused_name("open",names(x))
  close_var <- create_unused_name("close",names(x))
  is_end_var <- create_unused_name("is_end",names(x))  
  
  end_next_var <- create_unused_name("end_next",names(x))
  value_next_var <- create_unused_name("value_next",names(x))
  
  xd <- melt(x[,c(interval_vars,group_vars),with=FALSE],id.vars=group_vars,
             measure.vars=interval_vars,
             value.name=value_var,
             variable.name=variable_var)
  
  
  
  ##create end variable:
  EVAL("xd[",variable_var,"==interval_vars[2],(is_end_var):=TRUE]")
  EVAL("xd[",variable_var,"==interval_vars[1],(is_end_var):=FALSE]")
  setorderv(xd,c(group_vars, value_var,is_end_var))
  
  EVAL("xd[,(end_next_var):=shift(",is_end_var,",type='lead'),by=group_vars]")
  EVAL("xd[,(value_next_var):=shift(",value_var,",type='lead'),by=group_vars]")
  
  #the last row is missing because you can't get the next row when there aren't any more rows!
   #we don't need that row where end_next is missing so exclude it.
  #when data.table veresion 1.12.3 you can use fifelse to avoid coercing dates to numeric
  #in the mean time use dplyr if_else
  temp <- EVAL("xd[,.SD[!is.na(",end_next_var,"),list(
   ",interval_vars[1],"=dplyr::if_else(!",is_end_var,",",value_var,",",value_var,"+1L),
    ",interval_vars[2],"=dplyr::if_else(!",end_next_var,",",value_next_var,"-1L,",value_next_var,")
  )],by=group_vars]")
  
  temp <- EVAL("temp[",interval_vars[2],">=",interval_vars[1],"]") 

  
  setkeyv(temp,c(group_vars,interval_vars))
  if(!is.null(key(x))) setkeyv(x,c(group_vars,interval_vars))
  
  out <- foverlaps(x,temp)
  setorderv(out, c(group_vars,
                   paste0("i.",interval_vars),
                   interval_vars))
  setcolorder(out, c(group_vars,interval_vars,paste0("i.",interval_vars)))
  setnames(out, i.interval_vars, paste0("o.",interval_vars))
  
  setkeyv(x, xkey)
  out
}
