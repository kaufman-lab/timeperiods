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
#intervals must be non-overlapping within groups that are combinations of the group_vars columns
#a unit difference in these interval variables is assumed to be the smallest observable increment

#group_vars is a character vector corresponding to columns in x that represent groups

##value_vars are character vectores corresponding to columns in x which represent average values over the 
#intervals specified in the columns corresponding to interval_vars


#by default, the function ensures that periods are overlapping in x and y
#this is slow, but is a necessary condition of this function 
#if you're sure your intervals are non-overlapping you can skip this
#check by specifying skip_overlap_check=TRUE
#WARNING, it's possible skipping this check may result in a completely wrong, meaningless return value without error

#partially overlapping intervals in y are allowed but repeated identical intervals within groups will be deduplicated (with warning)


#required_percentage=100 basically means na.rm=TRUE
#if that percentage is less than 100, then this will be the percentage of nonmissing observations 
 #that need to exist in a period of y. if the percentage of nonmissing observations is less than required_percentage 
 #then the value returned for that period is NA

 #

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

    ###Variable names: reserved (these will be in the return data.table)
  
  xinterval_vars <- paste0("i.",interval_vars)
  #columns that will be named by foverlaps
  ##the ones prefixed with i. are from x 
  if(any(xinterval_vars%in% names(x)) | any(xinterval_vars%in% names(y)) ){
    stop("names(x) or names(y) contains columns ",paste0(interval_vars,collapse=" "),
         " and at least one column named ",paste0(xinterval_vars,collapse=" "),". Columns named ",paste0(xinterval_vars,collapse=" "),
         " cannot be present in x or y because they are reserved for use by foverlaps. please rename this/these column(s).")
  }

  if("yduration"%in%names(x)|"yduration"%in%names(y)){
    stop("names(x) or names(y) contains the column name 'yduration'. A column named 'yduration' cannot be present in x or y 
         because it will be a special column in the return value. please rename this column.")
  }
  
  if("xduration"%in%names(x)|"xduration"%in%names(y)){
    stop("names(x) or names(y) contains the column name 'xduration'. A column named 'xduration' cannot be present in x or y 
         because it will be a special column in the return value. please rename this column.")
  }
  
  ###nonreserved variable names--temporary variables that can be named anything and but shouldn't conflict with existing varnames:
  
  
  
  #create a length-1 character vector that will be a column name in x,y, and z that is not in use already
  
  dur <- create_unused_name("duration",c(names(x),names(y)))
  
  #temp nobs vars will be the count (number) of non-missing obs for each row resulting from the foverlaps merge
  
  temp_nobs_vars <- create_unused_name(paste("temp_nobs",value_vars, sep="_"), c(names(x),names(y)))
  
  
  #nobs vars will be the count of non-missing obs for each time-period in y (ie, summed from temp nobs)
  nobs_vars <- create_unused_name(paste("nobs",value_vars, sep="_"), c(names(x),names(y)))

  
  ##error checking:
  
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
  
  
  #stop if start_dates are before end dats
  if(x[, sum(.SD[[2]]-.SD[[1]] <0)!=0,.SDcols=interval_vars]){
    stop("there exist values in x[[interval_vars[1] ]] that are
         less than corresponding values in  x[[interval_vars[2] ]]. 
         interval_vars must specify columns corresponding to increasing intervals")
  }
  
  #check for exact overlaps
  if(sum(duplicated(x[,c(..group_vars,..interval_vars)]))!=0){
    stop("sum(duplicated(x[,c(..group_vars,..interval_vars)]))!=0 is not TRUE. 
         there are replicate/duplicate intervals within groups. 
         If you wish to average these together, then do this first")
  }
  
  group_vars_y <- intersect(group_vars,names(y)) 
  
  ydups <- duplicated(y[,c(..group_vars_y,..interval_vars)])
  if(sum(ydups)!=0){
    warning("sum(duplicated(y[,c(..group_vars_y,..interval_vars)]))!=0 is not TRUE. 
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
  
  #make a copy of y so a new column doesn't get created in the user's data.table
  y <- copy(y)
  
  #count length of each interval,
  #this will be used to count percentage of observed time x has in intervals of y
  y[,yduration:=as.numeric( (get(interval_vars[2])-get(interval_vars[1])) + 1)]
  
  ### merge x and y ####
  #group_vars_y are group variables in x AND y. (group_vars are known to be in x from an error check above)
  setkeyv(y,c(group_vars_y, interval_vars))
  setkeyv(x,c(group_vars_y, interval_vars))
  z <- foverlaps(x,y,nomatch=NULL)  
  
  #nomatch=NULL here means intervals in x that don't match to y are dropped
  #we dont' care about them because we only want averages over intervals specified in y
  
  #HOWEVER we *do* want intervals in y that aren't in x and these are not created 
  #afaik, foverlaps has no way of producing these 
  
  #get intervals in y that were not joined to x
  setkeyv(y,c(group_vars_y, interval_vars))
  setkeyv(z,c(group_vars_y, interval_vars))
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
    
    non_joins <- CJ.dt(non_joins,q,groups=group_vars_y)
    
    ##add in intervals in y that weren't matched to intervals in x 
    #separately for each combination of groups
    z <- rbindlist(list(z,non_joins),fill=TRUE)
  } 
  
  #z has rows for every period in x
  #z needs to be collapsed to sums and averages according to intervals in y
  #start by taking the row-wise length (duration) of the overlapping period in each row
  
  
  z[, (dur):=as.integer(pmin( get(xinterval_vars[2]), get(interval_vars[2]))-
                          pmax(get(xinterval_vars[1]),get(interval_vars[1]))) + 1L] 
  
  #dur being missing corresponds to xinterval_vars being missing, from merging in nonmatches above
  #the duration of overlap is zero, not missing.
  z[is.na(get(dur)),(dur):=0]
  
  #count the number of observations for each value_var
  #if the value_var is missing for this row, then the number of observations of that value in the period is zero
  #otherwise the number of observations is the length of the period
  for(i in 1:length(temp_nobs_vars)){
    tmp <- is.na(z[[value_vars[i]]])
    set(z,j=temp_nobs_vars[i],value=0L)
    set(z,i=which(!tmp),j=temp_nobs_vars[i],value=z[[dur]][!tmp])
  }
  

  setkeyv(z,c(group_vars,interval_vars)) 
  q <- z[,{
    
    #xdur will be the total summed length of all x intervals that overlap with a given y interval
    #note that this is all in giant by statement so this is within unique values of "interval_vars"
    #which are the y intervals
    xduration <- sum(get(dur)) 
    
    nobs_l <- list()
    value_l <- list()
    for(i in 1:length(value_vars)){
      
      #sum the number of obs over y intervals
      nobs_l[[nobs_vars[i]]] <- sum(get(temp_nobs_vars[i]))
      
      #take time-weighted averages over all value_vars 
      #we could just as well use the variable-specific n_obs_expanded instead of duration_expanded as the product
      #but n_obs_expanded are the same as duration_expanded except when value_vars is NA 
      #in which case it's removed from the sum
      value_l[[ value_vars[i] ]] <- sum(get(value_vars[i])*get(dur),na.rm=TRUE)/nobs_l[[i]]
    }
  
    c(
      list("xduration"=xduration,"yduration"=yduration[1]),
      nobs_l,
      value_l
      )
  },by=c(group_vars,interval_vars)]
  
  stopifnot(q[,all(xduration<=yduration)])

  
    #remove averages with too few observations in period
  #e.g. q[100*nobs_value/yduration < required_percentage, nobs_value:=NA]
  for(i in 1:length(value_vars)){
    q[100*get(nobs_vars[i])/yduration < required_percentage, value_vars[i]:=NA]
  }
  
  
  setcolorder(q, c(group_vars,interval_vars,value_vars,"yduration","xduration",nobs_vars))
  setkeyv(q,c(group_vars,interval_vars))
  q[]
  }



#slower algorithm. used for error checking since this is the simpler approach and less likely to have errors.
#Not recommended for large datasets since this expands the data into a row for every increment.
interval_weighted_avg_slow_f <- function(x, y,interval_vars,value_vars, group_vars=NULL,
                                         required_percentage=100,skip_overlap_check=FALSE){
  
  EVAL <- function(...)eval(parse(text=paste0(...)))
  
  
  #check for exact overlaps
  if(sum(duplicated(x[,c(..group_vars,..interval_vars)]))!=0){
    stop("sum(duplicated(x[,c(..group_vars,..interval_vars)]))!=0 is not TRUE. 
         there are replicate/duplicate intervals within groups. 
         If you wish to average these together, then do this first")
  }
  
  group_vars_y <- intersect(group_vars,names(y)) 
  
  ydups <- duplicated(y[,c(..group_vars_y,..interval_vars)])
  if(sum(ydups)!=0){
    warning("sum(duplicated(y[,c(..group_vars_y,..interval_vars)]))!=0 is not TRUE. 
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
                      by=c(interval_vars,group_vars_y)]")
  
  #y is expanded but if grouping variables are not in y (ie if group_vars_y is NULL) 
  #then there is only one set of intervals
  #in order to "complete" y, make a copy of y for groups correspnding to every combination of grouping variables
  #this will represent the desired output structure: 
  #for every combination of group_vars, have every time-interval in y
  
  if(!is.null(group_vars)){
    unique_id_list <- lapply(group_vars,function(r){
      x[,logical(1),keyby=r][[r]]
    })
    names(unique_id_list) <- group_vars
    unique_id_dt <- do.call(CJ,unique_id_list)
    
    y_expanded_complete <- CJ.dt(y_expanded,unique_id_dt,groups=group_vars_y)
  }else{
    y_expanded_complete <- y_expanded
  }
  
  
  #if grouping variables *are* in y...?
  
  ##take unique values of every group_var
  ##https://stackoverflow.com/questions/36953026/what-is-the-fastest-way-to-get-a-vector-of-sorted-unique-values-from-a-data-tabl
  
  # unique_id_list <- lapply(group_vars,function(r){
  #   x[,logical(1),keyby=r][[r]]
  # })
  # names(unique_id_list) <- group_vars
  # unique_time_list <- list(t=y_expanded[[t]])
  # y_expanded_complete <- do.call("CJ", c(unique_time_list, unique_id_list ))
  # setnames(y_expanded_complete,"t",t)
  # #merge back in start and end times
  # setkeyv(y_expanded_complete,t)
  # setkeyv(y_expanded,t)
  # y_expanded_complete <- y_expanded[y_expanded_complete]
  
  
  
  setkeyv(x_expanded,c(t, group_vars))
  setkeyv(y_expanded_complete,c(t, group_vars))
  
  z <- x_expanded[y_expanded_complete]
  
  avg_call <- paste0(value_vars,"=mean(",value_vars,",na.rm=TRUE)",collapse=",")
  paste0()
  nobs_call <- paste0(nobs_vars,"=sum(!is.na(",value_vars,"))",collapse=",")
  ydur_call <- paste0(ydur,"=length(unique(",t,"))")
  xdur_call <- paste0(xdur,"=sum(",measurement,",na.rm=TRUE)")
  
  out <- EVAL("z[,","list(",avg_call,",",ydur_call,",",xdur_call,",",nobs_call,")" ,
              ",by=c(interval_vars,group_vars)]")
  
  stopifnot(EVAL("out[,all(",xdur,"<=",ydur,")]"))
  
  for(i in 1:length(value_vars)){
    EVAL("out[100*",nobs_vars[i],"/",ydur,"< required_percentage,",value_vars[i],":=NA]")  
  }

  setcolorder(out, c(group_vars,interval_vars,value_vars,ydur,xdur,nobs_vars))
  
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
