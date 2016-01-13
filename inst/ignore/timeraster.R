# Creates TimeRaster class
# Timeraster is a subclass of stack in the Raster package
# it adds an xts time series (using only the index)
# To create a TimeRaster use the convenience constructor as:
#    tr=TimeRaster(stack,datelist)
#    tr=TimeRaster(filelist,datelist)
#    tr=TimeRaster(stack,xtsobj)
#    tr=TimeRaster(filelist,xtsobj)
# where
#   stack is an already loaded Raster stack or filelist is a list of files
#   a simple example to get a list of files from all geotifs in a directory would be
#     files <- list.files(path= "/data/subdir", pattern=".tif$", all.files=T, full.names=T)
#   datelist is a list of dates of the same length as nlayers(stack)
#   a simple creation of a datelist for the 365 days in 2014 is
#     as.Date("2014-01-01")+0:364
#  if an xts object is already made you can pass that in, but only index(xtsobj) is used
#  the datelist need not be sequentially ordered although that would be an intuitive way to store the data
#  however the datelist must be in the same order as the files or stack layers
#
# once created, you can use all raster methods including plot, show, arithemetic, etc
# the layer subscripting as tr[[3]] also still works
# however, the main feature is that the layer subscripting has been greatly extended
# if the [[]] index is a string then time subscripting is invoked
# the overall syntax for time subscripting is:
#   [daterange][aggregation][cycling]
# daterange:= startdate [TO enddate]
#   as with xts, dates can be higher level. So even if the data is daily
#   an index of "2010" can be used for all days in 2010 or "2010-10" for all days in October
# aggregation:=UPTO WEEKS|MONTHS|QUARTERS|YEARS [BY SUM|MIN|MAX|MEAN|SD|COUNT][KEEPNA]
#   this will summarize a data set to a more aggregate time level
#   default is to use sum with NA's omitted
# cycling=ACROSS MONTH|YEAR [BY SUM|MIN|MAX|MEAN|MEDIAN|VAR|STD|COUNT][KEEPNA]
#   this allows one to take say monthly averages across multiple years
#   so if one starts with monthly date from Jan 2010 to Dec 2015
#   ACROSS YEAR would give stack with 12 layers (JAN-DEC) with each layer
#   aggregated or averaged by month across the 6 years
#
# a full complex time subscript for daily data from 1980-2015 would be:
#  tr[["2000 TO 2009 UPTO MONTH ACROSS YEAR BY MEAN]]
#  this would subscript the data to one decade, aggregate daily up to monthly data
#  then average all 10 January data points together giving
#  a stack with 12 layers for the 12 months each month being
#  an average across the years 2000-2009
#
# Two additional methods are supported to ensure that when a vector is
# extracted across the layers it returns an xts object
#  getTS(tr,xcoord,ycoord)  returns an xts object at (xcoord,ycoord) in the raster
#  cellStats(tr,func,...) returns an xts object that has been summed/averaged  etc (depending on func) across the layers
# NB extract - have to think this through - multiple scenarios


require(raster)
require(xts)
setOldClass("xts") #make S3 object accessible to S4 objects

setClass("TimeRaster",
         representation(ts="xts"),
         contains="RasterStack",
         validity=function(object) {
           if (nlayers(object)!=length(object@ts))
             "Number of layers not equal length of timeseries"
           else
             TRUE
         }
)

setMethod(
  f="show",
  signature="TimeRaster",
  definition=function(object) {
    callNextMethod()
    cat("Time dimension has",toString(length(object@ts)),"time periods\n")
    show(head(index(object@ts)))

  }
)

#convenience constructor
TimeRaster<-function(rast,datelist) {
  if (class(rast)!="RasterStack" && class(rast)!="RasterBrick")
    #assume it is a list of files and see how that works out
    rast=stack(rast)
  if (!("xts" %in% class(datelist)))  #if an array of dates
      datelist=xts(1:length(datelist),datelist)
  datelist[]=1:length(datelist) #override content to be 1..n
  index(datelist)=trunc(index(datelist))+0.4 #midnight sometimes is previous day so put midday but still rounds down
  return(new("TimeRaster",rast,ts=datelist))
}

setMethod(
  f="[[",
  signature="TimeRaster",
  definition=function(x,i,...) {
    if (is.numeric(i)) {return(callNextMethod(x,i,...))}
    if (is.character(i)) {
      args=parseit(i)
      return(doit(x,args))
    }# end if is.character #process
  } #end [[ function method
)

setGeneric("getTS",function(object,...){standardGeneric("getTS")})
setMethod(f="getTS",signature="TimeRaster",
          definition<-function(object,x,y) {
            row=rowFromY(object,y)
            col=colFromX(object,x)
            m=getValues(object,row)[col,]
            return(xts(as.vector(m),index(object@ts)))
          }
        )


setMethod(
  f="cellStats",
  signature=(x="TimeRaster"),
  definition=function(x,stat='mean',na.rm=TRUE,asSample=TRUE, ...) {
    return(xts(callNextMethod(x,stat,na.rm,asSample),index(x@ts)))
  }
)


# doit() turn structured description into which bands to operate on how
#
doit<-function(obj,args) {  #have arguments parsed
  print(args)

  #first do selection/subsetting
  seltext=""
  if (!is.na(args$startdate))
    seltext=args$startdate
  else
    seltext=""
  if (!is.na(args$enddate)) {
    if (seltext!="") {
      seltext<-paste(seltext,"/",args$enddate,sep="")
    }
    else {
      seltext=args$enddate
    }
  }
  outobj=obj
  if (seltext!="") {
    newts=outobj@ts[seltext]
    if (length(newts)==0)
      stop("TimeRaster subscripted to zero layers")
    outobj=outobj[[as.numeric(newts)]]   ####
    newts[]=1:length(newts)
    outobj@ts=newts
    names(outobj)<-index(outobj@ts)
  }

  #OK subscripted - now do aggregation if any
  #args$  agglevel  aggfunc aggkeepna
  if (!is.na(args$agglevel)) {
    if (args$aggup) {
      ep=endpoints(outobj@ts,tolower(args$agglevel))
      idx=floor(approx(ep+1,1:length(ep),1:length(outobj@ts))$y)
      oldts=outobj@ts
      outobj=stack(stackApply(outobj,idx,tolower(args$aggfunc),na.rm=!args$aggkeepna))   ####
      newts=oldts[ep[-length(ep)]+1]
      newts[]=1:length(newts)
      outobj=new("TimeRaster",outobj,ts=newts)
      #DOITHERE - set raster layer names
      names(outobj)<-index(outobj@ts)
    }
    else
      stop("Error DOWNTO not implemented yet")
  }

  #OK now do periodicity
  # args$ acrosslevel   acrossfunc  acrosskeepna
  if (!is.na(args$acrosslevel)) {
    curperiod=periodicity(outobj@ts)$frequency/(24*60*60)
    if (args$acrosslevel=="MONTH") {
      if (curperiod!=1)
        stop("Invalid across month")
      idx=.indexmday(outobj@ts)
    }
    else if (args$acrosslevel=="YEAR") {
      if (curperiod==31) #monthly data
        idx=.indexmon(outobj@ts)+1
      else if (curperiod==7) {#weeklydata
        idx=.indexweek(outobj@ts)
        idx=idx-min(idx)+1 #week is relative to epoch
      }
      else if (curperiod==1) #daily
        idx=.indexyday(outobj@ts)+1
      else
        stop("invalid across year")
    }
    else
      stop(paste("Inavlid across level:",args$acrosslevel))
    #browser() #some errors in index/newdates
    oldts=outobj@ts
    outobj=stack(stackApply(outobj,idx,tolower(args$acrossfunc),na.rm=!args$acrosskeepna))  ####
    if (curperiod==31)
      newdates=seq(as.Date("2000-1-1")+0.4,by='month',length.out=12)
    else
      newdates=as.Date("2000-1-1")+0.4+unique(idx-1)*curperiod
    newts=xts(1:length(newdates),newdates)
    outobj=new("TimeRaster",outobj,ts=newts)
    #DOITHERE - set raster layer names
    names(outobj)<-index(outobj@ts)
  } #end if !is.na(args$acrosslevel) - i.e. do ACROSS functionality

  #OK now do lags??


  return(outobj)
}


#
# parseit()  turns a text string into a structured description of what to do
#
parseit<-function(textsub) {

  #show is on - need to parse this
  textsub=gsub("^ *|(?<= ) | *$", "", textsub, perl=T) #remove double spaces
  #cat("Will parse '",textsub,"'\n")
  toks=strsplit(textsub," ")[[1]]
  startdate<-enddate<-aggup<-agglevel<-aggfunc<-aggkeepna<-acrosslevel<-acrossfunc<-acrosskeepna<-NA
  while (length(toks)>0) {
    thetok=toupper(toks[[1]])
    #cat("thetok=",thetok,"\n")
    #DOIT - allow no start date or enddate
    if (grepl("^[0-9]{4}",thetok)) {
      #DOITHERE - ALLOW no start or end date with TO
      if (is.na(startdate)) {
        #cat("doing startdate\n")
        startdate=thetok
        if (length(toks)>2 && toupper(toks[[2]])=="TO" ) {
          if (!grepl("^[0-9]{4}",toks[[3]]))
            stop(cat("Syntax error: invalid 2nd date",toks[[3]]))
          enddate=toks[[3]]
          print(toks)
          toks=toks[-1]
          toks=toks[-1] #eat start date & TO token
        } #end process enddate
      } #end working on date
      else {
        stop("Syntax error: more than one date")
      }
    }#end dates
    else if (grepl("UPTO|DOWNTO",thetok)) {
      #cat("length=",length(toks),"tok[2]=",toks[[2]],"\n")
      if (length(toks)>1 && grepl("WEEKS|MONTHS|QUARTERS|YEARS",toupper(toks[[2]]))) {
        if  (thetok=="UPTO")
          aggup=TRUE
        else
          aggup=FALSE
        agglevel=toks[[2]]
        aggfunc="SUM"
        aggkeepna=FALSE
        if (length(toks)>3 && toks[[3]]=="BY") {
          if (grepl("SUM|MEAN|MIN|MAX|VAR|STD|COUNT|MEDIAN",toupper(toks[[4]]))) {
            aggfunc=toks[[4]]
            toks=toks[-1] #eat two more to account for BY SUM
            toks=toks[-1]
            cat("About check KEEPNA: t1=",toks[[1]],"t2=",toks[[2]],"t3=",toks[[3]],"\n")
            if (length(toks)>2 && toks[[3]]=="KEEPNA") {
              cat("Doing KEEPNA TRUE\n")
              aggkeepna=TRUE
              toks=toks[-1]
            }
          }
          else
            stop("Syntax error near aggregation BY")
        } #end if BY
        toks=toks[-1]
      }#end if aggto
      else {
        stop(cat("Syntax error on aggregation - valid periods are WEEKS, MONTHS, QUARTERS, YEARS"))
      }
    }
    else if (grepl("ACROSS",thetok)) {
      if (length(toks)>1 && grepl("YEAR|MONTH",toks[[2]])) {
        acrosslevel=toks[[2]]
        acrossfunc="SUM"
        acrosskeepna=FALSE
        if (length(toks)>3 && toks[[3]]=="BY") {
          if (grepl("SUM|MEAN|MIN|MAX|VAR|STD|COUNT|MEDIAN",toupper(toks[[4]]))) {
            acrossfunc=toks[[4]]
            toks=toks[-1] #eat two more to account for BY SUM
            toks=toks[-1]
            if (length(toks)>2 && toks[[3]]=="KEEPNA") {
              acrosskeepna=TRUE
              toks=toks[-1]
            }
          }
          else
            stop("Syntax error near across BY")
        } #end if BY
        toks=toks[-1]
      }
      else
        stop("Syntax error near ACROSS")
    }
    else {
      stop(cat("Syntax error: near",thetok))
    }
    toks=toks[-1] #eat the token
  } #end while
  args=list(startdate=startdate,enddate=enddate,aggup=aggup,agglevel=agglevel,aggfunc=aggfunc,aggkeepna=aggkeepna,acrosslevel=acrosslevel,acrossfunc=acrossfunc,acrosskeepna=acrosskeepna)
  return(args)
}

# #sample code to load 2014 stack
# files <- list.files(path= "~/github/sac/raster", pattern=".tif$", all.files=T, full.names=T)
# rf=stack(files)
# ts=xts(1:365,as.Date("2014-01-01")+0:364)
# rftr=new("TimeRaster",rf,ts=ts)
# #better to use constructor as one of:
# rftr=TimeRaster(files,ts)
# rftr=TimeRaster(rf,ts)
# rftr=TimeRaster(rf,as.Date("2014-01-01")+0:364)
# #now use it
# plot(rftr[["2014-10-01 TO 2014-10-05"]])
# monthlyrf=rftr[["UPTO MONTHS"]]
