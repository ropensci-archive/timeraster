# doit() turn structured description into which bands to operate on how
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
