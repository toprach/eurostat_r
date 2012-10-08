#this library is used to download data from eurostat and to find datasets
#later extend to extend to find datasets with certain dimensions

#download data from eurostat
#unpack and convert to dataframe
#load label descriptions
#load factors
#save as r data object

datasetname="jvs_a_nace2"

LANGUAGE="en"

 

read.eurostat=function(datasetname,LANGUAGE="en",save=FALSE){
  library(RCurl)
  library(data.table)
  library(reshape)
  library(stringr)
  
  baseurl="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=data%2F"
  
  fullfilename=paste(datasetname,".tsv.gz",sep="")
  temp <- paste(tempfile(),".gz",sep="")
  download.file(paste(baseurl,fullfilename,sep=""),temp)
  dataconnection <- gzfile(temp)
  d=read.delim(dataconnection)
  longdata=melt(d,id=colnames(d)[1])
  
  firstname=colnames(d)[1] # remove .time and count how many headings are there 
  firstname=substr(firstname,1,nchar(firstname)-nchar(".time"))
  headings=toupper(strsplit(firstname,".",fixed=TRUE)[[1]])
  headingcount=length(headings)
  colnames(longdata)=c("dimensions","time","value")
  
  
  #get the data on the dimension tables
  df=data.frame(dimensions=as.character(longdata[,"dimensions"]))
  df = transform(df, dimensions= colsplit(dimensions, split = "\\,",names=headings))
  dimensions=data.table(df$dimensions)
  
  #download the dimension labels - save headings as better variable
  dimfile=paste("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=dic%2F",LANGUAGE,"%2Fdimlst.dic",sep="")
  
  temp <- paste(tempfile(),".gz",sep="")
  download.file(dimfile,temp)
  dataconnection <- gzfile(temp)
  dimdata=read.delim(dataconnection,header=FALSE)
  colnames(dimdata)=c("colname","desc")
  lab=dimdata$desc
  names(lab)=dimdata$colname
  
  #create  headings that speak for themselves for columns
  speakingheadings=as.character(lab[headings])
  
  #download factors for each heading and add
  for(heading in headings){
    factorfile=paste("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=dic%2F",LANGUAGE,"%2F",tolower(heading),".dic",sep="")
    temp <- paste(tempfile(),".gz",sep="")
    download.file(factorfile,temp)
    dataconnection <- gzfile(temp)
    factordata=read.delim(dataconnection,header=FALSE)
    colnames(factordata)=c(heading,paste(heading,"_desc",sep=""))
    #join the heading to the heading dataset
    dimensions=merge(dimensions,factordata,by=heading,all.x=TRUE)
  }
  
  
    #at the end at speaking headings
  setnames(dimensions,colnames(dimensions)[1:length(speakingheadings)],speakingheadings)
  
    #add data columns by writing and reading again---FASTER ;-)
  temp=tempfile()
  values=data.frame(value=as.character(longdata$value),stringsAsFactors=FALSE)
  v=try(transform(values, value= colsplit(value, split = "\\ ",names=c("value","flag"))),silent=TRUE)
  
  
  if(class(v)=="try-error"){
    print("there are no flags")
    values=data.table(values)
    values$value=as.character(values$value)
    values$value=as.double(values$value)
  }else{
    values=v$value
    values=data.table(values)
    values$value=as.character(values$value)
    values$value=as.double(values$value)
    values$flag=as.character(values$flag)
    values[value==flag,flag:=NA]
  }
  
  #fix the time column
  identifytimedim=as.character(longdata$time[1])
  
  #TODO continue here: identify Q for quarter, M for month, - if non YEAR
  if found Q then zoo -> Q  else zoo -> M
  as.yearqtr("200706", "%Y%m")
  grep(,"\\")
  
  
  eurostatdata=cbind(dimensions,time=longdata$time,values) 
  if(save) save(eurostatdata,file=paste(datasetname,".RData",sep=""))
  eurostatdata
}
  
  
