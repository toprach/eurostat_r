#this library is used to download data from eurostat and to find datasets
#later extend to extend to find datasets with certain dimensions

#download data from eurostat
#unpack and convert to dataframe
#load label descriptions
#load factors
#save as r data object

datasetname="namq_gdp_c"
datasetname="une_rt_q"
LANGUAGE="en"


read.eurostat=function(datasetname,LANGUAGE="en",save=FALSE){
  library(data.table)
  library(reshape2)  
  baseurl="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=data%2F"
  fullfilename=paste(datasetname,".tsv.gz",sep="")
  temp <- paste(tempfile(),".gz",sep="")
  download.file(paste(baseurl,fullfilename,sep=""),temp)
  dataconnection <- gzfile(temp)
  d=data.table(read.delim(dataconnection))
  
  firstname=colnames(d)[1] # remove .time and count how many headings are there 
  firstname=substr(firstname,1,nchar(firstname)-nchar(".time"))
  headings=toupper(strsplit(firstname,".",fixed=TRUE)[[1]])
  headingcount=length(headings)
  setnames(d,colnames(d)[1],c("dimensions"))
  
  headings_split = data.table(colsplit(d[,dimensions], pattern = "\\,",names=headings))
  mydata=cbind(headings_split,d[,colnames(d)[-1],with=FALSE])
  longdata=data.table(melt(mydata,id=headings))
  setnames(longdata,"variable","time")
  
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  longdata$flag=as.factor(substr(longdata$value,nchar(longdata$value),nchar(longdata$value)))
  longdata$value=as.double(substr(longdata$value,1,nchar(longdata$value)-1))
  
  
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
    factordata=data.table(read.delim(dataconnection,header=FALSE))
    setnames(factordata,colnames(factordata),c(heading,paste(heading,"_desc",sep="")))
    #join the heading to the heading dataset
    longdata=merge(longdata,factordata,by=heading,all.x=TRUE)
  }
  
  
    #at the end at speaking headings
  setnames(longdata,headings,speakingheadings)
     
  longdata
#   
#   #fix the time column X2000 or 
#   identifytimedim=as.character(longdata$time[1])
#   
#   #TODO continue here: identify Q for quarter, M for month, - if non YEAR
#   if found Q then zoo -> Q  else zoo -> M
#   as.yearqtr("200706", "%Y%m")
#   grep(,"\\")
#   
#   
#   eurostatdata=cbind(dimensions,time=longdata$time,values) 
#   if(save) save(eurostatdata,file=paste(datasetname,".RData",sep=""))
#   eurostatdata
}
  
 
  
