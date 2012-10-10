#author & copyright: johannes kutsam october 2012 johannes.kutsam@gmail.com
#this function downloads datasets from eurostat by their identifier
#the dataset is returned as a data.table object
#the libraries plyr, zoo, data.table,reshape2  are used

#features:
#data sets and dimensions are downloaded from the bulk service from eurostat
# dimensions are correctly labeled as factors
# the time dimension is automatically detected as yearly, monthly or quarterly and converted to a
# respective zoo object
#nice labels for the columsn are included as an attribute "nicelabels" and can be used as nicelabel[columnname]
#an index is set on the dimensions+time - so that merge with another eurostat table works on euqually named columns
#possible extensions
#load dimensions into a local cache to speed up the labeling
#add map feature to include a map of countries / or NUTS2-3 Regions where applicable with maptools

library(data.table)
library(reshape2)
library(zoo)

read.eurostat=function(datasetname,LANGUAGE="en",cache=TRUE){
  dsfname=paste(datasetname,".Rdata",sep="")
  if(file.exists(dsfname) & cache==TRUE){
    load(file=dsfname)
  }else{
    baseurl="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=data%2F"
    fullfilename=paste(datasetname,".tsv.gz",sep="")
    temp <- paste(tempfile(),".gz",sep="")
    download.file(paste(baseurl,fullfilename,sep=""),temp)
    dataconnection <- gzfile(temp)
    d=data.table(read.delim(dataconnection))
    
    #split the dimension column into its components
    firstname=colnames(d)[1] # remove .time and count how many headings are there 
    firstname=substr(firstname,1,nchar(firstname)-nchar(".time"))
    headings=toupper(strsplit(firstname,".",fixed=TRUE)[[1]])
    headingcount=length(headings)
    setnames(d,colnames(d)[1],c("dimensions"))
    headings_split = data.table(colsplit(d[,dimensions], pattern = "\\,",names=headings))
    library(plyr)
    for(x in headings) headings_split[[x]]=as.factor(headings_split[[x]])
    
    #join dimensions with data, convert to long format,split the value from the flag
    mydata=cbind(headings_split,d[,colnames(d)[-1],with=FALSE])
    longdata=data.table(melt(as.data.frame(mydata),id=headings))
    longdata$flag=as.factor(substr(longdata$value,nchar(longdata$value),nchar(longdata$value)))
    longdata$value=as.double(substr(longdata$value,1,nchar(longdata$value)-1))
  
    #download factors for each heading and add
    for(heading in headings){
      fname=paste("dic/",heading,".Rdata",sep="")
      if(file.exists(fname)){
        load(file=fname)
      }else{
        factorfile=paste("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=dic%2F",LANGUAGE,"%2F",tolower(heading),".dic",sep="")
        temp <- paste(tempfile(),".gz",sep="")
        download.file(factorfile,temp)
        dataconnection <- gzfile(temp)
        factordata=data.table(read.delim(dataconnection,header=FALSE))
        setnames(factordata,colnames(factordata),c(heading,paste(heading,"_desc",sep="")))
        dir.create("dic")
        save(factordata,file=fname)
      }
      #join the heading to the heading dataset
      longdata=merge(longdata,factordata,by=heading,all.x=TRUE)
    }
  #download the dimension names to use as better column names
    
      # add the nicelabels as an attribute to the datatable
      fname="dic/dimlst.dic.Rdata"
      if(file.exists(fname)){
        load(file=fname)
      }else{
        dimfile=paste("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=dic%2F",LANGUAGE,"%2Fdimlst.dic",sep="")
        temp <- paste(tempfile(),".gz",sep="")
        download.file(dimfile,temp)
        dataconnection <- gzfile(temp)
        dimdata=read.delim(dataconnection,header=FALSE)
        colnames(dimdata)=c("colname","desc")
        lab=dimdata$desc
        names(lab)=dimdata$colname
        dir.create("dic")
        save(lab,file=fname)
      }
        setattr(longdata,"nicelabels",lab[headings])
   
    
  #fix time column:
    setnames(longdata,"variable","time")
    longdata$time=substring(longdata$time,2)
    mytime=longdata$time[1]                       
    if(nchar(mytime)==6 & substring(mytime,5,5)=="Q"){ #if it is QUARTERLY DATA
      myyear=as.numeric(substring(longdata$time,1,4))
      quarter=as.numeric(substring(longdata$time,6,6))
      longdata$time=yearqtr(myyear+(quarter-1)/4)
    }else if(nchar(mytime)==7 & substring(mytime,5,5)=="M"){ #if it is monthly DATA
      myyear=as.numeric(substring(longdata$time,1,4))
      month=as.numeric(substring(longdata$time,6,7))
      longdata$time=yearmon(myyear+(month-1)/12)
    }else if(nchar(mytime)==4){ #yearly data}
      longdata$time=as.integer(longdata$time)
    }
    eurostatdata=longdata
    save(eurostatdata,file=dsfname)
    setkeyv(longdata,c(headings,"time"))
    eurostatdata
  }
  
  
  eurostatdata
}
 
