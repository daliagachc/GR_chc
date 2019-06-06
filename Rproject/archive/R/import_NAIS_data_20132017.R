
importNAIS_2013_2017=function(year){
  yearsel=year
  
  
  #import all nais data
  getEndSize=function(vec,levels){
    out=c()
    for (i in 1:length(vec)){
      pos=which(levels==vec[i])
      if (pos < length(vec)){
        out=c(out,levels[pos+1])
      } else{
        out=c(out,0.7e-7)
      }
    }
    return(out)
  }#getEndSize
  
  workDir=getwd()
  # setwd("C:/fastFiles/springCourse")
  
  setwd(paste0(workDir,"/data/NAIS3/NAIS3_sum_20132017"))
  
  #creating a list pointing to all files with extension .dat
  tempall = list.files(pattern="*.sum")
  tempyears=substring(tempall,7,10)
  temp=tempall[which(tempyears==yearsel)]
  
  #creating a list of dataframes for the NAIS data.
  #Also creating a vector of names
  NAIS_ion_list_13_17=list()
  NAIS_ion_list_13_17_indices=c()
  for (i in 1:length(temp)){
    filename=temp[i]
    wd=fread(temp[i],header=F,skip=1)
    headers=names(fread(temp[i],header=T,skip=0))
    levels=as.numeric(as.character(headers))[-1]
    names(wd)=headers
    names(wd)[1]="Time"
    wd$day=wd$Time %/% 1
    wd$hour=wd$Time %%1
    wd$Time=paste(yearsel,wd$day,sep="/")
    wd$Time=as.POSIXct(wd$Time,format="%Y/%j")
    wd$Time=force_tz(wd$Time,tzone="UTC")
    wd$startTime=wd$Time+24*3600*wd$hour
    wd$Time=NULL
    wd$day=NULL
    wd$hour=NULL
    
    wd$endTime=lead(wd$startTime,1)
    
    
    wd2= melt(wd,id.vars = c("startTime","endTime"))
    wd2$startSize=as.numeric(as.character(wd2$variable))
    wd2 = wd2 %>%
      mutate(endSize=getEndSize(startSize,levels))
    wd2$startSize=ifelse(wd2$startSize > 0, wd2$startSize,1e-10)
    wd2$variable=NULL
    
    
    NAIS_ion_list_13_17_indices[i]=filename
    NAIS_ion_list_13_17[[i]]=wd2
  }
  
  #now I create a dataframe containing that information. Sometimes easier to work with.
  NAIS_ion_list_13_17_indices=gsub("nds","",NAIS_ion_list_13_17_indices)
  NAIS_ion_list_13_17_indices=gsub("NAIS","",NAIS_ion_list_13_17_indices)
  posindices=which((data.frame(date=NAIS_ion_list_13_17_indices,flag=F) %>%
                      mutate(flag=grepl("p",date)))$flag)
  negindices=which((data.frame(date=NAIS_ion_list_13_17_indices,flag=F) %>%
                      mutate(flag=grepl("n",date)))$flag)
  
  helplist=NAIS_ion_list_13_17
  for (i in 1:length(helplist)){
    if (i %in% posindices){
      helplist[[i]]$ion="positive"
    } else if (i %in% negindices){
      helplist[[i]]$ion="negative"
    }
  }
  
  
  #back to main workDir
  setwd(workDir)
  help=rbindlist(helplist, use.names=T, fill=T, idcol=NULL)
  attributes(help$startTime)$tzone="etc/GMT+4"
  attributes(help$endTime)$tzone="etc/GMT+4"
  return(help)
}

