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

setwd(paste0(workDir,"/data/Bolivia_ion_spec/NAIS3/ions"))

#creating a list pointing to all files with extension .dat
temp = list.files(pattern="*.sum")
# 
# NAIS_posion_list=list()
# NAIS_negion_list=list()
NAIS_ion_list=list()
NAIS_ion_list_indices=c()
for (i in 1:length(temp)){
  filename=temp[i]
  wd=fread(temp[i],header=F,skip=1)
  headers=names(fread(temp[i],header=T,skip=0))
  levels=as.numeric(as.character(headers))[-1]
  names(wd)=headers
  names(wd)[1]="Time"
  
  wd$startTime=as.POSIXct(wd$Time*24*3600-24*3600,origin='0000-01-01 00:00.00 UTC', tz="etc/GMT+4")
  wd$endTime=lag(wd$startTime,1)
  wd$Time=NULL
  
  
  wd2= melt(wd,id.vars = c("startTime","endTime"))
  wd2$startSize=as.numeric(as.character(wd2$variable))
  wd2 = wd2 %>%
    mutate(endSize=getEndSize(startSize,levels))
  wd2$startSize=ifelse(wd2$startSize > 0, wd2$startSize,1.3e-9)
  
  NAIS_ion_list_indices[i]=filename
  NAIS_ion_list[[i]]=wd2
}

setwd(workDir)