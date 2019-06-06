#imports NAIS ion file. Plots


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


#the date to be plotted. only uncomment if you want ONLY particle and don't run it
#via combined script
date = "2013-09-17"
datefile = paste0(date,".ions.nds")


#need reshape library for the practical melt function
library(reshape2)
#need scales library in order plot the time axis right
library(scales)
#I install and load the package "cowplot", which helps with aligning multiple plots
#install.packages("cowplot")

#I set the amount of decimals shown for seconds to 6, like in the data
options(digits.secs = 6)

# #load nais3data
# nais3DataDir = "D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3/data/level0"

workDir=getwd()
setwd(paste0(workDir,"/data_nais_chc/data/level0"))

NAIS3data_2013_2018= read.csv(file=datefile,
                    skip = 3, sep=",")

#separate the positive ion data
NAIS3_ion_neg = NAIS3data[,1:31] %>%
  mutate(ion="negative")
NAIS3_ion_pos = NAIS3data[,c(1:3,60:87)] %>%
  mutate(ion="positive")
names(NAIS3_ion_pos)=names(NAIS3_ion_neg)
NAIS3_ions=rbind(NAIS3_ion_neg,NAIS3_ion_pos)%>%
  rename("startTime"=begintime,
         "endTime"=endtime) %>%
  mutate(opmode=NULL)

#use melt function to convert columns into rows
m.NAIS3_ions = melt(NAIS3_ions,id.vars = c("startTime","endTime","ion")) %>%
  rename("startSize"=variable) %>%
  mutate(startSize=as.numeric(gsub("sp_","",startSize)),
         endSize=getEndSize(startSize,rev(unique(startSize))))
m.NAIS3_ions = m.NAIS3_ions %>%
  mutate(endSize=ifelse(is.na(endSize),5,endSize),
         startTime=as.POSIXct(startTime, format = "%Y-%m-%d %H:%M:%OS",tz="UTC"),
         endTime=as.POSIXct(endTime, format = "%Y-%m-%d %H:%M:%OS",tz="UTC"))




m.NAIS3_ion_pos = melt(NAIS3_ion_pos,id.vars = c("begintime","endtime","opmode"))
m.NAIS3_ion_neg = melt(NAIS3_ion_neg,id.vars = c("begintime","endtime","opmode"))
#now, make y values numeric
m.NAIS3_ion_neg$variable = gsub("sp_","",m.NAIS3_ion_neg$variable)
m.NAIS3_ion_neg$variable = as.numeric(m.NAIS3_ion_neg$variable)
m.NAIS3_ion_pos$variable = gsub("sp_","",m.NAIS3_ion_pos$variable)
m.NAIS3_ion_pos$variable = gsub('.{2}$', '', m.NAIS3_ion_pos$variable)
m.NAIS3_ion_pos$variable = as.numeric(m.NAIS3_ion_pos$variable)
#now convert the begintime column into date format
m.NAIS3_ion_pos$startTime = as.POSIXct(m.NAIS3_ion_pos$begintime, format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
m.NAIS3_ion_neg$startTime = as.POSIXct(m.NAIS3_ion_neg$begintime, format = "%Y-%m-%d %H:%M:%OS",tz="UTC")


#lowest value to be plotted. Lower values are set to this value for easier plotting
lower = 3
for (i in 1:nrow(m.NAIS3_ion_pos)){
  if (m.NAIS3_ion_pos[i,"value"] < lower){m.NAIS3_ion_pos[i,"value"] = lower}
}
for (i in 1:nrow(m.NAIS3_ion_neg)){
  if (m.NAIS3_ion_neg[i,"value"] < lower){m.NAIS3_ion_neg[i,"value"] = lower}
}

