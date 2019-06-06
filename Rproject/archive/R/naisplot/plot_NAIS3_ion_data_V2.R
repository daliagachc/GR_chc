#imports NAIS ion file. Plots


#
setwd("C:/fastFiles/springCourse/Data/Bolivia_ion_spec/NAIS3/ions")
#the date to be plotted. only uncomment if you want ONLY particle and don't run it
#via combined script
#date = "2013-09-17"
datefile = paste0("2017-12-02n.sum")

#i define the color palette for plotting
myPalette = colors()[c(30,26,69,48,652,91,33,34,36,36,36)]

jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))



#need reshape library for the practical melt function
library(reshape2)
#need scales library in order plot the time axis right
library(scales)
#I install and load the package "cowplot", which helps with aligning multiple plots
#install.packages("cowplot")
library(cowplot)
#cowplot changes the basic theme of ggplot2. This is not wanted, so I change it back
#to basic
theme_set(theme_gray())
#I install and load the package "ggplot2", which helps in plotting.
#(take into account that the package "scales" is loaded automatically)
#install.packages("ggplot2")
library(ggplot2)

#I set the amount of decimals shown for seconds to 6, like in the data
options(digits.secs = 6)

#I define a "reverselog_tra" function, that allows plotting an axis in both reverse and
#logaritmic scale (taken out of the web)
library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

#load nais3data
#nais3DataDir = "D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3/data/level0"

NAIS3data= read.csv(file=paste(getwd(), datefile, sep ="/"),
                      skip = 3, sep=",")

#separate the positive ion data
NAIS3_ion_neg = NAIS3data[,1:31]
NAIS3_ion_pos = NAIS3data[,c(1:3,60:87)]
#omit NA
NAIS3_ion_pos = na.omit(NAIS3_ion_pos)
NAIS3_ion_neg = na.omit(NAIS3_ion_neg)

#use melt function to convert columns into rows
#m.NAIS3data = melt(NAIS3data,id.vars = c("begintime","endtime","opmode"))

m.NAIS3_ion_pos = melt(NAIS3_ion_pos,id.vars = c("begintime","endtime","opmode"))
m.NAIS3_ion_neg = melt(NAIS3_ion_neg,id.vars = c("begintime","endtime","opmode"))
#now, make y values numeric
m.NAIS3_ion_neg$variable = gsub("sp_","",m.NAIS3_ion_neg$variable)
m.NAIS3_ion_neg$variable = as.numeric(m.NAIS3_ion_neg$variable)
m.NAIS3_ion_neg$begintime = as.POSIXlt(m.NAIS3_ion_neg$begintime, format = "%Y-%m-%d %H:%M:%OS",tz="UTC")


#lowest value to be plotted. Lower values are set to this value for easier plotting
lower = 3

for (i in 1:nrow(m.NAIS3_ion_neg)){
  if (m.NAIS3_ion_neg[i,"value"] < lower){m.NAIS3_ion_neg[i,"value"] = lower}
}
workData=m.NAIS3_ion_neg
workData$variable=factor(workData$variable)
workData$begintime=factor(as.character(workData$begintime))


# ggplot(data = workData)+
#   geom_raster(aes(x=begintime,y=variable,fill=value),na.rm = T)

ggplot(data = workData, aes(x=begintime,y=variable))+
  geom_raster(aes(x=begintime,y=variable,fill=value),interpolate=T)+
  scale_y_discrete()+scale_x_discrete()