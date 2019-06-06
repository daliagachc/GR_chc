#imports NAIS ion file. Plots

#the date to be plotted. only uncomment if you want ONLY particle and don't run it
#via combined script
#date = "2013-09-17"
datefile = paste0(date,".ions.nds")

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

# #load nais3data
# nais3DataDir = "D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3/data/level0"

NAIS3data= read.csv(file=paste(nais3DataDir, datefile, sep ="/"),
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
m.NAIS3_ion_pos$variable = gsub("sp_","",m.NAIS3_ion_pos$variable)
m.NAIS3_ion_pos$variable = gsub('.{2}$', '', m.NAIS3_ion_pos$variable)
m.NAIS3_ion_pos$variable = as.numeric(m.NAIS3_ion_pos$variable)
#now convert the begintime column into date format
m.NAIS3_ion_pos$begintime = as.POSIXlt(m.NAIS3_ion_pos$begintime, format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
m.NAIS3_ion_neg$begintime = as.POSIXlt(m.NAIS3_ion_neg$begintime, format = "%Y-%m-%d %H:%M:%OS",tz="UTC")


#lowest value to be plotted. Lower values are set to this value for easier plotting
lower = 3
for (i in 1:nrow(m.NAIS3_ion_pos)){
  if (m.NAIS3_ion_pos[i,"value"] < lower){m.NAIS3_ion_pos[i,"value"] = lower}
}
for (i in 1:nrow(m.NAIS3_ion_neg)){
  if (m.NAIS3_ion_neg[i,"value"] < lower){m.NAIS3_ion_neg[i,"value"] = lower}
}


#plot negative ions
workData=m.NAIS3_ion_neg

neg_ion_Plot = ggplot(data = workData, aes(x=begintime,y=variable))+
  geom_raster(aes(fill=value),interpolate=F,hjust = 1.5,na.rm = T)+
  geom_raster(aes(fill=value),interpolate=F)+
  scale_fill_gradientn(trans = "log10",
                       limits = c(lower,max(workData$value)),
                       colours=myPalette)+
  guides(fill=guide_colorbar(title="",direction="horizontal",label.position="top"))+
  theme(legend.justification = c(1,0),
        legend.position = "top",
        legend.title = element_blank(),
        legend.box = "horizontal", legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.width=unit(30,"pt"), legend.key.height=unit(12,"pt"),
        legend.text=element_text(colour="black"),legend.margin = margin(t=1,b=-23,r=1,l=1))+
  scale_y_continuous(trans=reverselog_trans(10),breaks = c(1,0.1,0.01),
                     limits = c(max(workData$variable),min(workData$variable)),
                     expand = c(0.0,0.0))+
  theme(axis.title.y = element_text(angle = 0,margin=margin(r=-2)))+
  scale_x_datetime(date_breaks = "2 hours",
                   labels = date_format("%H:%M", tz="UTC"),
                   position = "top", expand = c(0.01,0.01))+
  labs(x = date, y = "ions neg\n(cm^2/s\n/V)")+
  theme(panel.background = element_rect(color="black", fill ="white"),
        plot.background = element_rect(fill = "lightgrey",color="lightgrey"),
        panel.spacing.y = unit(-20,"pt"),
        axis.text = element_text(size=8),
        axis.title = element_text(size=9),
        plot.margin = margin(b=0,l=4,r=4))


#plot positive ions
workData=m.NAIS3_ion_pos

pos_ion_Plot = ggplot(data = workData, aes(x=begintime,y=variable))+
  geom_raster(aes(fill=value),interpolate=F,hjust = 1.5,na.rm = T)+
  geom_raster(aes(fill=value),interpolate=F)+
  scale_fill_gradientn(trans = "log10",
                       limits = c(lower,max(workData$value)),
                       colours=myPalette)+
  scale_y_continuous(trans=reverselog_trans(10),breaks = c(1,0.1,0.01),
                     limits = c(max(workData$variable),min(workData$variable)),
                     expand = c(0.0,0.0))+
  theme(axis.title.y = element_text(angle = 0,margin=margin(r=2)))+
  scale_x_datetime(date_breaks = "2 hours",
                   labels = date_format("%H:%M", tz="UTC"),
                   position = "top", expand = c(0.01,0.01))+
  labs(x = "", y = "ions pos\n(cm^2/s\n/V)")+
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),axis.text.x=element_blank())+
  theme(panel.background = element_rect(color="black", fill ="white"),
        plot.background = element_rect(fill = "lightgrey",color="lightgrey"),
        panel.grid = element_blank(),
        panel.border=element_blank(),
        panel.spacing.y = unit(-20,"pt"),
        axis.text = element_text(size=8),
        axis.title = element_text(size=9),
        plot.margin = margin(t=0,l=4,r=4))

NAIS3_ion_plot=cowplot::plot_grid(neg_ion_Plot,pos_ion_Plot,nrow = 2,
                                  rel_heights = c(1.0,0.75),align="v")
show(NAIS3_ion_plot)