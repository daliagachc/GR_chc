#plots all NAIS 15 data for a specific day
#first loads the two scripts plot_NAIS3_ion_data.R for ions and the script
#plot_NAIS3_particle_data.R for particles.
#Afterwards, creates a combined plot.

#the date to be plotted:
#date = "2018-04-01"

# #load nais3data
#nais3DataDir = "D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3/data/level0"

#the directory of where the two scripts "plot_NAIS3_ion_data.R" and
#plot_NAIS3_particle_data.R are found.
#backslash (\) has to be replaced with slash (/)
scriptDir = "D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3/Rcode"

#the directory where the images get saved to.
figureDir="D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3_plots"

#the directory of the nais 3 data


#now run both ion and particle scripts
source(paste(scriptDir,"plot_NAIS3_ion_data.R",sep="/"))
source(paste(scriptDir,"plot_NAIS3_particle_data.R",sep="/"))

#I strip off certain elements from one of the plots because we don't need doble axis
neg_part_Plot=neg_part_Plot+theme(axis.text.x=element_blank())+labs(x="")+
  theme(plot.margin=margin(t=2))

#now create the combined plot
cowplot::plot_grid(neg_ion_Plot,pos_ion_Plot,neg_part_Plot,pos_part_Plot,nrow = 4,
                   align = "v", rel_heights=c(1,0.6,0.92,0.6))


#save the image
ggsave(paste0(date,".png"), plot = last_plot(),
       path = figureDir,
       scale = 1,dpi=305, device = "png")

