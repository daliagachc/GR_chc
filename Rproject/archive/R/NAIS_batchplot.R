#batch creating all plots

vec=seq(as.Date("2017/12/01"), by = "day", length.out = 200)
vec=as.character(vec)


workDir=getwd()
setwd(paste0(workDir,"/R/figs/batch"))


dateList=gsub(".sum","",NAIS_ion_list_indices)
for (i in 1:length(vec)){
  k=vec[i]
  posfile_position=which(dateList==paste0(k,"p"))
  negfile_position=which(dateList==paste0(k,"n"))
  
  #only plot if data available
  if (length(posfile_position)>=1 & length(negfile_position)>=1){
    theplot=plotNaisData_YMD(k,T)
    
    ggsave(paste0(k,".png"), plot = theplot,
           scale = 1, dpi = 300, device = "png")  
  }
}

setwd(workDir)

