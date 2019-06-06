plotNaisData=function(timedate){
  dateToPlot=timedate
  #
  #------------------------------
  plotNAIS=function(data){
    out=ggplot(data =data, aes(x=startTime))+
      geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
      scale_fill_gradientn(trans = "log10",
                           limits = c(1,max(wd2$value)),
                           colours=rev(brewer.pal(5,"RdYlBu")))+
      annotation_logticks(base=10,sides = "lr")+
      scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,1e-7),
                         limits=c(1e-9,1e-7))+
      theme(axis.title.x=element_blank())
    
    return(out)
  }
  #------------------------------
  
  dateList=gsub(".sum","",NAIS_ion_list_indices)
  
  posfile_position=which(dateList==paste0(dateToPlot,"p"))
  negfile_position=which(dateList==paste0(dateToPlot,"n"))
  
  posfile=NAIS_ion_list[[posfile_position]]
  negfile=NAIS_ion_list[[negfile_position]]
  
  #plot the positive ions
  posplot=plotNAIS(posfile)+labs(subtitle=paste0(dateToPlot,"-positive ions"))
  
  #plot the negative ions
  negplot=plotNAIS(negfile)+labs(subtitle=paste0(dateToPlot,"-negative ions"))
  
  ptotal=cowplot::plot_grid(posplot,negplot,nrow=2)
  ptotal
  #return(ptotal)
}
