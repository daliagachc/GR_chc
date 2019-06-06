plotNaisData_YMD=function(time1, diameters){
  dateToPlot=time1
  #
  #------------------------------
  plotNAIS=function(data){
    out=ggplot(data =data, aes(x=startTime))+
      geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
      scale_fill_gradientn(trans = "log10",
                           limits = c(1,10000),
                           colours=rev(brewer.pal(5,"RdYlBu")),oob=squish)+
      annotation_logticks(base=10,sides = "lr")+
      scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,1e-7),
                         limits=c(1e-9,1e-7))+
      scale_x_datetime(date_breaks = "4 hours",
                       labels = date_format("%H:%M"),
                       position = "bottom", expand = c(0.01,0.01))+
      theme(axis.title.x=element_blank())+
      labs(fill="dN/d(logDp)[cm^-3]",y="Dp[m]",col="")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            legend.text=element_text(size=10),
            legend.title=element_text(size=10))
    
    return(out)
  }
  #------------------------------
  
  addLines=function(wdata,plot){
    wdata=subset(wdata,flag==1)
    plot=plot+geom_line(data=wdata, aes(x=UTCTime,y=dp*1e-9,group=eventID),size=2)+
      geom_line(data=wdata, aes(x=UTCTime,y=dp*1e-9,group=eventID,col=eventID),size=0.8)#+
      #geom_point(data=wdata,aes(x=UTCTime,y=dp*1e-9,group=eventID))

    return(plot)
  }
  
  #------------------
  
  dateList=gsub(".sum","",NAIS_ion_list_indices)
  
  posfile_position=which(dateList==paste0(dateToPlot,"p"))
  negfile_position=which(dateList==paste0(dateToPlot,"n"))
  
  posfile=NAIS_ion_list[[posfile_position]]
  negfile=NAIS_ion_list[[negfile_position]]
  
  #plot the positive ions
  posplot=plotNAIS(posfile)+labs(subtitle=paste0(dateToPlot,"-positive ions"))
  
  #plot the negative ions
  negplot=plotNAIS(negfile)+labs(subtitle=paste0(dateToPlot,"-negative ions"))
  
  
  if (diameters==T){
    wd=npfevent_size_frame%>%
      mutate(YMD=format(UTCTime,"%Y-%m-%d"))
    wd=subset(wd, YMD==time1)
    if (nrow(wd)==0){
      print("No growth rate data available for this day.")
    }
    
    
    wdpos=subset(wd,ion=="positive")
    wdneg=subset(wd,ion=="negative")
    
    posplot=addLines(wdpos,posplot)
    negplot=addLines(wdneg,negplot)
  }
  

  ptotal=cowplot::plot_grid(posplot,negplot,nrow=2)
  ptotal
  return(ptotal)
}
