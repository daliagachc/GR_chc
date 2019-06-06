plotNaisData_posixct=function(time1, time2, diameters){
  time1=as.POSIXct(time1,tz="etc/GMT+4")
  time2=as.POSIXct(time2,tz="etc/GMT+4")
  dateToPlot=time1
  
  
  
  #
  #------------------------------
  plotNAIS=function(data){
    out=ggplot(data =data, aes(x=startTime))+
      geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
      scale_fill_gradientn(trans = "log10",
                           limits = c(1,0.2*10e4),
                           colours=rev(brewer.pal(5,"RdYlBu")))+
      annotation_logticks(base=10,sides = "lr")+
      scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,1e-7),
                         limits=c(1e-9,1e-7))+
      scale_x_datetime(date_breaks = "1 day",
                       labels = date_format("%d", tz="etc/GMT+4"),
                       position = "bottom",
                       expand = c(0.01,0.01))+
      theme(axis.title.x=element_blank())+
      labs(fill="dN/d(logDp)[cm^-3]",y="Dp[m]",col="")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            legend.text=element_text(size=12),
            legend.title=element_text(size=12))
    
    return(out)
  }
  #------------------------------
  
  addLines=function(wdata,plot){
    wdata=subset(wdata,flag==1)
    plot=plot+geom_line(data=wdata, aes(x=posixTime,y=dp*1e-9,group=eventID),size=1)
      #geom_line(data=wdata, aes(x=posixTime,y=dp*1e-9,group=eventID,col=eventID),size=0.8)
      
    return(plot)
  }
  
  #------------------
  wd=NAIS_ion_df
  attributes(wd$startTime)$tzone <- "etc/GMT+4" 
  attributes(wd$endTime)$tzone <- "etc/GMT+4" 
  wd=subset(wd, startTime >= time1 & startTime < time2)
  
  #plot the positive ions
  posplot=plotNAIS(subset(wd,ion=="positive"))+labs(subtitle=paste0(time1,"--",time2,"-positive ions"))
  
  #plot the negative ions
  negplot=plotNAIS(subset(wd,ion=="negative"))+labs(subtitle=paste0(time1,"--",time2,"-negative ions"))
  
  
  if (diameters==T){
    wd=npfevent_size_frame
    

    wd=subset(wd, posixTime >= time1 & posixTime < time2)
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
  #return(ptotal)
}
