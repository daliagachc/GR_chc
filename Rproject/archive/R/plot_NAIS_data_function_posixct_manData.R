plotNaisData_posixct_manData=function(df,plotlabel){

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
      theme(axis.title.x=element_blank())+
      labs(fill="dN/d(logDp)[cm^-3]",y="Dp[m]",col="")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            legend.text=element_text(size=12),
            legend.title=element_text(size=12))
    
    return(out)
  }
  #------------------------------
  
  #plot the positive ions
  posplot=plotNAIS(subset(df,ion=="positive"))+labs(subtitle=paste0("-positive ions"))
  
  #plot the negative ions
  negplot=plotNAIS(subset(df,ion=="negative"))+labs(subtitle=paste0("-negative ions"))
  
  
  title <- cowplot::ggdraw() + draw_label(plotlabel, fontface='bold', size = 13)
  
  

  ptotal=cowplot::plot_grid(title,posplot,negplot,nrow=3,rel_heights = c(0.1,1,1))
  ptotal
  #return(ptotal)
}
