#' Plot NAIS
#'
#' @param data NAIS data to be plotted
#'
#' @return a ggplot
#' @export
#'
#' @examples none
plotNAIS=function(data){
  out=ggplot(data =data, aes(x=startTime))+
    geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
    scale_fill_gradientn(trans = "log10",
                         limits = c(1,0.2*10e4),
                         colours=rev(brewer.pal(5,"RdYlBu")))+
    # annotation_logticks(base=10,sides = "lr")+
    scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,1e-7),
                       limits=c(1e-9,1e-7))+
    scale_x_datetime(date_breaks = "1 day",
                     labels = date_format("%d", tz="etc/GMT+4"),
                     position = "top",
                     expand = c(0.01,0.01))+
    theme(axis.title.x=element_blank())+
    labs(fill="dN/d(logDp)[cm^-3]",y="Dp[m]",col="")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12))

  return(out)
}
