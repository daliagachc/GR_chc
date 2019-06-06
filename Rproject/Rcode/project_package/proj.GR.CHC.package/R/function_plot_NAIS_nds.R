#' plot NAIS nds
#'
#' plots nais data in nds format
#'
#' @param data data to be plotted
#'
#' @return a ggplot object
#' @export
#'
#' @examples none
plotNAIS_nds=function(data){
  out=ggplot(data =data, aes(x=startTime))+
    geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
    scale_fill_gradientn(trans = "log10",
                         colours=rev(brewer.pal(5,"RdYlBu")))+
    # annotation_logticks(base=10,sides = "lr")+
    scale_y_continuous(trans="log10")+
    scale_x_datetime(date_breaks = "1 hour",
                     labels = date_format("%H", tz="etc/GMT+4"),
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
