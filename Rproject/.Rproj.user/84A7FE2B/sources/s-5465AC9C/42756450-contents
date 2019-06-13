formatterExp <- function(x){ 
  round(exp(x),2)
}

#scale_y_continuous(labels = formatter1000)


#------------------------------DATA HANDLING
#getting the evaluated splines
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,30) %>%
  mutate(YMD=format(Time,"%Y-%m-%d"))

mask=npfevent_size_frame %>%
  select(eventID,ion)
mask=unique(mask)

wd=merge(splined_rep_df,mask) %>%
  arrange(eventID)

init=1
end=25
n=8
sequence=seq(log(init),log(end),(log(end)-log(init))/n)
sequence=exp(sequence)
sequence=round(sequence,1)
#sequence=seq(1,40,2)

wd$cuts=cut(wd$dp_splined,breaks=sequence,include.lowest=T)
wd=na.omit(wd)
#levels(wd$cuts)=as.character(seq(1,40,2))

wd.s=wd %>%
  dplyr::group_by(cuts)%>%
  dplyr::summarise(
    meanDer=mean(dp_splined_der,na.rm=T),
    medianDer=median(dp_splined_der,na.rm=T))
  
wd.s$lowcut=convertToLimit(wd.s$cuts,0)
wd.s$highcut=convertToLimit(wd.s$cuts,1)

wd.s$lowcut[1]=1
wd.s$midPoint=exp((log(wd.s$lowcut)+log(wd.s$highcut))/2)
wd.s$midPoint_log=log(wd.s$midPoint)

fit=lm(medianDer~midPoint_log,wd.s)
ggplotRegression(fit)+
  scale_x_continuous(breaks=log(sequence),labels=formatterExp)+
  labs(y="GR [nm/h]",x="Dp [nm]")
