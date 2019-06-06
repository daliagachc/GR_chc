evaluate_splined_trajectories=function(splined.smooth_functions,filt.tray.df,timestep){
  #returns a dataframe with the spline function evaluated (both y and first derivate) at
  #"timestep" intervals (in seconds). 
  
  #filt.tray.df is only used to get the starting times for trajectories
  wd=filt.tray.df %>%
    select(eventID,localTime,unfiltered,splined.smoothed.x)%>%
    dplyr::group_by(eventID)%>%
    mutate(timeSinceStart=splined.smoothed.x-min(splined.smoothed.x))%>%
    as.data.frame()%>%
    arrange(eventID,localTime)
  
  
  
  splined_rep_list=list()
  for (i in 1:length(unique(wd$eventID))){
    sel=unique(wd$eventID)[i]
    
    #creating the values for the splined function
    ind=which(splined.smooth_functions[[1]]==sel)
    sfunction=splined.smooth_functions[[2]][[ind]]
    
    help=subset(wd,eventID == sel)
    min=min(help$splined.smoothed.x,na.rm=T)
    max=max(help$splined.smoothed.x,na.rm=T)
    timesteps=seq(min,max,timestep)
    
    splined_rep_list[[i]]=data.frame(eventID=sel,secTime=timesteps,dp_splined=predict(sfunction,timesteps)$y,
                                     dp_splined_der=predict(sfunction,timesteps,deriv=1)$y*3600,
                                     timeSinceStart=timesteps-min) %>%
      mutate(Time=as.POSIXct(secTime,origin="1970/01/01 UTC"))
  }
  splined_rep_df=do.call("rbind",splined_rep_list) %>%
    mutate(YMD=format(Time,"%Y-%m-%d"))
  
  return(splined_rep_df)
}#evaluate_splined_trajectories