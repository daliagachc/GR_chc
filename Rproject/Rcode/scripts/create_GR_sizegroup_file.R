#create a table with GR for typical sizegroups


#' tempfun_create_GR_sizegroup_csv
#'
#' This script creates a table with GR for typical sizegroups
#'
#' tempfun because the function format is only for better encapsulation. 
#' It will get exectuted at the end of this script (Scroll down to change parameters)
#'
#' @param npfevent_size_frame This data has to get created first
#' @param eventIDclas same here
#' @param traj_sparfac the spar factor for the splines
#' @param size_breaks the sizebreaks for the dp binning
#' @param digits significant digits of numerical values in output file
#'
#' @return nothing ,creates a file
#'
#' @examples none
tempfun_create_GR_sizegroup_csv=function(npfevent_size_frame,
                                         eventIDclas,
                                         traj_sparfac,
                                         size_breaks,
                                         digits){
  #create the trajectories.
  #I select only continued trajectories wich are the FIRST event of the day
  workData=subset(npfevent_size_frame,continued==1 & flag==1 &dailyevent==1)
  
  helplist=create_splined_trajectories_V2(workData,eventIDclas,sparfac=traj_sparfac)
  filt.tray.df=helplist[[1]]
  splined.smooth_functions=helplist[[2]]
  
  #Now I create a dataframe which contains eventID, score10 (my score over 10 points for
  #the "cleaniness" of the trajectory) and airChange (my flag if I detected an airchange
  #)
  info_mask=data.frame(eventID=splined.smooth_functions[[1]],
                       score10=splined.smooth_functions[[3]],
                       airchange=splined.smooth_functions[[4]])
  
  #evaluating the splines every eval_time seconds
  splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                               filt.tray.df,eval_time)
  splined_rep_df=splined_rep_df%>%
    mutate(YMD=format(Time,"%Y-%m-%d"),
           ion=getCharge_eventID(eventID),
           index=getInd_eventID(eventID))


  #cutting into sizegroups and evaluating by group
  splined_rep_df$dp_sizegroup=cut(splined_rep_df$dp_splined,breaks=size_breaks,
                                  include.lowest=T)
  
  splined_rep_df.s=splined_rep_df %>%
    dplyr::group_by(YMD,dp_sizegroup,ion) %>%
    dplyr::summarise(GR_median=signif(median(dp_splined_der,na.rm=T),digits),
                     GR_mean=signif(mean(dp_splined_der,na.rm=T),digits),
                     GR_sd=signif(sd(dp_splined_der,na.rm=T),digits),
                     eventID=first(eventID))
  splined_rep_df.s=na.omit(splined_rep_df.s)
  
  #Now merging with the info_mask we created early to give access to all info
  splined_rep_df.s=dplyr::inner_join(splined_rep_df.s,info_mask, by="eventID") %>%
    rename("score_over_10"=score10,"airmass_change_flag"=airchange)
  
  
  #creating filename and writing the file
  filename="GR_CHC_splined_approach_all_events.csv"
  write.csv(splined_rep_df.s,file=paste0(here::here(),"/results/files/",filename))
  
}#tempfun_create_GR_sizegroup_csv

#parameters for execution
traj_sparfac=0.96
eval_time=30
size_breaks=c(1,3,7,20,60)
digits=3

tempfun_create_GR_sizegroup_csv(npfevent_size_frame,
                                         eventIDclas,
                                         traj_sparfac,
                                         size_breaks,
                                         digits)