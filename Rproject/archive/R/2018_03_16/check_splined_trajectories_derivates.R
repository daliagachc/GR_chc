#getting the evaluated splines
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,30) %>%
  mutate(YMD=format(Time,"%Y-%m-%d"))

ggplot(splined_rep_df,aes(x=dp_splined,y=dp_splined_der,group=eventID))+
  geom_line()+
  #facet_wrap(~eventID)+
  labs()
