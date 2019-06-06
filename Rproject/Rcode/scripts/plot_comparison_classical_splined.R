#getting the evaluated splines
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,30)


classic=plotGRclassic(npfevent_size_frame, event_GR_long, eventIDclas)+
  labs(title="classical approach")
splined=plotGR_splined_compa(splined_rep_df, npfevent_size_frame)+
                               labs(title="splined approach")+
  theme(axis.title.y=element_blank())

ggarrange(classic,splined,nrow=1,ncol=2,common.legend=T,legend="right")