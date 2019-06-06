mask=npfevent_size_frame[,c("posixTime","eventID")] %>%
  mutate(month=month(posixTime))
mask=unique(mask)


wd=merge(event_GR_wide,mask,by="eventID",all.x=T)

wd=wd %>%
  mutate(month=month(posixTime)) %>%
  dplyr::group_by(month)%>%
  dplyr::summarise(maxmedian=median(maxsize),
                   maxmean=mean(maxsize),
                   max=max(maxsize),
                   min=min(maxsize))


# wd=event_GR_wide %>%
#   mutate(month=month())
#   dplyr::group_by()

