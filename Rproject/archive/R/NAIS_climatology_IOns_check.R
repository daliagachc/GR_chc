wd1=subset(NAIS_climatology[[5]],ion=="negative")
wd2=subset(NAIS_climatology[[5]],ion=="positive")

wd3=NAIS_get_binned_ratio(wd1,wd2,"adwf",T)

plotNAIS_hourlyBinned_norm(wd3,"none")