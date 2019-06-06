#prepare the workspace

library(tidyverse)
library(data.table)
library(RColorBrewer)
library(scales)
library(lubridate)
#library(cowplot)
library(scales)
library(ggpubr)
library(signal)
library("R.matlab")
library(here)

library(proj.GR.CHC.package)

#imports eventID clasificationFIle
eventIDclas=fread(paste0(here::here(),"/data/raw","/eventID-spreadsheet.csv"))

#import GR-mode information
npfevent_size_frame=import_GR_mode_dat(paste0(here::here(),"/data/raw//NAIS3/NAIS3/ions"),F)

#now calculating growthrates the classical way
workData=subset(npfevent_size_frame,flag==1 & continued==1)
helplist=calculate_GR_classic(workData)
event_GR_wide=helplist[[1]]
event_GR_long=helplist[[2]]

#now creating the splined trajectories.
#selecting only the non-interrupted, marked as good and first events of the day.
workData=subset(npfevent_size_frame,continued==1 & flag==1 &dailyevent==1)
helplist=create_splined_trajectories(workData,eventIDclas,sparfac=0.96)
filt.tray.df=helplist[[1]]
splined.smooth_functions=helplist[[2]]

#preloading the acsm data
acsm_data_wide=import_acsm_dat(paste0(here::here(),"/data/raw"),"CHC_QACSM.csv")[[1]]
acsm_data_long=import_acsm_dat(paste0(here::here(),"/data/raw"),"CHC_QACSM.csv")[[2]]

#preloading the SA data
SA_data=import_SA_dat(paste0(here::here(),"/data/raw"),"GEORGE_SA.csv")

#preloading the HOM data
HOM_data=import_HOM_dat(paste0(here::here(),"/data/raw"),"HOMs_API_CIAPI.csv")

#preloading CS_kappa_data
CS_kappa_data=import_CS_kappa_dat(paste0(here::here(),"/data/raw"),"CS_dist_chacaltaya_kappa.sum")

#combining all that juicy data
ACSM_API_CS_KAPPA_combined=combine_acsm_SA_HOM_CS_kappa(acsm_data_long,HOM_data,SA_data,CS_kappa_data)

#loading meteo file
meteo_file=import_meteo_dat(paste0(here::here(),"/data/raw"),"meteo_cumbre.csv")

#loading the NAIS data
NAIS_ion_df=import_NAIS_data_2013_2018(paste(here::here(),"data/raw/NAIS3/NAIS3_sum_20132017",sep="/"),
                                       2018)
