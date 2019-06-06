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

#first, loading utils, example dp correction, NAIS plotting, MAD calculation
#source(paste0(here::here(),"/R/2018_03_16/load_utility_functions.R"),echo=F)

#imports eventID clasificationFIle
eventIDclas=fread(paste0(here::here(),"/data","/eventID-spreadsheet.csv"))

#import GR-mode information
#source(paste0(here::here(),"/R/2018_03_16/function_import_GR_data.R"),echo=F)
npfevent_size_frame=import_GR_mode_dat(paste0(here::here(),"/data/NAIS3/NAIS3/ions"),F)

#now calculating growthrates the classical way
#source(paste0(here::here(),"/R/2018_03_16/function_calculate_GR_classic.R"),echo=F)
workData=subset(npfevent_size_frame,flag==1 & continued==1)
helplist=calculate_GR_classic(workData)
event_GR_wide=helplist[[1]]
event_GR_long=helplist[[2]]

#now creating the splined trajectories.
#selecting only the non-interrupted, marked as good and first events of the day.
#source(paste0(here::here(),"/R/2018_03_16/function_create_splined_trajectories.R"),echo=F)
workData=subset(npfevent_size_frame,continued==1 & flag==1 &dailyevent==1)
helplist=create_splined_trajectories(workData,eventIDclas,sparfac=0.96)
filt.tray.df=helplist[[1]]
splined.smooth_functions=helplist[[2]]

#preloading the acsm data
#source(paste0(here::here(),"/R/2018_03_16/function_import_acsm_data.R"),echo=F)
acsm_data_wide=import_acsm_dat(paste0(here::here(),"/data"),"CHC_QACSM.csv")[[1]]
acsm_data_long=import_acsm_dat(paste0(here::here(),"/data"),"CHC_QACSM.csv")[[2]]

#preloading the SA data
#source(paste0(here::here(),"/R/2018_03_16/function_import_SA_data.R"),echo=F)
SA_data=import_SA_dat(paste0(here::here(),"/data"),"GEORGE_SA.csv")

#preloading the HOM data
#source(paste0(here::here(),"/R/2018_03_16/function_import_HOM_data.R"),echo=F)
HOM_data=import_HOM_dat(paste0(here::here(),"/data"),"HOMs_API_CIAPI.csv")

#preloading CS_kappa_data
#source(paste0(here::here(),"/R/2018_03_16/function_import_CS_kappa_data.R"),echo=F)
CS_kappa_data=import_CS_kappa_dat(paste0(here::here(),"/data"),"CS_dist_chacaltaya_kappa.sum")

#combining all that juicy data
#source(paste0(here::here(),"/R/2018_03_16/function_combine_acsm_SA_HOM_CS_kappa.R"),echo=F)
ACSM_API_CS_KAPPA_combined=combine_acsm_SA_HOM_CS_kappa(acsm_data_long,HOM_data,SA_data,CS_kappa_data)

#loading the function to re-evaluate the splined trajectories
#source(paste0(here::here(),"/R/2018_03_16/function_evaluate_splined_trajectories.R"),echo=F)

#loading meteo file
#source(paste0(here::here(),"/R/2018_03_16/function_import_meteo_file.R"),echo=F)
meteo_file=import_meteo_dat(paste0(here::here(),"/data"),"meteo_cumbre.csv")

#loading the NAIS data
#source(paste0(here::here(),"/R/2018_03_16/function_import_NAIS_data_2013_2018.R"),echo=F)
NAIS_ion_df=import_NAIS_data_2013_2018(2018)
