# GR_chc
info and data about growth rates at CHC during the SALTENA campaign 2017.12-2018.06
- folders
  - code 
  - results
  - data 
      - data in this folder should not be too large ~<100mb 
      - for larger datasets use server at uhel
      
- result figures are here:
[Rproject/results/figures/](Rproject/results/figures/)

- event classification csv is here:
[Rproject/data/processed/CHC_eventDays_Diego_Max.csv](Rproject/data/processed/CHC_eventDays_Diego_Max.csv)
      
- File containing calculated growth rates (GR) using the splined approach is here:

[Rproject/results/files/GR_CHC_splined_approach_all_events.csv](Rproject/results/files/GR_CHC_splined_approach_all_events.csv)

    Columns to be interpreted as follows: 
    - dp_sizegroup: Size interval for the growing mode [nm]
    - ion:  negative or positive ions
    - GR_median and GR_mean: Growth rates [nm/h] using the respective statistic
    - GR_iqr and GR_sd: interquartile range and standard derivation of growth rates [nm/h]
    - eventID: The event ID I (Max) assigned to the specific growing event.
    - score_over_10: The score (1-10) I (Max) gave the growing event on inspection. Empirical!
    - airmass_change_flag: A flag I (Max) created basing myself on wind and cluster data. If 0, no airmass change during the event. If 1,         airmass change during the event. If 2, unsure. Empirical!
    
    I suggest using "score_over_10" > 5 and airmass_change == 0 as filters to get "clean events". Alternatively, growing events with high dispersion (see IQR and SD) can be excluded
      
      
- presentation for the Bolivia meeting 2019.06 is
[here](Presentations/bolivia_meeting_2019/Presentation_bolivia_meeting_Max_Diego.md)
