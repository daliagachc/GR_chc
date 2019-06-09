
#' Combine ACSM and HOM data
#'
#' Combines ACSM data with HOM data after first getting the hourly median of each.
#'
#' @param acsmdata ACSM data
#' @param homdata HOM data
#'
#' @return a combined data frame
#' @export
#'
#' @examples none
combine_acsm_hom=function(acsmdata,homdata){
  wd1=acsm_data_wide %>%
    mutate(YMDH=format(localTime,"%Y-%m-%d %H"),
           localTime=NULL)

  wd2=HOM_data %>%
    mutate(YMDH=format(localTime,"%Y-%m-%d %H"),
           localTime=NULL)

  wd3=merge(wd1,wd2,by="YMDH") %>%
    dplyr::group_by(YMDH) %>%
    dplyr::summarise(Organics=median(Organics,na.rm=T),
                     Sulfate=median(Sulfate,na.rm=T),
                     Ammonium=median(Ammonium,na.rm=T),
                     Chloride=median(Chloride,na.rm=T),
                     Nitrate=median(Nitrate,na.rm=T),
                     HOM=median(HOM_FORM_APITOF,na.rm=T)) %>%
    mutate(localTime=as.POSIXct(YMDH, format="%Y-%m-%d %H",tz="etc/GMT+4"),
           dateTimeUTC=localTime)
  attributes(wd3$dateTimeUTC$dateTimeUTC)$tzone="UTC"

  return(wd3)
}
