#' import npf event clas DM
#'
#' Imports the npf-event clasification file created by Diego Aliaga and ALkuin Koenig
#'
#' @param where path to the folder containing the file
#' @param filename name of the file with extension
#'
#' @return data table of the clasification file
#' @export
#'
#' @examples import_npf_event_clas_DM(yourplace,"event_clasification_Diego_Max.csv")
import_npf_event_clas_DM=function(where,filename){

  out=data.table::fread(paste(where,filename,sep="/"))

  out[is.na(out)]=0

  out$special=out$V8
  out$V8=NULL


  out$dateUTC=as.POSIXct(out$Day,format="%d.%m.%Y",tz="UTC")

  return(out)

}
