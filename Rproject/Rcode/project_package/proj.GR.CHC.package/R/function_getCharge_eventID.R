#' Get charge from event ID
#'
#' EventIDs are given to positive and negative ion growing events. For example: Pos101 is the
#' growing event 101 of a positive ion. This function extracts the charge, "positive" in this case.
#'
#' @param eventID An eventID of the form "Pos123","Neg42",etc
#'
#' @return The charge of the event. "positive" or "negative"
#' @export
#'
#' @examples getCharge_eventID("Pos101")
getCharge_eventID = function(eventID){
  charge=gsub("Neg.*","Neg",eventID)
  charge=gsub("Pos.*","Pos",charge)
  charge[charge=="Pos"]="positive"
  charge[charge=="Neg"]="negative"
  return(charge)
}
