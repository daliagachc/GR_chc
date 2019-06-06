#' Get event ID index
#'
#' Gets the index of an event ID of the form "Pos42". In this case, it would return "42"
#'
#' @param eventID An event ID of form "Pos42","Neg203",etc
#'
#' @return The index as a character
#' @export
#'
#' @examples getInd_eventID("Neg12")
getInd_eventID = function(eventID){
  charge=gsub("Neg","",eventID)
  charge=gsub("Pos","",charge)
  return(charge)
}
