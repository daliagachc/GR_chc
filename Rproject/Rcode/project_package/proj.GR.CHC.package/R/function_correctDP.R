#' Correct particle diamater
#'
#' Uses the correction function found by Joonas Enroth for correction NAIS particle diamater
#'
#' @param dp particle diameter in nanometers
#'
#' @return corrected particle diameter
#' @export
#'
#' @details Correction factor is: dp_corr=(10^(log10(dp)*(1/1.073))) . Have to double check this
#'
#' @examples correctDP(2.42)
correctDP = function(dp){
  return(10^(log10(dp)*(1/1.073)))
}
