
#mad-distance calculation function. Double sided or single sided
#' Calculate MAD distance
#'
#' Calculates the MAD (median absolute derivation) distance for each point in a vector. \cr
#' This helps in detecting outliers in non-standard distributions
#'
#' @details It can be single sided (treat all values equally) or
#' double sided (treat values < median separately from values > median) \cr
#' I recommend to use double sided.
#'
#' @param vec The base vector for the MAD distance calculation. For each element in the vector
#' MAD distance respective to this vector will be calculated
#' @param how "single" or "double"
#'
#' @return a vector containing the MAD distances for elements in the vector
#' @export
#'
#' @examples get_MAD_distance(c(5,120,42,3,1,1,1,4,-3,6),"double")
get_MAD_distance=function(vec,how){
  x=na.omit(vec)
  m=median(x)
  abs.dev=abs(x-m)

  if (how == "single"){
    mad = median(abs.dev)
    mad_dist=abs.dev/mad
  }else  if (how =="double"){
    mad.left=median(abs.dev[x<=m])
    mad.right=median(abs.dev[x>=m])
    mad_dist=ifelse(x<=m, abs.dev/mad.left, abs.dev/mad.right)
  }
  else {mad_dist=NA}
  return(mad_dist)
}#get_MAD_distance
