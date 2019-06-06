#' normalize it
#'
#' a function offering different ways of normalizing a vector
#'
#' @param vec the (numerical) vector to be normalized
#' @param how how to normalize. Accepted: "norma","max","median","mean"
#'
#' @return the normalized vector
#' @export
#'
#' @examples normalizeIt(c(5,4,10,11,4),"norma")
#' normalizeIt(c(5,4,10,11,4),"max")
normalizeIt=function(vec,how){
  if (how=="norma"){
    normfac=sqrt(sum(vec^2,na.rm=T))
  }
  if (how=="max"){
    normfac=max(vec,na.rm=T)
  }
  if (how=="median"){
    normfac=median(vec,na.rm=T)
  }
  if (how=="mean"){
    normfac=mean(vec,na.rm=T)
  }
  return(vec/normfac)
}
