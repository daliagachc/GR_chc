#mad-distance calculation function. Double sided or single sided
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
}
