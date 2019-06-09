#' Convert to limit
#'
#' utility function that obtains factor limits from its respective character vector,
#' obtained by the cut function.
#'
#' @details For example: (0,5] could indicate the range "x >0, x <= 5". convertToLimit("(0,5]",0)
#' would then return 0 and convertToLimit("(0,5]",1) would return 5
#'
#' @param x the character/factor value/vector from which limits get extracted
#' @param i flag. If i = 0, the lower limit gets extracted. If i = 1, the higher limit.
#'
#' @return the desired limit
#' @export
#'
#' @examples convertToLimit("(0,5]",0)
#' convertToLimit("(0,5]",5)
convertToLimit=function(x,i){#gets the limit value (i=0, lower..i=1, upper) for an
  #interval created with the cut function.
  if (i==0){#get lower limit
    help=gsub('\\(','',x)
    help=gsub('\\[','',x)
    help=gsub(',.*',"",help)
  } else if (i==1){#get upper limit
    help=gsub('.*,','',x)
    help=gsub('\\]','',help)
  } else{return(-999.9)}#return far away value as error code
  return(as.numeric(help))
}

