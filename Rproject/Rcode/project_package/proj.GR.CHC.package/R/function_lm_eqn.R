#' Add linear model equation
#'
#' This function adds the results of a linear model to a plot. It is deprecated by myself,
#' but might linger around in some code. I will remove it at some point.
#'
#' @param df the dataframe. You calculate the model y~x
#'
#' @return an expression of the results of the linear model
#' @export
#'
#' @examples none
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
