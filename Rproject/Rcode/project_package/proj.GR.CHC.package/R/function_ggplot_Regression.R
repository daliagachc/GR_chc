#' ggplot Regression
#'
#' plot a linear regression fit with ggplot
#'
#' @param fit :A fit created with lm()
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' fit=lm(x,y,mydata)
#' ggplotRegression(fit)
#'
ggplotRegression <- function (fit) {

  require(ggplot2)

  p=ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(subtitle = paste("R2 = ",signif(summary(fit)$r.squared, 2),
                          "Intercept =",signif(fit$coef[[1]],2 ),"\n",
                          " Slope =",signif(fit$coef[[2]], 2),
                          " P =",signif(summary(fit)$coef[2,4], 2)))+
    theme(title=element_text(size=10))
  return(p)
}
