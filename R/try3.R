#' Compute correlation between two numerical variables
#'
#' Compute correlation between two numerical variables and return a "htest" object, the method is Spearman correlation
#' @export
#' @param x numerical vector of the 1st var
#' @param y numerical vector of the 2nd var
MyCorr <- function(x, y){
  return(cor.test(x, y, method = "spearman"))
}

#' Visualize correlation between two numerical variables
#'
#' Visualize correlation between two numerical variables by scatter plot, with Spearman correlation statistics shown in title
#' @export
#' @param x numerical vector of the 1st var
#' @param y numerical vector of the 2nd var
MyCorr.plot <- function(x, y){
  plot(x=x, y=y,
       xlab = "X", ylab = "Y",
       main = paste("Spearman Corr = ", signif(cor.test(x, y, method = "spearman")$estimate, 3), "\n", "Spearman P = ", signif(cor.test(x, y, method = "spearman")$p.value, 3), sep = ""))
  abline(lm(y ~ x), lwd = 2, col="red")
}
