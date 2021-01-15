chisqplot <-
function(data, is.data = TRUE, p = NULL, xlab = "theoretical quantiles",
    ylab = "sample quantiles", main = expression(chi^2*plain(" Plot")),
    labels = NULL, line = TRUE, lty = 3, lcol = "red", pch = 20,
    identify = FALSE, icol=rgb(0,0,1,0.5), ...)
#### Chisquare plot ####
#
# data --- data matrix if  is.data=TRUE, otherwise, squared Mahalanobis
#          distances
# is.data --- logical flag (default=TRUE) indicating that argument data is
#             a data matrix or data frame when TRUE, otherwise, argument
#             data is a numeric vector of all Mahalanobis distances
# p --- number of variables
# xlab, ylab --- axes titles
# main --- main title
# labels --- observation labels
# line --- whether a straight line should be drawn
# lty --- lINE tyPE of the straight line
# lcol --- colOR of the straight line
# pch --- pLOTTING chARACTER, either a character or plotting
#         symbol code (1 to 25)
# identify --- whether the user would like to identify plotting
#              points
# icol --- color (default is semi-transparent blue, rgb(0,0,1,0.5))
#          of the labels for identified points
# ... --- other graphical parameters used by plot
#

{if (is.data){
  data <- as.matrix(data)
  n <- nrow(data);  p <- ncol(data)
  if (is.null(labels))
    labels <- if(is.null(rownames(data))) as.character(1:n)
                else rownames(data)
  data <- mahalanobis(data, colMeans(data), var(data))
 }
  else {
   n <- length(data)
   if (is.null(labels))
     labels <- if(is.null(names(data))) as.character(1:n)
                else names(data)
  }
 names(data) <- labels
 quant <- qchisq(ppoints(n)[order(order(data))], p)
 plot(quant, data, pch=pch, xlab=xlab, ylab=ylab,
      main=main, ...)
 if (line) abline(0, 1, lty=lty, col=lcol)
 if (identify) {
   xpd <- par()$xpd
   par(xpd=NA)
   identify(quant, data, label=names(data), col=icol)
   par(xpd=xpd)
 }
 invisible(data)
}