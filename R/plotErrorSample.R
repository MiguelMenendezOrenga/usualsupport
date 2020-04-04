plotErrorSample <- function(x, point = TRUE, ...){
  graphics::plot(x$Evect, x$nvect, type = "l", ...)
  if(point == TRUE){
    graphics::points(x$Error, x$n, type = "p")
    graphics::text(x$Error + x$Error / 2, x$n, paste("n = ", round(x$n, 1)))
    graphics::legend("topright", legend = paste("alfa =", x$alfa), bty = "n")
  }
}
