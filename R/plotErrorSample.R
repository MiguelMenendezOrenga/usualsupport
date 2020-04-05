plotErrorSample <- function(x, point = TRUE, printlegend = TRUE, ...){
  graphics::plot(x$Evect, x$nvect, type = "l", ...)
  if(point == TRUE){
    graphics::points(x$Error, x$n, type = "p")
    graphics::text(x$Error + x$Error / 2, x$n, paste("n = ", round(x$n, 1)))
  }
  if(printlegend == TRUE){
    graphics::legend("topright",
                     legend = c(paste("alfa =", x$alfa),
                                paste("p/q =", x$p, "/", 1-x$p),
                                paste("Error =", x$Error)),
                     bty = "n")
  }
}
