plotErrorSample <- function(x, point = TRUE, printlegend = TRUE, ...){
  graphics::plot(x$Evect, x$nvect, type = "l", ...)
  if(point == TRUE){
    graphics::points(x$Error, x$n, type = "p")
    graphics::text(x$Error + x$Error / 2, x$n, paste("n = ", round(x$n, 1)))
  }
  if(printlegend == TRUE){
    if(any(names(x) %in% c("sigma", "p"))){
      graphics::legend("topright",
                       legend = c(paste("alfa =", x$alfa),
                                  # this chunk checks wether "sigma" or "p/q" should be printed
                                  if(any(names(x) == "p")){
                                    paste("p/q =", x$p, "/", 1-x$p)
                                  }else{
                                    if(any(names(x) == "sigma")){
                                      paste("σ =", x$sigma)
                                    }
                                  }
                       ),
                       bty = "n")

    }else{
      warning(
      "No sigma nor p/q values could be found. Please check the input used.
  If it's OK and you don't want to get this warning, set printlegend = FALSE")
    }
  }
}
