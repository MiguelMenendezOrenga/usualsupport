CochranCont <- function(alfa = 0.05, sigma, E, N = Inf, correct = FALSE){
  Zalpha <- stats::qnorm(1 - alfa / 2) # alpha=0.05, a cada lado de la cola 0.025, percentil 0.975
  n <- Zalpha^2 * sigma^2 / E^2  # (esto es la fórmula de Cochran)

  # Quiero tener distintos valores de n para distintos valores de Error
  Evect <- seq(from = 0, to = E * 5, by = 0.1) # quiero que el Error sea un vector
  nvect <- Zalpha^2 * sigma^2 / Evect^2

  if(correct == TRUE){
    if(n > 0.05 * N){
      n <- n / (1 + n/N) # corrección si el tamaño obtenido superara el 5% de la población
      nvect <- nvect / (1 + nvect/N)
    }
  }

  value <- list("n" = n, "Error" = E, "alfa" = alfa, "sigma" = sigma,  "Evect" = Evect, "nvector" = nvect)
  print(round(value$n, 1))
  return(invisible(value))
}
