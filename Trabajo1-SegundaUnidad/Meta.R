calcular_heterogeneidad <- function(efectos, varianzas) {
  k <- length(efectos)
  pesos <- 1 / varianzas
  
  efecto_global <- sum(pesos * efectos) / sum(pesos)
  
  Q <- sum(pesos * (efectos - efecto_global)^2)
  
  I2 <- ifelse(Q > (k-1), 
               ((Q - (k-1)) / Q) * 100, 0)
  
  C <- sum(pesos) - (sum(pesos^2) / sum(pesos))
  tau2 <- ifelse(Q > (k-1), 
                 (Q - (k-1)) / C, 0)
  
  return(list(Q = Q, I2 = I2, tau2 = tau2, 
              efecto_global = efecto_global, k = k))
}
# Ejemplo de uso
efectos <- c(0.8, 1.2, 0.5, 1.4)
varianzas <- c(0.1, 0.125, 0.083, 0.111)
resultados <- calcular_heterogeneidad(efectos, varianzas)

print(resultados)


