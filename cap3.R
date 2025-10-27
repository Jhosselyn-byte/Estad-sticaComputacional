# Función para análisis completo de heterogeneidad
heterogeneity_analysis <- function(effects, variances) {
  k <- length(effects)
  
  if (k < 2) {
    return(list(
      Q = 0,
      df = k - 1,
      p_value = 1,
      I2 = 0,
      tau_squared = 0,
      tau = 0,
      interpretation = "No aplicable (k < 2)"
    ))
  }
  
  # Calcular estadístico Q de Cochran
  weights <- 1 / variances
  weighted_mean <- sum(weights * effects) / sum(weights)
  Q <- sum(weights * (effects - weighted_mean)^2)
  
  # Grados de libertad y valor p
  df <- k - 1
  p_value <- 1 - pchisq(Q, df)
  
  # Calcular I²
  if (Q <= df) {
    I2 <- 0
  } else {
    I2 <- ((Q - df) / Q) * 100
  }
  
  # Interpretar I²
  interpret_i2 <- function(I2) {
    if (I2 <= 25) {
      return("Baja heterogeneidad")
    } else if (I2 <= 50) {
      return("Moderada heterogeneidad")
    } else if (I2 <= 75) {
      return("Alta heterogeneidad")
    } else {
      return("Heterogeneidad considerable")
    }
  }
  
  # Calcular tau² y tau
  tau_squared <- estimate_tau_squared(effects, variances)
  tau <- sqrt(tau_squared)
  
  return(list(
    Q = Q,
    df = df,
    p_value = p_value,
    I2 = I2,
    tau_squared = tau_squared,
    tau = tau,
    interpretation = interpret_i2(I2)
  ))
}

# Ejemplo de uso
effects <- c(0.5, 0.7, 0.3, 0.6, 0.8, 0.4)
variances <- c(0.1, 0.08, 0.12, 0.09, 0.11, 0.07)

hetero_results <- heterogeneity_analysis(effects, variances)

cat("=== ANÁLISIS DE HETEROGENEIDAD ===\n")
cat("Estadístico Q:", round(hetero_results$Q, 4), "\n")
cat("Grados de libertad:", hetero_results$df, "\n")
cat("Valor p:", round(hetero_results$p_value, 4), "\n")
cat("I²:", round(hetero_results$I2, 2), "% -", hetero_results$interpretation, "\n")
cat("Tau²:", round(hetero_results$tau_squared, 4), "\n")
cat("Tau:", round(hetero_results$tau, 4), "\n")

# Interpretación adicional
cat("\n=== INTERPRETACIÓN ===\n")
if (hetero_results$p_value < 0.05) {
  cat("• Heterogeneidad estadísticamente significativa (p < 0.05)\n")
} else {
  cat("• Heterogeneidad no significativa (p ≥ 0.05)\n")
}

cat("•", round(hetero_results$I2, 1), "% de la variabilidad se debe a heterogeneidad real\n")
cat("• La desviación estándar entre estudios es", round(hetero_results$tau, 3), "\n")

