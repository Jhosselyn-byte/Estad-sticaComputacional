# Función para modelo de efectos fijos
fixed_effects_model <- function(effects, variances) {
  k <- length(effects)
  
  if (k == 0) {
    return(NULL)
  }
  
  # Calcular pesos (inversos de las varianzas)
  weights <- 1 / variances
  total_weight <- sum(weights)
  
  # Efecto combinado (media ponderada)
  combined_effect <- sum(weights * effects) / total_weight
  
  # Error estándar (raíz cuadrada de la varianza del estimador)
  se <- sqrt(1 / total_weight)
  
  # Intervalo de confianza (distribución normal)
  ci_lower <- combined_effect - 1.96 * se
  ci_upper <- combined_effect + 1.96 * se
  
  # Pesos normalizados (para interpretación)
  normalized_weights <- weights / total_weight
  
  return(list(
    combined_effect = combined_effect,
    se = se,
    ci = c(ci_lower, ci_upper),
    weights = normalized_weights,
    total_weight = total_weight
  ))
}

# Ejemplo de uso
effects <- c(0.5, 0.7, 0.3, 0.6)
variances <- c(0.1, 0.08, 0.12, 0.09)

fe_result <- fixed_effects_model(effects, variances)

cat("=== MODELO DE EFECTOS FIJOS ===\n")
cat("Efecto combinado:", round(fe_result$combined_effect, 4), "\n")
cat("Error estándar:", round(fe_result$se, 4), "\n")
cat("IC 95%: (", round(fe_result$ci[1], 4), ", ", round(fe_result$ci[2], 4), ")\n", sep = "")
cat("\nPesos de los estudios:\n")
for (i in 1:length(fe_result$weights)) {
  cat("  Estudio", i, ":", round(fe_result$weights[i], 3), 
      "(", round(fe_result$weights[i] * 100, 1), "%)\n")
}
# Función para estimar tau² (método DerSimonian-Laird)
estimate_tau_squared <- function(effects, variances) {
  k <- length(effects)
  
  if (k < 2) {
    return(0)
  }
  
  # Primero calculamos el modelo de efectos fijos
  weights_fe <- 1 / variances
  weighted_mean <- sum(weights_fe * effects) / sum(weights_fe)
  
  # Estadístico Q de Cochran
  Q <- sum(weights_fe * (effects - weighted_mean)^2)
  
  # Cálculo de tau²
  sum_weights <- sum(weights_fe)
  sum_squared_weights <- sum(weights_fe^2)
  
  if (Q <= (k - 1)) {
    tau_squared <- 0
  } else {
    numerator <- Q - (k - 1)
    denominator <- sum_weights - (sum_squared_weights / sum_weights)
    tau_squared <- numerator / denominator
  }
  
  return(max(0, tau_squared))
}

# Función para modelo de efectos aleatorios
random_effects_model <- function(effects, variances) {
  k <- length(effects)
  
  if (k == 0) {
    return(NULL)
  }
  
  # Estimar tau² (heterogeneidad entre estudios)
  tau_squared <- estimate_tau_squared(effects, variances)
  
  # Pesos ajustados (incluyen varianza entre estudios)
  weights_re <- 1 / (variances + tau_squared)
  total_weight_re <- sum(weights_re)
  
  # Efecto combinado
  combined_effect <- sum(weights_re * effects) / total_weight_re
  
  # Error estándar
  se <- sqrt(1 / total_weight_re)
  
  # Intervalo de confianza
  ci_lower <- combined_effect - 1.96 * se
  ci_upper <- combined_effect + 1.96 * se
  
  # Pesos normalizados
  normalized_weights <- weights_re / total_weight_re
  
  return(list(
    combined_effect = combined_effect,
    se = se,
    ci = c(ci_lower, ci_upper),
    weights = normalized_weights,
    tau_squared = tau_squared,
    total_weight = total_weight_re
  ))
}

# Ejemplo de uso
effects <- c(0.5, 0.7, 0.3, 0.6)
variances <- c(0.1, 0.08, 0.12, 0.09)

re_result <- random_effects_model(effects, variances)

cat("=== MODELO DE EFECTOS ALEATORIOS ===\n")
cat("Tau² estimado:", round(re_result$tau_squared, 4), "\n")
cat("Efecto combinado:", round(re_result$combined_effect, 4), "\n")
cat("Error estándar:", round(re_result$se, 4), "\n")
cat("IC 95%: (", round(re_result$ci[1], 4), ", ", round(re_result$ci[2], 4), ")\n", sep = "")
cat("\nPesos ajustados de los estudios:\n")
for (i in 1:length(re_result$weights)) {
  cat("  Estudio", i, ":", round(re_result$weights[i], 3), 
      "(", round(re_result$weights[i] * 100, 1), "%)\n")
}