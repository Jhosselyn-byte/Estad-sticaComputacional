# Función para análisis completo de meta-análisis
complete_meta_analysis <- function(study_data, study_names = NULL, 
                                   model_type = "auto", make_plots = TRUE) {
  
  # Verificar y preparar datos
  if (is.data.frame(study_data)) {
    effects <- study_data$effect
    variances <- study_data$variance
    if (is.null(study_names) && "study" %in% names(study_data)) {
      study_names <- study_data$study
    }
  } else {
    effects <- study_data$effects
    variances <- study_data$variances
  }
  
  if (is.null(study_names)) {
    study_names <- paste("Estudio", 1:length(effects))
  }
  
  k <- length(effects)
  
  # Análisis de heterogeneidad
  hetero_results <- heterogeneity_analysis(effects, variances)
  
  # Decidir modelo basado en heterogeneidad
  if (model_type == "auto") {
    if (hetero_results$I2 >= 50 || hetero_results$p_value < 0.05) {
      use_model <- "random"
      model_result <- random_effects_model(effects, variances)
    } else {
      use_model <- "fixed"
      model_result <- fixed_effects_model(effects, variances)
    }
  } else if (model_type == "random") {
    use_model <- "random"
    model_result <- random_effects_model(effects, variances)
  } else {
    use_model <- "fixed"
    model_result <- fixed_effects_model(effects, variances)
  }
  
  # Crear plots si se solicita
  if (make_plots) {
    create_comprehensive_plots(effects, variances, study_names, 
                               model_result, hetero_results, use_model)
  }
  
  # Preparar resultados
  results <- list(
    study_data = data.frame(
      study = study_names,
      effect = effects,
      variance = variances,
      weight_fe = fixed_effects_model(effects, variances)$weights,
      weight_re = random_effects_model(effects, variances)$weights
    ),
    heterogeneity = hetero_results,
    model_used = use_model,
    model_results = model_result,
    recommendation = paste("Modelo recomendado:", use_model, 
                           "(I² =", round(hetero_results$I2, 1), "%)")
  )
  
  return(results)
}

# Función para crear gráficos completos
create_comprehensive_plots <- function(effects, variances, study_names, 
                                       model_result, hetero_results, model_type) {
  
  # Configurar área de gráficos
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  
  # 1. Forest plot básico
  ci_lower <- effects - 1.96 * sqrt(variances)
  ci_upper <- effects + 1.96 * sqrt(variances)
  
  plot(1, type = "n", xlim = c(min(ci_lower) - 0.1, max(ci_upper) + 0.1), 
       ylim = c(0.5, length(effects) + 1), 
       xlab = "Tamaño del efecto", ylab = "", yaxt = "n")
  
  # Añadir línea de efecto nulo
  abline(v = 0, col = "gray", lty = 2)
  
  # Añadir intervalos de confianza de estudios individuales
  for (i in 1:length(effects)) {
    lines(c(ci_lower[i], ci_upper[i]), c(i, i), col = "blue", lwd = 2)
    points(effects[i], i, pch = 19, col = "blue", cex = 1.2)
  }
  
  # Añadir efecto combinado
  abline(v = model_result$combined_effect, col = "red", lwd = 2, lty = 1)
  lines(model_result$ci, c(length(effects) + 0.5, length(effects) + 0.5), 
        col = "red", lwd = 3)
  
  axis(2, at = 1:length(effects), labels = study_names, las = 1, cex.axis = 0.7)
  title(paste("Forest Plot - Modelo de", ifelse(model_type == "random", "Efectos Aleatorios", "Efectos Fijos")))
  
  # 2. Gráfico de pesos
  weights_fe <- fixed_effects_model(effects, variances)$weights
  weights_re <- random_effects_model(effects, variances)$weights
  
  barplot(rbind(weights_fe, weights_re), beside = TRUE,
          names.arg = study_names, las = 2, cex.names = 0.7,
          col = c("lightblue", "lightcoral"),
          main = "Distribución de Pesos",
          ylab = "Peso del estudio")
  legend("topright", legend = c("Efectos Fijos", "Efectos Aleatorios"), 
         fill = c("lightblue", "lightcoral"), cex = 0.8)
  
  # 3. Funnel plot
  se <- sqrt(variances)
  plot(effects, 1/se, pch = 19, col = "blue",
       xlab = "Tamaño del efecto", ylab = "Precisión (1/SE)",
       main = "Funnel Plot")
  abline(v = model_result$combined_effect, col = "red", lty = 2)
  
  # 4. Gráfico de heterogeneidad
  barplot(c(hetero_results$I2, 100 - hetero_results$I2),
          names.arg = c("Heterogeneidad", "Azar"),
          col = c("orange", "lightgreen"),
          main = paste("I² =", round(hetero_results$I2, 1), "%"),
          ylab = "Porcentaje")
  
  # Restaurar configuración de gráficos
  par(mfrow = c(1, 1))
}

# Función para generar reporte
generate_meta_report <- function(results) {
  cat("=== REPORTE COMPLETO DE META-ANÁLISIS ===\n\n")
  cat("INFORMACIÓN GENERAL:\n")
  cat("Número de estudios:", nrow(results$study_data), "\n")
  cat("Modelo utilizado:", results$model_used, "\n")
  cat("Recomendación:", results$recommendation, "\n\n")
  
  cat("HETEROGENEIDAD:\n")
  cat("Q =", round(results$heterogeneity$Q, 3), 
      "(df =", results$heterogeneity$df, 
      ", p =", round(results$heterogeneity$p_value, 4), ")\n")
  cat("I² =", round(results$heterogeneity$I2, 1), "% -", 
      results$heterogeneity$interpretation, "\n")
  cat("Tau² =", round(results$heterogeneity$tau_squared, 4), "\n")
  cat("Tau =", round(results$heterogeneity$tau, 4), "\n\n")
  
  cat("RESULTADOS DEL MODELO:\n")
  cat("Efecto combinado:", round(results$model_results$combined_effect, 4), "\n")
  cat("Error estándar:", round(results$model_results$se, 4), "\n")
  cat("IC 95%: (", round(results$model_results$ci[1], 4), ", ", 
      round(results$model_results$ci[2], 4), ")\n\n", sep = "")
  
  cat("DISTRIBUCIÓN DE PESOS:\n")
  weights_df <- data.frame(
    Estudio = results$study_data$study,
    Efecto = round(results$study_data$effect, 3),
    `Peso FE` = paste0(round(results$study_data$weight_fe * 100, 1), "%"),
    `Peso RE` = paste0(round(results$study_data$weight_re * 100, 1), "%")
  )
  print(weights_df, row.names = FALSE)
}

# Ejemplo de uso completo
# Crear datos de ejemplo
example_data <- data.frame(
  study = c("A", "B", "C", "D", "E", "F", "G"),
  effect = c(0.45, 0.62, 0.28, 0.55, 0.38, 0.71, 0.49),
  variance = c(0.08, 0.06, 0.12, 0.07, 0.09, 0.05, 0.08)
)

#análisis completo
results <- complete_meta_analysis(example_data)

generate_meta_report(results)

