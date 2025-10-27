required_packages <- c("meta", "metafor", "forestplot")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 2. Datos de ejemplo
study_data <- data.frame(
  study = c("Smith et al.", "Johnson et al.", "Williams et al.", 
            "Brown et al.", "Davis et al.", "Miller et al."),
  effect = c(0.45, 0.62, 0.28, 0.55, 0.38, 0.71),
  variance = c(0.08, 0.06, 0.12, 0.07, 0.09, 0.05),
  sample_size = c(100, 150, 80, 120, 90, 200),
  quality_score = c(8, 9, 7, 8, 6, 9)  # Puntuación de calidad
)

# 3. Análisis completo
final_results <- complete_meta_analysis(study_data)

# 4. Reporte detallado
generate_meta_report(final_results)

# 5. Análisis adicional con metafor
cat("\n=== ANÁLISIS AVANZADO CON METAFOR ===\n")
metafor_result <- rma(yi = effect, vi = variance, data = study_data, method = "DL")
print(metafor_result)

# 6. Meta-regresión con calidad del estudio
cat("\n=== META-REGRESIÓN CON CALIDAD ===\n")
meta_reg <- rma(yi = effect, vi = variance, mods = ~ quality_score, 
                data = study_data, method = "DL")
print(meta_reg)

# 7. Análisis de influencia
cat("\n=== ANÁLISIS DE INFLUENCIA ===\n")
influence_analysis <- influence(metafor_result)
print(influence_analysis)

# 8. Guardar resultados
output <- list(
  data = study_data,
  basic_analysis = final_results,
  metafor_analysis = metafor_result,
  meta_regression = meta_reg,
  influence_analysis = influence_analysis
)

# Guardar en archivo RData
save(output, file = "meta_analysis_results.RData")

cat("\n=== ANÁLISIS COMPLETADO ===\n")
cat("Resultados guardados en 'meta_analysis_results.RData'\n")

