library(meta)
library(forestplot)

# Ejemplo 1: Meta-análisis de datos continuos
meta_analysis_continuous <- function() {
  # Datos de ejemplo para diferencia de medias
  data_continuous <- data.frame(
    study = c("Study A", "Study B", "Study C", "Study D", "Study E"),
    n.e = c(50, 60, 45, 55, 65),  # Tamaño muestra grupo experimental
    mean.e = c(25.3, 26.1, 24.8, 25.9, 26.5),  # Media grupo experimental
    sd.e = c(4.5, 4.2, 4.8, 4.1, 4.3),  # DE grupo experimental
    n.c = c(50, 60, 45, 55, 65),  # Tamaño muestra grupo control
    mean.c = c(22.1, 23.2, 21.8, 22.9, 23.1),  # Media grupo control
    sd.c = c(4.2, 4.0, 4.5, 4.3, 4.1)   # DE grupo control
  )
  
  # Meta-análisis de diferencia de medias
  meta_cont <- metacont(
    n.e = n.e,
    mean.e = mean.e,
    sd.e = sd.e,
    n.c = n.c,
    mean.c = mean.c,
    sd.c = sd.c,
    data = data_continuous,
    studlab = study,
    method.tau = "DL",  # Método DerSimonian-Laird para tau²
    comb.fixed = TRUE,
    comb.random = TRUE
  )
  
  # Mostrar resultados
  print(summary(meta_cont))
  
  # Crear forest plot
  forest(meta_cont, 
         leftcols = c("studlab", "mean", "sd", "n.e", "n.c"),
         rightcols = c("effect", "ci"))
  
  return(meta_cont)
}

# Ejemplo 2: Meta-análisis de datos binarios
meta_analysis_binary <- function() {
  # Datos de ejemplo para odds ratio
  data_binary <- data.frame(
    study = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5"),
    event.e = c(15, 20, 12, 18, 22),  # Eventos grupo experimental
    n.e = c(100, 120, 80, 110, 130),  # Total grupo experimental
    event.c = c(8, 12, 6, 10, 14),    # Eventos grupo control
    n.c = c(100, 120, 80, 110, 130)   # Total grupo control
  )
  
  # Meta-análisis de odds ratio
  meta_bin <- metabin(
    event.e = event.e,
    n.e = n.e,
    event.c = event.c,
    n.c = n.c,
    data = data_binary,
    studlab = study,
    method = "Inverse",
    method.tau = "DL",
    sm = "OR"  # Odds Ratio como medida de efecto
  )
  
  # Mostrar resultados
  print(summary(meta_bin))
  
  # Crear forest plot
  forest(meta_bin,
         leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
         rightcols = c("effect", "ci"))
  
  return(meta_bin)
}

# ejemplos
cat("=== META-ANÁLISIS DATOS CONTINUOS ===\n")
result_cont <- meta_analysis_continuous()

cat("\n=== META-ANÁLISIS DATOS BINARIOS ===\n")
result_bin <- meta_analysis_binary()

library(metafor)

# Ejemplo 1: Meta-análisis con efectos aleatorios
metafor_analysis <- function() {
  # Datos de ejemplo
  data <- data.frame(
    study = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5", 
              "Study 6", "Study 7", "Study 8"),
    yi = c(0.45, 0.62, 0.28, 0.55, 0.38, 0.71, 0.49, 0.33),  # Efectos
    vi = c(0.08, 0.06, 0.12, 0.07, 0.09, 0.05, 0.08, 0.10)   # Varianzas
  )
  
  # Meta-análisis de efectos aleatorios
  res <- rma(yi = yi, vi = vi, data = data, method = "DL")
  
  # Mostrar resultados detallados
  print(res)
  
  # Crear forest plot
  forest(res, slab = data$study,
         xlab = "Tamaño del efecto", 
         main = "Meta-Análisis - Modelo de Efectos Aleatorios")
  
  # Añadir intervalo de predicción
  addpoly(res, row = -1, cex = 0.8, mlab = "RE Modelo")
  
  # Crear funnel plot para sesgo de publicación
  funnel(res, main = "Funnel Plot - Evaluación de sesgo")
  
  # Test de Egger para sesgo de publicación
  regtest(res)
  
  return(res)
}

# Ejemplo 2: Meta-regresión
meta_regression_example <- function() {
  # Datos con variable moderadora
  data_mod <- data.frame(
    study = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
    yi = c(0.45, 0.62, 0.28, 0.55, 0.38, 0.71, 0.49, 0.33),
    vi = c(0.08, 0.06, 0.12, 0.07, 0.09, 0.05, 0.08, 0.10),
    moderator = c(2.1, 3.4, 1.8, 2.9, 2.3, 3.8, 2.7, 2.0)  # Variable moderadora
  )
  
  # Meta-regresión
  res_mod <- rma(yi = yi, vi = vi, mods = ~ moderator, data = data_mod, method = "DL")
  
  print(res_mod)
  
  # Graficar relación
  plot(data_mod$moderator, data_mod$yi, 
       xlab = "Variable moderadora", 
       ylab = "Tamaño del efecto",
       main = "Meta-Regresión",
       pch = 19, col = "blue", cex = 2)
  
  # Añadir línea de regresión
  abline(a = res_mod$b[1], b = res_mod$b[2], col = "red", lwd = 2)
  
  return(res_mod)
}

# ejemplos
cat("=== META-ANÁLISIS CON METAFOR ===\n")
result_metafor <- metafor_analysis()

cat("\n=== META-REGRESIÓN ===\n")
result_metareg <- meta_regression_example()