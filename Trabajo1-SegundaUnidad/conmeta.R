library(metafor)

# Crear conjunto de datos
datos_estudios <- data.frame(
  estudio = paste("Estudio", 1:6),
  yi = c(0.45, 0.68, 0.29, 0.55, 0.12, 0.78),
  vi = c(0.05, 0.08, 0.03, 0.06, 0.04, 0.07)
)

# Meta-análisis de efectos aleatorios
resultado <- rma(yi = yi, vi = vi, 
                 data = datos_estudios, 
                 method = "DL")
print(resultado)

# Visualizar resultados
forest(resultado, main = "Forest Plot")

