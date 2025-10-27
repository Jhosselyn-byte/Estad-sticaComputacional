#cap1
# Función para calcular diferencia de medias (MD)
calculate_md <- function(mean1, mean2, sd1, sd2, n1, n2) {
  return(mean1 - mean2)
}

# Función para calcular diferencia de medias estandarizada (SMD)
calculate_smd <- function(mean1, mean2, sd1, sd2, n1, n2) {
  pooled_sd <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
  return((mean1 - mean2) / pooled_sd)
}

# Función para calcular riesgo relativo (RR)
calculate_rr <- function(a, b, c, d) {
  risk_treatment <- a / (a + b)
  risk_control <- c / (c + d)
  return(risk_treatment / risk_control)
}

# Función para calcular odds ratio (OR)
calculate_or <- function(a, b, c, d) {
  return((a * d) / (b * c))
}

# Función para calcular diferencia de riesgos (RD)
calculate_rd <- function(a, b, c, d) {
  risk_treatment <- a / (a + b)
  risk_control <- c / (c + d)
  return(risk_treatment - risk_control)
}

# Ejemplo de uso
# Datos continuos
md <- calculate_md(25.3, 22.1, 4.5, 4.2, 50, 50)
smd <- calculate_smd(25.3, 22.1, 4.5, 4.2, 50, 50)

# Datos binarios
rr <- calculate_rr(30, 70, 20, 80)  # 30 eventos en 100 tratados, 20 en 100 controles
or_val <- calculate_or(30, 70, 20, 80)
rd <- calculate_rd(30, 70, 20, 80)

cat("MD:", round(md, 3), "\n")
cat("SMD:", round(smd, 3), "\n")
cat("RR:", round(rr, 3), "\n")
cat("OR:", round(or_val, 3), "\n")
cat("RD:", round(rd, 3), "\n")

