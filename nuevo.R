# Cargar librerías necesarias
library(tidyverse)
library(readxl)
library(easystats)

# ========================================================================
# 1. LECTURA Y PREPARACIÓN DE DATOS
# ========================================================================

# Leer el archivo Excel
datos <- read_excel("C:/Users/LENOVO/Downloads/Libro2HEMOPUNO3.xlsx")

# Ver estructura inicial
glimpse(datos)
head(datos)

# Eliminar filas con NA en CUALQUIER columna
datos <- na.omit(datos)

# Preparar datos paso por paso
datos$sexo <- as.factor(datos$sexo)
datos$anemia <- ifelse(datos$hemoglobina < 11.0, 1, 0)
datos$anemia_factor <- factor(datos$anemia, levels = c(0, 1), labels = c("No", "Sí"))
datos$sexo_num <- as.numeric(datos$sexo) - 1
datos$edad_anios <- datos$edad_meses / 12

# ========================================================================
# 2. ESTADÍSTICOS DESCRIPTIVOS
# ========================================================================

# Resumen general
datawizard::describe_distribution(datos)

# Hemoglobina por sexo
datawizard::describe_distribution(datos, select = "hemoglobina", by = "sexo")

# Detectar outliers
performance::check_outliers(datos$hemoglobina, method = "zscore")

# Tabla de contingencia: Sexo y Anemia
table(datos$sexo, datos$anemia_factor)
prop.table(table(datos$sexo, datos$anemia_factor), margin = 1)

# ========================================================================
# 3. REGRESIÓN LINEAL: Edad → Hemoglobina
# ========================================================================

modelo_lm <- lm(hemoglobina ~ edad_meses, data = datos)

# Métricas del modelo
performance::model_performance(modelo_lm)

# Verificar supuestos
performance::check_model(modelo_lm)
performance::check_normality(modelo_lm)
performance::check_heteroscedasticity(modelo_lm)

# Coeficientes
params_lm <- parameters::model_parameters(modelo_lm, ci = 0.95)
params_lm

# Tamaños del efecto
effectsize::eta_squared(modelo_lm)

# Visualización
plot(params_lm) +
  labs(title = "Coeficientes - Regresión Lineal",
       subtitle = "Hemoglobina ~ Edad (meses)")
# Predicciones
pred_edad <- modelbased::estimate_expectation(modelo_lm, 
                                              data = data.frame(edad_meses = seq(0, 60, by = 6)))
pred_edad


# Gráfico de dispersión
ggplot(datos, aes(x = edad_meses, y = hemoglobina)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre Edad y Hemoglobina",
       x = "Edad (meses)", y = "Hemoglobina (g/dL)") +
  theme_minimal()

# Reporte completo
report::report(modelo_lm)

# ========================================================================
# 4. REGRESIÓN LOGÍSTICA: Sexo → Anemia
# ========================================================================

modelo_glm <- glm(anemia ~ sexo_num, data = datos, family = binomial(link = "logit"))

# Métricas
performance::model_performance(modelo_glm)

# Odds Ratios
params_or <- parameters::model_parameters(modelo_glm, ci = 0.95, exponentiate = TRUE)
params_or

# Tamaños del efecto
effectsize::effectsize(modelo_glm)

# Visualización
plot(params_or) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Odds Ratios - Anemia por Sexo") +
  scale_x_log10()

# Predicciones
modelbased::estimate_expectation(modelo_glm, 
                                 data = data.frame(sexo_num = c(0, 1)))

# Reporte
report::report(modelo_glm)

# ========================================================================
# 5. COMPARACIÓN DE GRUPOS: t-test (Hemoglobina por Sexo)
# ========================================================================

# t-test
t_test <- t.test(hemoglobina ~ sexo, data = datos, var.equal = TRUE)
parameters::model_parameters(t_test)

# Cohen's d
cohens <- effectsize::cohens_d(hemoglobina ~ sexo, data = datos)
cohens
effectsize::interpret_cohens_d(abs(cohens$Cohens_d))

# Visualización
ggplot(datos, aes(x = sexo, y = hemoglobina, fill = sexo)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.3, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Distribución de Hemoglobina por Sexo",
       subtitle = paste("Cohen's d =", round(cohens$Cohens_d, 3),
                        "| p =", format.pval(t_test$p.value, digits = 3)),
       x = "Sexo", y = "Hemoglobina (g/dL)") +
  theme_minimal() +
  theme(legend.position = "none")

# Reporte
report::report(t_test)

# ========================================================================
# 6. MODELO CON INTERACCIÓN: Edad × Sexo → Hemoglobina
# ========================================================================

modelo_inter <- lm(hemoglobina ~ edad_meses * sexo, data = datos)
modelo_aditivo <- lm(hemoglobina ~ edad_meses + sexo, data = datos)

# Comparación de modelos
performance::compare_performance(modelo_aditivo, modelo_inter)
performance::test_performance(modelo_aditivo, modelo_inter)

# Coeficientes
params_inter <- parameters::model_parameters(modelo_inter, ci = 0.95)
params_inter

# Tamaños del efecto
eta_inter <- effectsize::eta_squared(modelo_inter, partial = TRUE, ci = 0.95)
eta_inter

# Efectos marginales
efectos_marg <- modelbased::estimate_means(modelo_inter,
                                           at = c("edad_meses = c(0, 12, 24, 36, 48, 60)", "sexo"))
efectos_marg

# Visualización
ggplot(datos, aes(x = edad_meses, y = hemoglobina, color = sexo)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  labs(title = "Interacción: Edad × Sexo → Hemoglobina",
       x = "Edad (meses)", y = "Hemoglobina (g/dL)",
       color = "Sexo") +
  theme_minimal()

# Reporte
report::report(modelo_inter)

# ========================================================================
# 7. CORRELACIONES
# ========================================================================

# Matriz de correlaciones
vars_numericas <- datos %>% 
  select(edad_meses, hemoglobina, sexo_num, anemia)

mat_cor <- correlation::correlation(vars_numericas, method = "pearson")
mat_cor

# Visualización
plot(mat_cor) +
  labs(title = "Matriz de Correlaciones") +
  theme_minimal()

# ========================================================================
# 8. RESUMEN FINAL
# ========================================================================

# Resumen por sexo
datos %>%
  group_by(sexo) %>%
  summarise(
    n = n(),
    media_hb = mean(hemoglobina, na.rm = TRUE),
    sd_hb = sd(hemoglobina, na.rm = TRUE),
    min_hb = min(hemoglobina, na.rm = TRUE),
    max_hb = max(hemoglobina, na.rm = TRUE),
    prev_anemia = mean(anemia, na.rm = TRUE) * 100
  )

