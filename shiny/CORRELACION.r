
library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(htmltools)
library(scales)

# --- Configuración de Colores Profesionales ---
COLORS <- list(
  primary = "#2C3E50",      # Azul oscuro profesional
  secondary = "#34495E",    # Gris azulado
  success = "#27AE60",      # Verde estadístico
  info = "#3498DB",         # Azul información
  warning = "#F39C12",      # Naranja advertencia
  danger = "#E74C3C",       # Rojo significancia
  light = "#ECF0F1",        # Gris claro fondo
  accent = "#9B59B6"        # Púrpura acento
)

# --- Dataset: INCORE 2023 ---
datos_peru <- data.frame(
  Region = c("Moquegua", "Lima", "Ica", "Arequipa", "Tacna", "La Libertad",
             "Lambayeque", "Cusco", "Piura", "San Martín", "Junín", "Pasco",
             "Ayacucho", "Puno", "Amazonas", "Huánuco", "Apurímac", 
             "Cajamarca", "Huancavelica", "Loreto"),
  PBI_per_capita_soles = c(49168, 24580, 21890, 21567, 18430, 14500,
                           13890, 16789, 12500, 11800, 11500, 15300,
                           9800, 9500, 10120, 8500, 8970, 9100, 7560, 13200),
  Esperanza_vida_anios = c(77.5, 78.9, 77.1, 76.8, 76.5, 75.8,
                           75.5, 75.2, 74.9, 74.6, 74.3, 74.0,
                           73.8, 73.5, 74.5, 73.0, 74.1, 73.2, 73.8, 74.2),
  Analfabetismo_porcentaje = c(2.1, 1.9, 3.5, 2.8, 3.1, 6.5,
                               7.1, 8.1, 8.5, 9.1, 9.8, 8.9,
                               11.2, 11.5, 9.5, 12.1, 12.5, 11.9, 11.8, 6.0),
  Acceso_Salud_porcentaje = c(85.2, 90.1, 88.4, 87.9, 86.5, 82.1,
                              81.5, 79.5, 78.2, 77.5, 76.8, 75.1,
                              74.3, 72.9, 78.0, 71.5, 75.3, 70.1, 72.1, 76.4)
)

# --- Nomenclatura de Variables ---
vars_dict <- c(
  "PBI per cápita (S/)" = "PBI_per_capita_soles",
  "Esperanza de Vida (años)" = "Esperanza_vida_anios",
  "Tasa de Analfabetismo (%)" = "Analfabetismo_porcentaje",
  "Acceso a Salud (%)" = "Acceso_Salud_porcentaje"
)

ui <- navbarPage(
  title = "INCORE 2023: Análisis Estadístico de Competitividad Regional",
  theme = shinytheme("cosmo"),
  windowTitle = "INCORE Dashboard",
  
  # ---------------------------------------------------------------------------
  # PESTAÑA 1: MARCO TEÓRICO
  # ---------------------------------------------------------------------------
  tabPanel(
    "Marco Teórico",
    icon = icon("graduation-cap"),
    
    fluidPage(
      withMathJax(),
      
      # Encabezado
      div(
        style = paste0("background: linear-gradient(135deg, ", COLORS$primary, " 0%, ", COLORS$secondary, " 100%); 
                       color: white; padding: 30px; margin: -15px -15px 30px -15px; border-radius: 0;"),
        h1("Fundamentos de Análisis de Correlación", style = "margin: 0; font-weight: 300;"),
        p("Marco conceptual para el análisis estadístico de asociación entre variables", 
          style = "margin: 10px 0 0 0; font-size: 16px; opacity: 0.9;")
      ),
      
      # Sección 1: Definición de Correlación
      fluidRow(
        column(12,
               div(
                 style = paste0("background: white; padding: 25px; border-left: 4px solid ", COLORS$info, "; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
                 h3(icon("link"), "Definición", style = paste0("color: ", COLORS$primary, "; margin-top: 0;")),
                 p("La correlación es una medida estadística que cuantifica el grado y dirección de la asociación lineal entre dos variables cuantitativas. Se fundamenta en los siguientes principios:", style = "font-size: 15px;"),
                 tags$ul(
                   style = "font-size: 15px; line-height: 1.8;",
                   tags$li(strong("Asociación, no causalidad:"), "Una correlación significativa indica que las variables covarían, pero no establece relación causa-efecto."),
                   tags$li(strong("Linealidad:"), "Los coeficientes paramétricos miden específicamente relaciones lineales, no captando patrones curvilíneos."),
                   tags$li(strong("Sensibilidad a outliers:"), "Los valores atípicos pueden distorsionar sustancialmente el coeficiente de correlación."),
                   tags$li(strong("Variables confusoras:"), "Una tercera variable no observada puede generar correlaciones espurias.")
                 )
               )
        )
      ),
      
      # Sección 2: Coeficientes
      fluidRow(
        # Pearson
        column(6,
               div(
                 style = paste0("background: white; padding: 20px; border-top: 3px solid ", COLORS$success, "; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
                 h4(icon("calculator"), " Coeficiente de Pearson (r)", style = paste0("color: ", COLORS$primary)),
                 p(strong("Definición:"), "Mide la fuerza y dirección de la relación lineal entre dos variables continuas mediante la covarianza normalizada.", style = "font-size: 14px;"),
                 
                 div(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; margin: 15px 0;",
                     p(strong("Fórmula:"), style = "margin-bottom: 10px;"),
                     withMathJax(p('$$r = \\frac{\\sum_{i=1}^{n}(x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum_{i=1}^{n}(x_i - \\bar{x})^2} \\sqrt{\\sum_{i=1}^{n}(y_i - \\bar{y})^2}}$$'))
                 ),
                 
                 p(strong("Propiedades:"), style = "margin-top: 15px;"),
                 tags$ul(
                   style = "font-size: 13px;",
                   tags$li("Rango: -1 ≤ r ≤ +1"),
                   tags$li("Invariante ante transformaciones lineales"),
                   tags$li("Requiere normalidad bivariada para inferencia"),
                   tags$li("Sensible a valores extremos")
                 ),
                 
                 div(style = paste0("background: ", COLORS$light, "; padding: 10px; border-radius: 4px; margin-top: 15px;"),
                     p(strong("Coeficiente de Determinación (R²):"), style = "margin: 0; font-size: 13px;"),
                     p("R² = r² representa la proporción de varianza explicada. Un R² = 0.64 indica que el 64% de la variabilidad de Y es explicada por X.", style = "margin: 5px 0 0 0; font-size: 13px;")
                 )
               )
        ),
        
        # Spearman y Kendall
        column(6,
               div(
                 style = paste0("background: white; padding: 20px; border-top: 3px solid ", COLORS$accent, "; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
                 h4(icon("sort-numeric-up"), " Coeficientes No Paramétricos", style = paste0("color: ", COLORS$primary)),
                 
                 div(style = "margin-bottom: 20px;",
                     p(strong("Spearman (ρ)"), style = "font-size: 14px; color: #8e44ad; margin-bottom: 10px;"),
                     p("Correlación de Pearson aplicada a los rangos de las observaciones:", style = "font-size: 13px;"),
                     div(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; margin: 10px 0;",
                         withMathJax(p('$$\\rho = 1 - \\frac{6\\sum d_i^2}{n(n^2-1)}$$')),
                         p("donde \\(d_i\\) es la diferencia entre rangos de cada par de observaciones", style = "font-size: 12px; margin-top: 10px;")
                     ),
                     tags$ul(
                       style = "font-size: 13px;",
                       tags$li("Evalúa relaciones monótonas (no necesariamente lineales)"),
                       tags$li("Robusto ante distribuciones no normales"),
                       tags$li("Adecuado para variables ordinales")
                     )
                 ),
                 
                 div(style = "margin-bottom: 20px;",
                     p(strong("Kendall (τ)"), style = "font-size: 14px; color: #8e44ad; margin-bottom: 10px;"),
                     p("Basado en la concordancia entre pares de observaciones:", style = "font-size: 13px;"),
                     div(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; margin: 10px 0;",
                         withMathJax(p('$$\\tau = \\frac{n_c - n_d}{\\frac{1}{2}n(n-1)}$$')),
                         p("donde \\(n_c\\) = pares concordantes y \\(n_d\\) = pares discordantes", style = "font-size: 12px; margin-top: 10px;")
                     ),
                     tags$ul(
                       style = "font-size: 13px;",
                       tags$li("Menor sensibilidad a empates que Spearman"),
                       tags$li("Preferible en muestras pequeñas (n < 30)"),
                       tags$li("Interpretación: P(concordancia) - P(discordancia)")
                     )
                 ),
                 
                 div(style = paste0("background: ", COLORS$light, "; padding: 10px; border-radius: 4px;"),
                     p(strong("Criterio de Selección:"), style = "margin: 0; font-size: 13px;"),
                     p("Use Pearson si: datos continuas, relación lineal, distribución normal. Use Spearman/Kendall si: datos ordinales, outliers, relación monótona no lineal.", style = "margin: 5px 0 0 0; font-size: 13px;")
                 )
               )
        )
      ),
      
      # Sección 3: Interpretación
      fluidRow(
        column(12,
               div(
                 style = paste0("background: white; padding: 25px; border-left: 4px solid ", COLORS$warning, "; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
                 h3(icon("chart-line"), " Interpretación de la Magnitud del Coeficiente", style = paste0("color: ", COLORS$primary, "; margin-top: 0;")),
                 p("La interpretación de |r| sigue las convenciones de Cohen (1988) adaptadas al contexto de investigación:", style = "font-size: 15px; margin-bottom: 20px;"),
                 
                 div(style = "overflow-x: auto;",
                     tags$table(
                       class = "table table-bordered table-hover",
                       style = "font-size: 14px; margin: 0;",
                       tags$thead(
                         style = paste0("background: ", COLORS$primary, "; color: white;"),
                         tags$tr(
                           tags$th("Magnitud |r|", style = "padding: 12px;"),
                           tags$th("Clasificación", style = "padding: 12px;"),
                           tags$th("R² (% Varianza)", style = "padding: 12px;"),
                           tags$th("Interpretación Visual", style = "padding: 12px;")
                         )
                       ),
                       tags$tbody(
                         tags$tr(
                           tags$td("0.90 - 1.00", style = "padding: 10px;"),
                           tags$td(tags$span(style = paste0("color: ", COLORS$success, "; font-weight: bold;"), "Muy Fuerte"), style = "padding: 10px;"),
                           tags$td("81% - 100%", style = "padding: 10px;"),
                           tags$td("Puntos muy próximos a la línea de regresión", style = "padding: 10px;")
                         ),
                         tags$tr(
                           style = "background: #f8f9fa;",
                           tags$td("0.70 - 0.89", style = "padding: 10px;"),
                           tags$td(tags$span(style = paste0("color: ", COLORS$info, "; font-weight: bold;"), "Fuerte"), style = "padding: 10px;"),
                           tags$td("49% - 79%", style = "padding: 10px;"),
                           tags$td("Patrón lineal claro con dispersión moderada", style = "padding: 10px;")
                         ),
                         tags$tr(
                           tags$td("0.40 - 0.69", style = "padding: 10px;"),
                           tags$td(tags$span(style = paste0("color: ", COLORS$accent, "; font-weight: bold;"), "Moderada"), style = "padding: 10px;"),
                           tags$td("16% - 48%", style = "padding: 10px;"),
                           tags$td("Tendencia visible pero con variabilidad notable", style = "padding: 10px;")
                         ),
                         tags$tr(
                           style = "background: #f8f9fa;",
                           tags$td("0.20 - 0.39", style = "padding: 10px;"),
                           tags$td(tags$span(style = paste0("color: ", COLORS$warning, "; font-weight: bold;"), "Débil"), style = "padding: 10px;"),
                           tags$td("4% - 15%", style = "padding: 10px;"),
                           tags$td("Relación difícil de percibir visualmente", style = "padding: 10px;")
                         ),
                         tags$tr(
                           tags$td("0.00 - 0.19", style = "padding: 10px;"),
                           tags$td(tags$span(style = paste0("color: ", COLORS$danger, "; font-weight: bold;"), "Muy Débil/Nula"), style = "padding: 10px;"),
                           tags$td("0% - 4%", style = "padding: 10px;"),
                           tags$td("Ausencia de patrón lineal discernible", style = "padding: 10px;")
                         )
                       )
                     )
                 )
               )
        )
      ),
      
      # Sección 4: Inferencia
      fluidRow(
        column(12,
               div(
                 style = paste0("background: white; padding: 25px; border-left: 4px solid ", COLORS$danger, "; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
                 h3(icon("flask"), " Prueba de Hipótesis y Significancia Estadística", style = paste0("color: ", COLORS$primary, "; margin-top: 0;")),
                 
                 fluidRow(
                   column(6,
                          p(strong("Hipótesis Nula (H₀):"), "ρ = 0 (No existe correlación poblacional)", style = "font-size: 14px;"),
                          p(strong("Hipótesis Alternativa (H₁):"), "ρ ≠ 0 (Existe correlación poblacional)", style = "font-size: 14px;"),
                          p(strong("Estadístico de Prueba:"), style = "font-size: 14px; margin-top: 15px;"),
                          withMathJax(p('$$t = r\\sqrt{\\frac{n-2}{1-r^2}} \\sim t_{n-2}$$')),
                          p("Donde n es el tamaño muestral y r el coeficiente observado.", style = "font-size: 13px;")
                   ),
                   column(6,
                          div(style = paste0("background: ", COLORS$light, "; padding: 15px; border-radius: 4px;"),
                              p(strong("Interpretación del P-valor:"), style = "margin: 0 0 10px 0;"),
                              tags$ul(
                                style = "font-size: 13px; margin: 0;",
                                tags$li(strong("p < 0.001:"), "Evidencia muy fuerte contra H₀"),
                                tags$li(strong("p < 0.01:"), "Evidencia fuerte contra H₀"),
                                tags$li(strong("p < 0.05:"), "Evidencia moderada contra H₀ (umbral convencional)"),
                                tags$li(strong("p ≥ 0.05:"), "Evidencia insuficiente para rechazar H₀")
                              )
                          ),
                          div(style = paste0("background: #fff3cd; padding: 10px; border-left: 3px solid ", COLORS$warning, "; margin-top: 15px;"),
                              p(icon("exclamation-triangle"), strong(" Advertencia:"), "La significancia estadística no implica relevancia práctica. Un n grande puede producir p-valores pequeños con correlaciones débiles.", style = "margin: 0; font-size: 13px;")
                          )
                   )
                 )
               )
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # PESTAÑA 2: ANÁLISIS BIVARIADO
  # ---------------------------------------------------------------------------
  tabPanel(
    "Análisis Bivariado",
    icon = icon("chart-scatter"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = paste0("background: ", COLORS$light, "; border-right: 1px solid #ddd;"),
        
        div(
          style = paste0("background: ", COLORS$primary, "; color: white; padding: 15px; margin: -15px -15px 20px -15px;"),
          h4(icon("sliders-h"), " Panel de Control", style = "margin: 0; font-weight: 300;")
        ),
        
        p("Configure los parámetros del análisis de correlación:", style = "font-size: 13px; margin-bottom: 20px;"),
        
        selectInput(
          "var_x",
          label = div(icon("arrow-right"), "Variable Independiente (X):"),
          choices = vars_dict,
          selected = "PBI_per_capita_soles"
        ),
        
        selectInput(
          "var_y",
          label = div(icon("arrow-up"), "Variable Dependiente (Y):"),
          choices = vars_dict,
          selected = "Esperanza_vida_anios"
        ),
        
        radioButtons(
          "metodo_corr",
          label = div(icon("calculator"), "Método de Correlación:"),
          choices = c(
            "Pearson (paramétrico)" = "pearson",
            "Spearman (no paramétrico)" = "spearman",
            "Kendall (no paramétrico)" = "kendall"
          ),
          selected = "pearson"
        ),
        
        # Nuevo: Panel de datos personalizados
        hr(),
        h5(icon("database"), " Datos Personalizados", style = paste0("color: ", COLORS$primary)),
        p("Ingrese datos para calcular correlación:", style = "font-size: 12px; margin-bottom: 10px;"),
        
        textAreaInput("datos_x", "Datos X (separados por coma):", 
                      value = paste(datos_peru$PBI_per_capita_soles, collapse = ","),
                      rows = 3),
        
        textAreaInput("datos_y", "Datos Y (separados por coma):", 
                      value = paste(datos_peru$Esperanza_vida_anios, collapse = ","),
                      rows = 3),
        
        actionButton("calcular_corr", "Calcular Correlación", 
                     style = paste0("background: ", COLORS$success, "; color: white;")),
        
        hr(),
        
        div(
          style = "background: white; padding: 10px; border-radius: 4px; font-size: 12px;",
          p(strong("Fuente de datos:"), style = "margin: 0 0 5px 0;"),
          p("INCORE 2023 - Instituto Peruano de Economía (IPE)", style = "margin: 0;"),
          p("n = 20 regiones", style = "margin: 5px 0 0 0; color: #7f8c8d;")
        )
      ),
      
      mainPanel(
        width = 9,
        
        # Título dinámico
        uiOutput("header_bivariado"),
        
        # Métricas principales
        fluidRow(
          column(3, uiOutput("metric_coef")),
          column(3, uiOutput("metric_r2")),
          column(3, uiOutput("metric_fuerza")),
          column(3, uiOutput("metric_pvalor"))
        ),
        
        br(),
        
        # Interpretación
        div(
          style = "background: white; padding: 20px; border-radius: 4px; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
          h4(icon("lightbulb"), " Interpretación Estadística", style = paste0("color: ", COLORS$primary, "; margin-top: 0;")),
          uiOutput("interpretacion_detallada")
        ),
        
        # Gráfico de dispersión
        div(
          style = "background: white; padding: 20px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
          h4(icon("chart-scatter"), " Diagrama de Dispersión y Modelo de Regresión Lineal", 
             style = paste0("color: ", COLORS$primary, "; margin-top: 0;")),
          plotlyOutput("plot_scatter", height = "550px"),
          p("Nota: La línea representa el ajuste por mínimos cuadrados ordinarios (MCO). Los intervalos de confianza muestran la incertidumbre del modelo.",
            style = "font-size: 12px; color: #7f8c8d; margin-top: 15px; margin-bottom: 0;")
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # PESTAÑA 3: ANÁLISIS MULTIVARIADO 3D
  # ---------------------------------------------------------------------------
  tabPanel(
    "Análisis Multivariado",
    icon = icon("cube"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = paste0("background: ", COLORS$light, "; border-right: 1px solid #ddd;"),
        
        div(
          style = paste0("background: ", COLORS$primary, "; color: white; padding: 15px; margin: -15px -15px 20px -15px;"),
          h4(icon("cubes"), " Configuración 3D", style = "margin: 0; font-weight: 300;")
        ),
        
        p("Explore relaciones entre tres variables simultáneamente:", style = "font-size: 13px; margin-bottom: 20px;"),
        
        selectInput("var_3d_x", label = "Eje X:", choices = vars_dict, selected = "PBI_per_capita_soles"),
        selectInput("var_3d_y", label = "Eje Y:", choices = vars_dict, selected = "Esperanza_vida_anios"),
        selectInput("var_3d_z", label = "Eje Z:", choices = vars_dict, selected = "Analfabetismo_porcentaje"),
        
        hr(),
        
        div(
          style = "background: white; padding: 15px; border-radius: 4px;",
          h5(icon("info-circle"), " Guía de Uso", style = paste0("color: ", COLORS$info, "; margin-top: 0;")),
          tags$ul(
            style = "font-size: 12px; margin: 0;",
            tags$li("Rote el gráfico arrastrando con el mouse"),
            tags$li("Acerque/aleje con la rueda del mouse"),
            tags$li("Pase el cursor sobre los puntos para ver detalles"),
            tags$li("Use el selector para resetear la vista")
          )
        )
      ),
      
      mainPanel(
        width = 9,
        
        div(
          style = paste0("background: linear-gradient(135deg, ", COLORS$primary, " 0%, ", COLORS$secondary, " 100%); 
                         color: white; padding: 20px; margin: -15px -15px 20px -15px; border-radius: 0;"),
          h3("Visualización Tridimensional del Espacio de Variables", 
             style = "margin: 0; font-weight: 300;")
        ),
        
        div(
          style = "background: white; padding: 20px; border-radius: 4px; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
          plotlyOutput("plot_3d", height = "600px")
        ),
        
        fluidRow(
          column(6,
                 div(
                   style = paste0("background: white; padding: 15px; border-left: 3px solid ", COLORS$info, "; box-shadow: 0 1px 3px rgba(0,0,0,0.1);"),
                   h5(icon("search"), " Análisis de Clústeres", style = paste0("color: ", COLORS$primary, "; margin-top: 0;")),
                   p("Observe si las regiones forman agrupaciones naturales en el espacio tridimensional. Los clústeres pueden revelar perfiles socioeconómicos similares.", style = "font-size: 13px;")
                 )
          ),
          column(6,
                 div(
                   style = paste0("background: white; padding: 15px; border-left: 3px solid ", COLORS$accent, "; box-shadow: 0 1px 3px rgba(0,0,0,0.1);"),
                   h5(icon("project-diagram"), " Relaciones Multivariadas", style = paste0("color: ", COLORS$primary, "; margin-top: 0;")),
                   p("La disposición espacial puede indicar correlaciones múltiples. Un patrón planar sugiere que las tres variables están interrelacionadas linealmente.", style = "font-size: 13px;")
                 )
          )
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # PESTAÑA 4: MATRIZ DE CORRELACIONES
  # ---------------------------------------------------------------------------
  tabPanel(
    "Matriz de Correlaciones",
    icon = icon("table"),
    
    fluidPage(
      div(
        style = paste0("background: linear-gradient(135deg, ", COLORS$primary, " 0%, ", COLORS$secondary, " 100%); 
                       color: white; padding: 30px; margin: -15px -15px 30px -15px; border-radius: 0;"),
        h2("Matriz de Correlación", style = "margin: 0; font-weight: 300;"),
        p("Análisis exhaustivo de todas las relaciones bivariadas", 
          style = "margin: 10px 0 0 0; font-size: 16px; opacity: 0.9;")
      ),
      
      fluidRow(
        column(8,
               div(
                 style = "background: white; padding: 20px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                 h4("Mapa de Calor de Correlaciones", style = paste0("color: ", COLORS$primary)),
                 plotlyOutput("heatmap_corr", height = "500px")
               )
        ),
        column(4,
               div(
                 style = "background: white; padding: 20px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                 h4("Valores Numéricos", style = paste0("color: ", COLORS$primary)),
                 tableOutput("tabla_corr")
               ),
               br(),
               div(
                 style = paste0("background: ", COLORS$light, "; padding: 15px; border-radius: 4px;"),
                 h5(icon("key"), " Leyenda", style = paste0("color: ", COLORS$primary, "; margin-top: 0;")),
                 p("Los valores representan coeficientes de correlación de Pearson. Los colores indican la magnitud y dirección de la asociación.", style = "font-size: 13px;")
               )
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # PESTAÑA 5: DATOS
  # ---------------------------------------------------------------------------
  tabPanel(
    "Datos",
    icon = icon("database"),
    
    fluidPage(
      div(
        style = paste0("background: linear-gradient(135deg, ", COLORS$primary, " 0%, ", COLORS$secondary, " 100%); 
                       color: white; padding: 30px; margin: -15px -15px 30px -15px; border-radius: 0;"),
        h2("Dataset Completo INCORE 2023", style = "margin: 0; font-weight: 300;"),
        p("Índice de Competitividad Regional del Perú - Indicadores Seleccionados", 
          style = "margin: 10px 0 0 0; font-size: 16px; opacity: 0.9;")
      ),
      
      div(
        style = "background: white; padding: 25px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
        DT::dataTableOutput("tabla_datos"),
        
        br(),
        
        fluidRow(
          column(4,
                 div(
                   style = paste0("background: ", COLORS$light, "; padding: 15px; border-radius: 4px;"),
                   h5(icon("info-circle"), " Información del Dataset", style = "margin-top: 0;"),
                   tags$ul(
                     style = "font-size: 13px; margin: 0;",
                     tags$li("N° de regiones: 20"),
                     tags$li("N° de variables: 4"),
                     tags$li("Año de referencia: 2023"),
                     tags$li("Fuente: IPE")
                   )
                 )
          ),
          column(8,
                 div(
                   style = "background: #d1ecf1; padding: 15px; border-left: 3px solid #0c5460; border-radius: 4px;",
                   p(strong("Descripción de Variables:"), style = "margin: 0 0 10px 0;"),
                   tags$ul(
                     style = "font-size: 13px; margin: 0;",
                     tags$li(strong("PBI per cápita:"), "Producto Bruto Interno por habitante en soles corrientes"),
                     tags$li(strong("Esperanza de vida:"), "Años promedio de vida esperada al nacer"),
                     tags$li(strong("Tasa de analfabetismo:"), "Porcentaje de población mayor de 15 años que no sabe leer ni escribir"),
                     tags$li(strong("Acceso a salud:"), "Porcentaje de población con cobertura de servicios de salud")
                   )
                 )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # --- Datos Reactivos para Análisis Personalizado ---
  datos_analisis <- reactiveVal(datos_peru)
  
  # Observador para datos personalizados
  observeEvent(input$calcular_corr, {
    req(input$datos_x, input$datos_y)
    
    tryCatch({
      # Convertir datos de texto a numéricos
      datos_x <- as.numeric(strsplit(input$datos_x, ",")[[1]])
      datos_y <- as.numeric(strsplit(input$datos_y, ",")[[1]])
      
      # Validar que tengan la misma longitud
      if (length(datos_x) != length(datos_y)) {
        stop("Los datos X e Y deben tener la misma cantidad de valores")
      }
      
      # Validar que sean numéricos
      if (any(is.na(datos_x)) || any(is.na(datos_y))) {
        stop("Los datos deben ser valores numéricos válidos")
      }
      
      # Crear dataset temporal
      datos_temp <- data.frame(
        Region = paste0("Dato_", 1:length(datos_x)),
        Variable_X = datos_x,
        Variable_Y = datos_y
      )
      
      # Actualizar nombres según selección
      names(datos_temp)[2:3] <- c(input$var_x, input$var_y)
      
      datos_analisis(datos_temp)
      
      showNotification("Datos personalizados cargados correctamente", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Observador para resetear datos cuando se cambian variables
  observeEvent(c(input$var_x, input$var_y), {
    datos_analisis(datos_peru)
  })
  
  # --- Funciones auxiliares ---
  
  get_var_label <- function(var_code) {
    names(vars_dict)[vars_dict == var_code]
  }
  
  clasificar_fuerza <- function(r) {
    abs_r <- abs(r)
    if (abs_r >= 0.90) {
      list(texto = "Muy Fuerte", color = COLORS$success)
    } else if (abs_r >= 0.70) {
      list(texto = "Fuerte", color = COLORS$info)
    } else if (abs_r >= 0.40) {
      list(texto = "Moderada", color = COLORS$accent)
    } else if (abs_r >= 0.20) {
      list(texto = "Débil", color = COLORS$warning)
    } else {
      list(texto = "Muy Débil", color = COLORS$danger)
    }
  }
  
  # ---------------------------------------------------------------------------
  # ANÁLISIS BIVARIADO
  # ---------------------------------------------------------------------------
  
  # Cálculo reactivo de correlación
  resultado_corr <- reactive({
    req(input$var_x, input$var_y)
    datos <- datos_analisis()
    
    if (input$var_x == input$var_y) {
      list(estimate = 1, p.value = 0, method = input$metodo_corr, statistic = NA)
    } else {
      tryCatch({
        cor.test(
          datos[[input$var_x]], 
          datos[[input$var_y]], 
          method = input$metodo_corr,
          exact = FALSE  # Para evitar problemas con empates en muestras pequeñas
        )
      }, error = function(e) {
        list(estimate = NA, p.value = NA, method = input$metodo_corr, statistic = NA)
      })
    }
  })
  
  # Header dinámico
  output$header_bivariado <- renderUI({
    datos <- datos_analisis()
    n_obs <- nrow(datos)
    
    div(
      style = paste0("background: ", COLORS$primary, "; color: white; padding: 20px; margin: -15px -15px 25px -15px; border-radius: 0;"),
      h3(paste("Análisis de Correlación:", get_var_label(input$var_x), "→", get_var_label(input$var_y)),
         style = "margin: 0; font-weight: 300;"),
      p(paste("Método:", tools::toTitleCase(input$metodo_corr), "| n =", n_obs, "observaciones"),
        style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 14px;")
    )
  })
  
  # Métrica: Coeficiente
  output$metric_coef <- renderUI({
    res <- resultado_corr()
    
    if (is.na(res$estimate)) {
      coef_text <- "Error"
      color_coef <- COLORS$danger
    } else {
      coef <- round(res$estimate, 4)
      fuerza <- clasificar_fuerza(coef)
      coef_text <- coef
      color_coef <- fuerza$color
    }
    
    div(
      style = paste0("background: white; padding: 20px; border-radius: 4px; border-top: 4px solid ", color_coef, "; box-shadow: 0 2px 4px rgba(0,0,0,0.1); text-align: center;"),
      h2(coef_text, style = paste0("color: ", color_coef, "; margin: 0; font-weight: 600;")),
      p("Coeficiente de Correlación", style = "margin: 5px 0 0 0; font-size: 13px; color: #7f8c8d;")
    )
  })
  
  # Métrica: R²
  output$metric_r2 <- renderUI({
    res <- resultado_corr()
    
    if (is.na(res$estimate)) {
      r2_text <- "Error"
      color_r2 <- COLORS$danger
    } else {
      r2 <- round(res$estimate^2, 4)
      r2_pct <- round(r2 * 100, 2)
      r2_text <- paste0(r2_pct, "%")
      color_r2 <- COLORS$secondary
    }
    
    div(
      style = paste0("background: white; padding: 20px; border-radius: 4px; border-top: 4px solid ", color_r2, "; box-shadow: 0 2px 4px rgba(0,0,0,0.1); text-align: center;"),
      h2(r2_text, style = paste0("color: ", color_r2, "; margin: 0; font-weight: 600;")),
      p("Varianza Explicada (R²)", style = "margin: 5px 0 0 0; font-size: 13px; color: #7f8c8d;")
    )
  })
  
  # Métrica: Fuerza
  output$metric_fuerza <- renderUI({
    res <- resultado_corr()
    
    if (is.na(res$estimate)) {
      fuerza_text <- "Error"
      color_fuerza <- COLORS$danger
    } else {
      fuerza <- clasificar_fuerza(res$estimate)
      fuerza_text <- fuerza$texto
      color_fuerza <- fuerza$color
    }
    
    div(
      style = paste0("background: white; padding: 20px; border-radius: 4px; border-top: 4px solid ", color_fuerza, "; box-shadow: 0 2px 4px rgba(0,0,0,0.1); text-align: center;"),
      h2(fuerza_text, style = paste0("color: ", color_fuerza, "; margin: 0; font-weight: 600; font-size: 24px;")),
      p("Magnitud de la Asociación", style = "margin: 5px 0 0 0; font-size: 13px; color: #7f8c8d;")
    )
  })
  
  # Métrica: P-valor
  output$metric_pvalor <- renderUI({
    res <- resultado_corr()
    
    if (is.na(res$p.value)) {
      p_text <- "Error"
      color_p <- COLORS$danger
      sig_text <- "Error"
    } else {
      p_val <- res$p.value
      es_sig <- p_val < 0.05
      color_p <- if (es_sig) COLORS$success else COLORS$danger
      sig_text <- if (es_sig) "Significativo" else "No Significativo"
      p_formatted <- if (p_val < 0.001) "< 0.001" else sprintf("%.4f", p_val)
      p_text <- p_formatted
    }
    
    div(
      style = paste0("background: white; padding: 20px; border-radius: 4px; border-top: 4px solid ", color_p, "; box-shadow: 0 2px 4px rgba(0,0,0,0.1); text-align: center;"),
      h2(sig_text, style = paste0("color: ", color_p, "; margin: 0; font-weight: 600; font-size: 24px;")),
      p(paste("p-valor:", p_text), style = "margin: 5px 0 0 0; font-size: 13px; color: #7f8c8d;")
    )
  })
  
  # Interpretación detallada
  output$interpretacion_detallada <- renderUI({
    res <- resultado_corr()
    datos <- datos_analisis()
    
    if (input$var_x == input$var_y) {
      return(
        div(
          style = "background: #fff3cd; padding: 15px; border-left: 4px solid #856404; border-radius: 4px;",
          p(icon("exclamation-triangle"), strong(" Advertencia:"), 
            "Ha seleccionado la misma variable para ambos ejes. La correlación de una variable consigo misma es siempre perfecta (r = 1.00).",
            style = "margin: 0; font-size: 14px;")
        )
      )
    }
    
    if (is.na(res$estimate)) {
      return(
        div(
          style = "background: #f8d7da; padding: 15px; border-left: 4px solid #dc3545; border-radius: 4px;",
          p(icon("times-circle"), strong(" Error en el cálculo:"), 
            "No se pudo calcular la correlación. Verifique que los datos sean válidos y que haya suficiente variabilidad.",
            style = "margin: 0; font-size: 14px;")
        )
      )
    }
    
    coef <- res$estimate
    p_val <- res$p.value
    fuerza <- clasificar_fuerza(coef)
    r2 <- round(coef^2 * 100, 2)
    n_obs <- nrow(datos)
    
    direccion <- if (coef > 0) "positiva" else "negativa"
    direccion_desc <- if (coef > 0) {
      "directamente proporcional: cuando una variable aumenta, la otra tiende a aumentar también"
    } else {
      "inversamente proporcional: cuando una variable aumenta, la otra tiende a disminuir"
    }
    
    # Significancia estadística
    if (p_val < 0.001) {
      sig_texto <- "muy fuerte (p < 0.001)"
      sig_color <- COLORS$success
      sig_conclusion <- "La probabilidad de que esta correlación sea producto del azar es menor al 0.1%."
    } else if (p_val < 0.01) {
      sig_texto <- "fuerte (p < 0.01)"
      sig_color <- COLORS$success
      sig_conclusion <- "La probabilidad de que esta correlación sea producto del azar es menor al 1%."
    } else if (p_val < 0.05) {
      sig_texto <- "significativa (p < 0.05)"
      sig_color <- COLORS$info
      sig_conclusion <- "La probabilidad de que esta correlación sea producto del azar es menor al 5%."
    } else {
      sig_texto <- "no significativa (p ≥ 0.05)"
      sig_color <- COLORS$danger
      sig_conclusion <- "No hay evidencia estadística suficiente para afirmar que existe una correlación real en la población."
    }
    
    tagList(
      p(
        paste0("El análisis revela una correlación ", strong(direccion), " de magnitud ", 
               strong(tolower(fuerza$texto)), " (", input$metodo_corr, " = ", round(coef, 4), ") entre ", 
               strong(get_var_label(input$var_x)), " y ", strong(get_var_label(input$var_y)), "."),
        style = "font-size: 15px; line-height: 1.7;"
      ),
      
      tags$ul(
        style = "font-size: 14px; line-height: 1.8;",
        tags$li(strong("Dirección de la relación:"), paste0("La asociación es ", direccion_desc, ".")),
        tags$li(strong("Varianza compartida:"), paste0("El coeficiente de determinación (R² = ", r2, "%) indica que el ", r2, "% de la variabilidad de ", get_var_label(input$var_y), " puede ser explicada por ", get_var_label(input$var_x), ".")),
        tags$li(strong("Tamaño muestral:"), paste0("Análisis basado en ", n_obs, " observaciones.")),
        tags$li(
          strong("Significancia estadística:"), 
          span(
            paste0("La evidencia es ", sig_texto, ". ", sig_conclusion),
            style = paste0("color: ", sig_color, ";")
          )
        )
      ),
      
      if (p_val >= 0.05) {
        div(
          style = paste0("background: #f8d7da; padding: 12px; border-left: 4px solid ", COLORS$danger, "; border-radius: 4px; margin-top: 15px;"),
          p(icon("times-circle"), strong(" Interpretación:"), 
            "Aunque se observa una correlación muestral, esta no es estadísticamente significativa. Los resultados podrían deberse a variación aleatoria y no deben generalizarse a la población.",
            style = "margin: 0; font-size: 13px;")
        )
      } else {
        div(
          style = paste0("background: #d4edda; padding: 12px; border-left: 4px solid ", COLORS$success, "; border-radius: 4px; margin-top: 15px;"),
          p(icon("check-circle"), strong(" Interpretación:"), 
            "La correlación es estadísticamente significativa, lo que sugiere una relación real entre las variables en la población. Sin embargo, recuerde que correlación no implica causalidad.",
            style = "margin: 0; font-size: 13px;")
        )
      }
    )
  })
  
  # Gráfico de dispersión
  output$plot_scatter <- renderPlotly({
    req(input$var_x, input$var_y)
    datos <- datos_analisis()
    
    res <- resultado_corr()
    
    if (is.na(res$estimate)) {
      # Gráfico de error
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Error en el cálculo de correlación\nVerifique los datos", 
                 size = 6, color = COLORS$danger) +
        theme_void()
      
      return(ggplotly(p) %>% layout(paper_bgcolor = "white", plot_bgcolor = "white"))
    }
    
    fuerza <- clasificar_fuerza(res$estimate)
    
    # Crear modelo lineal para la ecuación y métricas
    tryCatch({
      modelo <- lm(datos[[input$var_y]] ~ datos[[input$var_x]])
      intercepto <- coef(modelo)[1]
      pendiente <- coef(modelo)[2]
      
      # Calcular residuos y valores ajustados para análisis
      datos_plot <- datos
      datos_plot$fitted <- fitted(modelo)
      datos_plot$residuals <- residuals(modelo)
      datos_plot$label_var <- if ("Region" %in% names(datos)) datos$Region else paste("Obs", 1:nrow(datos))
      
      # Crear gráfico base con estética estadística profesional
      p <- ggplot(datos_plot, aes_string(x = input$var_x, y = input$var_y)) +
        # Intervalo de confianza primero (capa inferior)
        geom_smooth(
          method = "lm",
          se = TRUE,
          color = COLORS$primary,
          fill = COLORS$info,
          alpha = 0.15,
          size = 1.2,
          level = 0.95
        ) +
        # Puntos con borde para mejor visualización
        geom_point(
          aes(text = paste0("<b>", label_var, "</b><br>",
                            get_var_label(input$var_x), ": ", format(round(get(input$var_x), 2), big.mark = ","), "<br>",
                            get_var_label(input$var_y), ": ", format(round(get(input$var_y), 2), big.mark = ","), "<br>",
                            "Valor ajustado: ", round(fitted, 2), "<br>",
                            "Residuo: ", round(residuals, 2))),
          size = 4,
          alpha = 0.8,
          color = COLORS$primary,
          fill = "white",
          shape = 21,
          stroke = 1.5
        ) +
        # Línea de regresión destacada
        geom_smooth(
          method = "lm",
          se = FALSE,
          color = fuerza$color,
          size = 1.3,
          linetype = "solid"
        ) +
        # Etiquetas y títulos profesionales
        labs(
          x = get_var_label(input$var_x),
          y = get_var_label(input$var_y),
          title = paste0("Modelo de Regresión Lineal Simple (n = ", nrow(datos), ")"),
          subtitle = paste0("Ecuación: Ŷ = ", round(intercepto, 3), 
                            ifelse(pendiente >= 0, " + ", " - "), 
                            abs(round(pendiente, 5)), "X  |  ",
                            "R² = ", round(res$estimate^2, 4), "  |  ",
                            input$metodo_corr, " = ", round(res$estimate, 4))
        ) +
        # Tema estadístico profesional
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0, size = 14, color = COLORS$primary),
          plot.subtitle = element_text(color = COLORS$secondary, face = "italic", hjust = 0, size = 11, margin = margin(t = 5, b = 15)),
          panel.grid.major = element_line(color = "#e0e0e0", size = 0.4),
          panel.grid.minor = element_line(color = "#f5f5f5", size = 0.3),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          axis.title = element_text(face = "bold", size = 12, color = COLORS$primary),
          axis.text = element_text(color = COLORS$secondary, size = 10),
          axis.line = element_line(color = COLORS$secondary, size = 0.5),
          axis.ticks = element_line(color = COLORS$secondary, size = 0.5)
        )
      
      # Añadir escalas con formato apropiado
      if (max(datos[[input$var_x]]) > 1000) {
        p <- p + scale_x_continuous(labels = scales::comma_format())
      }
      if (max(datos[[input$var_y]]) > 1000) {
        p <- p + scale_y_continuous(labels = scales::comma_format())
      }
      
      # Convertir a plotly con configuración profesional
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12, color = COLORS$primary, family = "Arial"),
            bordercolor = COLORS$primary,
            align = "left"
          ),
          font = list(family = "Arial, sans-serif"),
          paper_bgcolor = "white",
          plot_bgcolor = "white",
          margin = list(t = 80, b = 60, l = 70, r = 40)
        ) %>%
        config(
          displayModeBar = TRUE, 
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("lasso2d", "select2d"),
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("correlacion_", input$var_x, "_", input$var_y),
            height = 600,
            width = 1000,
            scale = 2
          )
        )
      
    }, error = function(e) {
      # Gráfico de error alternativo
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Error en la creación del gráfico", 
                 size = 6, color = COLORS$danger) +
        theme_void()
      
      ggplotly(p) %>% layout(paper_bgcolor = "white", plot_bgcolor = "white")
    })
  })
  
  # ---------------------------------------------------------------------------
  # VISUALIZACIÓN 3D (se mantiene igual)
  # ---------------------------------------------------------------------------
  
  output$plot_3d <- renderPlotly({
    req(input$var_3d_x, input$var_3d_y, input$var_3d_z)
    
    # Calcular plano de regresión múltiple para visualización estadística
    formula_reg <- as.formula(paste(input$var_3d_z, "~", input$var_3d_x, "+", input$var_3d_y))
    modelo_3d <- lm(formula_reg, data = datos_peru)
    
    # Crear grilla para el plano de regresión
    x_range <- range(datos_peru[[input$var_3d_x]])
    y_range <- range(datos_peru[[input$var_3d_y]])
    x_grid <- seq(x_range[1], x_range[2], length.out = 10)
    y_grid <- seq(y_range[1], y_range[2], length.out = 10)
    grid <- expand.grid(x = x_grid, y = y_grid)
    names(grid) <- c(input$var_3d_x, input$var_3d_y)
    grid$z_pred <- predict(modelo_3d, newdata = grid)
    z_matrix <- matrix(grid$z_pred, nrow = 10, ncol = 10)
    
    # Crear gráfico 3D profesional
    plot_ly() %>%
      add_surface(
        x = x_grid,
        y = y_grid,
        z = z_matrix,
        colorscale = list(c(0, "#e8f4f8"), c(1, "#b3d9e6")),
        opacity = 0.4,
        showscale = FALSE,
        hoverinfo = "skip",
        name = "Plano de Regresión"
      ) %>%
      add_trace(
        data = datos_peru,
        x = ~get(input$var_3d_x),
        y = ~get(input$var_3d_y),
        z = ~get(input$var_3d_z),
        text = ~paste0(
          "<b>", Region, "</b><br>",
          "<br><b>Variables:</b>",
          "<br>", get_var_label(input$var_3d_x), ": ", format(get(input$var_3d_x), big.mark = ","),
          "<br>", get_var_label(input$var_3d_y), ": ", format(get(input$var_3d_y), big.mark = ","),
          "<br>", get_var_label(input$var_3d_z), ": ", format(get(input$var_3d_z), big.mark = ",")
        ),
        hoverinfo = "text",
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 7,
          opacity = 0.9,
          color = ~PBI_per_capita_soles,
          colorscale = list(
            c(0, "#d73027"),
            c(0.25, "#fc8d59"),
            c(0.5, "#fee090"),
            c(0.75, "#91cf60"),
            c(1, "#1a9850")
          ),
          showscale = TRUE,
          colorbar = list(
            title = list(
              text = "<b>PBI per cápita</b><br>(Soles)",
              font = list(size = 11, family = "Arial")
            ),
            thickness = 18,
            len = 0.65,
            x = 1.02,
            tickformat = ",",
            tickfont = list(size = 10)
          ),
          line = list(
            color = COLORS$primary,
            width = 1.5
          )
        ),
        name = "Regiones"
      ) %>%
      layout(
        scene = list(
          xaxis = list(
            title = list(
              text = paste0("<b>", get_var_label(input$var_3d_x), "</b>"),
              font = list(size = 12, color = COLORS$primary, family = "Arial")
            ),
            backgroundcolor = "#fafafa",
            gridcolor = "#e0e0e0",
            showbackground = TRUE,
            zerolinecolor = COLORS$secondary,
            tickfont = list(size = 10, color = COLORS$secondary)
          ),
          yaxis = list(
            title = list(
              text = paste0("<b>", get_var_label(input$var_3d_y), "</b>"),
              font = list(size = 12, color = COLORS$primary, family = "Arial")
            ),
            backgroundcolor = "#fafafa",
            gridcolor = "#e0e0e0",
            showbackground = TRUE,
            zerolinecolor = COLORS$secondary,
            tickfont = list(size = 10, color = COLORS$secondary)
          ),
          zaxis = list(
            title = list(
              text = paste0("<b>", get_var_label(input$var_3d_z), "</b>"),
              font = list(size = 12, color = COLORS$primary, family = "Arial")
            ),
            backgroundcolor = "#fafafa",
            gridcolor = "#e0e0e0",
            showbackground = TRUE,
            zerolinecolor = COLORS$secondary,
            tickfont = list(size = 10, color = COLORS$secondary)
          ),
          camera = list(
            eye = list(x = 1.6, y = 1.6, z = 1.3),
            center = list(x = 0, y = 0, z = 0)
          ),
          aspectmode = "cube"
        ),
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 11, color = COLORS$primary, family = "Arial"),
          bordercolor = COLORS$primary,
          align = "left"
        ),
        paper_bgcolor = "white",
        font = list(family = "Arial, sans-serif"),
        showlegend = FALSE
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "lasso2d"),
        toImageButtonOptions = list(
          format = "png",
          filename = "analisis_3d",
          height = 800,
          width = 1200,
          scale = 2
        )
      )
  })
  
  # ---------------------------------------------------------------------------
  # MATRIZ DE CORRELACIONES (se mantiene igual)
  # ---------------------------------------------------------------------------
  
  matriz_corr <- reactive({
    datos_num <- datos_peru[, -1]  # Excluir columna de Region
    cor(datos_num, method = "pearson")
  })
  
  output$heatmap_corr <- renderPlotly({
    mat <- matriz_corr()
    
    # Preparar datos para heatmap
    var_names <- c("PBI per cápita", "Esperanza de Vida", "Analfabetismo", "Acceso a Salud")
    
    # Crear anotaciones para mostrar los valores
    annotations <- list()
    for(i in 1:nrow(mat)) {
      for(j in 1:ncol(mat)) {
        annotations[[length(annotations) + 1]] <- list(
          x = var_names[j],
          y = var_names[i],
          text = sprintf("%.3f", mat[i, j]),
          showarrow = FALSE,
          font = list(
            color = ifelse(abs(mat[i, j]) > 0.5, "white", COLORS$primary),
            size = 13,
            family = "Arial, sans-serif"
          )
        )
      }
    }
    
    plot_ly(
      z = mat,
      x = var_names,
      y = var_names,
      type = "heatmap",
      colorscale = list(
        c(0, "#d73027"),
        c(0.25, "#fc8d59"),
        c(0.5, "#f7f7f7"),
        c(0.75, "#91bfdb"),
        c(1, "#4575b4")
      ),
      zmid = 0,
      zmin = -1,
      zmax = 1,
      hovertemplate = paste0(
        "<b>%{y}</b> vs <b>%{x}</b><br>",
        "Correlación de Pearson: %{z:.4f}<br>",
        "<extra></extra>"
      ),
      colorbar = list(
        title = list(
          text = "Coeficiente r",
          font = list(size = 12, family = "Arial")
        ),
        thickness = 20,
        len = 0.7,
        tickmode = "array",
        tickvals = c(-1, -0.5, 0, 0.5, 1),
        ticktext = c("-1.0", "-0.5", "0.0", "0.5", "1.0"),
        tickfont = list(size = 11)
      )
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickangle = -45,
          tickfont = list(size = 11, color = COLORS$primary),
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 11, color = COLORS$primary),
          showgrid = FALSE
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        margin = list(l = 150, b = 150, t = 40, r = 80),
        annotations = annotations,
        font = list(family = "Arial, sans-serif")
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "matriz_correlaciones",
          height = 600,
          width = 800,
          scale = 2
        )
      )
  })
  
  output$tabla_corr <- renderTable({
    mat <- matriz_corr()
    df <- as.data.frame(mat)
    var_names <- c("PBI", "Esp. Vida", "Analf.", "Acceso Salud")
    rownames(df) <- var_names
    colnames(df) <- var_names
    df
  }, rownames = TRUE, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---------------------------------------------------------------------------
  # TABLA DE DATOS (se mantiene igual)
  # ---------------------------------------------------------------------------
  
  output$tabla_datos <- DT::renderDataTable({
    datos_display <- datos_peru
    colnames(datos_display) <- c(
      "Región",
      "PBI per cápita (S/)",
      "Esperanza de Vida (años)",
      "Analfabetismo (%)",
      "Acceso a Salud (%)"
    )
    
    DT::datatable(
      datos_display,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = "frtip",
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json"
        ),
        columnDefs = list(
          list(className = "dt-center", targets = 1:4)
        )
      ),
      rownames = FALSE,
      class = "display cell-border stripe hover",
      style = "bootstrap"
    ) %>%
      DT::formatRound(columns = 2:5, digits = 2) %>%
      DT::formatStyle(
        columns = 1:5,
        backgroundColor = "white",
        border = "1px solid #ddd"
      ) %>%
      DT::formatStyle(
        columns = 2,
        background = styleColorBar(range(datos_peru$PBI_per_capita_soles), COLORS$info),
        backgroundSize = "90% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
}

shinyApp(ui = ui, server = server)
