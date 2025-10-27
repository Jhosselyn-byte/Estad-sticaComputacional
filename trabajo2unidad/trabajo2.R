# ==============================================================================
# META-ANÁLISIS: EFECTO FIJO PROMEDIO (EFP)
# ==============================================================================

# ----------------------- LIBRERÍAS REQUERIDAS ---------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(readxl)
library(scales)
library(dplyr)

# ----------------------- FUNCIONES AUXILIARES ---------------------------------

#' Calcular transformación logit
#' @param p Proporción (debe estar entre 0 y 1, exclusivo)
#' @return Valor logit transformado
calcular_logit <- function(p) {
  if (any(p <= 0 | p >= 1, na.rm = TRUE)) {
    stop("Los valores de p deben estar estrictamente entre 0 y 1")
  }
  return(log(p / (1 - p)))
}

#' Calcular varianza
#' @param n Tamaño de muestra
#' @param p Proporción
#' @return Varianza calculada
calcular_varianza <- function(n, p) {
  return(1 / (n * p * (1 - p)))
}

#' Calcular pesos
#' @param vi Vector de varianzas
#' @return Vector de pesos
calcular_pesos <- function(vi) {
  return(1 / vi)
}

#' Calcular Efecto Fijo Promedio
#' @param wi Vector de pesos
#' @param logit_p Vector de logit(p)
#' @return Efecto fijo promedio (theta)
calcular_efp <- function(wi, logit_p) {
  return(sum(wi * logit_p) / sum(wi))
}

#' Transformar de logit a probabilidad
#' @param theta Valor en escala logit
#' @return Valor en escala de probabilidad
logit_a_probabilidad <- function(theta) {
  return(exp(theta) / (1 + exp(theta)))
}

#' Calcular estadístico Q de homogeneidad
#' @param wi Vector de pesos
#' @param logit_p Vector de logit(p)
#' @param theta_ef Efecto fijo promedio
#' @return Lista con Q, Q_i, y grados de libertad
calcular_test_q <- function(wi, logit_p, theta_ef) {
  Q_i <- wi * (logit_p - theta_ef)^2
  Q <- sum(Q_i)
  df <- length(wi) - 1
  p_value <- 1 - pchisq(Q, df)
  
  return(list(
    Q = Q,
    Q_i = Q_i,
    df = df,
    p_value = p_value
  ))
}

#' Calcular heterogeneidad I²
#' @param Q Estadístico Q
#' @param df Grados de libertad
#' @return Valor de I² en porcentaje
calcular_i_cuadrado <- function(Q, df) {
  return(max(0, ((Q - df) / Q) * 100))
}

#' Validar datos de entrada
#' @param df Dataframe con datos
#' @return TRUE si válido, mensaje de error si no
validar_datos <- function(df) {
  if (any(is.na(df$n)) || any(is.na(df$p))) {
    return("Error: Existen valores faltantes (NA) en los datos")
  }
  
  if (any(df$n <= 0)) {
    return("Error: El tamaño de muestra (n) debe ser mayor que 0")
  }
  
  if (any(df$p <= 0 | df$p >= 1)) {
    return("Error: Los valores de p deben estar estrictamente entre 0 y 1")
  }
  
  return(TRUE)
}

# ----------------------- INTERFAZ DE USUARIO ----------------------------------

ui <- dashboardPage(
  skin = "black",
  
  # ========== HEADER ==========
  dashboardHeader(
    title = "Meta-análisis EFP",
    titleWidth = 320,
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.unap.edu.pe",
        target = "_blank",
        icon("university"),
        "UNAP - Puno",
        style = "color: #00ff00; font-weight: bold;"
      )
    )
  ),
  
  # ========== SIDEBAR ==========
  dashboardSidebar(
    width = 320,
    
    sidebarMenu(
      id = "tabs",
      menuItem("Datos", tabName = "datos", icon = icon("database")),
      menuItem("Cálculos", tabName = "calculos", icon = icon("calculator")),
      menuItem("Resultados", tabName = "resultados", icon = icon("chart-line")),
      menuItem("Forest Plot", tabName = "forest", icon = icon("chart-area")),
      menuItem("Test Q", tabName = "testq", icon = icon("vial")),
      menuItem("Fórmulas", tabName = "formulas", icon = icon("square-root-alt")),
      menuItem("Teoría", tabName = "teoria", icon = icon("book")),
      menuItem("ℹ Ayuda", tabName = "ayuda", icon = icon("question-circle"))
    ),
    
    hr(),
    
    # Panel de CARGA DE DATOS
    box(
      width = 12,
      title = tagList(icon("upload"), "1. Cargar Datos"),
      solidHeader = TRUE,
      status = "success",
      background = "black",
      
      fileInput(
        "archivo_sidebar",
        HTML("<b style='color: #00ff00;'>Seleccione archivo Excel o CSV:</b>"),
        accept = c(".xlsx", ".xls", ".csv"),
        buttonLabel = " Buscar",
        placeholder = "Ningún archivo",
        width = "100%"
      ),
      
      tags$div(
        style = "font-size: 12px; color: #00ff00; margin-top: -10px;",
        HTML("Formatos: .xlsx, .xls, .csv<br>Columnas requeridas: <b>n</b> y <b>p</b>")
      )
    ),
    
    hr(style = "border-color: #00ff00;"),
    
    # Panel de configuración
    box(
      width = 12,
      title = tagList(icon("cog"), " 2. Configuración"),
      solidHeader = TRUE,
      status = "success",
      background = "black",
      
      numericInput(
        "alpha", 
        HTML("<b style='color: #00ff00;'>Nivel de significancia (α):</b>"),
        value = 0.05, 
        min = 0.001, 
        max = 0.2, 
        step = 0.001
      )
    ),
    
    hr(style = "border-color: #00ff00;"),
    
    # Panel de cálculo
    box(
      width = 12,
      title = tagList(icon("play-circle"), "3. Ejecutar"),
      solidHeader = TRUE,
      status = "success",
      background = "black",
      
      tags$div(
        style = "display: flex; justify-content: center; align-items: center; width: 100%;",
        actionButton(
          "calcular", 
          HTML("<b style='font-size: 14px;'>CALCULAR<br>META-ANÁLISIS</b>"),
          class = "btn-lg",
          style = "padding: 15px 20px; font-weight: bold; 
background: linear-gradient(135deg, #00ff00 0%, #00aa00 100%); 
color: #000000; border: 3px solid #00ff00; border-radius: 12px;
box-shadow: 0 4px 15px rgba(0, 255, 0, 0.4);
transition: all 0.3s ease; height: auto; white-space: normal;
line-height: 1.3; width: 100%; text-align: center;"
        )
      )
    ),
    
    hr(style = "border-color: #00ff00;"),
    
    # Información del sistema
    box(
      width = 12,
      title = " Sistema",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      background = "black",
      
      HTML("<small style='color: #00ff00;'>
<b>Método:</b> Efecto Fijo Promedio<br>
<b>Modelo:</b> Meta-análisis<br>
<b>Versión:</b> 3.0 Negro-Verde<br>
<b>Fecha:</b> Octubre 2025
</small>")
    )
  ),
  
  # ========== BODY ==========
  dashboardBody(
    
    # CSS Personalizado Negro y Verde
    tags$head(
      tags$style(HTML("
/* === ESTILOS GENERALES === */
@import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap');

body, .content-wrapper, .right-side {
background: linear-gradient(135deg, #000000 0%, #1a1a1a 50%, #000000 100%);
font-family: 'Roboto', sans-serif;
color: #00ff00;
}

.main-header .logo {
font-weight: 700;
font-size: 22px;
background: linear-gradient(135deg, #000000 0%, #1a1a1a 100%) !important;
color: #00ff00 !important;
border-bottom: 3px solid #00ff00;
}

.main-header .navbar {
background: linear-gradient(135deg, #000000 0%, #1a1a1a 100%) !important;
border-bottom: 2px solid #00ff00;
}

.skin-black .main-sidebar {
background: #000000;
border-right: 2px solid #00ff00;
}

.sidebar-menu > li.active > a {
border-left: 5px solid #00ff00 !important;
font-weight: bold;
background: #1a1a1a !important;
color: #00ff00 !important;
}

.sidebar-menu > li > a {
color: #00ff00 !important;
transition: all 0.3s ease;
}

.sidebar-menu > li > a:hover {
background: #1a1a1a !important;
border-left: 3px solid #00ff00;
}

/* === BOXES === */
.box {
border-radius: 15px;
box-shadow: 0 6px 20px rgba(0, 255, 0, 0.3);
transition: all 0.3s ease;
background: #1a1a1a !important;
border: 2px solid #00ff00;
}

.box:hover {
box-shadow: 0 10px 30px rgba(0, 255, 0, 0.5);
transform: translateY(-3px);
border-color: #00ff00;
}

.box-header {
background: #000000 !important;
color: #00ff00 !important;
border-bottom: 2px solid #00ff00;
padding: 15px 20px;
}

.box-title {
font-weight: 700 !important;
font-size: 18px !important;
color: #00ff00 !important;
}

.box.box-solid.box-primary > .box-header {
background: linear-gradient(135deg, #00ff00 0%, #00aa00 100%);
color: #000000;
font-weight: bold;
}

.box.box-solid.box-success > .box-header {
background: linear-gradient(135deg, #00ff00 0%, #00cc00 100%);
color: #000000;
}

.box.box-solid.box-info > .box-header {
background: linear-gradient(135deg, #00ff00 0%, #009900 100%);
color: #000000;
}

.box.box-solid.box-warning > .box-header {
background: linear-gradient(135deg, #00ff00 0%, #00dd00 100%);
color: #000000;
}

/* === VALUE BOXES === */
.small-box {
border-radius: 15px;
box-shadow: 0 6px 20px rgba(0, 255, 0, 0.3);
transition: all 0.3s ease;
border: 2px solid #00ff00;
background: #1a1a1a !important;
min-height: 140px;
}

.small-box:hover {
transform: translateY(-5px) scale(1.02);
box-shadow: 0 12px 35px rgba(0, 255, 0, 0.5);
}

.small-box h3 {
font-size: 42px;
font-weight: 700;
color: #00ff00 !important;
text-shadow: 0 0 10px rgba(0, 255, 0, 0.8);
margin: 10px 0;
}

.small-box p {
font-size: 16px;
color: #00ff00 !important;
font-weight: 500;
}

.small-box .icon {
color: rgba(0, 255, 0, 0.2);
font-size: 90px;
top: 15px;
}

/* === BOTONES === */
.btn {
border-radius: 10px;
padding: 12px 24px;
font-weight: 600;
font-size: 16px;
transition: all 0.3s ease;
border: 2px solid #00ff00;
white-space: normal !important;
word-wrap: break-word !important;
height: auto !important;
min-height: 40px;
display: inline-block;
text-align: center;
}

.btn:hover {
transform: translateY(-2px);
box-shadow: 0 6px 20px rgba(0, 255, 0, 0.4);
}

.btn-success {
background: linear-gradient(135deg, #00ff00 0%, #00aa00 100%);
color: #000000;
border: 2px solid #00ff00;
}

.btn-success:hover {
background: linear-gradient(135deg, #00ff00 0%, #00cc00 100%);
box-shadow: 0 8px 25px rgba(0, 255, 0, 0.6);
}

.btn-primary {
background: linear-gradient(135deg, #00cc00 0%, #009900 100%);
color: #000000;
border: 2px solid #00ff00;
}

.btn-info {
background: linear-gradient(135deg, #00aa00 0%, #007700 100%);
color: #ffffff;
border: 2px solid #00ff00;
}

.btn-lg {
font-size: 14px !important;
padding: 15px 20px !important;
line-height: 1.4 !important;
}

.btn-block {
display: block;
width: 100%;
text-align: center;
}

/* Contenedor de botones centrado */
.form-group {
margin-bottom: 15px;
}

.shiny-input-container {
width: 100%;
}

/* === TABLAS === */
.dataTables_wrapper {
background: #000000;
padding: 20px;
border-radius: 12px;
border: 2px solid #00ff00;
}

table.dataTable {
border: 2px solid #00ff00 !important;
background: #1a1a1a !important;
}

table.dataTable thead th {
background: linear-gradient(135deg, #00ff00 0%, #00aa00 100%);
color: #000000 !important;
font-weight: 700;
font-size: 16px;
padding: 15px 12px;
border: 1px solid #00ff00 !important;
}

table.dataTable tbody td {
padding: 14px 12px;
font-size: 15px;
color: #00ff00 !important;
background: #1a1a1a !important;
border: 1px solid #00ff00 !important;
}

table.dataTable tbody tr:hover {
background: #2a2a2a !important;
}

table.dataTable tbody tr.selected {
background: #00ff00 !important;
color: #000000 !important;
}

/* === INPUTS === */
.form-control {
background: #1a1a1a !important;
border: 2px solid #00ff00 !important;
color: #00ff00 !important;
border-radius: 8px;
padding: 12px 15px;
font-size: 15px;
}

.form-control:focus {
border-color: #00ff00 !important;
box-shadow: 0 0 15px rgba(0, 255, 0, 0.5);
background: #000000 !important;
}

.form-control::placeholder {
color: #006600 !important;
}

/* File Input específico */
.btn-file {
background: #00ff00 !important;
color: #000000 !important;
border: 2px solid #00ff00 !important;
font-weight: bold !important;
padding: 8px 15px !important;
border-radius: 8px !important;
}

.btn-file:hover {
background: #00cc00 !important;
box-shadow: 0 4px 12px rgba(0, 255, 0, 0.5);
}

input[type='file'] {
color: #00ff00 !important;
}

.input-group-btn .btn {
background: #00ff00 !important;
color: #000000 !important;
border: 2px solid #00ff00 !important;
font-weight: bold !important;
}

/* === INFO BOXES === */
.info-box {
border-radius: 12px;
box-shadow: 0 4px 15px rgba(0, 255, 0, 0.3);
border: 2px solid #00ff00;
background: #1a1a1a;
min-height: 110px;
}

.info-box-icon {
background: linear-gradient(135deg, #00ff00 0%, #00aa00 100%) !important;
color: #000000 !important;
border-radius: 10px 0 0 10px;
}

.info-box-content {
color: #00ff00 !important;
padding: 15px 10px;
}

.info-box-text {
font-size: 15px;
font-weight: 600;
color: #00ff00 !important;
}

.info-box-number {
font-size: 28px;
font-weight: 700;
color: #00ff00 !important;
}

/* === TABS === */
.nav-tabs-custom {
background: #1a1a1a;
border: 2px solid #00ff00;
border-radius: 12px;
}

.nav-tabs-custom > .nav-tabs > li.active > a {
background: #00ff00 !important;
color: #000000 !important;
font-weight: bold;
border-top: 3px solid #00ff00;
}

.nav-tabs-custom > .nav-tabs > li > a {
color: #00ff00 !important;
background: #000000;
}

/* === ALERTAS === */
.alert {
border-radius: 10px;
border: 2px solid #00ff00;
background: #1a1a1a;
color: #00ff00;
font-size: 15px;
padding: 18px;
}

.alert-success {
background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
border-color: #00ff00;
color: #00ff00;
}

.alert-info {
background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
border-color: #00ff00;
color: #00ff00;
}

.alert-warning {
background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
border-color: #00ff00;
color: #00ff00;
}

/* === PROGRESS BARS === */
.progress {
background: #1a1a1a;
border: 2px solid #00ff00;
border-radius: 8px;
height: 28px;
}

.progress-bar {
background: linear-gradient(135deg, #00ff00 0%, #00aa00 100%);
font-size: 14px;
font-weight: 600;
color: #000000;
line-height: 24px;
}

/* === TÍTULOS Y TEXTO === */
h1, h2, h3, h4, h5, h6 {
color: #00ff00 !important;
font-weight: 700;
text-shadow: 0 0 10px rgba(0, 255, 0, 0.3);
}

p, span, label, div {
color: #00ff00 !important;
}

/* === HR === */
hr {
border-color: #00ff00 !important;
border-width: 2px;
}

/* === SCROLLBAR === */
::-webkit-scrollbar {
width: 14px;
height: 14px;
}

::-webkit-scrollbar-track {
background: #000000;
border: 1px solid #00ff00;
}

::-webkit-scrollbar-thumb {
background: linear-gradient(135deg, #00ff00 0%, #00aa00 100%);
border-radius: 10px;
border: 2px solid #000000;
}

::-webkit-scrollbar-thumb:hover {
background: #00ff00;
}

/* === CÓDIGO PRE === */
pre {
background: #000000 !important;
border: 2px solid #00ff00;
color: #00ff00 !important;
padding: 20px;
border-radius: 10px;
font-family: 'Courier New', monospace;
font-size: 14px;
line-height: 1.6;
overflow-x: auto;
}

code {
background: #1a1a1a;
color: #00ff00;
padding: 3px 8px;
border-radius: 5px;
border: 1px solid #00ff00;
}

/* === ANIMACIONES === */
@keyframes glow {
0% { box-shadow: 0 0 5px rgba(0, 255, 0, 0.5); }
50% { box-shadow: 0 0 20px rgba(0, 255, 0, 0.8); }
100% { box-shadow: 0 0 5px rgba(0, 255, 0, 0.5); }
}

.glow-effect {
animation: glow 2s infinite;
}

/* === RESPONSIVO === */
@media (max-width: 768px) {
.box-title {
font-size: 16px !important;
}

.small-box h3 {
font-size: 32px;
}

.btn {
font-size: 14px;
padding: 10px 20px;
}
}
"))
    ),
    
    # ========== PESTAÑAS ==========
    tabItems(
      
      # ========== PESTAÑA: DATOS ==========
      tabItem(
        tabName = "datos",
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("info-circle"), " Instrucciones"),
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            tags$div(
              style = "font-size: 16px; line-height: 1.8; color: #00ff00;",
              HTML("
<h4 style='color: #00ff00;'><b> Formato de Datos Requerido:</b></h4>
<ul style='font-size: 15px;'>
<li><b>Columna 'n':</b> Tamaño de muestra de cada estudio (debe ser > 0)</li>
<li><b>Columna 'p':</b> Proporción observada (debe estar entre 0 y 1, exclusivo)</li>
<li><b>Dos opciones:</b> Cargar archivo O ingresar manualmente</li>
<li><b>Sin valores faltantes:</b> Todas las celdas deben contener datos</li>
</ul>

<h4 style='color: #00ff00;'><b> Proceso:</b></h4>
<ol style='font-size: 15px;'>
<li>Elija CARGAR ARCHIVO o INGRESAR MANUALMENTE</li>
<li>Verifique que los datos se hayan cargado correctamente en la tabla</li>
<li>Configure el nivel de significancia (α) en el panel lateral</li>
<li>Presione el botón <b>'CALCULAR META-ANÁLISIS'</b></li>
<li>Explore los resultados en las diferentes pestañas</li>
</ol>

<div style='background: #1a1a1a; border: 2px solid #00ff00; 
border-radius: 10px; padding: 15px; margin-top: 20px;'>
<h4 style='color: #00ff00;'><b> Importante:</b></h4>
<p style='margin: 0;'>Asegúrese de que sus datos cumplan con todos los requisitos 
antes de proceder con el análisis. Los valores de <b>p</b> deben ser estrictamente 
mayores que 0 y menores que 1.</p>
</div>
")
            )
          )
        ),
        
        fluidRow(
          # OPCIÓN 1: Cargar Archivo
          box(
            width = 6,
            title = tagList(icon("upload"), " OPCIÓN 1: Cargar Archivo"),
            status = "primary",
            solidHeader = TRUE,
            
            fileInput(
              "archivo",
              HTML("<b style='color: #00ff00;'>Seleccione su archivo:</b>"),
              accept = c(".xlsx", ".xls", ".csv"),
              buttonLabel = "Examinar",
              placeholder = "Ningún archivo seleccionado"
            ),
            
            tags$div(
              style = "color: #00ff00; font-size: 14px; margin-top: 10px;",
              HTML("<b>Formatos aceptados:</b> .xlsx, .xls, .csv<br>
<b>Estructura requerida:</b> Columnas 'n' y 'p'")
            )
          ),
          
          # OPCIÓN 2: Ingresar Manualmente
          box(
            width = 6,
            title = tagList(icon("keyboard"), " OPCIÓN 2: Ingresar Manualmente"),
            status = "success",
            solidHeader = TRUE,
            
            tags$div(
              style = "color: #00ff00;",
              HTML("<p><b>Ingrese sus datos separados por comas:</b></p>")
            ),
            
            textAreaInput(
              "datos_manuales_n",
              HTML("<b style='color: #00ff00;'>Valores de n (tamaños de muestra):</b>"),
              value = "",
              placeholder = "Ejemplo: 100, 150, 200, 80, 120",
              rows = 3,
              width = "100%"
            ),
            
            textAreaInput(
              "datos_manuales_p",
              HTML("<b style='color: #00ff00;'>Valores de p (proporciones):</b>"),
              value = "",
              placeholder = "Ejemplo: 0.35, 0.42, 0.38, 0.45, 0.40",
              rows = 3,
              width = "100%"
            ),
            
            actionButton(
              "cargar_manual",
              HTML("<b>CARGAR DATOS MANUALES</b>"),
              class = "btn-success btn-block",
              style = "margin-top: 15px; padding: 12px; font-size: 16px; 
background: linear-gradient(135deg, #00ff00 0%, #00aa00 100%);
color: #000000; border: 2px solid #00ff00; border-radius: 8px;"
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("table"), " Vista Previa de Datos"),
            status = "primary",
            solidHeader = TRUE,
            
            DTOutput("tabla_datos")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = tagList(icon("database"), " Estadísticas de Datos"),
            status = "info",
            solidHeader = TRUE,
            
            uiOutput("stats_datos")
          ),
          
          box(
            width = 6,
            title = tagList(icon("check-circle"), " Estado de Validación"),
            status = "success",
            solidHeader = TRUE,
            
            uiOutput("validacion_datos")
          )
        )
      ),
      
      # ========== PESTAÑA: TEORÍA ==========
      tabItem(
        tabName = "teoria",
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("book"), " Marco Teórico del Meta-Análisis"),
            status = "success",
            solidHeader = TRUE,
            
            tags$div(
              style = "font-size: 15px; line-height: 1.9; color: #00ff00; padding: 15px;",
              HTML("
<h2 style='color: #00ff00; text-align: center; border-bottom: 3px solid #00ff00; padding-bottom: 15px;'>
<b> FUNDAMENTOS TEÓRICOS DEL META-ANÁLISIS</b>
</h2>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>1. ¿QUÉ ES UN META-ANÁLISIS?</b></h3>
<p style='text-align: justify;'>
El <b>meta-análisis</b> es una metodología estadística avanzada que permite <b>combinar 
y sintetizar</b> los resultados de múltiples estudios independientes que investigan la 
misma pregunta de investigación. Esta técnica proporciona una <b>estimación más precisa 
y robusta</b> del efecto o fenómeno estudiado que la que podría obtenerse de cualquier 
estudio individual.
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Objetivos Principales:</b></h4>
<ul>
<li><b>Incrementar el poder estadístico:</b> Al combinar múltiples estudios, se aumenta 
el tamaño de muestra efectivo, mejorando la capacidad de detectar efectos reales.</li>
<li><b>Resolver controversias:</b> Cuando estudios individuales muestran resultados 
contradictorios, el meta-análisis puede proporcionar una conclusión más clara.</li>
<li><b>Generar nuevas hipótesis:</b> Identificar patrones y relaciones que no son 
evidentes en estudios individuales.</li>
<li><b>Mejorar la precisión:</b> Obtener intervalos de confianza más estrechos y 
estimaciones más precisas del efecto verdadero.</li>
</ul>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>2. MODELO DE EFECTOS FIJOS</b></h3>
<p style='text-align: justify;'>
El <b>modelo de efectos fijos</b> es uno de los dos enfoques principales en meta-análisis. 
Este modelo asume que <b>existe un único efecto verdadero</b> que es común a todos los 
estudios incluidos en el análisis.
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Supuestos Fundamentales:</b></h4>
<ol>
<li><b>Homogeneidad de efectos:</b> Todos los estudios estiman el mismo efecto poblacional 
verdadero (θ).</li>
<li><b>Diferencias por error de muestreo:</b> Las variaciones observadas entre estudios 
se deben únicamente al error de muestreo aleatorio.</li>
<li><b>No hay heterogeneidad sistemática:</b> No existen diferencias reales entre las 
poblaciones, métodos o contextos de los estudios.</li>
</ol>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Cuándo usar Efectos Fijos:</b></h4>
<ul>
<li>Los estudios son muy similares en diseño, población y métodos</li>
<li>El test Q de homogeneidad NO es significativo (p > α)</li>
<li>El índice I² es bajo (< 25%)</li>
<li>Se desea hacer inferencias solo sobre los estudios incluidos</li>
</ul>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Ventajas y Limitaciones:</b></h4>
<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<p><b> Ventajas:</b></p>
<ul>
<li>Mayor poder estadístico cuando los supuestos se cumplen</li>
<li>Intervalos de confianza más estrechos</li>
<li>Interpretación más simple y directa</li>
<li>Menos parámetros a estimar</li>
</ul>

<p style='margin-top: 15px;'><b> Limitaciones:</b></p>
<ul>
<li>Puede subestimar la incertidumbre si hay heterogeneidad real</li>
<li>Los estudios grandes tienen influencia desproporcionada</li>
<li>Inferencias limitadas a los estudios específicos incluidos</li>
<li>No apropiado cuando existe heterogeneidad significativa</li>
</ul>
</div>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>3. TRANSFORMACIÓN LOGIT</b></h3>
<p style='text-align: justify;'>
En meta-análisis de proporciones, la <b>transformación logit</b> es esencial porque las 
proporciones tienen propiedades estadísticas problemáticas en su escala original.
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>¿Por qué es necesaria?</b></h4>
<ol>
<li><b>Escala restringida:</b> Las proporciones están limitadas entre 0 y 1, violando 
supuestos de normalidad.</li>
<li><b>Varianza heterogénea:</b> La varianza de una proporción depende de su valor 
(mayor cerca de 0.5, menor cerca de 0 o 1).</li>
<li><b>Asimetría:</b> La distribución de proporciones puede ser muy asimétrica, 
especialmente para valores extremos.</li>
</ol>

<h4 style='color: #00ff00; margin-top: 20px;'><b>La Transformación:</b></h4>
<div style='background: #000000; padding: 20px; border-radius: 10px; 
text-align: center; font-size: 18px; border: 2px solid #00ff00;'>
<b>logit(p) = ln(p / (1 - p))</b>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Propiedades del Logit:</b></h4>
<ul>
<li><b>Dominio ampliado:</b> Transforma [0,1] a (-∞, +∞)</li>
<li><b>Simetría:</b> logit(p) = -logit(1-p)</li>
<li><b>Linealidad:</b> Apropiado para modelos lineales</li>
<li><b>Interpretación:</b> Diferencias en escala logit son razones de odds</li>
</ul>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Transformación Inversa:</b></h4>
<div style='background: #000000; padding: 20px; border-radius: 10px; 
text-align: center; font-size: 18px; border: 2px solid #00ff00;'>
<b>p = exp(θ) / (1 + exp(θ))</b>
</div>
<p style='text-align: center; margin-top: 10px;'>
Donde θ es el valor en escala logit
</p>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>4. SISTEMA DE PONDERACIÓN</b></h3>
<p style='text-align: justify;'>
El meta-análisis utiliza un sistema de <b>ponderación por precisión</b> donde cada 
estudio contribuye al efecto combinado en proporción a su confiabilidad.
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Cálculo de Varianza:</b></h4>
<div style='background: #000000; padding: 20px; border-radius: 10px; 
text-align: center; font-size: 18px; border: 2px solid #00ff00;'>
<b>V(logit(p)) = 1 / (n × p × (1 - p))</b>
</div>
<p style='margin-top: 15px;'>Donde:</p>
<ul>
<li><b>n:</b> Tamaño de muestra del estudio</li>
<li><b>p:</b> Proporción observada</li>
<li><b>V:</b> Varianza de la transformación logit</li>
</ul>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Cálculo de Pesos:</b></h4>
<div style='background: #000000; padding: 20px; border-radius: 10px; 
text-align: center; font-size: 18px; border: 2px solid #00ff00;'>
<b>w<sub>i</sub> = 1 / V<sub>i</sub></b>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Principios de Ponderación:</b></h4>
<ul>
<li><b>Inversamente proporcional a la varianza:</b> Mayor precisión = mayor peso</li>
<li><b>Estudios grandes tienen más influencia:</b> Más datos = más confiable</li>
<li><b>Proporciones extremas tienen menos peso:</b> Cerca de 0 o 1 = menos información</li>
<li><b>Refleja la calidad de la información:</b> Peso = confiabilidad estadística</li>
</ul>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Ejemplo Práctico:</b></h4>
<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<p>Supongamos dos estudios:</p>
<ul>
<li><b>Estudio A:</b> n=100, p=0.3 → V≈0.0476 → w≈21.0</li>
<li><b>Estudio B:</b> n=500, p=0.3 → V≈0.0095 → w≈105.0</li>
</ul>
<p>El Estudio B, con 5 veces más participantes, tiene 5 veces más peso en el análisis.</p>
</div>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>5. EFECTO FIJO PROMEDIO (EFP)</b></h3>
<p style='text-align: justify;'>
El <b>Efecto Fijo Promedio</b> (θ<sub>EF</sub>) es la estimación central del meta-análisis 
y representa el mejor estimador del efecto verdadero común a todos los estudios.
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Fórmula del EFP:</b></h4>
<div style='background: #000000; padding: 20px; border-radius: 10px; 
text-align: center; font-size: 20px; border: 2px solid #00ff00;'>
<b>θ<sub>EF</sub> = Σ(w<sub>i</sub> × logit(p<sub>i</sub>)) / Σw<sub>i</sub></b>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Componentes:</b></h4>
<ul>
<li><b>Numerador:</b> Suma ponderada de los efectos (cada estudio × su peso)</li>
<li><b>Denominador:</b> Suma de todos los pesos</li>
<li><b>Resultado:</b> Promedio ponderado en escala logit</li>
</ul>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Interpretación del EFP:</b></h4>
<ol>
<li><b>En escala logit:</b> θ<sub>EF</sub> es directamente el efecto combinado</li>
<li><b>En escala de proporción:</b> T<sub>EF</sub> = exp(θ<sub>EF</sub>)/(1+exp(θ<sub>EF</sub>))</li>
<li><b>Intervalo de confianza:</b> Cuantifica la incertidumbre del estimador</li>
<li><b>Precisión:</b> SE(θ<sub>EF</sub>) = 1/√(Σw<sub>i</sub>)</li>
</ol>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Propiedades Estadísticas:</b></h4>
<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<ul>
<li><b>Insesgado:</b> E(θ<sub>EF</sub>) = θ cuando los supuestos se cumplen</li>
<li><b>Eficiente:</b> Tiene la menor varianza posible entre estimadores lineales</li>
<li><b>Consistente:</b> Converge al valor verdadero cuando k→∞</li>
<li><b>Suficiente:</b> Utiliza toda la información disponible de los estudios</li>
</ul>
</div>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>6. TEST DE HOMOGENEIDAD (TEST Q)</b></h3>
<p style='text-align: justify;'>
El <b>Test Q de Cochran</b> es la prueba estadística fundamental para evaluar si los 
efectos de los estudios son homogéneos (consistentes) o heterogéneos (variables).
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Hipótesis del Test:</b></h4>
<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<p><b>H₀ (Hipótesis Nula):</b> Los estudios son homogéneos<br>
θ₁ = θ₂ = ... = θₖ (todos los estudios estiman el mismo efecto)</p>
<p style='margin-top: 10px;'><b>H₁ (Hipótesis Alternativa):</b> Los estudios son heterogéneos<br>
Al menos un θᵢ ≠ θ (hay diferencias reales entre estudios)</p>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Fórmula del Estadístico Q:</b></h4>
<div style='background: #000000; padding: 20px; border-radius: 10px; 
text-align: center; font-size: 20px; border: 2px solid #00ff00;'>
<b>Q = Σ[w<sub>i</sub> × (logit(p<sub>i</sub>) - θ<sub>EF</sub>)²]</b>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Componentes del Estadístico:</b></h4>
<ul>
<li><b>w<sub>i</sub>:</b> Peso del estudio i (inversamente proporcional a su varianza)</li>
<li><b>logit(p<sub>i</sub>):</b> Efecto observado del estudio i en escala logit</li>
<li><b>θ<sub>EF</sub>:</b> Efecto fijo promedio (estimación combinada)</li>
<li><b>Diferencia al cuadrado:</b> Desviación de cada estudio respecto al efecto común</li>
</ul>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Distribución y Grados de Libertad:</b></h4>
<p>Bajo H₀, el estadístico Q sigue una distribución chi-cuadrado:</p>
<div style='background: #000000; padding: 15px; border-radius: 8px; 
text-align: center; border: 1px solid #00ff00;'>
<b>Q ~ χ²(k-1)</b><br>
<span style='font-size: 14px;'>donde k = número de estudios</span>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Regla de Decisión:</b></h4>
<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<p><b>Si p-value < α:</b> Rechazar H₀</p>
<ul>
<li>Conclusión: Los estudios son HETEROGÉNEOS</li>
<li>Interpretación: Hay variabilidad real más allá del azar</li>
<li>Acción: Investigar fuentes de heterogeneidad</li>
</ul>

<p style='margin-top: 15px;'><b>Si p-value ≥ α:</b> No rechazar H₀</p>
<ul>
<li>Conclusión: Los estudios son HOMOGÉNEOS</li>
<li>Interpretación: Diferencias explicadas por azar</li>
<li>Acción: Modelo de efectos fijos es apropiado</li>
</ul>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Limitaciones del Test Q:</b></h4>
<ul>
<li><b>Bajo poder con pocos estudios:</b> Difícil detectar heterogeneidad real con k < 10</li>
<li><b>Sobre-poder con muchos estudios:</b> Puede detectar heterogeneidad trivial con k > 50</li>
<li><b>Sensible al tamaño de muestra:</b> Estudios grandes influyen desproporcionadamente</li>
<li><b>Prueba binaria:</b> No cuantifica la magnitud de la heterogeneidad</li>
</ul>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>7. ÍNDICE I² DE HETEROGENEIDAD</b></h3>
<p style='text-align: justify;'>
El <b>índice I²</b> complementa el Test Q proporcionando una <b>medida cuantitativa</b> 
de la heterogeneidad que es más interpretable y menos sensible al número de estudios.
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Fórmula de I²:</b></h4>
<div style='background: #000000; padding: 20px; border-radius: 10px; 
text-align: center; font-size: 20px; border: 2px solid #00ff00;'>
<b>I² = max(0, [(Q - df) / Q] × 100%)</b>
</div>
<p style='text-align: center; margin-top: 10px;'>
donde df = k - 1 (grados de libertad)
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Interpretación de I²:</b></h4>
<p>I² representa el <b>porcentaje de variabilidad total</b> que se debe a heterogeneidad 
real entre estudios, en lugar de variabilidad por azar.</p>

<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<p><b>Escala de Interpretación de Higgins & Thompson (2002):</b></p>
<ul>
<li><b>I² = 0% - 25%:</b> 🟢 Heterogeneidad BAJA o NULA</li>
<li><b>I² = 25% - 50%:</b> 🟡 Heterogeneidad MODERADA</li>
<li><b>I² = 50% - 75%:</b> 🟠 Heterogeneidad SUSTANCIAL</li>
<li><b>I² > 75%:</b> Heterogeneidad ALTA o CONSIDERABLE</li>
</ul>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Ventajas de I² sobre el Test Q:</b></h4>
<ol>
<li><b>Independiente del número de estudios:</b> No cambia sistemáticamente con k</li>
<li><b>Escala estandarizada:</b> Siempre entre 0% y 100%, fácil de interpretar</li>
<li><b>Cuantifica magnitud:</b> Mide QUÉ TANTO heterogeneidad existe</li>
<li><b>Comparable entre meta-análisis:</b> Permite comparaciones directas</li>
<li><b>Interpretación intuitiva:</b> Porcentaje es universalmente comprensible</li>
</ol>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Relación entre Q e I²:</b></h4>
<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<ul>
<li><b>Q cercano a df:</b> I² ≈ 0% (homogeneidad)</li>
<li><b>Q >> df:</b> I² alto (heterogeneidad sustancial)</li>
<li><b>Q < df:</b> I² = 0% (se ajusta a 0 por construcción)</li>
</ul>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Ejemplo Interpretativo:</b></h4>
<p>Si I² = 75%, significa que:</p>
<ul>
<li>El 75% de la variabilidad observada es REAL (heterogeneidad verdadera)</li>
<li>Solo el 25% se debe al error de muestreo (azar)</li>
<li>Hay diferencias sustanciales entre los estudios</li>
<li>Se debe investigar las causas de esta heterogeneidad</li>
</ul>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>8. FOREST PLOT (GRÁFICO DE BOSQUE)</b></h3>
<p style='text-align: justify;'>
El <b>Forest Plot</b> es la representación gráfica estándar en meta-análisis, proporcionando 
una visualización completa e intuitiva de los resultados.
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Componentes del Forest Plot:</b></h4>
<ol>
<li><b>Cuadrados:</b> Representan las estimaciones puntuales de cada estudio
<ul>
<li>Tamaño del cuadrado = peso del estudio</li>
<li>Mayor cuadrado = mayor influencia en el meta-análisis</li>
</ul>
</li>
<li><b>Líneas horizontales:</b> Intervalos de confianza al 95% de cada estudio
<ul>
<li>Línea más corta = mayor precisión</li>
<li>Línea más larga = mayor incertidumbre</li>
</ul>
</li>
<li><b>Diamante:</b> Efecto combinado del meta-análisis
<ul>
<li>Centro del diamante = estimación puntual del EFP</li>
<li>Ancho del diamante = intervalo de confianza del EFP</li>
</ul>
</li>
<li><b>Línea vertical punteada:</b> Línea de no efecto o referencia</li>
</ol>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Interpretación Visual:</b></h4>
<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<p><b>Patrones de Homogeneidad:</b></p>
<ul>
<li><b>Estudios alineados:</b> Todos los intervalos se superponen → Homogeneidad</li>
<li><b>Estudios dispersos:</b> Poca superposición de intervalos → Heterogeneidad</li>
</ul>

<p style='margin-top: 15px;'><b>Precisión del Meta-análisis:</b></p>
<ul>
<li><b>Diamante estrecho:</b> Alta precisión, intervalo de confianza ajustado</li>
<li><b>Diamante ancho:</b> Baja precisión, mayor incertidumbre</li>
</ul>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Ventajas del Forest Plot:</b></h4>
<ul>
<li><b>Visualización intuitiva:</b> Fácil de entender para audiencias diversas</li>
<li><b>Información completa:</b> Muestra estimaciones, IC, pesos y efecto combinado</li>
<li><b>Detecta patrones:</b> Heterogeneidad y outliers son visualmente evidentes</li>
<li><b>Comunicación efectiva:</b> Estándar en publicaciones científicas</li>
</ul>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>9. MANEJO DE HETEROGENEIDAD</b></h3>
<p style='text-align: justify;'>
Cuando se detecta heterogeneidad significativa, es crucial investigar sus causas y 
considerar enfoques alternativos de análisis.
</p>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Fuentes Comunes de Heterogeneidad:</b></h4>
<div style='background: #000000; padding: 15px; border-radius: 8px; border: 1px solid #00ff00;'>
<ol>
<li><b>Características de los participantes:</b>
<ul>
<li>Edad, género, severidad de la condición</li>
<li>Comorbilidades, características demográficas</li>
</ul>
</li>
<li><b>Diferencias metodológicas:</b>
<ul>
<li>Diseño del estudio (aleatorizado vs. observacional)</li>
<li>Duración del seguimiento, criterios de inclusión/exclusión</li>
</ul>
</li>
<li><b>Intervenciones:</b>
<ul>
<li>Dosis, duración, modo de administración</li>
<li>Variaciones en la implementación</li>
</ul>
</li>
<li><b>Contexto:</b>
<ul>
<li>Ubicación geográfica, período temporal</li>
<li>Sistema de salud, recursos disponibles</li>
</ul>
</li>
</ol>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Estrategias de Manejo:</b></h4>
<ol>
<li><b>Meta-regresión:</b>
<ul>
<li>Identificar covariables que explican la heterogeneidad</li>
<li>Modelo: θᵢ = β₀ + β₁X₁ᵢ + β₂X₂ᵢ + ... + εᵢ</li>
</ul>
</li>
<li><b>Análisis de subgrupos:</b>
<ul>
<li>Dividir estudios en categorías homogéneas</li>
<li>Comparar efectos entre subgrupos</li>
</ul>
</li>
<li><b>Modelo de efectos aleatorios:</b>
<ul>
<li>Asume una distribución de efectos verdaderos</li>
<li>Incorpora varianza entre estudios (τ²)</li>
</ul>
</li>
<li><b>Análisis de sensibilidad:</b>
<ul>
<li>Exclusión de estudios outliers</li>
<li>Evaluación de la robustez de resultados</li>
</ul>
</li>
</ol>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Modelo de Efectos Aleatorios:</b></h4>
<p>Cuando la heterogeneidad es sustancial, el modelo de efectos aleatorios es más apropiado:</p>
<div style='background: #000000; padding: 15px; border-radius: 8px; 
text-align: center; border: 1px solid #00ff00;'>
<b>θᵢ = θ + δᵢ + εᵢ</b><br>
<span style='font-size: 14px;'>donde δᵢ ~ N(0, τ²) representa la variabilidad entre estudios</span>
</div>

<p style='margin-top: 15px;'><b>Diferencias clave con Efectos Fijos:</b></p>
<ul>
<li>Añade componente de varianza entre estudios (τ²)</li>
<li>Intervalos de confianza más amplios</li>
<li>Inferencias generalizables a población de estudios</li>
<li>Menos influencia de estudios grandes</li>
</ul>
</div>

<div style='background: #1a1a1a; border: 2px solid #00ff00; border-radius: 12px; 
padding: 25px; margin: 20px 0;'>
<h3 style='color: #00ff00;'><b>10. INTERPRETACIÓN Y REPORTE DE RESULTADOS</b></h3>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Elementos Esenciales a Reportar:</b></h4>
<ol>
<li><b>Características de los estudios:</b>
<ul>
<li>Número de estudios incluidos (k)</li>
<li>Tamaño total de muestra (N = Σnᵢ)</li>
<li>Rango de tamaños de muestra</li>
</ul>
</li>
<li><b>Efecto combinado:</b>
<ul>
<li>Estimación puntual: θ<sub>EF</sub> y T<sub>EF</sub></li>
<li>Error estándar: SE(θ<sub>EF</sub>)</li>
<li>Intervalo de confianza al 95%</li>
</ul>
</li>
<li><b>Evaluación de heterogeneidad:</b>
<ul>
<li>Estadístico Q y su p-value</li>
<li>Índice I² con interpretación</li>
<li>Conclusión sobre homogeneidad/heterogeneidad</li>
</ul>
</li>
<li><b>Visualización:</b>
<ul>
<li>Forest plot con todos los estudios</li>
<li>Gráfico chi-cuadrado del Test Q</li>
</ul>
</li>
</ol>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Ejemplo de Interpretación Completa:</b></h4>
<div style='background: #000000; padding: 20px; border-radius: 8px; 
border: 2px solid #00ff00; font-size: 14px;'>
<p><b>\"Se realizó un meta-análisis de efectos fijos incluyendo k=15 estudios con 
N=3,450 participantes. El efecto fijo promedio fue T<sub>EF</sub>=0.342 
(IC 95%: 0.315-0.370), lo que indica una proporción combinada del 34.2%.</b></p>

<p style='margin-top: 15px;'><b>El test de homogeneidad mostró Q=12.43 (df=14, 
p=0.572), indicando que no existe heterogeneidad significativa entre los estudios. 
Consistentemente, el índice I²=0%, confirmando homogeneidad total.</b></p>

<p style='margin-top: 15px;'><b>Estos resultados sugieren que el modelo de efectos 
fijos es apropiado y que los estudios estiman un efecto común. La estimación 
combinada puede interpretarse con confianza como representativa del efecto 
verdadero en la población.\"</b></p>
</div>

<h4 style='color: #00ff00; margin-top: 20px;'><b>Consideraciones Finales:</b></h4>
<ul>
<li><b>Contexto clínico/práctico:</b> Siempre interpretar en el contexto del área</li>
<li><b>Limitaciones:</b> Reconocer sesgos potenciales y limitaciones del análisis</li>
<li><b>Implicaciones:</b> Discutir relevancia práctica y recomendaciones</li>
<li><b>Transparencia:</b> Reportar todos los análisis realizados, no solo los favorables</li>
</ul>
</div>

<div style='background: #000000; border: 3px solid #00ff00; border-radius: 15px; 
padding: 25px; margin: 30px 0; text-align: center;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b> REFERENCIAS BIBLIOGRÁFICAS CLAVE</b>
</h3>
<div style='text-align: left; font-size: 14px;'>
<ol>
<li style='margin-bottom: 10px;'>
<b>Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2009).</b> 
Introduction to Meta-Analysis. John Wiley & Sons.
</li>
<li style='margin-bottom: 10px;'>
<b>Higgins, J. P., & Thompson, S. G. (2002).</b> Quantifying heterogeneity in a 
meta-analysis. Statistics in Medicine, 21(11), 1539-1558.
</li>
<li style='margin-bottom: 10px;'>
<b>Cochran, W. G. (1954).</b> The combination of estimates from different experiments. 
Biometrics, 10(1), 101-129.
</li>
<li style='margin-bottom: 10px;'>
<b>DerSimonian, R., & Laird, N. (1986).</b> Meta-analysis in clinical trials. 
Controlled Clinical Trials, 7(3), 177-188.
</li>
<li style='margin-bottom: 10px;'>
<b>Deeks, J. J., Higgins, J. P., & Altman, D. G. (2008).</b> Analysing data and 
undertaking meta-analyses. In Cochrane Handbook for Systematic Reviews of Interventions.
</li>
</ol>
</div>
</div>
")
            )
          )
        )
      ),
      
      # ========== PESTAÑA: CÁLCULOS ==========
      tabItem(
        tabName = "calculos",
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("calculator"), " Tabla de Cálculos Detallados"),
            status = "primary",
            solidHeader = TRUE,
            
            DTOutput("tabla_calculos"),
            
            hr(style = "border-color: #00ff00; margin: 25px 0;"),
            
            downloadButton(
              "descargar_calculos",
              label = " Descargar Cálculos (CSV)",
              class = "btn-success",
              style = "padding: 12px 24px; font-size: 16px; font-weight: 600;"
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("info-circle"), " Explicación de las Columnas"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            tags$div(
              style = "font-size: 15px; line-height: 1.8; color: #00ff00;",
              HTML("
<div style='background: #1a1a1a; padding: 20px; border-radius: 10px; border: 1px solid #00ff00;'>
<h4 style='color: #00ff00; border-bottom: 2px solid #00ff00; padding-bottom: 10px;'>
<b>Descripción Detallada de Cada Columna:</b>
</h4>

<div style='margin: 15px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<p><b style='color: #00ff00;'>1. Estudio:</b></p>
<p>Identificador único de cada estudio incluido en el meta-análisis. 
Numeración consecutiva desde Estudio_1 hasta Estudio_k.</p>
</div>

<div style='margin: 15px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<p><b style='color: #00ff00;'>2. n (Tamaño de muestra):</b></p>
<p>Número total de participantes o unidades observadas en cada estudio. 
Valores mayores proporcionan estimaciones más precisas.</p>
</div>

<div style='margin: 15px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<p><b style='color: #00ff00;'>3. p (Proporción):</b></p>
<p>Proporción observada del evento de interés (entre 0 y 1, exclusivo). 
Representa la frecuencia relativa del outcome en el estudio.</p>
</div>

<div style='margin: 15px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<p><b style='color: #00ff00;'>4. logit(p):</b></p>
<p>Transformación logit de la proporción: <b>ln(p/(1-p))</b>. 
Convierte la escala restringida [0,1] a la escala real completa (-∞, +∞), 
permitiendo aplicar métodos estadísticos estándar.</p>
</div>

<div style='margin: 15px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<p><b style='color: #00ff00;'>5. V (Varianza):</b></p>
<p>Varianza del logit transformado: <b>1/(n×p×(1-p))</b>. 
Mide la incertidumbre o dispersión de la estimación. Varianzas menores 
indican mayor precisión.</p>
</div>

<div style='margin: 15px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<p><b style='color: #00ff00;'>6. w (Peso):</b></p>
<p>Peso del estudio en el meta-análisis: <b>1/V</b>. 
Estudios con mayor precisión (menor varianza) reciben más peso. 
Los pesos determinan la contribución de cada estudio al efecto combinado.</p>
</div>

<div style='margin: 15px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<p><b style='color: #00ff00;'>7. w_porcentaje (%):</b></p>
<p>Contribución porcentual de cada estudio: <b>(wᵢ / Σw) × 100</b>. 
Facilita la interpretación de la influencia relativa de cada estudio. 
La suma de todos los porcentajes es siempre 100%.</p>
</div>

<div style='margin: 15px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<p><b style='color: #00ff00;'>8. Q_i (Contribución al estadístico Q):</b></p>
<p>Contribución individual de cada estudio al test de homogeneidad: 
<b>wᵢ × (logit(pᵢ) - θ_EF)²</b>. 
Valores altos indican que el estudio se desvía sustancialmente del efecto promedio.</p>
</div>

<div style='margin-top: 25px; padding: 20px; background: #000000; 
border: 2px solid #00ff00; border-radius: 10px;'>
<h4 style='color: #00ff00;'><b> Interpretaciones Clave:</b></h4>
<ul>
<li><b>Estudios con mayor peso (w ↑):</b> Tienen mayor influencia en el EFP</li>
<li><b>Q_i altos:</b> Sugieren heterogeneidad o posibles outliers</li>
<li><b>Varianzas uniformes:</b> Indican estudios de precisión similar</li>
<li><b>Distribución de pesos:</b> Muestra la estructura del meta-análisis</li>
</ul>
</div>
</div>
")
            )
          )
        )
      ),
      
      # ========== PESTAÑA: RESULTADOS ==========
      tabItem(
        tabName = "resultados",
        
        # Fila 1: Métricas principales
        fluidRow(
          valueBoxOutput("vbox_theta", width = 3),
          valueBoxOutput("vbox_t_ef", width = 3),
          valueBoxOutput("vbox_k", width = 3),
          valueBoxOutput("vbox_n_total", width = 3)
        ),
        
        # Fila 2: Test de homogeneidad
        fluidRow(
          valueBoxOutput("vbox_q", width = 3),
          valueBoxOutput("vbox_pvalue", width = 3),
          valueBoxOutput("vbox_i2", width = 3),
          valueBoxOutput("vbox_decision", width = 3)
        ),
        
        # Fila 3: Resumen interpretativo
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("file-alt"), " Resumen Interpretativo Completo"),
            status = "success",
            solidHeader = TRUE,
            
            verbatimTextOutput("resumen_consola", placeholder = TRUE),
            
            hr(style = "border-color: #00ff00; margin: 25px 0;"),
            
            downloadButton(
              "descargar_resultados",
              label = " Descargar Resultados (CSV)",
              class = "btn-success",
              style = "padding: 12px 24px; font-size: 16px; font-weight: 600;"
            )
          )
        ),
        
        # Fila 4: Información adicional
        fluidRow(
          box(
            width = 6,
            title = tagList(icon("info-circle"), " Intervalos de Confianza"),
            status = "info",
            solidHeader = TRUE,
            
            uiOutput("intervalos_confianza")
          ),
          
          box(
            width = 6,
            title = tagList(icon("chart-pie"), " Distribución de Pesos"),
            status = "info",
            solidHeader = TRUE,
            
            plotOutput("plot_pesos", height = "300px")
          )
        )
      ),
      
      # ========== PESTAÑA: FOREST PLOT ==========
      tabItem(
        tabName = "forest",
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("chart-area"), " Forest Plot Profesional"),
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput("forest_plot", height = "700px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("info-circle"), " Guía de Interpretación del Forest Plot"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            tags$div(
              style = "font-size: 15px; line-height: 1.8; color: #00ff00; padding: 15px;",
              HTML("
<div style='background: #1a1a1a; padding: 20px; border-radius: 10px; border: 2px solid #00ff00;'>
<h4 style='color: #00ff00; border-bottom: 2px solid #00ff00; padding-bottom: 10px;'>
<b> Cómo Leer el Forest Plot:</b>
</h4>

<div style='margin: 20px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<h5 style='color: #00ff00;'><b>1. Cuadrados (Estimaciones Puntuales):</b></h5>
<ul>
<li>Cada cuadrado representa la <b>estimación puntual</b> de un estudio</li>
<li><b>Tamaño del cuadrado:</b> proporcional al peso del estudio</li>
<li>Cuadrado grande = mayor influencia en el meta-análisis</li>
<li>La posición horizontal indica el valor de la proporción</li>
</ul>
</div>

<div style='margin: 20px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<h5 style='color: #00ff00;'><b>2. Líneas Horizontales (Intervalos de Confianza):</b></h5>
<ul>
<li>Representan la <b>incertidumbre</b> de cada estimación</li>
<li><b>Línea corta:</b> alta precisión, estimación confiable</li>
<li><b>Línea larga:</b> baja precisión, mayor incertidumbre</li>
<li>Si los intervalos se superponen → consistencia entre estudios</li>
</ul>
</div>

<div style='margin: 20px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<h5 style='color: #00ff00;'><b>3. Diamante Verde (Efecto Combinado):</b></h5>
<ul>
<li><b>Centro del diamante:</b> Efecto Fijo Promedio (T_EF)</li>
<li><b>Ancho del diamante:</b> Intervalo de confianza del meta-análisis</li>
<li>Diamante estrecho = alta precisión del efecto combinado</li>
<li>Posición indica la magnitud del efecto agregado</li>
</ul>
</div>

<div style='margin: 20px 0; padding: 15px; background: #000000; border-radius: 8px;'>
<h5 style='color: #00ff00;'><b>4. Línea Vertical Punteada (Referencia):</b></h5>
<ul>
<li>Línea de <b>referencia central</b> del gráfico</li>
<li>Facilita la comparación visual de los efectos</li>
<li>Estudios a la derecha/izquierda de la línea tienen diferentes magnitudes</li>
</ul>
</div>

<div style='margin-top: 25px; padding: 20px; background: #000000; 
border: 2px solid #00ff00; border-radius: 10px;'>
<h4 style='color: #00ff00;'><b> Patrones Visuales Importantes:</b></h4>
<ul>
<li><b>Estudios alineados verticalmente:</b> Sugiere HOMOGENEIDAD</li>
<li><b>Estudios dispersos:</b> Sugiere HETEROGENEIDAD</li>
<li><b>Outliers evidentes:</b> Estudios muy alejados del resto</li>
<li><b>Diamante dentro del rango:</b> Efecto combinado representativo</li>
<li><b>Intervalos no superpuestos:</b> Diferencias significativas entre estudios</li>
</ul>
</div>
</div>
")
            )
          )
        )
      ),
      
      # ========== PESTAÑA: TEST Q ==========
      tabItem(
        tabName = "testq",
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("vial"), " Test de Homogeneidad Q - Análisis Detallado"),
            status = "primary",
            solidHeader = TRUE,
            
            verbatimTextOutput("test_q_detalle", placeholder = TRUE)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("chart-line"), " Distribución Chi-Cuadrado con Test Q"),
            status = "success",
            solidHeader = TRUE,
            
            plotOutput("plot_q", height = "600px")
          )
        )
      ),
      
      # ========== PESTAÑA: FÓRMULAS ==========
      tabItem(
        tabName = "formulas",
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("square-root-alt"), " Fórmulas Matemáticas del Meta-Análisis"),
            status = "primary",
            solidHeader = TRUE,
            
            tags$div(
              style = "font-size: 16px; line-height: 2; color: #00ff00; padding: 20px;",
              HTML("
<div style='background: #1a1a1a; padding: 25px; border-radius: 12px; border: 2px solid #00ff00;'>
<h2 style='text-align: center; color: #00ff00; border-bottom: 3px solid #00ff00; 
padding-bottom: 15px; margin-bottom: 30px;'>
<b> FORMULARIO COMPLETO DE META-ANÁLISIS</b>
</h2>

<!-- Fórmula 1 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>1. Transformación Logit</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>logit(p) = ln(p / (1 - p))</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>p:</b> Proporción observada (0 < p < 1)</li>
<li><b>ln:</b> Logaritmo natural</li>
<li><b>Rango de salida:</b> (-∞, +∞)</li>
</ul>
<p style='margin-top: 15px;'><b>Propósito:</b> Transformar proporciones a escala 
ilimitada para aplicar métodos estadísticos estándar.</p>
</div>

<!-- Fórmula 2 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>2. Varianza del Logit</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>V<sub>i</sub> = 1 / (n<sub>i</sub> × p<sub>i</sub> × (1 - p<sub>i</sub>))</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>n<sub>i</sub>:</b> Tamaño de muestra del estudio i</li>
<li><b>p<sub>i</sub>:</b> Proporción observada del estudio i</li>
<li><b>V<sub>i</sub>:</b> Varianza del logit para el estudio i</li>
</ul>
<p style='margin-top: 15px;'><b>Interpretación:</b> Mide la precisión de cada 
estudio. Varianza menor = mayor precisión.</p>
</div>

<!-- Fórmula 3 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>3. Peso del Estudio</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>w<sub>i</sub> = 1 / V<sub>i</sub></b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>w<sub>i</sub>:</b> Peso del estudio i</li>
<li><b>V<sub>i</sub>:</b> Varianza del estudio i</li>
</ul>
<p style='margin-top: 15px;'><b>Principio:</b> Estudios más precisos (menor varianza) 
reciben mayor peso en el meta-análisis.</p>
</div>

<!-- Fórmula 4 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>4. Efecto Fijo Promedio (Escala Logit)</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>θ<sub>EF</sub> = Σ(w<sub>i</sub> × logit(p<sub>i</sub>)) / Σw<sub>i</sub></b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>θ<sub>EF</sub>:</b> Efecto fijo promedio en escala logit</li>
<li><b>w<sub>i</sub>:</b> Peso del estudio i</li>
<li><b>logit(p<sub>i</sub>):</b> Logit del estudio i</li>
<li><b>Σ:</b> Suma sobre todos los k estudios</li>
</ul>
<p style='margin-top: 15px;'><b>Interpretación:</b> Promedio ponderado de los 
efectos, donde cada estudio contribuye proporcionalmente a su precisión.</p>
</div>

<!-- Fórmula 5 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>5. Transformación Inversa (Logit a Proporción)</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>T<sub>EF</sub> = exp(θ<sub>EF</sub>) / (1 + exp(θ<sub>EF</sub>))</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>T<sub>EF</sub>:</b> Efecto fijo en escala de proporción</li>
<li><b>θ<sub>EF</sub>:</b> Efecto fijo en escala logit</li>
<li><b>exp:</b> Función exponencial (e<sup>x</sup>)</li>
</ul>
<p style='margin-top: 15px;'><b>Uso:</b> Convertir el resultado del meta-análisis 
de vuelta a la escala original de proporción para interpretación práctica.</p>
</div>

<!-- Fórmula 6 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>6. Error Estándar del EFP</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>SE(θ<sub>EF</sub>) = 1 / √(Σw<sub>i</sub>)</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>SE:</b> Error estándar (standard error)</li>
<li><b>Σw<sub>i</sub>:</b> Suma de todos los pesos</li>
<li><b>√:</b> Raíz cuadrada</li>
</ul>
<p style='margin-top: 15px;'><b>Utilidad:</b> Cuantifica la precisión de la 
estimación del efecto combinado.</p>
</div>

<!-- Fórmula 7 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>7. Estadístico Q de Homogeneidad</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>Q = Σ[w<sub>i</sub> × (logit(p<sub>i</sub>) - θ<sub>EF</sub>)<sup>2</sup>]</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>Q:</b> Estadístico de homogeneidad de Cochran</li>
<li><b>w<sub>i</sub>:</b> Peso del estudio i</li>
<li><b>logit(p<sub>i</sub>):</b> Efecto observado del estudio i</li>
<li><b>θ<sub>EF</sub>:</b> Efecto fijo promedio</li>
</ul>
<p style='margin-top: 15px;'><b>Distribución bajo H₀:</b> Q ~ χ²(k-1)</p>
<p><b>Hipótesis:</b> H₀: θ₁ = θ₂ = ... = θₖ (homogeneidad)</p>
</div>

<!-- Fórmula 8 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>8. Grados de Libertad</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>df = k - 1</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>df:</b> Grados de libertad (degrees of freedom)</li>
<li><b>k:</b> Número total de estudios</li>
</ul>
<p style='margin-top: 15px;'><b>Uso:</b> Parámetro de la distribución chi-cuadrado 
para el test Q.</p>
</div>

<!-- Fórmula 9 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>9. Índice I² de Heterogeneidad</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>I² = max(0, [(Q - df) / Q] × 100%)</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>I²:</b> Proporción de variabilidad debida a heterogeneidad</li>
<li><b>Q:</b> Estadístico de homogeneidad</li>
<li><b>df:</b> Grados de libertad (k-1)</li>
<li><b>max:</b> Función que toma el máximo entre 0 y el cálculo</li>
</ul>
<p style='margin-top: 15px;'><b>Escala de interpretación:</b></p>
<ul>
<li>0% - 25%: Heterogeneidad baja</li>
<li>25% - 50%: Heterogeneidad moderada</li>
<li>50% - 75%: Heterogeneidad sustancial</li>
<li>> 75%: Heterogeneidad alta</li>
</ul>
</div>

<!-- Fórmula 10 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>10. Intervalo de Confianza (95%)</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>IC<sub>95%</sub> = θ<sub>EF</sub> ± 1.96 × SE(θ<sub>EF</sub>)</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>IC<sub>95%</sub>:</b> Intervalo de confianza al 95%</li>
<li><b>θ<sub>EF</sub>:</b> Estimación puntual del efecto</li>
<li><b>1.96:</b> Valor crítico de la distribución normal (α=0.05)</li>
<li><b>SE:</b> Error estándar de θ<sub>EF</sub></li>
</ul>
<p style='margin-top: 15px;'><b>Interpretación:</b> Rango en el que esperamos 
que se encuentre el verdadero efecto poblacional con 95% de confianza.</p>
<p style='margin-top: 10px;'><b>Para escala de proporción:</b> Aplicar 
transformación inversa a los límites del IC.</p>
</div>

<!-- Fórmula 11 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>11. Contribución Individual al Q (Q<sub>i</sub>)</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>Q<sub>i</sub> = w<sub>i</sub> × (logit(p<sub>i</sub>) - θ<sub>EF</sub>)<sup>2</sup></b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>Q<sub>i</sub>:</b> Contribución del estudio i al estadístico Q total</li>
<li><b>w<sub>i</sub>:</b> Peso del estudio i</li>
<li><b>logit(p<sub>i</sub>):</b> Efecto del estudio i</li>
<li><b>θ<sub>EF</sub>:</b> Efecto fijo promedio</li>
</ul>
<p style='margin-top: 15px;'><b>Uso:</b> Identificar estudios que contribuyen 
desproporcionadamente a la heterogeneidad (posibles outliers).</p>
</div>

<!-- Fórmula 12 -->
<div style='margin: 30px 0; padding: 25px; background: #000000; 
border-radius: 10px; border: 2px solid #00ff00;'>
<h3 style='color: #00ff00; margin-bottom: 20px;'>
<b>12. Peso Porcentual</b>
</h3>
<div style='text-align: center; font-size: 22px; padding: 20px; 
background: #1a1a1a; border-radius: 8px; margin: 15px 0;'>
<b>w%<sub>i</sub> = (w<sub>i</sub> / Σw<sub>i</sub>) × 100</b>
</div>
<p><b>Donde:</b></p>
<ul>
<li><b>w%<sub>i</sub>:</b> Contribución porcentual del estudio i</li>
<li><b>w<sub>i</sub>:</b> Peso del estudio i</li>
<li><b>Σw<sub>i</sub>:</b> Suma de todos los pesos</li>
</ul>
<p style='margin-top: 15px;'><b>Propiedad:</b> Σw%<sub>i</sub> = 100%</p>
<p><b>Utilidad:</b> Facilita la interpretación de la influencia relativa de 
cada estudio en términos porcentuales.</p>
</div>

<!-- Nota final -->
<div style='margin-top: 40px; padding: 25px; background: #000000; 
border: 3px solid #00ff00; border-radius: 12px; text-align: center;'>
<h3 style='color: #00ff00; margin-bottom: 15px;'>
<b> NOTA IMPORTANTE</b>
</h3>
<p style='font-size: 15px;'>
Todas estas fórmulas trabajan en conjunto para proporcionar una <b>síntesis 
estadísticamente rigurosa</b> de múltiples estudios. El meta-análisis de 
efectos fijos es apropiado cuando los estudios son <b>homogéneos</b> 
(Test Q no significativo, I² bajo).
</p>
<p style='font-size: 15px; margin-top: 15px;'>
Para heterogeneidad sustancial, considere <b>modelos de efectos aleatorios</b> 
o <b>análisis de subgrupos</b>.
</p>
</div>
</div>
")
            )
          )
        )
      ),
      
      # ========== PESTAÑA: AYUDA ==========
      tabItem(
        tabName = "ayuda",
        
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("question-circle"), " Guía de Uso de la Aplicación"),
            status = "primary",
            solidHeader = TRUE,
            
            tags$div(
              style = "font-size: 16px; line-height: 1.9; color: #00ff00; padding: 20px;",
              HTML("
<div style='background: #1a1a1a; padding: 25px; border-radius: 12px; border: 2px solid #00ff00;'>
<h2 style='text-align: center; color: #00ff00; border-bottom: 3px solid #00ff00; 
padding-bottom: 15px; margin-bottom: 30px;'>
<b> GUÍA COMPLETA DE USO</b>
</h2>

<div style='margin: 25px 0; padding: 20px; background: #000000; border-radius: 10px;'>
<h3 style='color: #00ff00;'><b>Paso 1: Preparación de Datos</b></h3>
<ol>
<li>Prepare un archivo Excel (.xlsx) o CSV (.csv) con sus datos</li>
<li>Incluya dos columnas obligatorias:
<ul>
<li><b>'n':</b> Tamaño de muestra (enteros positivos)</li>
<li><b>'p':</b> Proporción observada (valores entre 0 y 1, exclusivo)</li>
</ul>
</li>
<li>Asegúrese de que NO haya valores faltantes (NA)</li>
<li>Verifique que todos los valores de 'p' estén entre 0 y 1 (sin incluir 0 y 1)</li>
</ol>
</div>

<div style='margin: 25px 0; padding: 20px; background: #000000; border-radius: 10px;'>
<h3 style='color: #00ff00;'><b>Paso 2: Cargar Datos</b></h3>
<ol>
<li>Vaya a la pestaña <b>' Datos'</b></li>
<li>Haga clic en <b>'Examinar...'</b> para seleccionar su archivo</li>
<li>Los datos se cargarán automáticamente en la tabla</li>
<li>Verifique que los datos se hayan cargado correctamente</li>
<li>Revise las <b>'Estadísticas de Datos'</b> y el <b>'Estado de Validación'</b></li>
</ol>
</div>

<div style='margin: 25px 0; padding: 20px; background: #000000; border-radius: 10px;'>
<h3 style='color: #00ff00;'><b>Paso 3: Configurar Análisis</b></h3>
<ol>
<li>En el panel lateral izquierdo, localice el cuadro <b>'Configuración'</b></li>
<li>Ajuste el <b>Nivel de significancia (α)</b> si lo desea (por defecto: 0.05)</li>
<li>Este valor determina el criterio para el test de homogeneidad</li>
</ol>
</div>

<div style='margin: 25px 0; padding: 20px; background: #000000; border-radius: 10px;'>
<h3 style='color: #00ff00;'><b>Paso 4: Ejecutar Meta-Análisis</b></h3>
<ol>
<li>Presione el botón verde <b>' CALCULAR META-ANÁLISIS'</b></li>
<li>El sistema procesará sus datos y calculará todos los estadísticos</li>
<li>Espere unos segundos mientras se completan los cálculos</li>
<li>Los resultados aparecerán en todas las pestañas relevantes</li>
</ol>
</div>

<div style='margin: 25px 0; padding: 20px; background: #000000; border-radius: 10px;'>
<h3 style='color: #00ff00;'><b>Paso 5: Explorar Resultados</b></h3>
<p><b>Navegue por las diferentes pestañas para analizar los resultados:</b></p>
<ul>
<li><b> Cálculos:</b> Tabla detallada con todos los cálculos intermedios</li>
<li><b> Resultados:</b> Resumen de métricas principales y interpretación</li>
<li><b> Forest Plot:</b> Visualización gráfica del meta-análisis</li>
<li><b> Test Q:</b> Análisis detallado del test de homogeneidad</li>
<li><b> Fórmulas:</b> Referencia matemática de todas las fórmulas</li>
<li><b> Teoría:</b> Marco teórico completo del meta-análisis</li>
</ul>
</div>

<div style='margin: 25px 0; padding: 20px; background: #000000; border-radius: 10px;'>
<h3 style='color: #00ff00;'><b>Paso 6: Descargar Resultados</b></h3>
<ol>
<li>En las pestañas <b>'Cálculos'</b> y <b>'Resultados'</b>, encontrará botones 
de descarga</li>
<li>Haga clic en <b>' Descargar Cálculos (CSV)'</b> para obtener la tabla 
detallada</li>
<li>Haga clic en <b>' Descargar Resultados (CSV)'</b> para obtener el 
resumen de estadísticos</li>
<li>Los archivos se guardarán en su carpeta de descargas</li>
</ol>
</div>

<div style='margin: 30px 0; padding: 25px; background: #000000; 
border: 3px solid #00ff00; border-radius: 12px;'>
<h3 style='color: #00ff00; text-align: center; margin-bottom: 20px;'>
<b> SOLUCIÓN DE PROBLEMAS COMUNES</b>
</h3>

<div style='margin: 15px 0;'>
<h4 style='color: #00ff00;'><b>Error: \"Los valores de p deben estar entre 0 y 1\"</b></h4>
<p><b>Solución:</b> Asegúrese de que todas las proporciones sean estrictamente 
mayores que 0 y menores que 1. Si tiene proporciones de 0% o 100%, considere 
agregar una corrección de continuidad (ej: 0.5/n en lugar de 0).</p>
</div>

<div style='margin: 15px 0;'>
<h4 style='color: #00ff00;'><b>Error: \"El tamaño de muestra debe ser mayor que 0\"</b></h4>
<p><b>Solución:</b> Verifique que todos los valores en la columna 'n' sean 
números enteros positivos. No puede haber tamaños de muestra iguales a 0 o negativos.</p>
</div>

<div style='margin: 15px 0;'>
<h4 style='color: #00ff00;'><b>Error: \"Valores faltantes (NA)\"</b></h4>
<p><b>Solución:</b> Revise su archivo de datos y elimine o complete todas las 
celdas vacías. El meta-análisis requiere datos completos para todos los estudios.</p>
</div>

<div style='margin: 15px 0;'>
<h4 style='color: #00ff00;'><b>No aparecen los resultados después de calcular</b></h4>
<p><b>Solución:</b> Verifique que haya presionado el botón 'CALCULAR META-ANÁLISIS'. 
Si el problema persiste, intente recargar la aplicación y volver a cargar los datos.</p>
</div>
</div>

<div style='margin: 30px 0; padding: 25px; background: #000000; 
border: 2px solid #00ff00; border-radius: 12px; text-align: center;'>
<h3 style='color: #00ff00; margin-bottom: 15px;'>
<b> SOPORTE ADICIONAL</b>
</h3>
<p>Para soporte técnico o preguntas sobre el uso de esta aplicación:</p>
<p><b>Universidad Nacional del Altiplano - Puno</b></p>
<p>Web: <a href='https://www.unap.edu.pe' target='_blank' 
style='color: #00ff00;'>www.unap.edu.pe</a></p>
</div>
</div>
")
            )
          )
        )
      )
    )
  )
)

# ----------------------- SERVIDOR -----------------------------------------

server <- function(input, output, session) {
  
  # Objeto reactivo para almacenar datos y resultados
  datos_reactive <- reactiveValues(
    datos = NULL,
    resultados = NULL
  )
  
  # ========== CARGAR DATOS (desde sidebar o pestaña datos) ==========
  cargar_datos <- function(archivo_input) {
    req(archivo_input)
    
    tryCatch({
      # Leer el archivo según su extensión
      if (grepl("\\.xlsx$|\\.xls$", archivo_input$name, ignore.case = TRUE)) {
        datos <- read_excel(archivo_input$datapath)
      } else if (grepl("\\.csv$", archivo_input$name, ignore.case = TRUE)) {
        datos <- read.csv(archivo_input$datapath)
      } else {
        showNotification(
          "Formato de archivo no compatible. Use .xlsx o .csv",
          type = "error",
          duration = 5
        )
        return(NULL)
      }
      
      # Validar columnas requeridas
      if (!all(c("n", "p") %in% names(datos))) {
        showNotification(
          "El archivo debe contener las columnas 'n' y 'p'",
          type = "error",
          duration = 5
        )
        return(NULL)
      }
      
      # Guardar datos
      datos_reactive$datos <- datos
      
      showNotification(
        " Datos cargados exitosamente",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error al cargar el archivo:", e$message),
        type = "error",
        duration = 5
      )
    })
  }
  
  # Observar carga desde sidebar
  observeEvent(input$archivo_sidebar, {
    cargar_datos(input$archivo_sidebar)
  })
  
  # Observar carga desde pestaña Datos
  observeEvent(input$archivo, {
    cargar_datos(input$archivo)
  })
  
  # ========== CARGAR DATOS MANUALMENTE ==========
  observeEvent(input$cargar_manual, {
    req(input$datos_manuales_n, input$datos_manuales_p)
    
    tryCatch({
      # Limpiar y parsear los datos de n
      n_text <- gsub("\\s+", "", input$datos_manuales_n) # Eliminar espacios
      n_values <- as.numeric(unlist(strsplit(n_text, ",")))
      
      # Limpiar y parsear los datos de p
      p_text <- gsub("\\s+", "", input$datos_manuales_p) # Eliminar espacios
      p_values <- as.numeric(unlist(strsplit(p_text, ",")))
      
      # Validar que ambos tengan la misma longitud
      if (length(n_values) != length(p_values)) {
        showNotification(
          paste0("Error: Número diferente de valores. ",
                 "n tiene ", length(n_values), " valores y ",
                 "p tiene ", length(p_values), " valores."),
          type = "error",
          duration = 5
        )
        return(NULL)
      }
      
      # Validar que no haya NAs
      if (any(is.na(n_values)) || any(is.na(p_values))) {
        showNotification(
          "Error: Algunos valores no son números válidos. Verifique el formato.",
          type = "error",
          duration = 5
        )
        return(NULL)
      }
      
      # Crear dataframe
      datos <- data.frame(
        n = n_values,
        p = p_values
      )
      
      # Guardar datos
      datos_reactive$datos <- datos
      
      showNotification(
        paste0(" Datos manuales cargados exitosamente: ", 
               nrow(datos), " estudios"),
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error al procesar datos manuales:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # ========== TABLA DE DATOS ==========
  output$tabla_datos <- renderDT({
    req(datos_reactive$datos)
    
    datatable(
      datos_reactive$datos,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
  })
  
  # ========== ESTADÍSTICAS DE DATOS ==========
  output$stats_datos <- renderUI({
    req(datos_reactive$datos)
    datos <- datos_reactive$datos
    
    tags$div(
      style = "font-size: 15px; color: #00ff00;",
      HTML(sprintf("
<p><b>Número de estudios:</b> %d</p>
<p><b>Tamaño de muestra total:</b> %d</p>
<p><b>Rango de n:</b> [%d, %d]</p>
<p><b>Rango de p:</b> [%.4f, %.4f]</p>
",
                   nrow(datos),
                   sum(datos$n),
                   min(datos$n),
                   max(datos$n),
                   min(datos$p),
                   max(datos$p)
      ))
    )
  })
  
  # ========== VALIDACIÓN DE DATOS ==========
  output$validacion_datos <- renderUI({
    req(datos_reactive$datos)
    datos <- datos_reactive$datos
    
    resultado_validacion <- validar_datos(datos)
    
    if (isTRUE(resultado_validacion)) {
      tags$div(
        style = "color: #00ff00; font-size: 15px;",
        HTML("
<p><b> Estado: VÁLIDO</b></p>
<p>Los datos cumplen con todos los requisitos.</p>
<p>Puede proceder con el análisis.</p>
")
      )
    } else {
      tags$div(
        style = "color: #ff0000; font-size: 15px;",
        HTML(sprintf("
<p><b> Estado: ERROR</b></p>
<p>%s</p>
", resultado_validacion))
      )
    }
  })
  
  # ========== CALCULAR META-ANÁLISIS ==========
  observeEvent(input$calcular, {
    req(datos_reactive$datos)
    
    # Validar datos
    validacion <- validar_datos(datos_reactive$datos)
    if (!isTRUE(validacion)) {
      showNotification(validacion, type = "error", duration = 5)
      return(NULL)
    }
    
    tryCatch({
      datos <- datos_reactive$datos
      alpha <- input$alpha
      
      # Añadir identificador de estudio
      datos$estudio <- paste0("Estudio_", 1:nrow(datos))
      
      # Calcular transformaciones y pesos
      datos$logit_p <- calcular_logit(datos$p)
      datos$varianza <- calcular_varianza(datos$n, datos$p)
      datos$peso <- calcular_pesos(datos$varianza)
      datos$peso_porcentaje <- (datos$peso / sum(datos$peso)) * 100
      
      # Calcular EFP
      theta_EF <- calcular_efp(datos$peso, datos$logit_p)
      T_EF <- logit_a_probabilidad(theta_EF)
      
      # Error estándar
      se_theta_EF <- 1 / sqrt(sum(datos$peso))
      
      # Test Q
      test_q <- calcular_test_q(datos$peso, datos$logit_p, theta_EF)
      datos$Q_i <- test_q$Q_i
      
      # I²
      I2 <- calcular_i_cuadrado(test_q$Q, test_q$df)
      
      # Intervalos de confianza
      z_critico <- qnorm(1 - alpha/2)
      ic_lower_logit <- theta_EF - z_critico * se_theta_EF
      ic_upper_logit <- theta_EF + z_critico * se_theta_EF
      ic_lower_prop <- logit_a_probabilidad(ic_lower_logit)
      ic_upper_prop <- logit_a_probabilidad(ic_upper_logit)
      
      # Guardar resultados
      datos_reactive$resultados <- list(
        df = datos,
        theta_EF = theta_EF,
        T_EF = T_EF,
        se_theta_EF = se_theta_EF,
        ic_lower_logit = ic_lower_logit,
        ic_upper_logit = ic_upper_logit,
        ic_lower_prop = ic_lower_prop,
        ic_upper_prop = ic_upper_prop,
        Q = test_q$Q,
        df_Q = test_q$df,
        p_value_Q = test_q$p_value,
        I2 = I2,
        k = nrow(datos),
        n_total = sum(datos$n),
        alpha = alpha
      )
      
      showNotification(
        " Meta-análisis calculado exitosamente",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error en el cálculo:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # ========== TABLA DE CÁLCULOS ==========
  output$tabla_calculos <- renderDT({
    req(datos_reactive$resultados)
    df <- datos_reactive$resultados$df
    
    df_mostrar <- df %>%
      select(estudio, n, p, logit_p, varianza, peso, peso_porcentaje, Q_i) %>%
      mutate(
        logit_p = round(logit_p, 4),
        varianza = round(varianza, 6),
        peso = round(peso, 4),
        peso_porcentaje = round(peso_porcentaje, 2),
        Q_i = round(Q_i, 4)
      )
    
    datatable(
      df_mostrar,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c(
        "Estudio", "n", "p", "logit(p)",
        "Varianza", "Peso (w)", "Peso (%)", "Q_i"
      )
    )
  })
  
  # ========== VALUE BOXES ==========
  output$vbox_theta <- renderValueBox({
    req(datos_reactive$resultados)
    valueBox(
      value = round(datos_reactive$resultados$theta_EF, 4),
      subtitle = "θ_EF (Escala Logit)",
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  output$vbox_t_ef <- renderValueBox({
    req(datos_reactive$resultados)
    valueBox(
      value = sprintf("%.4f", datos_reactive$resultados$T_EF),
      subtitle = "T_EF (Proporción)",
      icon = icon("percentage"),
      color = "green"
    )
  })
  
  output$vbox_k <- renderValueBox({
    req(datos_reactive$resultados)
    valueBox(
      value = datos_reactive$resultados$k,
      subtitle = "Número de Estudios (k)",
      icon = icon("list"),
      color = "green"
    )
  })
  
  output$vbox_n_total <- renderValueBox({
    req(datos_reactive$resultados)
    valueBox(
      value = format(datos_reactive$resultados$n_total, big.mark = ","),
      subtitle = "Tamaño Total de Muestra",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$vbox_q <- renderValueBox({
    req(datos_reactive$resultados)
    valueBox(
      value = round(datos_reactive$resultados$Q, 2),
      subtitle = "Estadístico Q",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$vbox_pvalue <- renderValueBox({
    req(datos_reactive$resultados)
    res <- datos_reactive$resultados
    p_val <- format.pval(res$p_value_Q, digits = 4)
    color_box <- ifelse(res$p_value_Q < res$alpha, "red", "green")
    
    valueBox(
      value = p_val,
      subtitle = "p-value (Test Q)",
      icon = icon("vial"),
      color = color_box
    )
  })
  
  output$vbox_i2 <- renderValueBox({
    req(datos_reactive$resultados)
    i2_val <- datos_reactive$resultados$I2
    
    color_box <- if(i2_val < 25) {
      "green"
    } else if(i2_val < 50) {
      "yellow"
    } else if(i2_val < 75) {
      "orange"
    } else {
      "red"
    }
    
    valueBox(
      value = sprintf("%.1f%%", i2_val),
      subtitle = "Índice I²",
      icon = icon("percentage"),
      color = color_box
    )
  })
  
  output$vbox_decision <- renderValueBox({
    req(datos_reactive$resultados)
    res <- datos_reactive$resultados
    
    if(res$p_value_Q < res$alpha) {
      valueBox(
        value = "HETEROGÉNEO",
        subtitle = "Conclusión Test Q",
        icon = icon("times-circle"),
        color = "red"
      )
    } else {
      valueBox(
        value = "HOMOGÉNEO",
        subtitle = "Conclusión Test Q",
        icon = icon("check-circle"),
        color = "green"
      )
    }
  })
  
  # ========== RESUMEN EN CONSOLA ==========
  output$resumen_consola <- renderText({
    req(datos_reactive$resultados)
    res <- datos_reactive$resultados
    
    paste0(
      "\n",
      " RESUMEN DEL META-ANÁLISIS - EFECTO FIJO PROMEDIO (EFP) \n",
      "\n",
      sprintf(" Número de estudios (k): %-30d \n", res$k),
      sprintf(" Tamaño total de muestra (N): %-30d \n", res$n_total),
      sprintf(" Nivel de significancia (α): %-30.4f \n", res$alpha),
      "\n",
      " EFECTO FIJO PROMEDIO \n",
      "\n",
      sprintf(" θ_EF (Escala logit): %-30.6f \n", res$theta_EF),
      sprintf(" T_EF (Proporción): %-30.6f \n", res$T_EF),
      sprintf(" Error Estándar SE(θ_EF): %-30.6f \n", res$se_theta_EF),
      " \n",
      sprintf(" IC 95%% (Escala logit): [%.4f, %.4f] \n",
              res$ic_lower_logit, res$ic_upper_logit),
      sprintf(" IC 95%% (Proporción): [%.4f, %.4f] \n",
              res$ic_lower_prop, res$ic_upper_prop),
      "\n",
      " TEST DE HOMOGENEIDAD (Q) \n",
      "\n",
      sprintf(" Estadístico Q: %-30.4f \n", res$Q),
      sprintf(" Grados de libertad (df): %-30d \n", res$df_Q),
      sprintf(" p-value: %-30s \n",
              format.pval(res$p_value_Q, digits = 6)),
      " \n",
      ifelse(res$p_value_Q < res$alpha,
             paste0(" DECISIÓN: Se rechaza H₀ → HETEROGENEIDAD SIGNIFICATIVA \n",
                    " Los estudios NO son homogéneos. \n",
                    " Considere modelo de efectos aleatorios. \n"),
             paste0(" DECISIÓN: NO se rechaza H₀ → HOMOGENEIDAD \n",
                    " Los estudios son homogéneos. \n",
                    " El modelo de efectos fijos es apropiado. \n")
      ),
      "\n",
      " ÍNDICE I² DE HETEROGENEIDAD \n",
      "\n",
      sprintf(" I²: %-28.2f%% \n", res$I2),
      " \n",
      ifelse(res$I2 < 25,
             " Interpretación: Heterogeneidad BAJA o NULA \n",
             ifelse(res$I2 < 50,
                    " Interpretación: Heterogeneidad MODERADA \n",
                    ifelse(res$I2 < 75,
                           " Interpretación: Heterogeneidad SUSTANCIAL \n",
                           " Interpretación: Heterogeneidad ALTA \n")
             )
      ),
      "\n"
    )
  })
  
  # ========== INTERVALOS DE CONFIANZA ==========
  output$intervalos_confianza <- renderUI({
    req(datos_reactive$resultados)
    res <- datos_reactive$resultados
    
    tags$div(
      style = "font-size: 15px; color: #00ff00; line-height: 2;",
      HTML(sprintf("
<p><b>Escala Logit:</b></p>
<p>IC 95%% = [%.4f, %.4f]</p>
<hr style='border-color: #00ff00;'>
<p><b>Escala de Proporción:</b></p>
<p>IC 95%% = [%.4f, %.4f]</p>
<hr style='border-color: #00ff00;'>
<p><b>Amplitud del IC:</b> %.4f</p>
",
                   res$ic_lower_logit,
                   res$ic_upper_logit,
                   res$ic_lower_prop,
                   res$ic_upper_prop,
                   res$ic_upper_prop - res$ic_lower_prop
      ))
    )
  })
  
  # ========== GRÁFICO DE DISTRIBUCIÓN DE PESOS ==========
  output$plot_pesos <- renderPlot({
    req(datos_reactive$resultados)
    df <- datos_reactive$resultados$df
    
    ggplot(df, aes(x = reorder(estudio, -peso_porcentaje), y = peso_porcentaje)) +
      geom_col(fill = "#00ff00", color = "#000000", width = 0.7) +
      geom_text(
        aes(label = sprintf("%.1f%%", peso_porcentaje)),
        vjust = -0.5,
        color = "#00ff00",
        size = 4,
        fontface = "bold"
      ) +
      labs(
        title = "Distribución de Pesos por Estudio",
        x = "",
        y = "Peso (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 16,
          color = "#00ff00"
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          color = "#00ff00"
        ),
        axis.text.y = element_text(color = "#00ff00"),
        axis.title = element_text(color = "#00ff00", face = "bold"),
        panel.grid.major = element_line(color = "#1a1a1a"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#000000", color = NA),
        panel.background = element_rect(fill = "#000000", color = NA)
      )
  })
  
  # ========== FOREST PLOT ==========
  output$forest_plot <- renderPlot({
    req(datos_reactive$resultados)
    res <- datos_reactive$resultados
    df <- res$df
    
    # Calcular intervalos de confianza para cada estudio
    z_crit <- qnorm(1 - res$alpha/2)
    df$ic_lower <- logit_a_probabilidad(df$logit_p - z_crit * sqrt(df$varianza))
    df$ic_upper <- logit_a_probabilidad(df$logit_p + z_crit * sqrt(df$varianza))
    
    # Crear gráfico
    p <- ggplot(df, aes(y = reorder(estudio, p), x = p)) +
      # Intervalos de confianza
      geom_errorbarh(
        aes(xmin = ic_lower, xmax = ic_upper),
        height = 0.3,
        color = "#00ff00",
        size = 1
      ) +
      # Puntos de estimación
      geom_point(
        aes(size = peso),
        color = "#00ff00",
        fill = "#000000",
        shape = 22,
        stroke = 2
      ) +
      # Línea de referencia
      geom_vline(
        xintercept = res$T_EF,
        linetype = "dashed",
        color = "#00ff00",
        size = 1
      ) +
      # Diamante del efecto combinado
      annotate(
        "segment",
        x = res$ic_lower_prop,
        xend = res$ic_upper_prop,
        y = 0.5,
        yend = 0.5,
        color = "#00ff00",
        size = 2,
        lineend = "round"
      ) +
      annotate(
        "point",
        x = res$T_EF,
        y = 0.5,
        color = "#00ff00",
        fill = "#000000",
        shape = 23,
        size = 8,
        stroke = 2
      ) +
      # Etiqueta del diamante
      annotate(
        "text",
        x = res$T_EF,
        y = 0.2,
        label = sprintf("EFP = %.4f", res$T_EF),
        color = "#00ff00",
        size = 5,
        fontface = "bold"
      ) +
      # Escalas y etiquetas
      scale_size_continuous(range = c(3, 12), guide = "none") +
      scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = scales::pretty_breaks(n = 10)
      ) +
      labs(
        title = "Forest Plot - Meta-Análisis de Efectos Fijos",
        subtitle = sprintf(
          "k = %d estudios | N = %d | I² = %.1f%% | p-value(Q) = %s",
          res$k,
          res$n_total,
          res$I2,
          format.pval(res$p_value_Q, digits = 4)
        ),
        x = "Proporción",
        y = "",
        caption = sprintf(
          "Cuadrados = estimaciones puntuales (tamaño ∝ peso) | Líneas = IC 95%% | Diamante = Efecto Fijo Promedio\nLínea punteada = T_EF (%.4f) | %s",
          res$T_EF,
          ifelse(res$p_value_Q < res$alpha,
                 " Heterogeneidad Significativa",
                 " Homogeneidad")
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 20,
          color = "#00ff00"
        ),
        plot.subtitle = element_text(
          hjust = 0.5,
          size = 14,
          color = "#00ff00",
          margin = margin(b = 20)
        ),
        plot.caption = element_text(
          hjust = 0.5,
          size = 11,
          color = "#00ff00",
          margin = margin(t = 15)
        ),
        axis.title.x = element_text(
          size = 15,
          face = "bold",
          color = "#00ff00",
          margin = margin(t = 15)
        ),
        axis.text = element_text(
          size = 13,
          color = "#00ff00"
        ),
        axis.text.y = element_text(
          face = "bold"
        ),
        panel.grid.major.x = element_line(color = "#1a1a1a", size = 0.5),
        panel.grid.major.y = element_line(color = "#1a1a1a", size = 0.3),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#000000", color = NA),
        panel.background = element_rect(fill = "#000000", color = NA),
        plot.margin = margin(25, 25, 25, 25)
      )
    
    print(p)
  }, width = 1200, height = 700)
  
  # ========== TEST Q DETALLE ==========
  output$test_q_detalle <- renderText({
    req(datos_reactive$resultados)
    res <- datos_reactive$resultados
    
    paste0(
      "\n",
      " TEST DE HOMOGENEIDAD Q - ANÁLISIS COMPLETO \n",
      "\n",
      " \n",
      " HIPÓTESIS: \n",
      " H₀: θ₁ = θ₂ = ... = θₖ (Los estudios son homogéneos) \n",
      " H₁: Al menos un θᵢ ≠ θ (Existe heterogeneidad) \n",
      " \n",
      "\n",
      " ESTADÍSTICOS PRINCIPALES: \n",
      "\n",
      sprintf(" Estadístico Q observado: %-30.6f \n", res$Q),
      sprintf(" Grados de libertad (df = k-1): %-30d \n", res$df_Q),
      sprintf(" p-value: %-30s \n",
              format.pval(res$p_value_Q, digits = 8)),
      sprintf(" Nivel de significancia (α): %-30.4f \n", res$alpha),
      sprintf(" Valor crítico χ²(%.2f%%): %-30.4f \n",
              (1-res$alpha)*100, qchisq(1-res$alpha, res$df_Q)),
      " \n",
      "\n",
      " DECISIÓN ESTADÍSTICA: \n",
      "\n",
      ifelse(res$p_value_Q < res$alpha,
             paste0(
               sprintf(" p-value (%.6f) < α (%.4f) \n",
                       res$p_value_Q, res$alpha),
               " \n",
               " DECISIÓN: Se rechaza H₀ \n",
               " \n",
               " Conclusión principal: \n",
               " → Existe heterogeneidad SIGNIFICATIVA entre los estudios \n",
               " → Los estudios NO son homogéneos \n",
               " \n",
               " Implicaciones: \n",
               " → Los efectos de los estudios NO son homogéneos \n",
               " → Las diferencias observadas NO se deben solo al azar \n",
               " → Hay variabilidad real entre los estudios \n",
               " \n",
               " Recomendaciones: \n",
               " → Investigar las fuentes de heterogeneidad \n",
               " → Considerar análisis de subgrupos \n",
               " → Evaluar modelo de efectos aleatorios \n",
               " → Realizar análisis de sensibilidad \n"
             ),
             paste0(
               sprintf(" p-value (%.6f) ≥ α (%.4f) \n",
                       res$p_value_Q, res$alpha),
               " \n",
               " DECISIÓN: NO se rechaza H₀ \n",
               " \n",
               " Conclusión principal: \n",
               " → NO existe heterogeneidad significativa \n",
               " → Los estudios son HOMOGÉNEOS \n",
               " \n",
               " Implicaciones: \n",
               " → Los efectos de los estudios son consistentes \n",
               " → Las diferencias observadas se explican por azar \n",
               " → El modelo de efectos fijos es apropiado \n",
               " \n",
               " Recomendaciones: \n",
               " → El meta-análisis de efectos fijos es válido \n",
               " → Los resultados combinados son confiables \n",
               " → Proceder con la interpretación del EFP \n"
             )
      ),
      "\n\n",
      
      "\n",
      " FÓRMULA UTILIZADA \n",
      "\n",
      " Q = Σ[wi × (logit(Pi) - θ_EF)²] \n",
      " \n",
      " Donde: \n",
      " • wi = Peso del estudio i \n",
      " • logit(Pi) = Logit transformado del estudio i \n",
      " • θ_EF = Efecto fijo promedio \n",
      " • Σ = Suma sobre todos los estudios \n",
      " \n",
      " Distribución: Q ~ χ²(k-1) bajo H₀ \n",
      "\n"
    )
  })
  
  # ========== GRÁFICO CHI-CUADRADO PROFESIONAL ==========
  output$plot_q <- renderPlot({
    req(datos_reactive$resultados)
    res <- datos_reactive$resultados
    
    # Preparar datos para el gráfico
    x_max <- max(res$Q + 5, qchisq(0.995, res$df_Q))
    x <- seq(0, x_max, length.out = 1500)
    y <- dchisq(x, res$df_Q)
    
    df_chi <- data.frame(x = x, y = y)
    valor_critico <- qchisq(1 - res$alpha, res$df_Q)
    
    # Crear el gráfico
    p <- ggplot(df_chi, aes(x = x, y = y)) +
      # Área bajo la curva (región de no rechazo)
      geom_area(
        data = subset(df_chi, x < valor_critico),
        aes(x = x, y = y),
        fill = "#00ff00",
        alpha = 0.3
      ) +
      # Área de rechazo
      geom_area(
        data = subset(df_chi, x >= valor_critico),
        aes(x = x, y = y),
        fill = "#ff0000",
        alpha = 0.4
      ) +
      # Curva principal
      geom_line(size = 1.8, color = "#00ff00") +
      # Línea del Q observado
      geom_vline(
        xintercept = res$Q,
        linetype = "solid",
        color = "#ff0000",
        size = 1.8
      ) +
      # Línea del valor crítico
      geom_vline(
        xintercept = valor_critico,
        linetype = "dashed",
        color = "#00ff00",
        size = 1.3
      ) +
      # Anotación del Q observado
      annotate(
        "label",
        x = res$Q,
        y = max(y) * 0.75,
        label = sprintf(
          "Q observado = %.2f\np-value = %s",
          res$Q,
          format.pval(res$p_value_Q, digits = 4)
        ),
        hjust = ifelse(res$Q < x_max * 0.7, -0.1, 1.1),
        color = "#00ff00",
        size = 5,
        fontface = "bold",
        fill = "#000000",
        label.padding = unit(0.5, "lines"),
        label.size = 1.5
      ) +
      # Anotación del valor crítico
      annotate(
        "label",
        x = valor_critico,
        y = max(y) * 0.45,
        label = sprintf(
          "Valor crítico\nχ²(%.0f%%) = %.2f",
          (1-res$alpha)*100,
          valor_critico
        ),
        hjust = ifelse(valor_critico < x_max * 0.7, -0.1, 1.1),
        color = "#00ff00",
        size = 4.5,
        fontface = "bold",
        fill = "#000000",
        label.padding = unit(0.5, "lines"),
        label.size = 1.5
      ) +
      # Anotación de la región de rechazo
      annotate(
        "text",
        x = x_max * 0.85,
        y = max(y) * 0.15,
        label = "Región de\nRechazo H₀",
        color = "#ff0000",
        size = 5,
        fontface = "bold.italic"
      ) +
      # Etiquetas
      labs(
        title = sprintf("Distribución Chi-cuadrado (χ²) con df = %d", res$df_Q),
        subtitle = sprintf(
          "Test de Homogeneidad Q | α = %.4f | %s",
          res$alpha,
          ifelse(res$p_value_Q < res$alpha,
                 " Se rechaza H₀ (Heterogéneo)",
                 " No se rechaza H₀ (Homogéneo)")
        ),
        x = "Valor del estadístico Q",
        y = "Densidad de probabilidad",
        caption = "Área verde = Región de no rechazo de H₀ | Área roja = Región de rechazo"
      ) +
      # Tema
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 19,
          color = "#00ff00",
          margin = margin(b = 10)
        ),
        plot.subtitle = element_text(
          hjust = 0.5,
          size = 13,
          color = ifelse(res$p_value_Q < res$alpha, "#ff0000", "#00ff00"),
          face = "bold",
          margin = margin(b = 15)
        ),
        plot.caption = element_text(
          hjust = 0.5,
          size = 11,
          color = "#00ff00",
          margin = margin(t = 15)
        ),
        axis.title = element_text(
          size = 13,
          face = "bold",
          color = "#00ff00"
        ),
        axis.text = element_text(
          size = 12,
          color = "#00ff00"
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#1a1a1a", size = 0.4),
        plot.margin = margin(25, 25, 25, 25),
        plot.background = element_rect(fill = "#000000", color = NA),
        panel.background = element_rect(fill = "#000000", color = NA)
      )
    
    print(p)
  }, width = 1200, height = 550)
  
  # ========== DESCARGAS ==========
  output$descargar_calculos <- downloadHandler(
    filename = function() {
      paste0("meta-analisis-calculos-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(datos_reactive$resultados)
      write.csv(datos_reactive$resultados$df, file, row.names = FALSE)
    }
  )
  
  output$descargar_resultados <- downloadHandler(
    filename = function() {
      paste0("meta-analisis-resultados-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(datos_reactive$resultados)
      res <- datos_reactive$resultados
      
      resultados_df <- data.frame(
        Estadistico = c("theta_EF", "T_EF", "SE_theta", "Q", "df", "p_value", "I2", "k"),
        Valor = c(
          res$theta_EF,
          res$T_EF,
          res$se_theta_EF,
          res$Q,
          res$df_Q,
          res$p_value_Q,
          res$I2,
          res$k
        )
      )
      
      write.csv(resultados_df, file, row.names = FALSE)
    }
  )
}

# ==============================================================================
# EJECUTAR LA APLICACIÓN
# ==============================================================================


shinyApp(ui = ui, server = server)
