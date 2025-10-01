library(ggplot2)   
library(GGally)   
library(plotly)   
#glc
genera_glc <- function(n=200, a=5, c=0, M=37, semilla=11){
  x <- numeric(n)
  x[1] <- semilla
  for(i in 2:n){
    x[i] <- (a * x[i-1] + c) %% M
  }
  x / M  
}
u_glc <- genera_glc(200)
datos_glc <- data.frame(iter = 1:200, u = u_glc)
u_glc
#Gráficas
g1_glc <- ggplot(datos_glc, aes(iter, u)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1) +
  labs(title = "GLC: serie temporal") +
  theme_minimal()
g1_glc
g2_glc <- ggplot(datos_glc, aes(u)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "GLC: histograma + densidad") +
  theme_minimal()
g2_glc
#QQ-plot
qqplot(qunif(ppoints(200)), u_glc,
       main = "GLC: QQ-plot vs U(0,1)",
       xlab = "Teóricos U(0,1)", ylab = "Generados", col = "blue")
abline(0, 1, col = "red", lwd = 2)
#Matriz de dispersión
lags_glc <- data.frame(u1 = u_glc[-c(199:200)],
                       u2 = u_glc[-c(1, 200)],
                       u3 = u_glc[-c(1:2)])
ggpairs(lags_glc, title = "GLC: matriz de dispersión")
#Gráfica3d
plot_ly(lags_glc, x = ~u1, y = ~u2, z = ~u3,
        type = "scatter3d", mode = "markers",
        marker = list(size = 3, color = "purple")) %>%
  layout(title = "GLC: dispersión 3D")
#mt
genera_mt <- function(n=200, semilla=123){
  set.seed(semilla, kind = "Mersenne-Twister")
  runif(n)
}
u_mt <- genera_mt(200)
datos_mt <- data.frame(iter = 1:200, u = u_mt)
u_mt

#Gráficas
g1_mt <- ggplot(datos_mt, aes(iter, u)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "orange", size = 1) +
  labs(title = "MT: serie temporal") +
  theme_minimal()
g1_mt
g2_mt <- ggplot(datos_mt, aes(u)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "lightgreen", color = "black") +
  geom_density(color = "red") +
  labs(title = "MT: histograma + densidad") +
  theme_minimal()
g2_mt
#QQ-plot
qqplot(qunif(ppoints(200)), u_mt,
       main = "MT: QQ-plot vs U(0,1)",
       xlab = "Teóricos U(0,1)", ylab = "Generados", col = "darkgreen")
abline(0, 1, col = "red", lwd = 2)
# Matriz de dispersión
lags_mt <- data.frame(u1 = u_mt[-c(199:200)],
                      u2 = u_mt[-c(1, 200)],
                      u3 = u_mt[-c(1:2)])
ggpairs(lags_mt, title = "MT: matriz de dispersión")
# Gráfica3d
plot_ly(lags_mt, x = ~u1, y = ~u2, z = ~u3,
        type = "scatter3d", mode = "markers",
        marker = list(size = 3, color = "blue")) %>%
  layout(title = "MT: dispersión 3D")

