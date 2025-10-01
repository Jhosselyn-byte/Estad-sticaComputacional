#temp_max

Temp_Max = c(15.2, 16.5, 14.8, 17.0, 16.0, 15.5, 18.0, 17.2, 15.0, 16.8, 16.0, 17.5, 15.8, 18.5, 16.3, 17.8, 15.5, 16.2, 17.1, 18.2, 17.5, 16.8, 18.0, 19.5, 18.8, 17.0, 19.0, 18.2, 17.7, 19.2, 18.5, 17.9, 19.8, 19.0, 18.3, 17.0, 20.0, 19.5, 18.8, 17.5, 20.5, 19.7, 19.2, 18.0, 21.0, 20.2, 19.5, 18.5, 20.8, 20.1, 19.6, 18.8, 21.5, 20.7, 20.0, 19.0, 22.0, 21.2, 20.5, 19.5, 21.8, 21.0, 20.3, 19.3, 22.5, 21.7, 20.9, 20.0, 23.0, 22.2, 21.4, 20.5, 23.5, 22.7, 21.9, 21.0, 24.0, 23.2, 22.4, 21.5, 23.8, 23.0, 22.2, 21.3, 24.5, 23.7, 22.9, 22.0, 25.0, 24.2, 23.4, 22.5, 25.5, 24.7, 23.9, 23.0, 26.0, 25.2, 24.4, 23.5, 25.8, 25.0, 24.2, 23.3, 26.5, 25.7, 24.9, 24.0, 27.0, 26.2, 25.4, 24.5, 27.5, 26.7, 25.9, 25.0, 28.0, 27.2, 26.4, 25.5)

sTemp_Max <- sum(Temp_Max)
sTemp_Max
p <- sTemp_Max / length(Temp_Max)
p
mean(Temp_Max)
sd(Temp_Max)
cv = sd(Temp_Max)/mean(Temp_Max)
cv*100

hist(Temp_Max,
     main = "Distribución de la Temperatura Máxima",
     xlab = "Temperatura (°C)",
     ylab = "Frecuencia",
     col = 'lightblue')


plot(Temp_Max,
     main = "Serie de Tiempo de la Temperatura Máxima",
     xlab = "Días",
     ylab = "Temperatura (°C)",
     col = 'darkblue')

#temp_min

Temp_Min = c(4.1, 5.0, 3.5, 6.2, 4.8, 4.0, 6.5, 5.5, 3.0, 5.3, 4.5, 6.0, 4.2, 7.0, 5.0, 6.8, 3.8, 4.7, 5.9, 6.4, 5.8, 4.5, 6.1, 7.5, 7.0, 5.2, 7.2, 6.3, 5.5, 7.8, 6.7, 5.9, 8.0, 7.3, 6.5, 4.9, 8.2, 7.9, 7.1, 5.8, 8.5, 8.1, 7.5, 6.3, 9.0, 8.8, 7.7, 6.8, 8.9, 8.4, 7.9, 7.0, 9.5, 9.1, 8.3, 7.4, 10.0, 9.6, 8.7, 7.8, 9.8, 9.4, 8.6, 7.7, 10.5, 10.1, 9.3, 8.5, 11.0, 10.6, 9.7, 8.9, 11.5, 11.1, 10.2, 9.2, 11.8, 11.4, 10.5, 9.6, 12.5, 12.1, 11.2, 10.3, 13.0, 12.6, 11.7, 10.8, 13.5, 13.1, 12.2, 11.3, 14.0, 13.6, 12.7, 11.8, 13.8, 13.4, 12.5, 11.6, 14.5, 14.1, 13.2, 12.3, 15.0, 14.6, 13.7, 12.8, 15.5, 15.1, 14.2, 13.3, 16.0, 15.6, 14.7, 13.8)
sTemp_Min <- sum(Temp_Min)
sTemp_Min
p <- sTemp_Min / length(Temp_Min)
p
mean(Temp_Min)
sd(Temp_Min)
cv = sd(Temp_Min)/mean(Temp_Min)
cv*100

hist(Temp_Min,
     main = "Distribución de la Temperatura Mínima",
     xlab = "Temperatura (°C)",
     ylab = "Frecuencia",
     col = 'red')


plot(Temp_Min,
     main = "Serie de Tiempo de la Temperatura Mínima",
     xlab = "Días",
     ylab = "Temperatura (°C)",
     col = 'darkred')

# precipitacion

Precipitacion = c(5.5, 2.1, 12.0, 8.4, 3.8, 7.1, 0.0, 1.5, 9.5, 2.3, 2.8, 3.2, 4.5, 3.7, 2.5, 2.2, 8.2, 3.0, 4.0, 6.5, 2.8, 3.1, 2.4, 2.4, 3.7, 5.8, 7.2, 3.2, 1.8, 5.3, 3.7, 2.5, 3.1, 2.1, 2.9, 4.2, 4.3, 4.6, 4.5, 1.5, 4.7, 2.9, 2.8, 3.0, 7.1, 2.4, 2.2, 2.1, 6.7, 3.3, 1.9, 1.8, 2.6, 5.3, 4.2, 2.5, 3.1, 7.5, 2.3, 1.5, 4.3, 5.6, 2.9, 1.2, 2.5, 3.9, 9.5, 3.2, 2.2, 4.4, 4.5, 5.5, 4.3, 4.4, 2.3, 1.1, 2.1, 3.1, 4.3, 5.1, 6.4, 2.9, 3.1, 2.5, 3.7, 1.1, 2.8, 3.1, 8.8, 4.3, 2.9, 3.1, 2.4, 2.7, 4.3, 1.5, 1.7, 1.9, 3.9, 5.5, 1.6, 6.2, 6.1, 3.4, 5.3, 5.1, 1.3, 2.7, 3.2, 2.9, 3.1, 4.2, 7.1, 5.7, 4.3, 2.7, 5.1, 2.2, 2.0, 3.9, 4.4)
sPrecipitacion <- sum(Precipitacion)
sPrecipitacion
p <- sPrecipitacion / length(Precipitacion)
p
mean(Precipitacion)
sd(Precipitacion)
cv = sd(Precipitacion)/mean(Precipitacion)
cv*100

hist(Precipitacion,
     main = "Distribución de la Precipitación",
     xlab = "Precipitación (mm)",
     ylab = "Frecuencia",
     col = 'blue')


plot(Precipitacion,
     main = "Serie de Tiempo de la Precipitación",
     xlab = "Días",
     ylab = "Precipitación (mm)",
     col = 'darkblue')

#humedad_relativa

Humedad_Relativa = c(78, 75, 85, 70, 80, 82, 68, 72, 88, 71, 74, 69, 81, 65, 76, 67, 84, 73, 70, 66, 72, 79, 70, 65, 68, 83, 66, 71, 77, 64, 69, 75, 63, 67, 70, 80, 62, 65, 68, 76, 60, 64, 67, 74, 58, 61, 65, 73, 59, 62, 66, 72, 57, 60, 64, 71, 55, 58, 62, 69, 56, 59, 63, 70, 54, 57, 61, 68, 52, 55, 59, 65, 50, 53, 57, 62, 49, 52, 56, 61, 46, 49, 54, 58, 44, 47, 52, 56, 42, 45, 50, 54, 40, 43, 48, 52, 41, 44, 49, 53, 38, 41, 46, 50, 36, 39, 44, 48, 35, 38, 43, 47, 34, 37, 42, 46)
sHumedad_Relativa <- sum(Humedad_Relativa)
sHumedad_Relativa
p <- sHumedad_Relativa / length(Humedad_Relativa)
p
mean(Humedad_Relativa)
sd(Humedad_Relativa)
cv = sd(Humedad_Relativa)/mean(Humedad_Relativa)
cv*100

hist(Humedad_Relativa,
     main = "Distribución de la Humedad Relativa",
     xlab = "Humedad Relativa (%)",
     ylab = "Frecuencia",
     col = 'green')


plot(Humedad_Relativa,
     main = "Serie de Tiempo de la Humedad Relativa",
     xlab = "Días",
     ylab = "Humedad Relativa (%)",
     col = 'darkgreen')

#velocidad_viento

Velocidad_Viento = c(12, 15, 10, 20, 14, 11, 18, 16, 9, 17, 13, 19, 11, 22, 15, 21, 10, 14, 18, 20, 16, 13, 18, 23, 21, 12, 20, 17, 14, 24, 19, 15, 25, 22, 18, 11, 26, 23, 20, 14, 28, 24, 21, 16, 30, 27, 22, 17, 29, 25, 23, 18, 31, 28, 24, 19, 33, 30, 26, 21, 32, 29, 25, 20, 35, 31, 27, 22, 38, 34, 29, 24, 40, 36, 31, 26, 41, 37, 31, 27, 45, 41, 35, 30, 48, 44, 38, 32, 50, 46, 40, 34, 52, 48, 42, 36, 51, 47, 41, 35, 55, 50, 44, 38, 58, 54, 48, 42, 60, 56, 50, 44, 62, 58, 52, 46)

sVelocidad_Viento <- sum(Velocidad_Viento)
sVelocidad_Viento
p <- sVelocidad_Viento / length(Velocidad_Viento)
p
mean(Velocidad_Viento)
sd(Velocidad_Viento)
cv = sd(Velocidad_Viento)/mean(Velocidad_Viento)
cv*100

hist(Velocidad_Viento,
     main = "Distribución de la Velocidad del Viento",
     xlab = "Velocidad (km/h)",
     ylab = "Frecuencia",
     col = 'orange')


plot(Velocidad_Viento,
     main = "Serie de Tiempo de la Velocidad del Viento",
     xlab = "Días",
     ylab = "Velocidad (km/h)",
     col = 'darkorange')

#radiacion_solar

Radiacion_Solar = c(250, 280, 200, 320, 240, 210, 350, 290, 180, 310, 300, 340, 230, 380, 270, 360, 190, 260, 330, 370, 300, 240, 350, 400, 380, 220, 370, 310, 250, 410, 360, 280, 420, 390, 340, 200, 450, 410, 370, 290, 480, 430, 390, 260, 500, 460, 410, 280, 490, 440, 400, 300, 520, 470, 420, 320, 550, 500, 450, 350, 530, 480, 430, 330, 560, 510, 460, 360, 580, 530, 480, 380, 600, 550, 500, 400, 610, 560, 510, 410, 650, 600, 550, 450, 680, 630, 580, 480, 700, 650, 600, 500, 720, 670, 620, 520, 710, 660, 610, 510, 750, 700, 650, 550, 780, 730, 680, 580, 800, 750, 700, 600, 820, 770, 670, 570)

sRadiacion_Solar <- sum(Radiacion_Solar)
sRadiacion_Solar
p <- sRadiacion_Solar / length(Radiacion_Solar)
p
mean(Radiacion_Solar)
sd(Radiacion_Solar)
cv = sd(Radiacion_Solar)/mean(Radiacion_Solar)
cv*100

hist(Radiacion_Solar,
     main = "Distribución de la Radiación Solar",
     xlab = "Radiación Solar (W/m²)",
     ylab = "Frecuencia",
     col = 'yellow', border = 'orange')


plot(Radiacion_Solar,
     main = "Serie de Tiempo de la Radiación Solar",
     xlab = "Días",
     ylab = "Radiación Solar (W/m²)",
     col = 'darkorange')

#horas_sol

Horas_Sol = c(4.5, 6.0, 3.0, 7.5, 5.0, 4.0, 8.0, 6.5, 2.5, 7.0, 6.0, 8.0, 4.5, 9.0, 5.5, 8.5, 3.5, 5.0, 7.0, 8.0, 6.5, 4.0, 7.5, 9.0, 8.5, 3.5, 8.0, 6.5, 5.0, 9.5, 8.0, 5.5, 10.0, 9.0, 7.5, 3.0, 10.5, 9.5, 8.0, 6.0, 11.0, 10.0, 9.0, 5.0, 11.5, 10.5, 9.0, 5.5, 11.0, 10.0, 8.5, 6.0, 12.0, 10.5, 9.5, 7.0, 12.5, 11.5, 10.0, 8.0, 12.0, 11.0, 9.5, 7.5, 13.0, 12.0, 10.5, 8.5, 13.5, 12.5, 11.0, 9.0, 14.0, 13.0, 11.5, 9.5, 14.0, 13.0, 11.5, 9.5, 15.0, 14.0, 12.5, 10.5, 15.5, 14.5, 13.0, 11.0, 16.0, 15.0, 13.5, 11.5, 16.5, 15.5, 14.0, 12.0, 16.0, 15.0, 13.5, 11.5, 17.0, 16.0, 14.5, 12.5, 17.5, 16.5, 15.0, 13.0, 18.0, 17.0, 15.5, 13.5, 18.5, 17.5, 16.0, 14.0)

sHoras_Sol <- sum(Horas_Sol)
sHoras_Sol
p <- sHoras_Sol / length(Horas_Sol)
p
mean(Horas_Sol)
sd(Horas_Sol)
cv = sd(Horas_Sol)/mean(Horas_Sol)
cv*100

hist(Horas_Sol,
     main = "Distribución de las Horas de Sol",
     xlab = "Horas de Sol (h)",
     ylab = "Frecuencia",
     col = 'pink')


plot(Horas_Sol,
     main = "Serie de Tiempo de las Horas de Sol",
     xlab = "Días",
     ylab = "Horas de Sol (h)",
     col = 'purple')

#presion_atmosferica

Presion_Atmosferica = c(780, 782, 778, 785, 781, 779, 787, 784, 777, 786, 783, 786, 779, 788, 782, 787, 778, 781, 785, 789, 784, 780, 786, 790, 788, 779, 789, 785, 782, 791, 787, 783, 792, 789, 786, 778, 793, 791, 788, 784, 795, 792, 789, 783, 797, 794, 790, 785, 796, 793, 790, 786, 798, 795, 791, 787, 800, 797, 793, 789, 799, 796, 792, 788, 801, 798, 794, 790, 803, 800, 796, 792, 805, 802, 798, 794, 807, 804, 800, 796, 806, 803, 799, 795, 810, 807, 803, 799, 812, 809, 805, 801, 814, 811, 807, 803, 816, 813, 809, 805, 815, 812, 808, 804, 819, 816, 812, 808, 821, 818, 814, 810, 823, 820, 816, 812, 825, 822, 818, 814)

sPresion_Atmosferica <- sum(Presion_Atmosferica)
sPresion_Atmosferica
p <- sPresion_Atmosferica / length(Presion_Atmosferica)
p
mean(Presion_Atmosferica)
sd(Presion_Atmosferica)
cv = sd(Presion_Atmosferica)/mean(Presion_Atmosferica)
cv*100

hist(Presion_Atmosferica,
     main = "Distribución de la Presión Atmosférica",
     xlab = "Presión (hPa)",
     ylab = "Frecuencia",
     col = 'gray')


plot(Presion_Atmosferica,
     main = "Serie de Tiempo de la Presión Atmosférica",
     xlab = "Días",http://127.0.0.1:13843/graphics/5fd884cb-57ca-42df-9cd2-8f351874e056.png
     ylab = "Presión (hPa)",
     col = 'darkgray')

