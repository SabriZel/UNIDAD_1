---
title: "Basic Example"
author: "Sabrina Adrian"
date: "2025-10-01"
output:
  html_document: default
  github_document: default
---

# Librerías

```{r setup, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)

#Lectura de datos

data_path <- "C:/Users/sabri/OneDrive/Documentos/4TO_SEM/ESTADISTICA4"
data_file <- file.path(data_path, "vgsales.csv")

data.vg.raw <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  na.strings = ".",
  header = TRUE,
  sep = ","
)
head(data.vg.raw)
str(data.vg.raw)

#Limpieza de datos
data.vg <- data.vg.raw
data.vg$Platform <- factor(data.vg$Platform)
data.vg$Year <- as.numeric(data.vg$Year)
summary(data.vg)

unique(data.vg$Year)
head(filter(data.vg, is.na(Year)))

unique(data.vg$Platform)
filter(data.vg, Name == "FIFA 15")
hist(data.vg$NA_Sales, main = "Ventas en Norteamérica", xlab = "NA_Sales")

#Datos faltantes(NA)

na_por_columna <- colSums(is.na(data.vg))
print("NA por columna:")
print(na_por_columna)

porcentaje_na <- colMeans(is.na(data.vg)) * 100
print("Porcentaje de NA por columna:")
print(round(porcentaje_na, 2))

length(unique(data.vg$Publisher))
filter(data.vg, Publisher == "")

data.vg <- data.vg[complete.cases(data.vg), ]
dim(data.vg)

#Tendencia central

summary(data.vg$Global_Sales)
hist(filter(data.vg, Global_Sales < 5)$Global_Sales,
     main = "Distribución de ventas globales (<5M)",
     xlab = "Ventas Globales")

#Muestreo

set.seed(13)
N <- nrow(data.vg)
indices_muestra <- sample(1:N, 200, replace = TRUE)
data.muestra <- data.vg[indices_muestra, ]
summary(data.muestra$Global_Sales)

#Población de interés (ventas < 2 millones)

data.vg.pobint <- filter(data.vg, Global_Sales < 2)
summary(data.vg.pobint$Global_Sales)

#Crear variable Peso_NA

data.vg.pobint <- data.vg.pobint %>%
  mutate(Peso_NA = ifelse(Global_Sales > 0, NA_Sales / Global_Sales, NA))
summary(data.vg.pobint$Peso_NA)

#Muestras

set.seed(101)
N <- nrow(data.vg.pobint)
indices_m1 <- sample(1:N, 500, replace = TRUE)
muestra1 <- data.vg.pobint[indices_m1, ]

set.seed(202)
indices_m2 <- sample(1:N, 500, replace = TRUE)
muestra2 <- data.vg.pobint[indices_m2, ]

#Estadísticas básicas

summary(muestra1$Peso_NA)
summary(muestra2$Peso_NA)
summary(data.vg.pobint$Peso_NA)

#Comparación de medias

media_pob <- mean(data.vg.pobint$Peso_NA, na.rm = TRUE)
media_m1 <- mean(muestra1$Peso_NA, na.rm = TRUE)
media_m2 <- mean(muestra2$Peso_NA, na.rm = TRUE)

cat("Media población:", round(media_pob, 3), "\n")
cat("Media muestra 1:", round(media_m1, 3), "\n")
cat("Media muestra 2:", round(media_m2, 3), "\n")

#GRAFICOS
#Poblacion

ggplot(data.vg.pobint, aes(x = Peso_NA)) +
  geom_histogram(fill = "blue", color = "black", bins = 25, alpha = 0.6) +
  labs(title = "Distribución del Peso_NA en la población de interés",
       x = "Peso relativo de ventas en Norteamérica",
       y = "Frecuencia") +
  theme_minimal()

#Muestra 1

ggplot(muestra1, aes(x = Peso_NA)) +
  geom_histogram(fill = "orange", color = "black", bins = 25, alpha = 0.6) +
  labs(title = "Distribución del Peso_NA - Muestra 1",
       x = "Peso relativo de ventas en Norteamérica",
       y = "Frecuencia") +
  theme_minimal()

#Muestra 2

ggplot(muestra2, aes(x = Peso_NA)) +
  geom_histogram(fill = "green", color = "black", bins = 25, alpha = 0.6) +
  labs(title = "Distribución del Peso_NA - Muestra 2",
       x = "Peso relativo de ventas en Norteamérica",
       y = "Frecuencia") +
  theme_minimal()

