# ============================================================
# ANÁLISIS ESTADÍSTICO - LONGITUD DE SOLAPA
# ============================================================

library(readxl)
library(qcc)
library(nortest)


# ============================================================
# PREMUESTREO
# ============================================================

premuestreo <- c(62.357, 62.857, 59.785, 61.57, 59.5)

mean(premuestreo)
sd(premuestreo)

shapiro.test(premuestreo)

par(mfrow = c(1, 2))
hist(premuestreo,
     main = "Histograma - Premuestreo",
     xlab = "Longitud de solapa (mm)",
     ylab = "Frecuencia",
     col  = "#9370DB", border = "white")

qqnorm(premuestreo, main = "QQ Plot - Premuestreo",
       pch = 19, col = "#9370DB")
qqline(premuestreo, col = "red", lwd = 2)
par(mfrow = c(1, 1))


# ============================================================
# CARGA DE DATOS
# ============================================================

datos_raw <- read_excel("C:/Users/User/Downloads/tomaDatos.xlsx", sheet = "Hoja 1")

mediciones <- as.data.frame(datos_raw[1:24, c("n=1", "n=2", "n=3", "n=4")])
mediciones <- data.frame(lapply(mediciones, as.numeric))
colnames(mediciones) <- c("n1", "n2", "n3", "n4")

valores <- c(as.matrix(mediciones))


# ============================================================
# ANÁLISIS DESCRIPTIVO
# ============================================================

summary(mediciones)

mean(valores)
median(valores)
sd(valores)
min(valores)
max(valores)

media_subgrupos <- rowMeans(mediciones)
rango_subgrupos <- apply(mediciones, 1, function(x) max(x) - min(x))

hist(valores,
     main = "Histograma - Longitud de solapa",
     xlab = "Longitud de solapa (mm)",
     ylab = "Frecuencia",
     col  = "#9370DB", border = "white")

boxplot(valores,
        main = "Boxplot - Longitud de solapa",
        ylab = "Longitud de solapa (mm)",
        col  = "#9370DB", border = "#5B3FA6")

plot(valores,
     type = "b", pch = 19, cex = 0.7, col = "#9370DB",
     main = "Observaciones individuales",
     xlab = "Observación", ylab = "Longitud de solapa (mm)")
abline(h = mean(valores), col = "red", lty = 2)

plot(1:24, media_subgrupos,
     type = "b", pch = 19, col = "#9370DB",
     main = "Media por subgrupo",
     xlab = "Subgrupo", ylab = "Media (mm)", xaxt = "n")
axis(1, at = 1:24, labels = 1:24, cex.axis = 0.75)
abline(h = mean(media_subgrupos), col = "red", lty = 2)

plot(1:24, rango_subgrupos,
     type = "b", pch = 19, col = "#5B8DB8",
     main = "Rango por subgrupo",
     xlab = "Subgrupo", ylab = "Rango (mm)", xaxt = "n")
axis(1, at = 1:24, labels = 1:24, cex.axis = 0.75)
abline(h = mean(rango_subgrupos), col = "red", lty = 2)


# ============================================================
# ANÁLISIS DE NORMALIDAD
# ============================================================

par(mfrow = c(1, 2))
hist(valores,
     main = "Histograma - Datos de produccion",
     xlab = "Longitud de solapa (mm)",
     ylab = "Frecuencia",
     col  = "#9370DB", border = "white")

qqnorm(valores, main = "QQ Plot - Datos de produccion",
       pch = 19, cex = 0.7, col = "#9370DB")
qqline(valores, col = "red", lwd = 2)
par(mfrow = c(1, 1))

shapiro.test(valores)
lillie.test(valores)
ad.test(valores)

# p > 0.05 -> normalidad aceptada -> continuar con cartas X-barra y R
# p < 0.05 -> revisar causas o transformar: log(valores) / sqrt(valores)


# ============================================================
# CARTAS DE CONTROL X-BARRA y R  (L = 2.97)
# ============================================================

n  <- 4
L  <- 2.97
d2 <- 2.059
d3 <- 0.880

Xbarbar <- mean(media_subgrupos)
Rbar    <- mean(rango_subgrupos)

# Límites carta X-barra
A2_custom <- L / (d2 * sqrt(n))
LCS_xbar  <- Xbarbar + A2_custom * Rbar
LCI_xbar  <- Xbarbar - A2_custom * Rbar

# Límites carta R
LCS_R <- Rbar * (1 + L * d3 / d2)
LCI_R <- max(0, Rbar * (1 - L * d3 / d2))

matriz_qcc <- as.matrix(mediciones)

carta_R <- qcc(data   = matriz_qcc,
               type   = "R",
               sizes  = n,
               limits = c(LCI_R, LCS_R),
               title  = "Carta R - Longitud de solapa  (L = 2.97)",
               xlab   = "Subgrupo",
               ylab   = "Rango (mm)",
               digits = 3)

carta_Xbar <- qcc(data   = matriz_qcc,
                  type   = "xbar",
                  sizes  = n,
                  limits = c(LCI_xbar, LCS_xbar),
                  title  = "Carta X-barra - Longitud de solapa  (L = 2.97)",
                  xlab   = "Subgrupo",
                  ylab   = "Media (mm)",
                  digits = 3)
