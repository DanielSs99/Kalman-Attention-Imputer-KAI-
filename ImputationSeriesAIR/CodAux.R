# Library ======================================================================
# library(tidyverse) # Manipulation of set !! Generates conflicts with several packages
# library(dyplr)
library(dplyr) ;  library(ggplot2)
library(readxl) # Lecture set in excel formats 
library(lubridate) # Constum datasets
library(imputeTS) # Imputation for time series univariates  
library(Amelia) # Imputation for multivariate time series 
library(tseries) # Tools Times series 
library(TSA)
library(forecast)
# library(rMIDAS) # KNN methos imputations 
library(miceRanger)
# library(doParallel)
library(vars)
library(mice)
library(mixgb)
# library(missForest)
library(SynthTools)
library(mtsdi) # MVN-IM
library(t)
# Lecture Data set =============================================================
Usaquen <- read_excel("H:/ESCRITORIO/SemilleroEs/Modelado/Script-Principal/Estaciones/Usaquen.xlsx") |>
  dplyr::select(-c(SO2, CO))
Usaquen$Dir.Viento <- as.numeric(Usaquen$Dir.Viento)
Suba <- read_excel("H:/ESCRITORIO/SemilleroEs/Modelado/Script-Principal/Estaciones/Suba.xlsx", 
                   col_types = c("text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"))
Kennedy <- read_excel("H:/ESCRITORIO/SemilleroEs/Modelado/Script-Principal/Estaciones/Kennedy.xlsx", 
                      col_types = c("text", "text", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "text", "numeric", "numeric", "numeric", 
                                    "numeric"))
Ferias <- read_excel("H:/ESCRITORIO/SemilleroEs/Modelado/Script-Principal/Estaciones/Las Ferias.xlsx", 
                         col_types = c("text", "text", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "text", "numeric", "numeric", 
                                       "numeric", "numeric"))
# Subset 2014-2018, remove columns more 50% of missing values ==================
Usa <- subset(Usaquen, Fecha > "2014-01-01") |> dplyr::select(-c(Temperatura,Hora))
Ken <- subset(Kennedy, Fecha > "2014-01-01") |> dplyr::select(-c(OZONO,Hora))
Sub <- subset(Suba, Fecha > "2014-01-01") |> dplyr::select(-c(Hora))
Fer <- subset(Ferias, Fecha > "2014-01-01") |> dplyr::select(-c(Hora))

Usa <- Usa |>
  mutate(Dia = day(Fecha),
         Año = year(Fecha),
         Mes = month(Fecha),
         Semana = week(Fecha)) |>
  group_by(Año, Mes, Semana) |> 
  group_by(Año, Semana) |>
  summarise(Suma_NA = sum(is.na(PM2.5)),
            PromedioP10 = ifelse(Suma_NA > 84, NA, mean(PM10, na.rm = TRUE)),
            OZONO =  ifelse(sum(is.na(OZONO)) > 84, NA, mean(OZONO, na.rm = TRUE)),
            VientoVe =  ifelse(sum(is.na(Vel.Viento)) > 84, NA, mean(Vel.Viento, na.rm = TRUE)),
            VientoDr =  ifelse(sum(is.na(Dir.Viento)) > 84, NA, mean(Dir.Viento, na.rm = TRUE)),
            Presipitacion =  ifelse(sum(is.na(Precipitacion)) > 84, NA, mean(Precipitacion, na.rm = TRUE)),
            PM2.5 =  ifelse(sum(is.na(PM2.5)) > 84, NA, mean(PM2.5, na.rm = TRUE)))

Ken <- Ken |>
  mutate(Dia = day(Fecha),
         Año = year(Fecha),
         Mes = month(Fecha),
         Semana = week(Fecha)) |>
  group_by(Año, Mes, Semana) |> 
  group_by(Año, Semana) |>
  summarise(Suma_NA = sum(is.na(PM2.5)),
            PromedioP10 = ifelse(Suma_NA > 84, NA, mean(PM10, na.rm = TRUE)),
            VientoVe =  ifelse(sum(is.na(Vel.Viento)) > 84, NA, mean(Vel.Viento, na.rm = TRUE)),
            VientoDr =  ifelse(sum(is.na(Dir.Viento)) > 84, NA, mean(Dir.Viento, na.rm = TRUE)),
            Presipitacion =  ifelse(sum(is.na(Precipitacion)) > 84, NA, mean(Precipitacion, na.rm = TRUE)),
            PM2.5 =  ifelse(sum(is.na(PM2.5)) > 84, NA, mean(PM2.5, na.rm = TRUE)))

Sub <- Sub |>
  mutate(Dia = day(Fecha),
         Año = year(Fecha),
         Mes = month(Fecha),
         Semana = week(Fecha)) |>
  group_by(Año, Mes, Semana) |> 
  group_by(Año, Semana) |>
  summarise(Suma_NA = sum(is.na(PM2.5)),
            PromedioP10 = ifelse(Suma_NA > 84, NA, mean(PM10, na.rm = TRUE)),
            OZONO =  ifelse(sum(is.na(OZONO)) > 84, NA, mean(OZONO, na.rm = TRUE)),
            VientoVe =  ifelse(sum(is.na(Vel.Viento)) > 84, NA, mean(Vel.Viento, na.rm = TRUE)),
            VientoDr =  ifelse(sum(is.na(Dir.Viento)) > 84, NA, mean(Dir.Viento, na.rm = TRUE)),
            Presipitacion =  ifelse(sum(is.na(Precipitacion)) > 84, NA, mean(Precipitacion, na.rm = TRUE)),
            PM2.5 =  ifelse(sum(is.na(PM2.5)) > 84, NA, mean(PM2.5, na.rm = TRUE)))

Fer <- Fer |>
  mutate(Dia = day(Fecha),
         Año = year(Fecha),
         Mes = month(Fecha),
         Semana = week(Fecha)) |>
  group_by(Año, Mes, Semana) |> 
  group_by(Año, Semana) |>
  summarise(Suma_NA = sum(is.na(PM2.5)),
            PromedioP10 = ifelse(Suma_NA > 84, NA, mean(PM10, na.rm = TRUE)),
            OZONO =  ifelse(sum(is.na(OZONO)) > 84, NA, mean(OZONO, na.rm = TRUE)),
            VientoVe =  ifelse(sum(is.na(Vel.Viento)) > 84, NA, mean(Vel.Viento, na.rm = TRUE)),
            VientoDr =  ifelse(sum(is.na(Dir.Viento)) > 84, NA, mean(Dir.Viento, na.rm = TRUE)),
            Presipitacion =  ifelse(sum(is.na(Precipitacion)) > 84, NA, mean(Precipitacion, na.rm = TRUE)),
            PM2.5 =  ifelse(sum(is.na(PM2.5)) > 84, NA, mean(PM2.5, na.rm = TRUE)))
# ==============================================================================
#write.csv(Usa, file = "Usa.csv")
#write.csv(Sub, file = "Sub.csv")
#write.csv(Fer, file = "Fer.csv")
#write.csv(Ken, file = "Ken.csv")

# Missing Values    ============================================================
Set <- list(Usa, Ken, Fer, Sub)
Na_forset <- list()
for (i in 1:4) {
  data <- Set[[i]]  
  Na_forset[[i]] <- colSums(is.na(data), na.rm = T)
}
Na_forset[[1]]/264
png("G1.png", width = 1200, height = 600, units = "px")
par(oma = c(4,1,1,1), mfrow = c(2, 2), mar = c(2, 2, 1, 1))
# par(mfrow = c(2,2),mar = c(2, 2, 2, 2))
plot(Usa$PromedioP10, type = "l", xlab = "", ylab = "PM10", col = "black", main = "Usaquen", lwd = 1)
grid(4,4, col = "lightgray")
abline(v = c(32,  34,  35,  36,  37,  38,  39,  40,  61,  62,  70,
             85,  87, 108, 124, 125, 167, 170, 171), col = "red", lty = 2)
plot(Ken$PromedioP10, type = "l", xlab = "", ylab = "PM2.5", col = "black", main = "Kennedy")
grid(4,4, col = "lightgray")
abline(v = c(27,  28,  29,  30, 114, 115, 116, 117, 118, 159), col = "red", lty = 2)
plot(Fer$PromedioP10, type = "l", xlab = "", ylab = "PM10", col = "black", main = "Ferias", xlim = c(0,260))
grid(4,4, col = "lightgray")
abline(v = c(9,  10,  11,  23,  32,  33,  34,  42,  46,  47,  50,
             51,  52,  54,  55,  56,  57,  58,  59,  60,  85, 104,
             124, 125, 161, 255), col = "red", lty = 2)
plot(Sub$PromedioP10, type = "l", xlab = "", ylab = "PM2.5", col = "black", main = "Suba")
grid(4,4, col = "lightgray")
abline(v = c(1,   2,   3,   4,   5,   6,   7,   8,   9,  36,  39,
              65,  98, 102, 106, 116, 121, 122, 132, 136, 137, 138,
              139, 141, 142, 143, 144, 145, 146, 170, 171, 172, 178), col = "red", lty = 2)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("NAs"), col = c("red"),
       lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')
dev.off()


png("G2.png", width = 1200, height = 600, units = "px")
par(oma = c(4,1,1,1), mfrow = c(2, 2), mar = c(2, 2, 1, 1))
# par(mfrow = c(2,2),mar = c(2, 2, 2, 2))
plot(Usa$PM2.5, type = "l", xlab = "", ylab = "PM10", col = "black", main = "Usaquen", lwd = 1)
grid(4,4, col = "lightgray")
abline(v = c(32,  34,  35,  36,  37,  39,  40,  61,  62,  70,  85,  87, 106, 108, 124, 125, 170,
             171), col = "red", lty = 2)
plot(Ken$PM2.5, type = "l", xlab = "", ylab = "PM2.5", col = "black", main = "Kennedy")
grid(4,4, col = "lightgray")
abline(v = c(27,  28,  29,  30, 114, 115, 116, 117, 118, 159), col = "red", lty = 2)
plot(Fer$PM2.5, type = "l", xlab = "", ylab = "PM10", col = "black", main = "Ferias", xlim = c(0,260))
grid(4,4, col = "lightgray")
abline(v = c(9,  10,  11,  23,  32,  33,  34,  42,  46,  47,  50,
             51,  52,  54,  55,  56,  57,  58,  59,  60,  85, 104,
             124, 125, 161, 255), col = "red", lty = 2)
plot(Sub$PromedioP10, type = "l", xlab = "", ylab = "PM2.5", col = "black", main = "Suba")
grid(4,4, col = "lightgray")
abline(v = c(1,   2,   3,   4,   5,   6,   7,   8,   9,  36,  65,  98, 102, 116, 121, 122, 132,
              136, 137, 138, 139, 142, 143, 144, 145, 146), col = "red", lty = 2)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("NAs"), col = c("red"),
       lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')
dev.off()



# Imputacion series univariadas 4 metodos ======================================
# Usa, Ken, Fer, Sub
set.seed(1999)
kal <- list()
intespline <- list()
interlinear <- list()
impuna <- list()

for (i in 1:4) {
  data <- Set[[i]]$PromedioP10
  kal[[i]] <- na_kalman(data, model = "StructTS")
  intespline[[i]] <- na_interpolation(data, option = "spline")
  interlinear[[i]] <- na_interpolation(data, option = "linear")
  impuna[[i]] <- na_mean(data)
}

lapply(kal, auto.arima)
lapply(kal, Box.test)
lapply(kal, adf.test)
lapply(kal, kpss.test)
lapply(intespline, auto.arima)
lapply(intespline, Box.test)
lapply(intespline, adf.test)
lapply(intespline, kpss.test)
lapply(interlinear, auto.arima)
lapply(interlinear, Box.test)
lapply(interlinear, adf.test)
lapply(interlinear, kpss.test)
lapply(impuna, auto.arima)
lapply(impuna, Box.test)
lapply(impuna, adf.test)
lapply(impuna, kpss.test)
# Usa, Ken, Fer, Sub
fit <- auto.arima(Usa$PromedioP10)
fit1 <- auto.arima(kal[[1]])
fit2 <- auto.arima(Sub$PromedioP10)
fit21 <- auto.arima(kal[[4]])

fit3 <- auto.arima(Ken$PromedioP10)
fit31 <- auto.arima(kal[[2]])
fit4 <- auto.arima(Fer$PromedioP10)
fit41 <- auto.arima(kal[[3]])

#par(oma = c(4,1,1,1), mfrow = c(2, 2), mar = c(2, 2, 1, 1))
png("plot2.png", width = 1200, height = 600, units = "px")
par(mfrow = c(2,2),mar = c(2, 2, 2, 2))
plot(forecast(fit,fan=TRUE, h = 15),include=200, type = "l", fcol = "orange", shadecols="oldstyle",
     main = "ARIMA(4,0,0)-Usaquen")
grid(4,4, col = "lightgray")
plot(forecast(fit1,fan=TRUE, h = 10),include=200, type = "l", col = "green" ,fcol = "orange", shadecols="oldstyle",
     main = "ARIMA(2,0,1)-Usaquen")
lines(Usa$PromedioP10, col = "black")
grid(4,4, col = "lightgray")
plot(forecast(fit2,fan=TRUE, h = 15),include=200, type = "l", fcol = "orange", shadecols="oldstyle",
     main = "ARIMA(5,0,0)-Suba")
grid(4,4, col = "lightgray")
plot(forecast(fit21,fan=TRUE, h = 10),include=200, type = "l", col = "green" ,fcol = "orange", shadecols="oldstyle",
     main = "ARIMA(1,0,1)-Suba")
lines(Sub$PromedioP10, col = "black")
grid(4,4, col = "lightgray")
dev.off()

png("plot3.png", width = 1200, height = 600, units = "px")
par(mfrow = c(2,2),mar = c(2, 2, 2, 2))
plot(forecast(fit3,fan=TRUE, h = 15),include=200, type = "l", fcol = "orange", shadecols="oldstyle",
     main = "ARIMA(1,1,0)-Kennedy")
grid(4,4, col = "lightgray")
plot(forecast(fit31,fan=TRUE, h = 10),include=200, type = "l", col = "green" ,fcol = "orange", shadecols="oldstyle",
     main = "ARIMA(2,1,1)-Kennedy")
lines(Ken$PromedioP10, col = "black")
grid(4,4, col = "lightgray")
plot(forecast(fit4,fan=TRUE, h = 15),include=200, type = "l", fcol = "orange", shadecols="oldstyle",
     main = "ARIMA(0,0,1)-Ferias")
grid(4,4, col = "lightgray")
plot(forecast(fit41,fan=TRUE, h = 10),include=200, type = "l", col = "green" ,fcol = "orange", shadecols="oldstyle",
     main = "ARIMA(2,0,2)-Ferias")
lines(Fer$PromedioP10, col = "black")
grid(4,4, col = "lightgray")
dev.off()



# Series de tiempo multivariada - Imputacion Multiple ==========================
# Imputacion miceRanger --------------------------------------------------------
# cl <- makeCluster(2) Parallel Processes
# registerDoParallel(cl)
set.seed(1999)
Su1 <- Sub[,-c(1:3)]
Us1 <- Usa[,-c(1:3)]
Ke1 <- Ken[,-c(1:3)]
Fe1 <- Fer[,-c(1:3)]
v <- list(
  PromedioP10 = c("OZONO","VientoVe","VientoDr", "Presipitacion", "PM2.5"),
  PM2.5 = c("OZONO","VientoVe","VientoDr", "Presipitacion", "PromedioP10"),
  OZONO = c("PM2.5","VientoVe","VientoDr", "Presipitacion", "PromedioP10"),
  VientoVe = c("OZONO","PM2.5","VientoDr", "Presipitacion", "PromedioP10"),
  VientoDr = c("OZONO","VientoVe","PM2.5", "Presipitacion", "PromedioP10"),
  Presipitacion = c("OZONO","VientoVe","VientoDr", "PM2.5", "PromedioP10")
)
miceobjranger <- miceRanger(Su1, m = 5, maxiter = 5, verbose = T,
                            vars = v, num.trees = 500)
miceobjranger2 <- miceRanger(Us1, m = 5, maxiter = 5, verbose = T,
                            vars = v, num.trees = 500)
v1 <- list(
  PromedioP10 = c("VientoVe","VientoDr", "Presipitacion", "PM2.5"),
  PM2.5 = c("VientoVe","VientoDr", "Presipitacion", "PromedioP10"),
  VientoVe = c("PM2.5","VientoDr", "Presipitacion", "PromedioP10"),
  VientoDr = c("VientoVe","PM2.5", "Presipitacion", "PromedioP10"),
  Presipitacion = c("VientoVe","VientoDr", "PM2.5", "PromedioP10")
)
miceobjranger3 <- miceRanger(Ke1, m = 5, maxiter = 5, verbose = T,
                             vars = v1, num.trees = 500)
miceobjranger4 <- miceRanger(Fe1, m = 5, maxiter = 5, verbose = T,
                             vars = v,num.trees = 500)


micerangercom <- miceRanger::completeData(miceobjranger)
micerangercom1 <- miceRanger::completeData(miceobjranger2)
micerangercom2 <- miceRanger::completeData(miceobjranger3)
micerangercom3 <- miceRanger::completeData(miceobjranger4)

listR <- c(micerangercom, micerangercom1, micerangercom2, micerangercom3)
Rangerl <- list()
for (i in 1:5) {
  data <- listR[i]
  for (j in 1:30) {
    model <- VAR(as.matrix(data[j]))
  }
}


# Mixgb imputation =============================================================
botsub <- mixgb(Su1, m = 5, maxit = 5)
botus <- mixgb(Us1, m = 5, maxit = 5)
botken <- mixgb(Ke1, m = 5, maxit = 5)
botfe <- mixgb(Fe1, m = 5, maxit = 5)

show_var(botsub, var.name = "PM2.5", original.data = Su1)
# Multivariate Normal imputacion ==============================================
UsN <- mnimput(PM2.5  ~ VientoVe + VientoDr  + Presipitacion +  PromedioP10, 
        Us1, maxit = 10, ts = T)
SubN <- mnimput(PM2.5  ~ VientoVe + VientoDr  + Presipitacion +  PromedioP10, 
                Su1, maxit = 10, ts = T)
FeN <- mnimput(PM2.5 ~ VientoVe + VientoDr +  Presipitacion + PromedioP10, 
                Fe1, maxit = 10, ts = T)
KebN <- mnimput(PM2.5 ~ VientoVe + VientoDr +  Presipitacion + PromedioP10, 
                Ke1, maxit = 10, ts = T)

plot(density(UsN$filled.dataset$PM2.5))
lines(density(Usa$PM2.5, na.rm = T), col = "red")
# Amelia =======================================================================
UsAM <- amelia(as.data.frame(Us1), m = 5, max.resample = 1000)
SubAM <- amelia(as.data.frame(Su1), m = 5, max.resample = 1000)
FeAM <- amelia(as.data.frame(Fe1), m = 5, max.resample = 1000)
KeAM <- amelia(as.data.frame(Ke1), m = 5, max.resample = 1000)

# ==============================================================================
plot(density(Su1$PM2.5, na.rm = T))
lines(density(micerangercom$Dataset_1$PM2.5), col = "red")
lines(density(as.data.frame(botsub)$PM2.5), col = "green")
lines(density(UsN$filled.dataset$PM2.5), col = "blue")
lines(density(UsAM$imputations$imp1$PM2.5), col = "orange")

plot(micerangercom$Dataset_1$PM2.5, col = "red", type = "l")
lines(as.data.frame(botsub)$PM2.5, col = "green")
lines(Su1$PM2.5)

# Random forest y cart =========================================================
# Usaquen #500 ARBOLES -> mtry = 3
modref <- VAR(na.omit(Us1), p = 2)
pre <- predict(modref, n.ahead = 12, ci = 0.95)
mod1 <- VAR(micerangercom$Dataset_1, p = 2)
plot(pre, names = "PM2.5")
# ==============================================================================
# Suba 
refSub <- VAR(na.omit(Su1), p = 1)
SuRan <- VAR(micerangercom$Dataset_26, p = 1)
SuXG <- VAR(botsub[[1]], p = 1)
SuMVN <- VAR(SubN$filled.dataset, p = 1)
Sum1 <- mice(Su1, m = 5, method = "cart")
Sum1 <- complete(Sum1)
Sum2 <- mice(Su1, m = 5, method = "norm.boot")
Sum2 <- complete(Sum2)
pol1Sub <- with(Sum1, expr = lm(PM2.5 ~ PromedioP10))
pool(pol1Sub)

# Usaquen 
refuS <- VAR(na.omit(Us1), p = 1)
UsRan <- VAR(micerangercom1$Dataset_26, p = 1)
UsXG <- VAR(botus[[1]], p = 1)
UsMVN <- VAR(UsN$filled.dataset, p = 1)
Usm1 <- mice(Us1, m = 5, method = "cart")
Usm1 <- complete(Usm1)
Usm2 <- mice(Us1, m = 5, method = "norm.boot")
Usm2 <- complete(Usm2)
# Ken 
refKe <- VAR(na.omit(Ke1), p = 1)
KeRan <- VAR(micerangercom2$Dataset_26, p = 1)
KeXG <- VAR(botken[[1]], p = 1)
KeMVN <- VAR(KebN$filled.dataset, p = 1)
Kem1 <- mice(Ke1, m = 5, method = "cart")
Kem1 <- complete(Kem1)
Kem2 <- mice(Ke1, m = 5, method = "norm.boot")
Kem2 <- complete(Kem2)
# Fer 
refFer <- VAR(na.omit(Fe1), p = 1)
FeRan <- VAR(micerangercom3$Dataset_26, p = 1)
FeXG <- VAR(botfe[[1]], p = 1)
FeMVN <- VAR(FeN$filled.dataset, p = 1)
Fem1 <- mice(Fe1, m = 5, method = "cart")
Fem1 <- complete(Fem1)
Fem2 <- mice(Fe1, m = 5, method = "norm.boot")
Fem2 <- complete(Fem2)

png("G3.png", width = 1200, height = 600, units = "px")
par(oma = c(4,1,1,1), mfrow = c(2, 2), mar = c(2, 2, 1, 1))
plot(density(Su1$PM2.5, na.rm = T), lwd = 2, xlab = "", ylab = "", main = "Suba")
lines(density(micerangercom$Dataset_26$PM2.5), col = "blue", lwd = 2)
lines(density(as.data.frame(botsub)$PM2.5), col = "orange", lwd = 2)
lines(density(SubN$filled.dataset$PM2.5), col = "green", lwd = 2)
lines(density(Sum1$PM2.5), col = "purple", lwd = 2)
lines(density(Sum2$PM2.5), col = "red", lwd = 2)
grid(4,4, col = "lightgrey")

plot(density(Us1$PM2.5, na.rm = T), lwd = 2, xlab = "", ylab = "", main = "Usaquen")
lines(density(micerangercom1$Dataset_26$PM2.5), col = "blue", lwd = 2)
lines(density(as.data.frame(botus)$PM2.5), col = "orange", lwd = 2)
lines(density(UsN$filled.dataset$PM2.5), col = "green", lwd = 2)
lines(density(Usm1$PM2.5), col = "purple", lwd = 2)
lines(density(Usm2$PM2.5), col = "red", lwd = 2)
grid(4,4, col = "lightgrey")


plot(density(Ke1$PM2.5, na.rm = T), lwd = 2, xlab = "", ylab = "", main = "Kennedy")
lines(density(micerangercom2$Dataset_26$PM2.5), col = "blue", lwd = 2)
lines(density(as.data.frame(botken)$PM2.5), col = "orange", lwd = 2)
lines(density(KebN$filled.dataset$PM2.5), col = "green", lwd = 2)
lines(density(Kem1$PM2.5), col = "purple", lwd = 2)
lines(density(Kem2$PM2.5), col = "red", lwd = 2)
grid(4,4, col = "lightgrey")


plot(density(Fe1$PM2.5, na.rm = T), lwd = 2, xlab = "", ylab = "", main = "Las Ferias")
lines(density(micerangercom3$Dataset_26$PM2.5), col = "blue", lwd = 2)
lines(density(as.data.frame(botfe)$PM2.5), col = "orange", lwd = 2)
lines(density(FeN$filled.dataset$PM2.5), col = "green", lwd = 2)
lines(density(Fem1$PM2.5), col = "purple", lwd = 2)
lines(density(Fem2$PM2.5), col = "red", lwd = 2)
grid(4,4, col = "lightgrey")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("Ref", "RMF-MI", "XGboots", "MVN", "AMELIA", "LSTM"),
       col = c("black", "blue", "orange", "green", "purple", "red"),
       lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')
dev.off()

# =============================================================================
serial.test(refSub)
serial.test(UsRan)
normality.test(refSub)
causality(refSub)
causality(UsRan)
arch.test(refSub)
arch.test(UsRan)
normality.test(refSub)
# ==============================================================================
png("G4.png", width = 1200, height = 600, units = "px")
par(mfrow = c(2,2),mar = c(2, 2, 2, 2))
acf(na.omit(Us1$PM2.5), lag.max = 100, demean = T, main = "Usaquen - Ref")
grid(4,4, col = "lightgrey")
acf(micerangercom$Dataset_26$PM2.5, lag.max = 100, demean = T, main = "Usaquen Random-Forest Mice")
grid(4,4, col = "lightgray")

acf(na.omit(Su1$PM2.5), lag.max = 100, demean = T, main = "Suba - Ref")
grid(4,4, col = "lightgrey")
acf(micerangercom1$Dataset_26$PM2.5, lag.max = 100, demean = T, main = "Usaquen Random-Forest Mice")
grid(4,4, col = "lightgray")
dev.off()


png("G5.png", width = 1200, height = 600, units = "px")
par(mfrow = c(2,2),mar = c(2, 2, 2, 2))
acf(na.omit(Ke1$PM2.5), lag.max = 100, demean = T, main = "Kennedy - Ref")
grid(4,4, col = "lightgrey")
acf(micerangercom3$Dataset_26$PM2.5, lag.max = 100, demean = T, main = "Kennedy Random-Forest Mice")
grid(4,4, col = "lightgray")

acf(na.omit(Fe1$PM2.5), lag.max = 100, demean = T, main = "Las Ferias - Ref")
grid(4,4, col = "lightgrey")
acf(micerangercom1$Dataset_26$PM2.5, lag.max = 100, demean = T, main = "Las Ferias Random-Forest Mice")
grid(4,4, col = "lightgray")
dev.off()