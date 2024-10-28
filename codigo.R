#Importacion librerias y base de datos
library(MASS)
library(ggplot2)
library(graphics)
library(scatterplot3d)
library(car)

datos <- laptop_Price_Prediction_Dataset
summary(datos)

##Clasificación de variables
datos$Brand <- as.factor(datos$Brand)
datos$Model <- as.factor(datos$Model)
datos$Processor <- as.factor(datos$Processor)
datos$Graphics <- as.factor(datos$Graphics)
datos$Operating <- as.factor(datos$Operating)
#datos$RAM <- as.factor(datos$RAM)
#datos$Storage <- as.factor(datos$Storage)
#datos$Warranty <- as.factor(datos$Warranty)

var_cuanti <- subset(datos, select = c("Weight", "Battery", "Price", "Screen","Storage","RAM"))
summary(datos)

## MODELO
m1 <- lm(Price ~ . - Model, data=datos)
summary(m1)
p = 10

library(car)
vif(m1) 

library(tseries)
library(nortest)

shapiro.test(m1$residuals)
# No hay problema de multocolinealidad porque están muy cerca de 1,
# lo que es un excelente resultado, y que ambas variables son independientes 
# entre si y no hay relacion lineal fuerte que afecte el resultado del modelo

# Detección de outliers y puntos de influencia
residuales <- rstandard(m1) #Calcula residuales estandarizados
residuales

# a. Procedimiento gráfico
# - Gráfico de residuales estandarizados vs variable respuesta
plot(var_cuanti$Price,residuales)
abline(h=0)
#Ideal

# b. Pruebas estadisticas:
#influence.measures(m1)


#Distancia de cook
#Nos sirve para ver si es un punto de influencia o de outlier

#DFFITS 
#H gorro
#DFFITS|>1 ES un punto de outlier

#DFBETA
#
#DFBETA|>1 Es un punto de influencia


influence.measures(m1)
#lOS PUNTOS QUE TENGA ASTERISCO SIGNIFICA QUE SON CANDIDATOS A TENER A INFLUENCIA
#dFitt para cada observacion
#cook para cada observacion
#dFbeta para cada variable
n = 3000
prueba_ddfits = 2*sqrt(p/n) #0.1154700
prueba_ddfits

prueba_dfbeta = 2/sqrt(n)
prueba_dfbeta #0.03651484
#Para cada observacion verificar si son puntos de influencia

#4. Identificar qué marcas cumple los tres criterios---------------------------
#Doce marcas aparecieron con el asterisco 
#Con el crtiterio de distancia de cook ninguna cumple

#Con el criterio de los dffits sólo hay 8


#Con el criterio de dfbeta sólo hay 5

#Miro estos puntos de influencia en la base de datos original y decido cuales eliminar,
#Basandome en la diferncia respecto a los demas datos

#chrysler imperial - WT Varia respecto a los demas
#datsun 710
#influence.measures(m1)

quantile(datos$Price, probs=c(0.25,0.75))

quantile(datos$Battery, probs=c(0.25,0.75))


shapiro.test(m1$residuals)


datos1 <- datos[-c(6,8,10,11,14,17,25,28,34,35,52,72,84,103,106,125,133,137,139,149,
                   155,157,160,161,162,163,164,166,170,174,175,176,177,183,185,188,
                   192,197,203,204,206),]
datos1


mfinal <- lm(Price ~ . - Model, data=datos1)
summary(mfinal)
vif(mfinal)

Residuales <- mfinal$residuals
Residuales
datos1$Residuales <- rstandard(mfinal)


library(car)
qqPlot(mfinal$residuals, xlab = 'Cuantiles de distribucion normal',
       ylab = 'Cuantiles de residuales', pch = 16, col = "orange",
       col.lines = "red")


#------------
mfinal_log <- lm(log(Price) ~ . - Model, data=datos1)
summary(mfinal_log)

vif(mfinal_log)
# Revisar los residuos transformados
Residuales_log <- rstandard(mfinal_log)

# Evaluar normalidad de los nuevos residuos
#qqnorm(Residuales_log)
#qqline(Residuales_log, col = "red")

qqPlot(mfinal_log$residuals, xlab = 'Cuantiles de distribucion normal',
       ylab = 'Cuantiles de residuales', pch = 16, col = "orange",
       col.lines = "red")
#----------
#NORMALIDAD
library(tseries)
library(nortest)

shapiro.test(mfinal_log$residuals)
ad.test(mfinal_log$residuals)
jarque.bera.test(mfinal$residuals)

#Es normal

#INDEPENDENCIA

library(lmtest)
bgtest(m1)
dwtest(m1)

#Es independiente

ggplot(datos1, aes(x=Price, y=Residuales)) + 
  geom_point(color= 'black', size= 2)+
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= 'dotted',
             color="blue", size= 1)

#HOMOCEDATICIDAD

library(lmtest)
bptest(m1)

#Es HOMOCEDASTICO

datos1$Valores_Ajustados <- m1$fitted.values

ggplot(datos1, aes(x=Valores_Ajustados, y=Residuales)) + 
  geom_point(color= 'black', size= 2)+
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= 'dotted',
             color="blue", size= 1)

#---------------------------------------------------------------

#CLASE5

#seleccion de variables 
modb<- step(mfinal_log, trace = T, direction = "backward")
modb
summary(modb)


modf<-step(mfinal_log, trace = T, direction = "both")
modf
summary(modf)

#evaluacion final 
set.seed(123) #se usa para fijar semilla 

#calcular muestra
sample <- sample.int(n= nrow(datos1), size = floor(0.8*nrow(datos1)), replace = F)

#muestra de entrenamiento 
train <- datos1[sample, ]

test <- datos1[-sample, ]

modelotraining <- lm(formula= Price ~ Processor + RAM + Storage + Screen, data= train)
summary(modelotraining)


modelotesting <- lm(formula = Price ~ Processor + Storage + Screen + Graphics, data = test)
summary(modelotesting)

prediccion <- predict.lm(modelotesting, data= test[,c("Processor" , "RAM" + "Storage" + "Screen" + "Graphics")])
summary(prediccion)

plot(test$Price, prediccion)


library(Metrics)
metricas <- c(mae(test$Price, prediccion),
              mape(test$Price, prediccion),
              mse(test$Price, prediccion),
              rmse(test$Price, prediccion),
              AIC(modelotesting),
              BIC(modelotesting),
              summary(modelotesting)$r.squared)
#R*2= mayor mejor
#Estos que acabamos de ver mientras mas pequeños mejor, y lo minimo que me puede dar es 0
#R*2, MSE, RMSE, MAE, MAPE

metricas

names(metricas) <- c('MAE', 'MAPE', 'MSE', 'RMSE', 'AIC', 'BIC', 'R2')
metricas


 # ----------------- 

## GRAFICAS
# Boxplot de precio en cuanto al tamaño de pantalla
ggplot(datos, aes(x=Screen, y=Price)) + 
  geom_boxplot() +
  labs(title="Distribución del precio por tamaño de pantalla")

# Diagrama de dispersion de garantia y precio
ggplot(datos, aes(x=Weight, y=Price)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 

# Boxplot
ggplot(datos, aes(x = `Battery`, y = `Price`)) + 
  geom_boxplot()

# Histograma de Price
ggplot(datos, aes(x = `Price`)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Price ($)", x = "Price ($)", y = "Frequency") +
  theme_minimal()

# Histograma de Weight
ggplot(datos, aes(x = `Weight`)) +
  geom_histogram(binwidth = 0.2, fill = "lightgreen", color = "black") +
  labs(title = "Histograma de Weight (kg)", x = "Weight (kg)", y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras de la variable Brand
ggplot(datos, aes(x = Brand)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribucion de Brand", x = "Brand", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de barras de la variable Operating System
ggplot(datos, aes(x = `Operating System`)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Distribucion de Operating Systems", x = "Operating System", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Diagrama de violín de Price por Brand
ggplot(datos, aes(x = Brand, y = `Price`)) +
  geom_violin(fill = "skyblue", color = "black") +
  labs(title = "Precio by Brand", x = "Brand", y = "Price ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

