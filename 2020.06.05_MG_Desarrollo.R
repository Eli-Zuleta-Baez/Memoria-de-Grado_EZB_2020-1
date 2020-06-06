# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # #  VIS: Indicadores adelantados del ciclo de crecimiento economico colombiano. # # # # # 
# # # # # Memoria de grado. Economia - Uniandes

library(ggplot2)
library(xts)
library(zoo)
library(plotrix)
library(plotly)
library(randomForest)
#library(scales) # to access breaks/formatting functions



###PARTE I: descripcion del crecimiento colombiano a traves de los datos

# # Se carga el directorio (ubicacion o carpeta en su pc): donde estan los archivos que necesita el codigo para funcionar 

setwd("C:/Users/ezbae/OneDrive - Universidad de los andes/10th/Memoria de grado/Books EZB/Data")

#base de datos - PIB trimestral

pib_dt <- read.csv("pib.csv", stringsAsFactors =TRUE,sep=";")
names(pib_dt) <- c("Fecha","PIB","Crecimiento PIB","log_PIB","d_logPIB","const","IED","IED_%")
pib_dt$Fecha <- as.Date(pib_dt$Fecha,"%d/%m/%Y")

# Organizar la base de datos en formato eXtensible Time Series
xts_pib <- xts(pib_dt[,setdiff(colnames(pib_dt),"Fecha")], order.by=as.Date(pib_dt$Fecha, format="%d/%m/%Y"))

# Grafico de densidad de la serie de Crecimiento trimestral real

crecimiento_real <- as.data.frame(pib_dt$`Crecimiento PIB`)
#dev.off()
d <- density(crecimiento_real[-1,])
plot(d, main="Crecimiento trimestral",cex.main=1)
polygon(d, col="royalblue", border="darkblue",
        ny=NULL)

abline(v = quantile(crecimiento_real[-1,],0.10),
       col = "darkblue",
       lwd = 2,
       lty="dotted")

pib_xts <- na.locf(pib_dt, fromLast=TRUE)

media_crec = quantile(crecimiento_real[-1,],0.5)
media_crec

# Plot crecimiento PIB

r <-ggplot(data = pib_xts, aes(x = Fecha, y = `Crecimiento PIB`)) +
  geom_point(size=1.2,colour = "#1e5160")+
  geom_hline(yintercept = 0, col="red",lwd = 0.5)+
  geom_hline(yintercept = media_crec, col="darkblue",lwd = 0.5)+
  geom_line(color = '#1e5160', size = 0.73) +
  labs(title = 'Crecimiento trimestral del PIB'
       ,caption="Fuente: Banrep") +
  theme(text = element_text(color = "#444444")
        ,plot.title = element_text(size = 18, color = '#333333')
        ,axis.title = element_blank()
        ,axis.title.y = element_blank(),
        axis.text.x = element_text(colour="#333333",size=15),
        axis.text.y = element_text(colour="#333333",size=15))       

r
fig <- ggplotly(r, dynamicTicks = TRUE)
fig <- fig %>% layout()
fig

##################################################################
###### Modelo LR (logistic regression)############################

# Se cargan los datos que por simplicidad se tienen ya organizadas en archivos csv
data_lr <- read.csv("dt_mod_2.csv", stringsAsFactors =TRUE,sep=";")
  #data_lr <- read.csv("datos_logit.csv", stringsAsFactors =TRUE,sep=";")
# Quitar na's
data_lr <- as.data.frame(na.omit(data_lr))
names(data_lr) <- c("Fecha","Yld_sprd","r_Crudo","r_FtseColndx","TC","Contraccion")
data_lr$Fecha <- as.Date(data_lr$Fecha,format="%d/%m/%Y")

#se organiza la base de datos en formato eXtensible Time Series

xts <- xts(data_lr[,-1],order.by = data_lr$Fecha,format="%d/%m/%Y")
# Plots de las series (opcional)
plot.xts(xts$Yld_sprd)
plot.xts(xts$r_Crudo)
plot.xts(xts$r_FtseColndx)
old.par <- par(mfrow=c(2, 3))
plot.xts(xts$Yld_sprd, main = "Term spread", yaxis.right =FALSE)
plot.xts(xts$r_Crudo,main = "Retornos Crudo", yaxis.right =FALSE)
plot.xts(xts$r_FtseColndx,main = "Retornos Ftse Index", yaxis.right =FALSE)
##Eliminando outliers
Q <- quantile(xts$Yld_sprd, probs=c(.01, .99), na.rm = FALSE)
iqr <- IQR(xts$Yld_sprd)

eliminated<- subset(xts, xts$Yld_sprd > (Q[1] - iqr) & xts$Yld_sprd < (Q[2]+iqr))
Q <- quantile(xts$r_Crudo, probs=c(.001, .999), na.rm = FALSE)
iqr <- IQR(xts$r_Crudo)
eliminated<- subset(eliminated, eliminated$r_Crudo > (Q[1] - iqr) & eliminated$r_Crudo < (Q[2]+iqr))
Q <- quantile(xts$r_FtseColndx, probs=c(.001, .999), na.rm = FALSE)
iqr <- IQR(xts$r_FtseColndx)
dt_mod2<- subset(eliminated, eliminated$r_FtseColndx > (Q[1] - iqr) & eliminated$r_FtseColndx < (Q[2]+iqr))

plot.xts(dt_mod2$Yld_sprd, main = "Term spread", yaxis.right =FALSE, yaxis.left =TRUE )
plot.xts(dt_mod2$r_Crudo,main = "Retornos Crudo", yaxis.right =FALSE)
plot.xts(dt_mod2$r_FtseColndx,main = "Retornos Ftse Index", yaxis.right =FALSE)
par(old.par)

crudo <- to.period(dt_mod2$r_Crudo, period = "months", OHLC = FALSE)


#Se estandarizan los datos  a media 0 y sd 1 por medio de la funcion scale
stdrd_data_lr <- scale(xts[,-5])
stdrd_data_lr <- cbind(stdrd_data_lr,data_lr$Contraccion)
colMeans(stdrd_data_lr)


names(stdrd_data_lr) <- c("Yld_sprd","r_Crudo","r_FtseColndx","TC","Contraccion")

nPeriodos <- length(data_lr$Fecha)
dt_mod2 <- (stdrd_data_lr)


dt_mod2_1lags <- diff(dt_mod2[,-5], lag = 30)
dt_mod2_2lags <- diff(dt_mod2[,-5], lag = 60)
dt_mod2_3lags <- diff(dt_mod2[,-5], lag = 90)
dt_mod2_4lags <- diff(dt_mod2[,-5], lag = 120)
dt_mod2_5lags <- diff(dt_mod2[,-5], lag = 150)
dt_mod2_6lags <- diff(dt_mod2[,-5], lag = 180)
dt_mod2_12lags <- diff(dt_mod2[,-5], lag = 365)

todos_lags <- cbind(dt_mod2[,5],dt_mod2_1lags,dt_mod2_2lags,dt_mod2_3lags,dt_mod2_4lags,dt_mod2_5lags,dt_mod2_6lags,dt_mod2_12lags)
train.df <- todos_lags[index(todos_lags)<=as.Date("01/01/2017", format="%d/%m/%Y")]
test.df <- todos_lags[index(todos_lags)>as.Date("01/01/2017", format="%d/%m/%Y")]
train.df.2008 <- todos_lags[index(todos_lags)<=as.Date("01/06/2008", format="%d/%m/%Y")]
train.df.2016 <- todos_lags[index(todos_lags)<=as.Date("01/06/2016", format="%d/%m/%Y")]

mod2 <- glm(Contraccion ~   Yld_sprd*r_FtseColndx*r_Crudo*TC+Yld_sprd.1*r_FtseColndx.1*r_Crudo.1*TC.1+Yld_sprd.3*r_FtseColndx.3*r_Crudo.3*TC.3+Yld_sprd.4*r_FtseColndx.4*r_Crudo.4*TC.4+Yld_sprd.5*r_FtseColndx.5*r_Crudo.5*TC.5+Yld_sprd.6*r_FtseColndx.6*r_Crudo.6*TC.6,data = train.df, family = "binomial")
mod2_2008 <- glm(Contraccion ~ Yld_sprd.1*r_FtseColndx.1*r_Crudo.1*TC.1+Yld_sprd.3 +r_FtseColndx.3+r_Crudo.3+TC.3,data = train.df.2008, family = "binomial") #any of the vars meaningful
mod2_2016 <- glm(Contraccion ~ +Yld_sprd.1*r_FtseColndx.1*r_Crudo.1*TC.1+Yld_sprd.3*r_FtseColndx.3*r_Crudo.3*TC.3+Yld_sprd.6*r_FtseColndx.6*r_Crudo.6*TC.6,data = train.df.2016, family = "binomial") #any of the vars meaningful

summary(mod2)
library(tidyverse)
library(caret)
probabilities <- mod2 %>% predict(todos_lags, type = "response")
probabilities_2008 <- mod2_2008 %>% predict(train.df.2008, type = "response")
probabilities_2016 <- mod2_2016 %>% predict(train.df.2016, type = "response")

summary(probabilities)


probabilities_temp <- xts(probabilities, order.by=index(todos_lags),format="%d/%m/%Y")
probabilities_temp_2008 <- xts(probabilities_2008, order.by=index(train.df.2008),format="%d/%m/%Y")
probabilities_temp_2016 <- xts(probabilities_2016, order.by=index(train.df.2016),format="%d/%m/%Y")

summary(probabilities_temp)
summary(probabilities_temp_2008)
summary(probabilities_temp_2016)
names(probabilities_temp) <- c("fit")
plot.xts(probabilities_temp, main = "Probabilidad de contracción del crecimiento estimada por regresión logística", yaxis.right =FALSE)
plot.xts(probabilities_temp_2008, main = "Probabilidad de contracción del crecimiento estimada por regresión logística", yaxis.right =FALSE)
plot.xts(probabilities_temp_2016, main = "Probabilidad de contracción del crecimiento estimada por regresión logística", yaxis.right =FALSE)
dev.off()


confmatrix <- table(Actual_value=todos_lags$Contraccion, Predicted_value=probabilities>0.5)
confmatrix
(confmatrix[1,1]+confmatrix[2,2]) /sum(confmatrix)
points(todos_lags$Yld_sprd,fitted.values(mod2))



#############################################################
###### Modelo RF (random forests)############################


# Se organiza y se modifican las  bases de entrenamiento y prueba para poder estimar RF

train.df <- na.omit(as.data.frame(train.df)) #mismas bases del modelo LR
train.df$Contraccion <- as.factor(train.df$Contraccion)
todos_lags <- na.omit(as.data.frame(todos_lags))
todos_lags$data_lr.Contraccion <- as.factor(todos_lags$Contraccion)
test.df <- na.omit(as.data.frame(test.df))
test.df$Contraccion <- as.factor(test.df$Contraccion)


# La funcion tuneRF permite ver cuales son los mejores parametros con los cuales estimar el modelo de RF
# t <- tuneRF(train.df[,-1],
#             train.df[,1],
#             stepFactor= 0.2,
#             plot=TRUE,
#             ntreeTry=100,
#             trace=TRUE,
#             improve=0.05)

t <- tuneRF(train.df[,-1],
            train.df[,1],
            stepFactor= 0.2,
            plot=TRUE,
            ntreeTry=100,
            trace=TRUE,
            improve=0.05)
# Se emplea la funcion ranfomForest del paquete con el mismo nombre para
modelo_RF <- randomForest(Contraccion ~ ., data = train.df, ntree=100,
                       importance=TRUE,
                       mtry=9,
                       proximity=TRUE)
modelo_RF

modelo_RF$confusion
p1 <- predict(modelo_RF,test.df)
confusionMatrix(p1,test.df$Contraccion)
plot(modelo_RF)
varImpPlot(modelo_RF,
           sort = T,
           n.var = 7,
           main = "Variables más importantes para Random Forest")

varUsed(modelo_RF)

