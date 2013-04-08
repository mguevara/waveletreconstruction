#wavelet reconstruction
#@author miguel.guevara@postgrado.utfsm.cl
#@last_modification 17/oct/2012
#Reconstruye series wavelet 
library('wavelets')
library('forecast')

#MAIN CONFIGURATION
N = 10;  #N steps ahead
min = 1;  #valor inicial para serie
max = 100; #valor final para serie
levels = 3;
serie_id = "obama";  #obama, mccain, dolar

serie_get <- function(X,min,max)
{
  
  t.series <- X[min:max]
  #plot(t.series, type = "b")
  t.series
  
}

serie_predict <- function(X,N=10)
{
  fit <- auto.arima(X)
  predict <- predict(fit,n.ahead=N)
  
}  

serie_accuracy <- function(X,Y)
{
  ans = accuracy(X,Y);
}


switch(serie_id,
       obama={
          #DATA +++++++++++++++
          #serie id 1 Obama
          
          t.series_name = "Obama"
          
          #mac data
          t.series <- data <- read.csv("/Users/miguelguevara/Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/data.csv",sep=";", dec="," )
          
          #win data
          #t.series <- data <- read.csv("C:/Users/Miguel/Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/data.csv", sep=";", dec=",", quote="")
          
          
          #win data2
          #t.series <- data <- read.csv("~/My Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/US08.csv", sep=";", dec=",")
          #+++++++++++++++++++++++++++++
          
          t.series_complete <-t.series$Obama
       },
       
       mccain={
          #serie id 2 McCain
          
          t.series_name = "McCain"
          
          #mac data
          t.series <- data <- read.csv("/Users/miguelguevara/Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/data.csv",sep=";", dec="," )
          
          #win data
          #t.series <- data <- read.csv("C:/Users/Miguel/Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/data.csv", sep=";", dec=",", quote="")
          
          #+++++++++++++++++++++++++++++
          
          #win data2
          #t.series <- data <- read.csv("~/My Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/US08.csv", sep=";", dec=",")
          #+++++++++++++++++++++++++++++
          
          t.series_complete <-t.series$McCain
       },

       dolar={
          #serie id 3 Dólar.observado
          #mac data
          t.series_name = "Dolar Observado"
          t.series <- data <- read.delim("~/Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/bd_wavelet/dolar_americano:peso_chileno.txt", dec=",", quote="")
          #win data
          #t.series <- data <- read.csv("C:/Users/Miguel/Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/data.csv", sep=";", dec=",", quote="")
          t.series_complete <-t.series$Dólar.observado
          #++++++++++++++++++++++++++++
       },
       
       tweets={
         #serie id 3 Dólar.observado
         #mac data
         t.series_name = "Tweets"
         t.series <-data <- tweets <- read.csv("~/Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/bd_wavelet/tweets.csv", sep=";", quote="")
         #win data
         #t.series <- data <- read.csv("C:/Users/Miguel/Dropbox/DOCTORADO/BASE-DATOS-DOCUMENTALES/TallerR/data.csv", sep=";", dec=",", quote=""
         t.series_complete <- c(t.series[1])
         #++++++++++++++++++++++++++++
       },
       stop("No se encontró el ID de la serie!")
) #end switch data


#obtiene datos de la serie
t.series_part <- serie_get(t.series_complete,min,max)
t.series_ahead <- serie_get(t.series_complete, max+1, max+N)

#CALCULA PREDICCION CON MEJOR ARIMA MODEL
#++++++++++++++++++++++++++++++++++++++++++
#calcular predicci?n con model selection 
t.series_predict <- serie_predict(t.series_part, N)

#calcular accuracy
t.series_accuracy <- serie_accuracy( t.series_predict$pred, t.series_ahead)

#CALCULA PREDICCION CON METODO RECONSTRUCCION D WAVELET T
#++++++++++++++++++++++++++++++++++++++++++
dwt_original <- dwt(t.series_part, n.levels=levels)
dwt_forecasted <- dwt_original  #aqui se guardara la wavelet reconstruida con las predicciones
#dwt_forecasted
plot(dwt_original)
scaling_factor = 8

for (i in 1:levels)
{
 ind <- i
 #ind <- 3 #debuggin manual
  #AJUSTAR PREDICCION PARA WAVELET
  #temp = dwt_original@W$W1;  #original acceso
  temp <- c(dwt_original@W[ind][1]) #rescata la sequence wavelet ind
  fit_w_temp <- auto.arima(temp[[1]])  #ajusta modelo arima para el wavelet
  #factor = scaling_factor*(2^(levels-ind)) #calcular cuantos pasos adelante
  factor = N/(2^(ind)) #calcular cuantos pasos adelante PROPUESTA
  factor
  fore_w_temp <- predict(fit_w_temp, n.ahead=factor)  # predice ej para w1 3 ahead.
  fore_w_temp
  #REEMPLAZAR SECUENCIA WAVELET POR NUEVA COMPUESTA
  reemp <- matrix(  c(temp[[1]],fore_w_temp$pred),ncol=1)
  list_temp <- list() #lista a usarse para reemplazar el W completo
  list_temp <- slot(dwt_forecasted, 'W') #copia lista original W
  list_temp[[ind]] <- reemp
  #list_temp
  slot(dwt_forecasted, 'W') <- list_temp
  #slot(dwt_forecasted,'W')
  #dwt_forecasted
}

#para V ó Scale Sequences
for (i in 1:levels)
{
  ind <- i
  #ind <- 3 #debuggin manual
  #AJUSTAR PREDICCION PARA WAVELET
  #temp = dwt_original@W$W1;  #original acceso
  temp <- c(dwt_original@V[ind][1]) #rescata la sequence wavelet ind
  fit_v_temp <- auto.arima(temp[[1]])  #ajusta modelo arima para el wavelet
  #factor = scaling_factor*(2^(levels-ind)) #calcular cuantos pasos adelante
  factor = N/(2^(ind)) #calcular cuantos pasos adelante PROPUESTA
  factor
  fore_v_temp <- predict(fit_v_temp, n.ahead=factor)  # predice ej para w1 3 ahead.
  fore_v_temp   #algo no esta funcionando aqui
  #REEMPLAZAR SECUENCIA WAVELET POR NUEVA COMPUESTA
  reemp <- matrix(  c(temp[[1]],fore_v_temp$pred),ncol=1)
  list_temp <- list() #lista a usarse para reemplazar el W completo
  list_temp <- slot(dwt_forecasted, 'V') #copia lista original W
  list_temp[[ind]] <- reemp
  #list_temp
  slot(dwt_forecasted, 'V') <- list_temp
  #slot(dwt_forecasted,'V')
  #dwt_forecasted
}
dwt_forecasted
#plot(dwt_forecasted)

#obtener inversa DWT
#completar serie con pedazo faltante, con cualquier dato
factor <- scaling_factor * (2^levels)
#dwt_forecasted@series <- matrix(c(dwt_forecasted@series, t.series_part[1:factor]),ncol=1)
dwt_forecasted@series <- matrix(c(dwt_forecasted@series, t.series_part[1:N]),ncol=1)  #PROPUESTA, DE SOLO N
plot(dwt_forecasted)
t.series_idwt_forecasted <- idwt(dwt_forecasted)
t.series_idwt_forecasted
t.series_part
t.series_idwt_ahead <- serie_get(t.series_idwt_forecasted, max+1, max+N)  #obtiene datos ahead de la serie reconstruida
t.series_idwt_accuracy <- serie_accuracy( t.series_idwt_ahead, t.series_ahead)

#RESULTADOS
t.series_accuracy #resultados metodo arima
t.series_idwt_accuracy # resultados metodo idwt