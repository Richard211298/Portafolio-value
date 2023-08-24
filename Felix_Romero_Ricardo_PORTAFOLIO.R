##########Examen Primer parcial###############################
#########Portafolio###########################################
###Felix Romero Ricardo#######################################
#4em1#########################################################
##############BETA PORTAFOLIO CORRER POR PARTES###############
#############################################Instalar paquetes####
#install.packages("TTR")
#install.packages("matrixStats")
#install.packages("tidyquant") # Para descargar los datos
#install.packages("tidyr") # Permite manipular y transformar los datos
#install.packages("plotly") # Para hacer gráficos chulos
#install.packages("recipes")
#install.packages("timetk") # Para manipular las series de tiempo
#install.packages("quantmod") # Para descargar los datos
#install.packages("xts") #Convertir dataframes en series de tiempo
#install.packages("tseries") #Trabajar con series de tiempo
#install.packages("forecast") #Pronosticar series de tiempo
#install.packages("timeSeries") #Trabajar con series de tiempo
#install.packages("tframePlus") #Trabajar con series de tiempo
#install.packages("ggplot2") #Gráficos coquetos
#install.packages("dplyr") #Gráficos coquetos y dinámicos
#install.packages("plotly") #Gráficos coquetos y dinámicos
#install.packages("hrbrthemes") #Elegir diferentes diseños para gráficos
#install.packages("ggthemes") #Elegir diferentes diseños para gráficos
#install.packages("tidyverse")  # Permite manipular y transformar los datos
#install.packages("dygraphs") #Gráficos dinámicos
#install.packages("gridExtra") #Gráficos dinámicos
#install.packages("corrplot")
###################################Llamar Paqueterias a ocupar####
library(TTR)
library(matrixStats)
library(tidyquant) # Para descargar los datos
library(tidyr) # Permite manipular y transformar los datos
library(plotly) # Para hacer gráficos chulos
library(recipes)
library(timetk) # Para manipular las series de tiempo
library(quantmod) # Para descargar los datos
library(xts) #Convertir dataframes en series de tiempo
library(tseries) #Trabajar con series de tiempo
library(forecast) #Pronosticar series de tiempo
library(timeSeries) #Trabajar con series de tiempo
library(tframePlus) #Trabajar con series de tiempo
library(ggplot2) #Gráficos coquetos
library(dplyr) #Gráficos coquetos y dinámicos
library(plotly) #Gráficos coquetos y dinámicos
library(hrbrthemes) #Elegir diferentes diseños para gráficos
library(ggthemes) #Elegir diferentes diseños para gráficos
library(tidyverse)  # Permite manipular y transformar los datos
library(dygraphs) #Gráficos dinámicos
library(gridExtra) #Gráficos dinámicos
library(corrplot)



####Seleccionar acciones (Tickerlist) y fechas################
#####correr por partes las secciones de la A)B)...############
###############################A)Cantidad de Tickets(MAS DE 1)####
NuA=readline(prompt="�Cuantas acciones quiere en el portafolio?: ")
 
#######################################B)Nombre de los tickers####

NuA=as.numeric(NuA)
TickerList= c("list",NuA)
for (i in 1:NuA)
{
  TickerList[[i]] = readline(prompt="Ticket: ") # I used print to see the results in the screen
}

#######################C)CONFIRMAR TICKETS(por si se equivoca)####

print(TickerList)
NN=readline(prompt="�Estan bien escritas?(1=Si|0=NO): ")

###########################################D)confirmar tickets####

#hat = gorrito, LOS INDICES SIEMPRE LLEVAN HAT

NN=as.numeric(NN)
YANOSE=split(TickerList,NuA)
if (NN==0) { for (i in 1:NuA)
{TickerList[[i]] <- readline(prompt="Siglas del ticket: ") 
}
} else 
  message("TickerList lista para el portafolio   ",YANOSE)



#################################E)Especificar fecha de inicio####

Fechadeinicio=readline(prompt="Desde que fecha quiere recopilar datos?(A�o-Mes-Dia): ")

#################################F)Especificar hasta que fecha####

NC=readline(prompt="�Quiere ocupar la fecha de hoy?(1=Si|0=NO): ")

######################################################G)CORRER#####

NC=as.numeric(NC)
if (NC==0) { 
  Hoy=readline(prompt="Hasta que fecha quiere que termine?(A�o-Mes-Dia): ")
} else 
  Hoy=today("UTC")

##############################################################
##############################################################
###########################H)Descargar Ticker List seleccionda####


Fechadeinicio=as.Date(Fechadeinicio)
Hoy=as.Date(Hoy)


precios<- tq_get(TickerList,
                  from = Fechadeinicio,
                  to = Hoy,
                  get = 'stock.prices')


######################################I)Graficas de TickerList####
  
  QUEHACEESTO = unique(precios$symbol) #Lista con los niveles de la variable Grupo
 QUEHACEESTO=unique(TickerList)
  for (i in seq_along(QUEHACEESTO)) {  #Para cada uno de los valores de la lista...
    RickAndMorty =
      ggplot(subset(precios, precios$symbol==QUEHACEESTO[i]),
             aes(x=date, y=close)) +
      geom_line(stat="identity") +
      theme_bw() +
      labs(x="Fecha",    #Elimino title de este apartado para configurarlo al final
           y="Precio de cierre") +
      theme(text = element_text(size=10),
            plot.title = element_text(size=rel(2), vjust=2, face="bold", color="firebrick4", lineheight=1),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="none",
            axis.title.x = element_text(face="bold", vjust=1.5, colour="goldenrod4", size=rel(1)),
            axis.title.y = element_text(face="bold", vjust=1.5, colour="goldenrod4", size=rel(1)),
            axis.text = element_text(colour = "dodgerblue4")) +
      
      ggtitle(paste("Precio de cierre de", QUEHACEESTO[i])) #Configuro un t�tulo por grupo
    
    ABDFG=ggplotly(RickAndMorty) 
    print (ABDFG) #Mostrar gr�ficos en la pantalla
  }
  
##############################################################
##############################################################
##############################################################
##############################################################
##################Empieza portafolio##########################
############################################1)ANTES DE INICIAR####

TM=readline(prompt="�Nombre de la tasa a Comparar?: ")

############################################2)ANTES DE INICIAR####

TR=readline(prompt="�Cual es el valor de la tasa(en decimales)?: ")
############################################3)ANTES DE INICAR ####


TL=readline(prompt="�Cuantos portafolios quiere simular?: ")

#############################################4)ANTES DE INICAR####

NP=readline(prompt="�A que tiempo(A�O=1,2A�OS=2,Mes=.01,2Meses=.02...)?: ")

#############################################5)ANTES DE INICAR####

SED=readline(prompt="�SEMILLA PARA LOS PORTAFOLIO?: ")

#########################################################CORER####
TM=as.character(TM)
TR=as.numeric(TR)
TL=as.numeric(TL)
NP=as.numeric(NP)
SED=as.numeric(SED)

if(NP==1){
  NP=252
}else if (NP<=.2){
  NP=21*(NP*100)
}else NP=(NP)*252
  
################################################6)RENDIMIENTOS#####
#Calcula los rendimientos logaritmicos de los precios#
  
precios <- precios[apply(precios ,1,function(x) all(!is.na(x))),]
View(precios)

str(TickerList)
  log_ret_tidy <- precios %>% #Pipe
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = 'daily',
                 col_rename = 'ret',
                 type = 'log')
  
  #Vemos las primeras columnas
  
  #view(log_ret_tidy)
  #str(log_ret_tidy)
  
  ###Cambiamos el formato tidy a series de tiempo###
  
  log_ret_xts <- log_ret_tidy %>%
    spread(symbol, value = ret) %>%
    tk_xts()
  log_ret_xts <- log_ret_xts[apply(log_ret_xts ,1,function(x) all(!is.na(x))),]
  log_ret_xts[1,] <- 0
  View(log_ret_xts)
  #Vemos las primeras columnas del objeto xts
  
  #head(log_ret_xts)#opcional
  
  #Calcula el rendimiento promedio de las series
  
  mean_ret <- colMeans(log_ret_xts)
  #print(round(mean_ret, 5))  #redondea a 5 decimales
###############################7)PORTAFOLIO MATRIZ CORRELACION####
  #Calcula la matriz de covarianza y anualiza los datos
  
  cov_mat <- cov(log_ret_xts) * NP
  #print(round(cov_mat,4))   #redondea a 4 decimales
  
  #Calcula la matriz de correlación
  cor_mat <- cor(log_ret_xts)
  corrplot.mixed(cor_mat)
  print(round(cor_mat,4))   #redondea a 4 decimales
  
####################8)SIMULACION Y OPTIMIZACION DEL PORTAFOLIO####
  
  # Crea matrices para almacenar los pesos
  set.seed(SED)
  
  num_port <- TL
  
  all_wts <- matrix(nrow = num_port,
                    ncol = length(TickerList))
  
  # Crea un vector para guardar los rendimientos
  
  port_returns <- vector('numeric', length = num_port)
  
  # Crea un vector para guardar el riesgo o desviaciones
  
  port_risk <- vector('numeric', length = num_port)
  
  # Crea un vector para guardar el Sharpe con RF=0%
  
  sharpe_ratio <- vector('numeric', length = num_port)
  
  
  # Crea un vector para guardar el Sharpe con RF=0.25%

  sharpe_ratio_manual <- vector('numeric', length = num_port)
  
  # Hacemos el loop o ciclo#
  
  for (i in seq_along(port_returns)) {
    
    wts <- runif(length(TickerList))
    wts <- wts/sum(wts)
    
    # Guarda todos los pesos en la matriz
    all_wts[i,] <- wts
    
    # Rendimientos del protafolio
    
    port_ret <- sum(wts * mean_ret)
    port_ret <- ((port_ret + 1)^NP) - 1
    
    # Guarda los rendimientos
    port_returns[i] <- port_ret
    
    # Guarda las desviaciones
    port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
    port_risk[i] <- port_sd
    
    # Crea los portafolios con Sharpe 0%
    
    sr <- port_ret/port_sd
    sharpe_ratio[i] <- sr
    
    # Crea los portafolios con Sharpe y tasa
    
    sr1 <- Return.excess(port_ret, Rf = TR)/port_sd
    sr2 <- mean(sr1)/port_sd
    sharpe_ratio_manual[i] <- sr2
  }
  
  # Resultados de los portafolios
  
  portfolio_values <- tibble(Return = port_returns,
                             Risk = port_risk,
                             SharpeRatio = sharpe_ratio,
                             SharpeRatioModificado=sharpe_ratio_manual)
  
  
  # Junta todos los pesos calculados
  
  all_wts <- tk_tbl(all_wts)
  colnames(all_wts) <- colnames(log_ret_xts)
  
  # Muestra todos los resultados
  portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
  view(portfolio_values)
 
  
###################GRAFICAS PORTAFOLIO######################## 
##############################10)Portafolio de minima varianza####
  
 
  
  min_var <- portfolio_values[which.min(portfolio_values$Risk),]
  
  p <- gather(min_var[,1:NuA], key = Asset,
           value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Acciones', y = 'Pesos', title = "Portafolio de minima varianza") +
    scale_y_continuous(labels = scales::percent)
  
  view(min_var)
  ggplotly(p)
  
##############################11)Portafolio de maxima varianza#######
  
  
  
  max_var <- portfolio_values[which.max(portfolio_values$Risk),]
  
  k <- gather(max_var[,1:NuA], key = Asset,
              value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Acciones', y = 'Pesos', title = "Portafolio de maxima varianza") +
    scale_y_continuous(labels = scales::percent)
  
  view(max_var)
  ggplotly(k)
  
############################### 12)Portafolio con mayor Sharpe#########
  max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
  
  q <-gather(max_sr[,1:NuA], key = Asset,
           value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Acciones', y = 'Pesos', title = "Mejor portafolio con Sharpe") +
    scale_y_continuous(labels = scales::percent)
  
  view(max_sr)
  ggplotly(q)
  
#################### 13)Portafolio con mayor Sharpe Modificado#########
  max_srm <- portfolio_values[which.max(portfolio_values$SharpeRatioModificado),]
  
  v <-gather(max_srm[,1:NuA], key = Asset,
             value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Acciones', y = 'Pesos', title = "Mejor portafolio con Sharpe Modificado",TM) +
    scale_y_continuous(labels = scales::percent)
  
 
  view(max_srm)
  ggplotly(v)
  
######################################## 14)Frontera eficiente####
  
  #plot(portfolio_values)
  
  e <- portfolio_values %>%
    ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
    geom_point() +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'Riesgo',
         y = 'Rendimiento ',
         title = "Optimizacion de portafolios y frontera eficiente") +
    geom_point(aes(x = Risk,
                   y = Return), data = min_var, color = 'red') +
    geom_point(aes(x = Risk,
                   y = Return), data = max_sr, color = 'pink')+
   geom_point(aes(x = Risk,
                   y = Return), data = max_srm, color = 'green')+
   geom_point(aes(x = Risk,
                  y = Return), data = max_var, color = 'yellow')
  #annotate('text', x = 0.20, y = 0.42, label = "Mejor Sharpe") +
  #annotate('text', x = 0.18, y = 0.01, label = "Portafolio de mínima varianza") +
  #annotate(geom = 'segment', x = 0.14, xend = 0.135,  y = 0.01,
  #       yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
  #annotate(geom = 'segment', x = 0.22, xend = 0.2275,  y = 0.405,
  #      yend = 0.365, color = 'red', arrow = arrow(type = "open"))
  
  
  ggplotly(e)
  
  
  
###############################EXTRA-pronostico para convencer####

QUESTION=readline(prompt="�Quiere ver el pronostico de una accion del portafolio?(Si=1|No=0): ")

##################################################A)QUE TICKET#### 
  
accion=readline(prompt="�Cual?: ")  
  
##(INSTALAR PRIMERA VEZ)#B)INSTALAR PAQUETES O LLAMAR PAQUETES####
  
  #install.packages("randomForest")
  #install.packages("caret")
  
  library(caret)
  library(randomForest)
  
######################################################C)CORRER####
if  (QUESTION==1){
  

  ################################
  Acciones = getSymbols(accion,from=Fechadeinicio,to=Hoy,src="yahoo",auto.assign =F)[,6]
  
  Acciones=data.frame(Acciones)
  ##view(acciones)
  #####agregamos las fechas#######
  Acciones$fecha=row.names(Acciones)
  #view(acciones)
  #####quitamos Index fecha######
  row.names(Acciones)=NULL
  #view(acciones)
  #####cambiamos nombres de columnas##############
  names(Acciones)=c("precio","fecha")
  #view(acciones) 
  #str(acciones)
  ####cambiar formato a fecha########
  Acciones$fecha= as.Date(Acciones$fecha)
  #view(acciones)
  #str(acciones)
  #############################################
  ####crear fechas adcionales####
  rango_fecha=(Hoy+1):(Hoy+30)
  precio=as.numeric(NA)###CREAR VARIABLE PRECIOS CON NA
  Rango_fecha=as.data.frame(cbind(precio,rango_fecha))#####UNIR PRECIO Y FECHAS NUEVAS
  #view(Rango_fecha)
  Rango_fecha$fecha=as.Date(Rango_fecha$rango_fecha)##DAR FORMATO FECHA
  #view(Rango_fecha)
  Rango_fecha$rango_fecha=NULL##ELIMINAR COLUMNA
  ###unir las dos tablas#####
  tablapro=rbind(Acciones,Rango_fecha)
  #view(tablapro)
  
  
  ################################################################################
  ####separar fechas####
  tablapro$fecha_dup=tablapro$fecha#####crear dubplicado fecha
  tablapro=tablapro %>% separate(fecha,c("A�o","Mes","Dia"))####separar fecha
  ###str(tablapro)
  ###convertir en numeros 
  tablapro$A�o=as.numeric(tablapro$A�o)
  tablapro$Mes=as.numeric(tablapro$Mes)
  tablapro$Dia=as.numeric(tablapro$Dia)
  ###
  ##############crear tabla nueva escalada dia mes y a�o ##########################3
  ####semilla
  set.seed(SED)
  tablapro_sc=as.data.frame (cbind(tablapro$precio,tablapro$fecha_dup,scale(tablapro[,c(2:4)])))####Escalado
  names(tablapro_sc)[1]="Precio"###cambiar nombre columna
  names(tablapro_sc)[2]="Fecha"
  tablapro_sc$Fecha=as.Date(tablapro_sc$Fecha)###dar formato fecha
  ###view(tablapro_sc)
  ###OCUPAMOS MISMA SEMILLA##########
  set.seed(SED)
  train_data=createDataPartition(na.omit(subset(tablapro,tablapro$fecha_dup<today("UTC")))$precio,
                                 p = 0.7 , list=F)
  test=rbind(tablapro[-train_data,],subset(tablapro,tablapro$fecha_dup>=today("UTC")))
  
  test_sc=as.data.frame(cbind(test$precio,test$fecha_dup,scale(test[,c(2,3,4)])))
  names(test_sc)[1]="Precio"
  names(test_sc)[2]="Fecha"
  test_sc$Fecha=as.Date(test_sc$Fecha)
  
  
  ######regresion arboles con 100 sin datos escalados #####################
  mod_rf=randomForest( precio~ A�o + Mes + Dia ,data=tablapro[train_data,],
                       type= "regression", ntree= 100)
  
  pred_rf=predict(mod_rf,test)
  
  datos_rf=cbind(pred_rf,test)
  
  
  error_abs_rf=RMSE(datos_rf$precio,datos_rf$pred_rf,na.rm=TRUE)
  
  error_por_rf=error_abs_rf/datos_rf[datos_rf$fecha_dup==max(na.omit(datos_rf)$fecha_dup),]$precio
  
  error_por_rf*100
  
  Graficapro=ggplot()+geom_line(data=datos_rf,aes(x=fecha_dup,y=precio),color="blue")+
    geom_line(data=datos_rf,aes(x=fecha_dup, y=pred_rf),color="green")
  
  ABDasG=ggplotly(Graficapro)
print(ABDasG)
} 
 

  
 
  
  
##############FIN#############################################
  
  
##############################
  
view(datos_rf)

  
write_excel_csv(datos_rf,"Datos pronostico uber")
write_excel_csv(portfolio_values,"Datos portafolio")
write_excel_csv(precios,"uber y fb")
log_ret_xts=data.frame(log_ret_xts)
write_excel_csv(log_ret_xts,"rend")
