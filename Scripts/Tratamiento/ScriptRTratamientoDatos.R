#Carga de datos tráfico
ficheros<-c('06-2016','07-2016','08-2016','09-2016','10-2016','11-2016',
            '12-2016','01-2017','02-2017','03-2017','04-2017','05-2017','06-2017')


#Carga de datos clima
weather<- read.csv(paste("D:\\Clase\\Master BigData\\TFM\\Climatología\\Weather2.csv", sep=""), header=T,sep=";")
#elimino variables que no necesito
weather$nombre<-NULL
weather$provincia<-NULL
weather$altitud<-NULL
weather$indicativo<-NULL
weather$X<-NULL
weather$X_id<-NULL
weather$horatmax<-NULL
weather$presMin<-NULL
weather$horaPresMin<-NULL
weather$presMax<-NULL
weather$horaPresMax<-NULL
weather$dir<-NULL
weather["fecha"]<-as.Date(weather$fecha)


#itero por los ficheros
for(fichero in ficheros)
{
  cat('start: ', fichero, '\n')
  trafico<- read.csv(paste("D:\\Clase\\Master BigData\\TFM\\Trafico\\",fichero,".csv", sep=""), header=T,sep=";")
  
  #elimino las filas con error
  trafico<-trafico[(trafico$error=='N'),]
  
  #eliminamos los datos sobrantes:
  #trafico$idelem<-NULL
  trafico$tipo<-NULL
  trafico$tipo_elem<-NULL
  trafico$periodo_integracion<-NULL
  trafico$error<-NULL
  
  #tratamiento de fechas
  trafico["hora"]<-format(strptime(trafico$fecha, format="%Y-%m-%d %H:%M:%S"), "%H:%M")
  trafico["fecha"]<-as.Date(trafico$fecha)
  #añado un indentificador propio
  trafico$myId<-paste(trafico$fecha,trafico$identif)
  
  #duplico datos de trafico y me quedo lo comun
  traficocomun<-trafico
  traficocomun$intensidad<-NULL
  traficocomun$ocupacion<-NULL
  traficocomun$carga<-NULL
  traficocomun$vmed<-NULL
  traficocomun$hora<-NULL
  
  #tratamiento datos especificos
  trafico$data<-paste(
    '{"hora":"',trafico$hora,'", ',
    '"intensidad":',trafico$intensidad,', ',
    '"ocupacion":',trafico$ocupacion,', ',
    '"carga":',trafico$carga,', ',
    '"vmed":',trafico$vmed,'} '
    ,sep=""
  )
  
  
  
  trafico$intensidad<-NULL
  trafico$ocupacion<-NULL
  trafico$carga<-NULL
  trafico$vmed<-NULL
  trafico$hora<-NULL
  trafico$fecha<-NULL
  trafico$identif<-NULL
  
  #aggregate de tráfico
  trafico<-aggregate.data.frame(x = trafico,by=list(trafico$myId), FUN = paste, collapse=",")
  trafico$myId<-trafico$Group.1
  trafico$Group.1<-NULL
  
  #unicos de datos comunes
  traficocomun<-unique(traficocomun)
  
  #merge de ambas
  trafico<-merge(x=trafico,y=traficocomun, by="myId")
  
  #Merge con datos del tiempo
  trafico<-merge(x=trafico, y=weather, by="fecha")
  
  
  #tratamiento de los datos
  trafico$JSON<-paste(
    '{"fecha": {$date:"',paste(trafico$fecha,'T00:00:00Z', sep=""),'"}, ',
    '"id":"',trafico$identif,'", ',
    '"data":[',paste(trafico$data,sep=","), '],',
    '"tmed":"',trafico$tmed,'", ',
    '"tmax":"',trafico$tmax,'", ',
    '"tmin":"',trafico$tmin,'", ',
    '"prec":"',trafico$prec,'", ',
    '"presmax":"',trafico$presmax,'", ',
    '"presmin":"',trafico$presmin,'", ',
    '"velmed":"',trafico$velmedia,'", ',
    '"dir":"',trafico$dir,'"} '
    ,sep=""
    
  )
  
  
  
  #GRABAR EN FICHERO
  fileConn<-file(paste("D:\\",fichero,"-tratado.txt", sep=""))
  
  write(trafico$JSON, fileConn,append=F)
  close(fileConn)
  #fin prueba
  rm(trafico)
  rm(traficocomun)
  cat(paste("end: ", fichero))
  
}