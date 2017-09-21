#Carga de datos tráfico
#Enero2017<- read.csv("D:\\Clase\\Master BigData\\TFM\\Trafico\\2017\\01-2017.csv", header=T,sep=";")
Enero2015<- read.csv("D:\\Clase\\Master BigData\\TFM\\Trafico\\2015\\01-2015.csv", header=T,sep=";")
#Consultamos la columnas 
colnames(Enero2015)

#elimino las filas con error
Enero2015<-Enero2015[(Enero2015$error=='N'),]

#eliminamos los datos sobrantes:
Enero2015$idelem<-NULL
Enero2015$tipo<-NULL
Enero2015$tipo_elem<-NULL
Enero2015$periodo_integracion<-NULL
Enero2015$error<-NULL

Enero2015Test<-Enero2015
#tratamiento de fechas
Enero2015["hora"]<-format(strptime(Enero2015$fecha, format="%Y-%m-%d %H:%M:%S"), "%H:%M")
Enero2015["fecha"]<-as.Date(Enero2015$fecha)

#Merge con datos del tiempo

#Convertir a Json. (No puedo usar csv por la estructura seleccionada)
require(rjson)
#cargo json
ClimaEnero2015List=fromJSON(file = "D:\\Clase\\Master BigData\\TFM\\Climatología\\2015\\01-2015")
#paso a dataframe
Climaenero2015<-data.frame(matrix(unlist(ClimaEnero2015List),nrow=31,byrow=T))

#cargo cabeceras
headers=c("fecha","indicativo","nombre", "provincia", "altitud", "tmed", "prec", "tmin", "horamin", "tmax",
          "horamax", "dir", "velmedia", "racha", "horaracha", "presmax","horapresmax", "presmin","horapresmin")
names(Climaenero2015)<-headers
#limpio datos no necesarios
rm(ClimaEnero2015List)
rm(headers)

#merge de datos
#revisar uso
Climaenero2015["fecha"]<-as.Date(Climaenero2015$fecha)
transformed<-merge(x=Enero2015, y=Climaenero2015, by="fecha", all=T)


#file
fileConn<-file("D:\\output.txt", 'a')
#unicos del dataframe
fechas<-unique(transformed$fecha)
sensores<-unique(transformed$identif)
#inicio prueba
#copio el df  inicial
transformed2<- Enero2015
#hago un csv de la muestra
transformed$data<-paste(
  '{"fecha":new Date("',transformed$fecha,'"), ',
  '"id":"',transformed$identif,'", ',
  '"data":{
  "hora":"',transformed$hora,'", ',
  '"intensidad":"',transformed$intensidad,'", ',
  '"ocupacion":"',transformed$ocupacion,'", ',
  '"carga":"',transformed$carga,'", ',
  '"vmed":"',transformed$vmed,'"}, ',
  '"tmed":"',transformed$tmed,'", ',
  '"tmax":"',transformed$tmax,'", ',
  '"tmin":"',transformed$tmin,'", ',
  '"prec":"',transformed$prec,'", ',
  '"presmax":"',transformed$presmax,'", ',
  '"presmin":"',transformed$presmin,'", ',
  '"velmed":"',transformed$velmedia,'", ',
  '"dir":"',transformed$dir,'"}, '
  ,sep=""
  
)
transformed$data[1]<-paste("[",transformed$data[1])
transformed$data[nrow(transformed)]<-paste(substring(transformed$data[nrow(transformed)],0,nchar(transformed$data[nrow(transformed)])-1),"]")
#elimino las columnas innecesarias
transformed2$hora<-NULL
transformed2$intensidad<-NULL
transformed2$ocupacion<-NULL
transformed2$carga<-NULL
transformed2$vmed<-NULL
#json de transformed2$data
cat(transformed$data[1])
write(transformed$data, fileConn,append=F)   
#fin prueba


#iteración por listas
for(auxfecha in fechas)
{
  
  for(auxsensor in sensores)
  {
    #write(toString(auxfecha), fileConn,append=T)    
    #write(auxsensor, fileConn,append=T)   
    dfTest<-rbind(dfTest,toString(auxfecha))
    dfTest<-rbind(dfTest,auxsensor)
    auxsub<-subset(transformed,fecha==auxfecha & identif==auxsensor)
    #write(toJSON(auxsub), fileConn,append=TRUE)     
  }
  
  
}
close(fileConn)

#subset de los ficheros
a<-subset(transformed, identif=='67405'&fecha=='2015-01-31')

