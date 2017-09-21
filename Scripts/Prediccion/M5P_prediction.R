#librerías. 
library("tidyr")
library("jsonlite")
library("neuralnet")
#prueba con caret
#require(caret)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_144') # for 64-bit version
require(rJava)
require(RMongo)
require(RWeka)



#extracción de sensores a consultar
mongo <- mongoDbConnect("TFM", "192.168.1.111", 27017)
sensores=dbGetDistinct(mongo, "Sensors","id")
dbDisconnect(mongo)

#extracción de previsión meteorológica para nuevas previsiones
#mongo <- mongoDbConnect("TFM", "192.168.1.111", 27017)
#recojo los últimos días.
#weather<-dbGetQuery(mongo, "Weather","{}.sort({'fecha':-1}).limit(5)", skip=0, limit=10000000)
#dbDisconnect(mongo)


#iteración por sensores. 
for (sensor in sensores)

{
  tryCatch({
  start <- Sys.time()
  #cargo datos

  
  mongo <- mongoDbConnect("TFM", "192.168.1.111", 27017)
  dataset=dbGetQuery(mongo, "Sensors",paste("{'id':",sensor,"}", sep=''), skip=0, limit=10000000)
  dbDisconnect(mongo)
  
  
  
  
  dataset$intensidad<-NULL
  dataset$x_id<-NULL
  dataset$id_old<-NULL
  dataset$presmin<-NULL
  dataset$presmax<-NULL
  dataset$dir<-NULL
  dataset$X<-NULL
  
  #funcion que transforma data a lista
  dataToList<-function (x)
  {fromJSON(toString(x))}
  dataset$data<-lapply(dataset$data, dataToList)
  #expansiónn de dataset
  dataset<-unnest(dataset, as.list(dataset$data))
  
  #nueva limpieza de datos
  dataset$vmed<-NULL
  
  #extraccion de fecha para entrada
  dataset$dia<-substr(dataset$fecha,1,3)
  dataset$mes<-substr(dataset$fecha, 5,7)
  #eliminar años
  dataset$anio<-substr(dataset$fecha, 25,29)
  
  #primera prueba, eliminando los inutiles
  #dataset$fecha<-NULL
  dataset$velmed<-NULL
  dataset$tmax<-NULL
  dataset$tmin<-NULL
  dataset$X_id<-NULL
  #dataset$id<-NULL
  dataset$ocupacion<-NULL
  
  
  #elimino las horas de 00 a 06 por ajuste 
  dataset<-dataset[!(substr(dataset$hora, 1,2)=='00' | substr(dataset$hora, 1,2)=='02' |substr(dataset$hora, 1,2)=='03' |substr(dataset$hora, 1,2)=='01'|
                       substr(dataset$hora, 1,2)=='04' |substr(dataset$hora, 1,2)=='05' |substr(dataset$hora, 1,2)=='06') ,]
  
  ClustersLectivo<-function(x)
  {
    if(is.na(x))
      
    {
      
      return("No")
    }
    else if (as.integer(x)==0)
      
    {
      
      return("No")
      
    }
    else 
    {
      return("Si")
    }
  }
  
  
  ClustersFest<-function(x)
  {
    
    if(x=='')
      
    {
      
      return("L")
    }
    else if(x=="V")
      
    {
      
      return("V")
    }
    else
    {
      
      return("F")
    }
  }
  
  ClusterFest2<-function(x)
  {
    
    if(x=="FSat" | x=="FSun"|x=="LSat" | x=="LSun"|x=="VSat" | x=="VSun")
    {
      return("F")
    }
    else if (x=="LFri"|x=="VFri")
    {
      return("V")
    }
    else if (substr(x, 1,1)=="F")
    {
      
      return ("F")
    }
    else if (substr(x, 1,1)=="V")
    {
      return ("V")
    }
    else
    {
      
      return('L')}
  }
  
  ClustersLLuvia<-function(x)
  {
    if(x=='Ip')
    {
      return(0)
    }
    else
    {
      return(as.numeric(sub(",", ".", x, fixed = TRUE)))
      
    }
  }
  

  #pruebas a pasado, lluvia completa
  #data$prec<-floor(as.numeric(data$prec))
  dataset$id<-paste("S",as.character(dataset$id),sep="")
  
  
  dataset$tmed<-floor(as.numeric(sub(",", ".", dataset$tmed, fixed = TRUE)))
  #lluvia discretizada
  dataset$prec<-sapply(dataset[,grep("prec",colnames(dataset))], FUN = ClustersLLuvia)
  dataset$lectivo<-sapply(dataset$lectivo, ClustersLectivo)
  dataset$fest<-sapply(dataset$fest, ClustersFest)
  dataset$fest<-sapply(paste(dataset$fest,dataset$dia,sep=''), ClusterFest2)
  
  
  
  #Saco test de datos
  test<-dataset[as.integer(dataset$anio)==2017&(dataset$mes=='Jun'),]
  dataset<-dataset[!(as.integer(dataset$anio)==2017&(dataset$mes=='May')),]
  
  #test con RWEKA
  dataset$dia<-NULL
  dataset$mes<-NULL
  dataset$anio<-NULL
  dataset$id<-NULL
  dataset$intensidad<-NULL
  dataset$fecha<-NULL
  
  #m<- Discretize(prec~ carga,data = dataset)
  #fit.rweka<-M5P(carga ~.   , data=dataset)
  
  
  set.seed(1)
  #pruebas con Arbol
  #fit.rweka<-train(carga ~ ., data=dataset, method='rf', ntree=200, metric="RMSE", maximize=F, trControl=trainControl(method="none", verboseIter = T,repeats=10))
  #jugar con ntree para convergencia en menor tiempo posible 
  
  #plot(fit.rweka$finalModel)
  
#  Discretize( ~ .,data=dataset)
  #fit.rweka<-train(carga ~ ., data=dataset, method='M5',  metric="RMSE", maximize=F)
  dataset$fest<-as.factor(dataset$fest)
  dataset$lectivo<-as.factor(dataset$lectivo)
  dataset$hora<-as.factor(dataset$hora)
  
  #cálculo de árbol y predicción de datos
  m5<-M5P(carga ~ ., data=dataset,control=Weka_control(N=F,R=T,M=4))
  test$prediction<-predict(m5, newdata = test)
  #WOW("rf")
  
  
  #e<-evaluate_Weka_classifier(rf, numFolds = 10,complexity=T,class=T)
  
  test$SquareError<-(test$carga-test$prediction)^2
  test$Error<-((test$carga-test$prediction)^2)^0.5
  Squaredmean<-mean(test$SquareError)
  mean<-Squaredmean^0.5
  max<-max(test$SquareError)
  funcPercentile<-ecdf(test$Error)
  ErrorOvermean2x<-funcPercentile(2*mean)
  ErrorOver10<-funcPercentile(10)
  ErrorOver5<-funcPercentile(5)
  test$ErrorOver10<-gsub(pattern="\\.", replacement=",",ErrorOver10)
  test$ErrorOver5<-gsub(pattern="\\.", replacement=",",ErrorOver5)
  test$Error<-gsub(pattern="\\.", replacement=",",test$Error)
  test$SquareError<-gsub(pattern="\\.", replacement=",",test$SquareError)
 
  #registro de log.
  line<-gsub(pattern = "\\.",replacement=",",paste(sensor, Squaredmean,
                                                    mean, max, ErrorOvermean2x,
                                                    ErrorOver10,ErrorOver5,"Ok", sep=";"))
  write(line,file="D:\\Log.csv",append=TRUE)
 
  
   write.table(test,file="D:\\Ddata", sep=";",append=T)
   #write.csv2(test,file="D:\\data.csv" )
  cat(paste(sensor, Sys.time()-start, '\n', sep=" "))
 },error=function(err){
   line<-gsub(pattern = "\\.",replacement=",",paste(sensor, 0, 0, 0, 0,0,0,"error", sep=";"))
   write(line,file="D:\\Log.csv",append=TRUE)
  cat(sensor,"error\n")
})
}
 