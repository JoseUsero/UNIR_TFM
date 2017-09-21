require("twitteR")

#el api de Twitter requiere la configuración de aplicación en una cuenta activa. 
api_key <- ""
api_secret <- ""
token= ""
token_secret=""

#autenticación en la red social
setup_twitter_oauth(api_key, api_secret, token, token_secret)
1

#lista de temas a buscar
topics<-c('#DGT', '#DGTMadrid', '#M30', 'Madrid accidente')


for(topic in topics)
{
  #busqueda de tweets con  un límite de 1000
  tweets<- searchTwitter(topic,n=1000)
  tweets.df <- twListToDF(tweets)
  #limpio aquellos no geolocalizados y que sean respuestas
  tweets.df<-tweets.df[!is.na(tweets.df$longitude),]
  #inserto en csv para análisis posterior
  write.csv2(tweets.df,file=paste("D:\\tweets_",topic,"_",format(Sys.time(),"%d%m%y%H%M%S"),".csv", sep=""))  
}
