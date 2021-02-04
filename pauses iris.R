
library(ggplot2) 
library(tidyr) 
library(dplyr) 
library(lubridate) 
library(svDialogs)

monkey <- dlgInput("Enter monkey", Sys.info()["user"])$res

save.path <- file.path("/Users/coque/documents/01 Cours/Semestre 8/stage/les pauses/Data1");

load(file = paste(save.path,"/Data_Iris_",monkey,".Rdata",sep = ""))

#filtrer les données pour ne conserver qu'un seul évènement
DataEvent <- dplyr::filter(Data,event == 75)

#création d'une nouvelle colone en décalant => utiliser fct "lag", default pour le décalage
DataEvent<-dplyr::mutate(DataEvent, time2 = lag(time, default=0))
#supprimer les colones inutiles (=garder les colones utiles)
DataEvent<-dplyr::select(DataEvent, session, trial, time, time2)
#temps entre chaque évènement
DataEvent<-dplyr::mutate(DataEvent, ecart=time-time2)
#enlève les écarts qui ne correspondent pas à des pauses (d'une session à l'autre)
DataEvent<-dplyr::filter(DataEvent, ecart>0)

#graphiques pour déterminer ce qu'on considère comme une pause (à revoir en fct des résutats)
ggplot(DataEvent, aes(x=ecart)) + geom_histogram(binwidth = 1000)
# pour plus tard x, y, alpha, color, fill, linetype, size, weight
# xlim + scale_y_log10()

#séparer le graphique en 2 parties pour voir où démarrent les pauses
ggplot(DataEvent, aes(x=ecart)) + geom_histogram(binwidth = 500) + xlim(0,40000)
ggplot(DataEvent, aes(x=ecart)) + geom_histogram(binwidth = 1000) + xlim(40000,1000000)

ggplot(DataEvent, aes(x=ecart)) + geom_density() + xlim(0,40000)
ggplot(DataEvent, aes(x=ecart)) + geom_density() + xlim(40000,1000000)

#ok maintenant on a déterminé la pause => quand dans la session
DataPause<-dplyr::filter(DataEvent,ecart>120000)
#graphique pause
ggplot(DataPause, aes(x=time)) + geom_histogram()
ggplot(DataPause, aes(x=time)) + geom_density()


#on veut voir la relation entre un évènement (les pauses) et un autre
DataEvent2 <- dplyr::filter(Data,event == 101)
