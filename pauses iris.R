
library(ggplot2) 
library(tidyr) 
library(dplyr) 
library(lubridate) 
library(svDialogs)

monkey <- dlgInput("Enter monkey", Sys.info()["user"])$res

save.path <- file.path("/Users/coque/documents/01 Cours/Semestre 8/stage/les pauses/Data1");

load(file = paste(save.path,"/Data_Iris_",monkey,".Rdata",sep = ""))

#filtrer les donn�es pour ne conserver qu'un seul �v�nement
DataEvent <- dplyr::filter(Data,event == 75)

#cr�ation d'une nouvelle colone en d�calant => utiliser fct "lag", default pour le d�calage
DataEvent<-dplyr::mutate(DataEvent, time2 = lag(time, default=0))
#supprimer les colones inutiles (=garder les colones utiles)
DataEvent<-dplyr::select(DataEvent, session, trial, event, time, time2)
#temps entre chaque �v�nement
DataEvent<-dplyr::mutate(DataEvent, ecart=time-time2)
#enl�ve les �carts qui ne correspondent pas � des pauses (d'une session � l'autre)
DataEvent<-dplyr::filter(DataEvent, ecart>0)

#graphiques pour d�terminer ce qu'on consid�re comme une pause (� revoir en fct des r�sutats)
ggplot(DataEvent, aes(x=ecart)) + geom_histogram(binwidth = 1000)
# pour plus tard x, y, alpha, color, fill, linetype, size, weight
# xlim + scale_y_log10()

#s�parer le graphique en 2 parties pour voir o� d�marrent les pauses
ggplot(DataEvent, aes(x=ecart)) + geom_histogram(binwidth = 500) + xlim(0,40000)
ggplot(DataEvent, aes(x=ecart)) + geom_histogram(binwidth = 1000) + xlim(40000,1000000)

ggplot(DataEvent, aes(x=ecart)) + geom_density() + xlim(0,40000)
ggplot(DataEvent, aes(x=ecart)) + geom_density() + xlim(40000,1000000)

#ok maintenant on a d�termin� la pause => quand dans la session
DataPause<-dplyr::filter(DataEvent,ecart>120000)
save(file = paste(save.path,"/Data_Iris_Pauses_",monkey,".Rdata",sep = ""),DataPause)
#graphique pause
ggplot(DataPause, aes(x=time)) + geom_histogram()
ggplot(DataPause, aes(x=time)) + geom_density()


#on veut voir la relation entre un �v�nement (les pauses) et un autre
DataEvent2 <- dplyr::filter(Data,event == 101)
