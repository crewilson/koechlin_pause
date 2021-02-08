

library(ggplot2) 
library(tidyr) 
library(dplyr) 
library(lubridate) 
library(svDialogs)

#-------récupérer les données--------

#ouvre les data du singe
monkey <- dlgInput("Enter monkey", Sys.info()["user"])$res
save.path <- file.path("/Users/coque/documents/01 Cours/Semestre 8/stage/les pauses/Data1");
load(file = paste(save.path,"/Data_Iris_",monkey,".Rdata",sep = ""))

#ouvre les pauses du singe
save.path <- file.path("/Users/coque/documents/01 Cours/Semestre 8/stage/les pauses/Data1");
load(file = paste(save.path,"/Data_Iris_Pauses_",monkey,".Rdata",sep = ""))

#-------comparer le nombre de switch (78) par rapport au nombre de pauses---------

Nb.Sess1 <-Data %>% group_by(session) %>% filter(event==78) %>% count(event)
Nb.Sess2 <- DataPause %>%group_by(session) %>% filter(event==75) %>% count(event)
Nb.Switch <- left_join(Nb.Sess1, Nb.Sess2, by="session")
colnames(Nb.Switch) <- c("session","code Switch","Switch","code Pauses","Pauses");
  # pas besoin : Nb.sess <- subset(Nb.sess, select= -c(xx,yy));
remove(Nb.Sess1, Nb.Sess2)

ggplot(Nb.Switch, aes(x=session)) + 
  geom_point(aes(y=Switch),color='blue') + geom_point(aes(y=Pauses),color='black')

Nb.Switch$rapport <- Nb.Switch$Pauses/Nb.Switch$Switch

ggplot(Nb.Switch, aes(x=session, y=rapport)) +
  geom_point(color='blue') + geom_smooth(color='blue')

#-------comparer le nombre de récompenses (65) par rapport au nombre de pauses---------

Nb.Sess1 <-Data %>% group_by(session) %>% filter(event==65) %>% count(event)
Nb.Sess2 <- DataPause %>%group_by(session) %>% filter(event==75) %>% count(event)
Nb.Reward <- left_join(Nb.Sess1, Nb.Sess2, by="session")
colnames(Nb.Reward) <- c("session","code Reward","Reward","code Pauses","Pauses");
# pas besoin : Nb.sess <- subset(Nb.sess, select= -c(xx,yy));
remove(Nb.Sess1, Nb.Sess2)

ggplot(Nb.Reward, aes(x=session)) + 
  geom_point(aes(y=Reward),color='orange') + geom_point(aes(y=Pauses),color='black')

Nb.Reward$rapport <- Nb.Reward$Pauses/Nb.Reward$Reward

ggplot(Nb.Reward, aes(x=session, y=rapport)) +
  geom_point(color='orange') + geom_smooth(color='orange')

#-------comparer le nombre de tâche completée (103) par rapport au nombre de pauses---------

Nb.Sess1 <-Data %>% group_by(session) %>% filter(event==103) %>% count(event)
Nb.Sess2 <- DataPause %>%group_by(session) %>% filter(event==75) %>% count(event)
Nb.Complet <- left_join(Nb.Sess1, Nb.Sess2, by="session")
colnames(Nb.Complet) <- c("session","code Complet","Complet","code Pauses","Pauses");
# pas besoin : Nb.sess <- subset(Nb.sess, select= -c(xx,yy)); #remove superfluous columns
remove(Nb.Sess1, Nb.Sess2)

ggplot(Nb.Complet, aes(x=session)) + 
  geom_point(aes(y=Complet),color='pink') + geom_point(aes(y=Pauses),color='black')

Nb.Complet$rapport <- Nb.Complet$Pauses/Nb.Complet$Complet

ggplot(Nb.Complet, aes(x=session, y=rapport)) +
  geom_point(color='pink') + geom_smooth(color='pink')

#-------comparer le nombre d'incorrect (69) par rapport au nombre de pauses---------

Nb.Sess1 <-Data %>% group_by(session) %>% filter(event==69) %>% count(event)
Nb.Sess2 <- DataPause %>%group_by(session) %>% filter(event==75) %>% count(event)
Nb.Neg <- left_join(Nb.Sess1, Nb.Sess2, by="session")
colnames(Nb.Neg) <- c("session","code Erreur","Erreur","code Pauses","Pauses");
# pas besoin : Nb.sess <- subset(Nb.sess, select= -c(xx,yy)); #remove superfluous columns
remove(Nb.Sess1, Nb.Sess2)

ggplot(Nb.Neg, aes(x=session)) + 
  geom_point(aes(y=Erreur),color='red') + geom_point(aes(y=Pauses),color='black')

Nb.Neg$rapport <- Nb.Neg$Pauses/Nb.Neg$Erreur

ggplot(Nb.Neg, aes(x=session, y=rapport)) +
  geom_point(color='red') + geom_smooth(color='red')

#-------comparer le nombre de fb positifs (68) par rapport au nombre de pauses---------

Nb.Sess1 <-Data %>% group_by(session) %>% filter(event==68) %>% count(event)
Nb.Sess2 <- DataPause %>%group_by(session) %>% filter(event==75) %>% count(event)
Nb.Pos <- left_join(Nb.Sess1, Nb.Sess2, by="session")
colnames(Nb.Pos) <- c("session","code FB positif","FB.positif","code Pauses","Pauses");
# pas besoin : Nb.sess <- subset(Nb.sess, select= -c(xx,yy)); #remove superfluous columns
remove(Nb.Sess1, Nb.Sess2)

ggplot(Nb.Pos, aes(x=session)) + 
  geom_point(aes(y=FB.positif),color='red') + geom_point(aes(y=Pauses),color='black')

Nb.Pos$rapport <- Nb.Pos$Pauses/Nb.Pos$FB.positif

ggplot(Nb.Pos, aes(x=session, y=rapport)) +
  geom_point(color='green') + geom_smooth(color='green')
