 # Projet Portant sur la création d'un DashBoard ---------------------------

 # 0 - Packages ------------------------------------------------------------
 library(tidyverse)
 
 # 1 - Performances d'un joueur -------------------------------------------
 
 #Automatisation du code pour qu'il s'adapte à tous les joueurs
 Firstname <- 'Juan Martin'
 Lastname <- 'Del Potro'
 
 #On commence par extraire son identifiant dans la base des joueurs
 player %>% 
   filter(Prenom == Firstname & Nom == Lastname) %>%
   select(id) %>% 
   as.numeric() -> id_joueur   #numero 105 223
 
 #On recupere tous les matchs joues par Delpo au cours de sa carriere
 atp %>%
   filter(winner_id == id_joueur | loser_id == id_joueur) -> joueur  #620 matchs en tout
 
 #Matchs par surface
 joueur %>%
   mutate(surface = factor(surface, levels = names(sort(table(surface))), ordered = TRUE)) %>%
   group_by(surface) %>%
   summarize(N = n()) %>%
   rename(Surface = surface) %>%
   mutate(Surface = case_when(Surface == "Clay" ~ "Terre battue",
                              Surface == "Grass" ~ "Herbe",
                              Surface == "Hard" ~ "Dur")) -> surfaces
 
 #Defaites et victoires selon les opposants
 joueur %>%
   select(tourney_name, surface, round, winner_name, winner_id, loser_name, loser_id, score, minutes) %>%
   mutate(opponent = case_when(winner_id == id_joueur ~ loser_name,
                               winner_id != id_joueur ~ winner_name),
          result =case_when(winner_id == id_joueur ~ 'Won',
                            winner_id != id_joueur ~ 'Lost')) %>%
   select(-winner_name, -loser_name, -winner_id, -loser_id) -> summary_joueur

 
 #Tableau des effectifs croises entre le resultat et l'adversaire
 summary_joueur %>%
   group_by(opponent, result) %>%
   summarize(N = n()) %>%
   ungroup() %>%   #penser a degrouper pour pouvoir calculer le total par la suite
   #on arrange un peu la presentation du tableau
   pivot_wider(names_from = result, values_from  = N) %>%
   mutate(Total = apply(select(., Won, Lost), 1, sum, na.rm = TRUE)) %>%
   arrange(desc(Total)) -> bilan_opp
 
 #Tournois disputes, adversaires et resultats obtenus
 joueur %>%
   mutate(opponent = case_when(winner_id == id_joueur ~ loser_name,
                               winner_id != id_joueur ~ winner_name),
          result =case_when(winner_id == id_joueur ~ 'Won',
                            winner_id != id_joueur ~ 'Lost'),
          tourney_result = case_when(round == 'F' & result == 'Won' ~ 'W',   #W en cas de victoire finale
                                     result == 'Lost' ~ round)) %>% #rand du tour de la defaite sinon
   select(tourney_name, winner_name, winner_id, loser_name, loser_id, opponent, tourney_result, tourney_date, result) %>%
   filter(!is.na(tourney_result)) %>%
   distinct() %>%
   arrange(tourney_date)-> tournoi_joueur

 View(tournoi_joueur)
 
 # 2 - Graphiques des competitions -----------------------------------------
 
 #Repartition des matchs joues en fonction de la surface, sur toute sa carriere
 surfaces %>%
   drop_na() %>% #NA supprimé
   mutate(textpos = cumsum(N) - N/2) %>%
   ggplot(mapping = aes(x = 1, y = N, fill = Surface)) +
   geom_col(position = 'stack', width = 1) +
   coord_polar(theta = 'y', start = 0) +
   scale_fill_discrete(type = c('Terre battue' = 'chocolate', 'Herbe' = 'chartreuse4', 'Dur' = "deepskyblue4")) + 
   theme_void() +
   geom_text(aes(y = textpos, 
                 label = paste(N, ' (', round(N/sum(N)*100,1), '%' ,')', sep = '')),
                 size=5, 
                 position = position_stack(vjust=0.5)) +
   ggtitle("Nombre de matchs joués en fonction du terrain.")
 
 #Nombre de matchs gagnes et perdus en focntion des adversaires au cours de sa carriere
 bilan_opp_reduit <- bilan_opp[1:30,] #sélection es 30 joueurs qu'il a le plus affronté, sinon le graphique était illisible

 bilan_opp_reduit%>%
   pivot_longer(cols = c(Won, Lost), names_to = "result", values_to = "eff") %>%
   ggplot(mapping = aes(x = reorder(opponent, Total), y = eff, fill = result)) +
   geom_col() +
   scale_fill_discrete(type = c('Lost' = "tomato3", "Won" = "springgreen3")) +
   coord_flip() +
   theme_bw() +
   labs(y = "Nombre de confrontations", x = 'Adversaires') +
   geom_text(mapping = aes(y = -0.1, label = Total), size = 2) +
   labs(fill = "Résultat")
 
 #Grand chelem
 tournoi_joueur %>%
    filter(tourney_name %in% c('Roland Garros', 'Australian Open', 'Wimbledon', 'US Open')) %>%
    group_by(tourney_name) %>%
    summarize(N = n()) %>%
    arrange(desc(N)) %>%
    mutate(textpos = cumsum(N) - N/2) %>%
    ggplot(mapping = aes(x = 1, y = N, fill = reorder(tourney_name,N))) +
    geom_col(position = 'stack') +
    coord_polar(theta = 'y', start = 0) +
    theme_void() +
    geom_text(aes(y = textpos, 
                  label = paste(N,paste0("(",round((N/sum(N))*100),"%)"))),
                  size=4) +
    ggtitle("Tournois du grand chelem joués.") +
    labs(fill = "Nom du tournoi")
 
    
 
 #Résultat par tournoi Grand chelem
 joueur %>%
    filter(tourney_name %in% c('Roland Garros', 'Australian Open', 'Wimbledon', 'US Open')) %>%
    mutate(result =case_when(winner_id == id_joueur ~ 'Gagné',
                             winner_id != id_joueur ~ 'Perdu')) %>%
    group_by(tourney_name, result) %>%
    summarise(N=n()) %>%
    arrange(N) %>%
    mutate(y2=round((N/sum(N))*100), ypos= cumsum(N)-N/2, labels= paste(N,paste0("(",y2,"%)")) )  %>%
    ggplot(mapping=aes(x=reorder(tourney_name,desc(N)), y=N, fill=result))+
    geom_col()+
    scale_fill_discrete(type = c('Perdu' = "tomato3", "Gagné" = "springgreen3")) +
    theme_bw() +
    labs(y = "Nombre de confrontations", x = "Nom du tournoi") +
    geom_text(mapping = aes(y = ypos, label = labels), size = 4) +
    ggtitle("Matchs gagnés et matchs perdus par tournoi du grand chelem") +
    labs(fill = "Résultat")
 
 
 
 #Evolution au classement ATP depuis les debuts en professionnel
 atp_classement %>%
    filter(player == 105223) -> classement_delpo   #classement hebdomadaire
 
 classement_delpo <- classement_delpo[-c(1:31),]   #on ne garde qu'a partir de 2005
 
 ggplot(classement_delpo, aes(x = ranking_date, y = rank)) + 
    labs(title = "Evolution du classement ATP de Del Potro depuis 2005", x = "Années", y = "Classement") +
    geom_line(col = "blue") + 
    scale_y_reverse() +
    geom_hline(yintercept = 100, col = "red")





#rm(list = ls())
library(fmsb)
# Qui est Del Potro -------------------------------------------------------
rm(list=c('lst','lst_data','lst_names',"lst_tib"))
library(DT)

player %>%
  mutate(Hand=case_when(Hand=="R"~"Droitier",
                        Hand=="L"~"Gaucher",
                        Hand=='A'~"Ambidextre",
                        Hand=="U"~"Inconnu"))->a

a<-a[which(player$id==id_joueur),]
b=fread("https://sql.sh/ressources/sql-pays/sql-pays.csv",encoding = 'UTF-8')
b<- b[which(b$V4==a$Nat),5]
Nom<-a$Prenom
Prenom<-a$Nom
DTN <- format(a$Birth,"%d %B %Y")
Age=year(today())-year(a$Birth)
Main <- a$Hand
Nationalite<-b$V5
a<-(paste("le Joueur selectionné est ",a$Nom, a$Prenom,
          "né le ",format(a$Birth,"%d %B %Y"),".","Il est ",a$Hand,".","il vient de ",b$V5))
a
rm(list=c("a","b"))


#Analyse du premier match !
atp %>%
  filter(winner_id==id_joueur | loser_id==id_joueur) %>%
  summarise(tourney_date)%>%
  min() %>%
  ymd() -> b
#Le premier match de Del est le 2006-01-03 perdu 


# Evolution de la Cariière del Del Gang -----------------------------------
atp_classement %>%
  filter(player==id_joueur) %>%
  ggplot(aes(x=ranking_date,y=points))+
  geom_line()+
  theme_grey()+
  xlab("Date")+
  ylab("Point Marquée")+
  ggtitle(paste('Graphique representant les points Marqué Par',Lastname,sep=" ")) -> Graph_point
Graph_point



#Graphique utile
potro_radar <- data.frame(
  "Rafael" = c(1,0,0.33),
  "Federer" = c(1,0,0.28),
  "Djokovic" = c(1,0,0.2))
radarchart(potro_radar,axistype=1 , 
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 
)
title("Del Potro face aux top3")

library(tidyverse)

#on trie les matchs par surfaces
battue<-filter(joueur, surface == "Clay")
nrow(battue) #121 matchs sur terre battue
battue<-filter(joueur, surface == "Clay" & winner_id == 105223)  #84 victoires
nrow(battue)


tapis<-filter(joueur, surface == "Carpet")
nrow(tapis)#8 matchs sur tapis

tapis<-filter(joueur, surface == "Capert" & winner_id == 105223) #6 victoires
nrow(tapis)


dur<-filter(joueur, surface == "Hard")
view(dur)
nrow(dur) #433 matchs sur dur
dur<-filter(joueur, surface == "Hard" & winner_id == 105223) #313 victoires
nrow(dur)

herbe<-filter(joueur, surface == "Grass")
nrow(herbe) #58 macths sur herbe
herbe<-filter(joueur, surface == "Grass" & winner_id ==105223)#40 victoires
nrow(herbe)

del_radar <- data.frame(     #1 et 0 sont les bornes max et min , 1 et 0 parce qu'on
  "Herbe" = c(1,0,40/58),   # a fait le nbre de victoire sur le nbre total de match
  "Dur" = c(1,0,313/433),   # on pouvait multiplier par 100 
  "Terre battue" = c(1,0,84/121),
  "Tapis"= c(1,0,6/8)
)
view(del_radar)
library(fmsb)
radarchart(del_radar,axistype=1 , 
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 
)
title("Caractéristiques de Del Potro")

library(tidyverse)
View(joueur)
#ses meilleurs années, on suppose que ses meilleures années sont celles 
#où il est dans top 5 
joueur%>%
  filter(winner_rank <= 5 & winner_id == 105223)-> annee_meil ##on a ses matchs où il gagnait et etait dans le top10 
view(annee_meil) 
str(annee_meil$tourney_id)
unique(annee_meil$tourney_id)
#voir la 1e colonne, les dates sont 2009;2010;2013;2014;2018;2019
joueur%>%
  filter(loser_id == 105223 & winner_rank <= 5)-> los5

#Nombre de points par tournoi gagné
#roland garros rg
joueur%>%
  filter(tourney_name == "Roland Garros" & winner_id == 105223) ->rga
view(rga$winner_rank_points)
liste<-rga$winner_rank_points #on peut faire la moyenne
moy<-mean(liste) #3348,32 points en moy au roland garos

#autralie open
joueur%>%
  filter(tourney_name == "Australian Open" & winner_id == 105223) ->aop
moy_aop<-mean(aop$winner_rank_points) #3495 points en moy en Open Aust

#Wimbledon
joueur%>%
  filter(tourney_name == "Wimbledon" & winner_id == 105223) ->wbn
moy_wbn<-mean(wbn$winner_rank_points) #2996,28 points en moy en winbledon

#Us open
joueur%>%
  filter(tourney_name == "US Open" & winner_id == 105223) ->uso
moy_uso<-mean(uso$winner_rank_points)#3102,77 points en moy en US open

