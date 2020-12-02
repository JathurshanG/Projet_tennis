#Projet Delpo - Partie 2 - Etude sur toute la carriere


# 0 - Préparation de l'environnement de travail -------------------------------------------------
#Importation des données via le script Export.R
#Base de données nécessaires pour ce script: player, atp, atp_classement
#Vider la mémoire
rm(list=c('lst','lst_data','lst_names',"lst_tib"))


# 1 - Packages ----------------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(fmsb)
library(rvest)
library(magick)
library(formattable) 


# 2 - Récupération des données du joueur selectionné --------------------------------------------

#Automatisation du code pour qu'il s'adapte à tous les joueurs
Firstname <- 'Juan Martin'
Lastname <- 'del Potro'

#On commence par extraire son identifiant dans la base des joueurs
player %>% 
  filter(Prenom == Firstname & Nom == Lastname) %>%
  select(id) %>% 
  as.numeric() -> id_joueur   #numero 105 223 pour Del Popo

#On recupere tous les matchs joues par le joueur au cours de sa carriere
atp %>%
  filter(winner_id == id_joueur | loser_id == id_joueur) -> joueur  #620 matchs en tout


# 3 - Préparation des jeux de données utilisés pour les graphes ----------------------------------

#Matchs par surface
joueur %>%
  mutate(surface = factor(surface, levels = names(sort(table(surface))), ordered = TRUE)) %>%
  group_by(surface) %>%
  summarize(N = n()) %>%
  rename(Surface = surface) %>%
  mutate(Surface = case_when(Surface == "Carpet" ~ "Tapis",
                             Surface == "Clay" ~ "Terre battue",
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


# 4 - Résultats en fonction des adversaires ---------------------------------

#Nombre de matchs gagnes et perdus en focntion des adversaires au cours de sa carriere
bilan_opp_reduit <- bilan_opp[1:20,] #sélection des 20 joueurs qu'il a le plus affronté

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

#Comparaison avec le Big Four de la derniere decennie : Roger Federer, Rafael Nadal, Novak Djokovic et Andy Murray
bilan_opp %>%
  filter(opponent %in% c('Roger Federer', 'Rafael Nadal', 'Novak Djokovic', 'Andy Murray')) %>%
  mutate(ratio = Won/Total,
         max = 1,
         min = 0) %>%
  select('opponent','max','min','ratio') -> radar1

#Tableau
radar1 %>%
  select("opponent","ratio") %>%
  mutate(ratio = round(ratio*100,2)) %>%
  rename('Matchs gagnés (%)' = ratio, Adversaire = opponent) -> tab_top3
formattable(tab_top3) 

#barplot
radar1 %>%
  ggplot(mapping = aes(x = opponent, y=ratio*100, fill = opponent)) +
  geom_col() +
  labs(y = "Pourcentage de matchs gagnés", x = "Adversaire") +
  ggtitle("Matchs gagnés contre le top 3 mondial") +
  labs(fill = "Adversaire")

#radarchart
radar1 <- transpose(radar1)
colnames(radar1) <- radar1[1,]
radar1 <- radar1[-1,]
str(radar1)
radar1 <-map_dfr(radar1, as.numeric)
radarchart(radar1,axistype=1 ,
           seg = 10,
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,10), cglwd=0.8,
           #custom labels
           vlcex=0.8 ) +
  title("Résultats face au Big 4")


# 5 - Résultats en Grand Chelem -------------------------------------------

#Résultats des Grands Chelem par année
joueur %>%
  filter(tourney_name %in% c('Roland Garros', 'Australian Open', 'Wimbledon', 'US Open')) %>%
  mutate(year = str_sub(tourney_id, 1, 4),
         result =case_when(winner_id == id_joueur ~ 'Gagné',
                           winner_id != id_joueur ~ 'Perdu')) %>%
  select('tourney_name','tourney_id','year','result','winner_id','loser_id') -> annee

annee %>%
  group_by(year, tourney_name, result) %>%
  summarise(N=n()) %>%
  arrange(N) %>%
  mutate(y2=round((N/sum(N))*100), ypos= cumsum(N)-N/2, labels= paste(N,paste0("(",y2,"%)")) )  %>%
  ggplot(mapping=aes(x=year, y=N, fill=result))+
  geom_col()+
  scale_fill_discrete(type = c('Perdu' = "tomato3", "Gagné" = "springgreen3")) +
  facet_wrap(facets = "tourney_name", ncol  = 2)+
  theme_bw() +
  labs(y = "Nombre de confrontations", x = 'year') +
  geom_text(mapping = aes(y = ypos, label = N), size = 4) +
  labs(fill = "Résultat")


#Résultats par année en fonction des grands chelems
annee %>%
  mutate(tourney_abv=str_sub(tourney_name,1,3))%>%
  group_by(year, tourney_abv,result) %>%
  summarise(N=n()) %>%
  arrange(N) %>%
  mutate(y2=round((N/sum(N))*100), ypos= cumsum(N)-N/2, labels= paste(N,paste0("(",y2,"%)")) )  %>%
  ggplot(mapping=aes(x=tourney_abv, y=N, fill=result))+
  geom_col()+
  scale_fill_discrete(type = c('Perdu' = "tomato3", "Gagné" = "springgreen3")) +
  facet_wrap(facets = "year", ncol  = 4)+
  theme_bw() +
  labs(y = "Nombre de confrontations", x = 'year') +
  geom_text(mapping = aes(y = ypos, label = N), size = 3) +
  labs(fill = "Résultat")

#tableau
annee %>%
  group_by(year, tourney_name, result) %>%
  summarise(N=n()) %>%
  arrange(year) %>%
  mutate(pourcentage_gagne = case_when(result == 'Gagné' ~ round(N/sum(N)*100,2)),
         pourcentage_perdu = 100 - pourcentage_gagne,
         Nb = sum(N)) %>%
  drop_na() %>%
  select("year","tourney_name","pourcentage_gagne","pourcentage_perdu","Nb") %>%
  rename('Matchs gagnés (%)' = pourcentage_gagne,
         'Matchs perdus (%)' = pourcentage_perdu,
         Tournoi = tourney_name,
         'Année' = year,
         'Nombre de participation au tournoi' = Nb) -> tab_chelem_annee
formattable(tab_chelem_annee) 


# 6 - Points gagnés -------------------------------------------------------

#Récupération de la date du premier tournoi
atp %>%
  filter(winner_id==id_joueur | loser_id==id_joueur) %>%
  summarise(tourney_date)%>%
  min() %>%
  ymd() -> date_premier_tournoi

atp_classement %>%
  filter(player==id_joueur) %>%
  ggplot(aes(x=ranking_date,y=points))+
  geom_line()+
  theme_grey()+
  xlab("Date")+
  ylab("Points Marqués")+
  ggtitle(paste('Points marqués par',Lastname,'durant sa carrière professionnelle.',sep=" "))

# 7 - Classement du joueur ------------------------------------------------

#Année où le joueur est dans le top 3
atp_classement %>%
  filter(rank <=3 & player == id_joueur)-> best_annee_3
annee_top3 <- unique(format(best_annee_3$ranking_date, format = "%Y"))
annee_top3 #2018

#Voir quand il a perdu contre un joueur du top 5
joueur%>%
  filter(loser_id == id_joueur & winner_rank <= 5) %>%
  select("winner_name", "winner_rank","loser_name","winner_id","loser_id","tourney_name","tourney_date") -> los5

#Evolution au classement ATP depuis les debuts en professionnel
atp_classement %>%
  filter(player == id_joueur) -> classement_joueur #classement hebdomadaire

#Classement dans le Top 50
classement_joueur %>%
  filter(rank <= 50) %>%
  ggplot(aes(x = ranking_date, y = rank)) + 
  labs(title = "Evolution du joueur au classement ATP", x = "Années", y = "Classement") +
  geom_line(col = "blue") + 
  scale_y_reverse() +
  geom_hline(yintercept = 3, col = "red")

##Remarque : pour faire cette partie, il est necessaire de mettre aussi les performances des autres grands
#joueurs (Big 4), pour voir si Delpo est au meme niveau qu'eux dans ses grandes annees (comme 2009 ou 2018)




