 # Projet portant sur la création d'un DashBoard -------------------------------------------------
 

 # 0 - Préparation de l'environnement de travail -------------------------------------------------
 #Importation des données via le script Export.R
 #Base de données nécessaires pour ce script: player, atp, atp_classement
 #Vider la mémoire
 rm(list=c('lst','lst_data','lst_names',"lst_tib"))
 
 # 1 - Packages ----------------------------------------------------------------------------------
 library(tidyverse)
 library(fmsb)
 library(data.table)
 
 # 2 - Récupération des données du joueur selectionné --------------------------------------------
 
 #Automatisation du code pour qu'il s'adapte à tous les joueurs
 Firstname <- 'Juan Martin'
 Lastname <- 'Del Potro'
 
 #On commence par extraire son identifiant dans la base des joueurs
 player %>% 
   filter(Prenom == Firstname & Nom == Lastname) %>%
   select(id) %>% 
   as.numeric() -> id_joueur   #numero 105 223
 
 #On recupere tous les matchs joues par le joueur au cours de sa carriere
 atp %>%
   filter(winner_id == id_joueur | loser_id == id_joueur) -> joueur  #620 matchs en tout
 
 # 3 - Préparation des jeux de données utilisés pour les graphs ----------------------------------
 
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
 
 # 4 - Identité et caractéristiques du joueur --------------------------------------------------
 
 #Affichage de l'identité
 
 player %>%
   mutate(Hand=case_when(Hand=="R"~"Droitier",
                         Hand=="L"~"Gaucher",
                         Hand=='A'~"Ambidextre",
                         Hand=="U"~"Inconnu")) -> a
 
 a <- a[which(player$id==id_joueur),]
 
 b <- fread("https://sql.sh/ressources/sql-pays/sql-pays.csv",encoding = 'UTF-8')
 b <- b[which(b$V4==a$Nat),5]
 
 Nom <- a$Prenom
 Prenom <- a$Nom
 DTN <- format(a$Birth,"%d %B %Y")
 Age <- year(today())-year(a$Birth)
 Main <- a$Hand
 Nationalite <- b$V5
 
 a <- (paste("Le joueur selectionné est",a$Nom, a$Prenom,
             ", né le",format(a$Birth,"%d %B %Y"),".","Il est",a$Hand,"et il vient de",b$V5))
 a
 rm(list=c("a","b"))
 
 # 5 - Graphiques sur les surfaces de jeu --------------------------------------------------------
 
 #Repartition des matchs joues en fonction de la surface, sur toute sa carriere
 surfaces %>%
   mutate(textpos = cumsum(N) - N/2) %>%
   ggplot(mapping = aes(x = 1, y = N, fill = Surface)) +
   geom_col(position = 'stack', width = 1) +
   coord_polar(theta = 'y', start = 0) +
   scale_fill_discrete(type = c('Terre battue' = 'chocolate', 'Herbe' = 'chartreuse4', 'Dur' = "deepskyblue4", 'Tapis' = "grey")) + 
   theme_void() +
   geom_text(aes(y = textpos, 
                 label = paste(N, ' (', round(N/sum(N)*100,1), '%' ,')', sep = '')),
                 size=5, 
                 position = position_stack(vjust=0.6)) +
   ggtitle("Nombre de matchs joués en fonction du terrain.")
 
 #Radarchart du pourcentage de matchs gagné par sufaces
 # On récupère les matchs gagnés en fonction de la surface
 joueur %>%
   filter(winner_id == id_joueur) %>%
   group_by(surface) %>%
   summarize (G = n()) %>% #Nb matchs gagnés
   rename(Surface = surface) %>%
   mutate(Surface = case_when(Surface == "Carpet" ~ "Tapis",
                              Surface == "Clay" ~ "Terre battue",
                              Surface == "Grass" ~ "Herbe",
                              Surface == "Hard" ~ "Dur")) -> matchs_gagnes
 
 match_joue_gagne <- merge(surfaces,matchs_gagnes, by = "Surface")
 match_joue_gagne %>%
   mutate(max = 1, 
          min = 0, 
          rapport = G/N) %>%
   select('Surface','max','min','rapport') -> radar2
 
 #barplot
 radar2 %>%
   ggplot(mapping = aes(x = Surface, y=rapport*100, fill = Surface)) +
   geom_col() +
   scale_fill_discrete(type = c('Terre battue' = 'chocolate', 'Herbe' = 'chartreuse4', 'Dur' = "deepskyblue4", 'Tapis' = "grey")) + 
   labs(y = "Matchs gagnés", x = "Surface") +
   ggtitle("Pourcentage de matchs gagnés par surface")

 #radarchart
 radar2 <- transpose(radar2)
 colnames(radar2) <- radar2[1,]
 radar2 <- radar2[-1,]
 radar2 <-map_dfr(radar2, as.numeric)
 
 radarchart(radar2,axistype=1 ,
            seg = 10,
            title = "Pourcentage de matchs gagnés par surface",
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,10), cglwd=0.8,
            #custom labels
            vlcex=0.8 )
 
 # 6 - Résultats en fonction des adversaires --------------------------------------------------------
 
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
 
 #Comparaison avec le top 3 mondial (A AUTOMATISER)
 radar1 <- data.frame(
   "Rafael" = c(1,0,0.33),
   "Federer" = c(1,0,0.28),
   "Djokovic" = c(1,0,0.2))
 
 radarchart(radar1,axistype=1 , 
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 ) +
   title("Résultats face au Top3")
 
 # 7 - Résultats aux grands chelems --------------------------------------------------------
 
 #Nombre de matchs joués pargrand chelem
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
 
 #Résultats des grands chelems par année
 rm(annee)
 joueur %>%
   filter(tourney_name %in% c('Roland Garros', 'Australian Open', 'Wimbledon', 'US Open')) %>%
   mutate(year = str_sub(tourney_id, 1, 4),
          result =case_when(winner_id == id_joueur ~ 'Gagné',
                            winner_id != id_joueur ~ 'Perdu')) %>%
   select('tourney_name','tourney_id','year','result','winner_id','loser_id') -> annee
 
 View(annee)
 
 annee %>%
   group_by(year, tourney_name, result) %>%
   summarise(N=n()) %>%
   arrange(desc(N)) %>%
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
   arrange(desc(N)) %>%
   mutate(y2=round((N/sum(N))*100), ypos= cumsum(N)-N/2, labels= paste(N,paste0("(",y2,"%)")) )  %>%
   ggplot(mapping=aes(x=tourney_abv, y=N, fill=result))+
   geom_col()+
   scale_fill_discrete(type = c('Perdu' = "tomato3", "Gagné" = "springgreen3")) +
   facet_wrap(facets = "year", ncol  = 4)+
   theme_bw() +
   labs(y = "Nombre de confrontations", x = 'year') +
   geom_text(mapping = aes(y = ypos, label = N), size = 3) +
   labs(fill = "Résultat")
 
 
 # 8 - Points gagnés par années et tournois -----------------------------------------
 
 #Récupération de la date du premier tournoi
 atp %>%
   filter(winner_id==id_joueur | loser_id==id_joueur) %>%
   summarise(tourney_date)%>%
   min() %>%
   ymd() -> b
 
 #Parcours en carrière professionnelle
 #Points gagnés
 atp_classement %>%
   filter(player==id_joueur) %>%
   ggplot(aes(x=ranking_date,y=points))+
   geom_line()+
   theme_grey()+
   xlab("Date")+
   ylab("Points Marqués")+
   ggtitle(paste('Points marqués par',Lastname,'durant sa carrière professionnelle.',sep=" ")) -> Graph_point
 Graph_point
 
 #partie suivante à retravailler pour voir si on peut sortir des graphs intéressants
 
 #ses meilleurs années (celles où il est dans top 5) 
 atp_classement %>%
   filter(rank <=5 & player == id_joueur)-> best_annee #on a ses matchs où il gagnait et etait dans le top10 
unique(best_annee$ranking_date)
#OU, c'est peut-être plus logique d'utiliser la table atp_classement
 joueur%>%
   filter(winner_rank <= 5 & winner_id == id_joueur)-> annee_meil #on a ses matchs où il gagnait et etait dans le top10 
 view(annee_meil) 
 str(annee_meil$tourney_id)
 unique(annee_meil$tourney_id)
 #les dates sont 2009;2010;2013;2014;2018;2019
 
 #Voir quand il a perdu contre un joueur du top 5
 joueur%>%
   filter(loser_id == id_joueur & winner_rank <= 5)-> los5
 View(los5)
 
 #Nombre de points par tournoi gagné
 #roland garros rg
 joueur%>%
   filter(tourney_name == "Roland Garros" & winner_id == id_joueur) ->rga
 view(rga$winner_rank_points)
 liste<-rga$winner_rank_points #on peut faire la moyenne
 moy<-mean(liste) #3348,32 points en moy au roland garos
 #autralie open
 joueur%>%
   filter(tourney_name == "Australian Open" & winner_id == id_joueur) ->aop
 moy_aop<-mean(aop$winner_rank_points) #3495 points en moy en Open Aust
 #Wimbledon
 joueur%>%
   filter(tourney_name == "Wimbledon" & winner_id == id_joueur) ->wbn
 moy_wbn<-mean(wbn$winner_rank_points) #2996,28 points en moy en winbledon
 #Us open
 joueur%>%
   filter(tourney_name == "US Open" & winner_id == id_joueur) ->uso
 moy_uso<-mean(uso$winner_rank_points)#3102,77 points en moy en US open
 
 
 # 9 - Classement du joueur --------------------------------------------------------
 
 #Evolution au classement ATP depuis les debuts en professionnel

 atp_classement %>%
    filter(player == id_joueur) -> classement_joueur #classement hebdomadaire
 
 ggplot(classement_joueur, aes(x = ranking_date, y = rank)) + 
    labs(title = "Evolution du classement ATP de Del Potro depuis 2005", x = "Années", y = "Classement") +
    geom_line(col = "blue") + 
    scale_y_reverse() +
    geom_hline(yintercept = 100, col = "red")




