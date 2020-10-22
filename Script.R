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

