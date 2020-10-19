 # Projet Portant sur la création d'un DashBoard ---------------------------

 # 0 - Packages ------------------------------------------------------------
 library(tidyverse)
 
 # 1 - Performances de Del Potro -------------------------------------------
 
 #On commence par extraire son identifiant dans la base des joueurs
 player %>% 
   filter(Prenom == 'Juan Martin' & Nom == 'Del Potro') %>%
   select(id) %>% 
   as.numeric() -> id_delpo   #numero 105 223
 
 #On recupere tous les matchs joues par Delpo au cours de sa carriere
 atp %>%
   filter(winner_id == id_delpo | loser_id == id_delpo) -> delpo   #620 matchs en tout
 
 #Matchs par surface
 delpo %>%
   mutate(surface = factor(surface, levels = names(sort(table(surface))), ordered = TRUE)) %>%
   group_by(surface) %>%
   summarize(N = n()) %>%
   #renommage des surfaces --> mieux pour des graphes
   rename(Surface = surface) %>%
   mutate(Surface = case_when(Surface == "Clay" ~ "Terre battue",
                              Surface == "Grass" ~ "Herbe",
                              Surface == "Hard" ~ "Dur")) -> surfaces
 
 #Defaites et victoires selon les opposants
 #on transforme la base pour acceder au resultat et a l'adversaire
 delpo %>%
   select(tourney_name, surface, round, winner_name, loser_name, score, minutes) %>%
   mutate(opponent = case_when(winner_name == 'Juan Martin del Potro' ~ loser_name,
                               winner_name != 'Juan Martin del Potro' ~ winner_name),
          result =case_when(winner_name == 'Juan Martin del Potro' ~ 'Won',
                            winner_name != 'Juan Martin del Potro' ~ 'Lost')) %>%
   select(-winner_name, -loser_name) -> summary_delpo
 
 #Tableau des effectifs croises entre le resultat et l'adversaire
 summary_delpo %>%
   group_by(opponent, result) %>%
   summarize(`Nombre de matchs` = n()) %>%
   ungroup() %>%   #penser a degrouper pour pouvoir calculer le total par la suite
   #on arrange un peu la presentation du tableau
   pivot_wider(names_from = result, values_from  = `Nombre de matchs`) %>%
   mutate(Total = apply(select(., Won, Lost), 1, sum, na.rm = TRUE)) %>%
   arrange(desc(Total)) -> bilan_opp
 
 #Tournois disputes et resultats obtenus
 delpo %>%
   mutate(opponent = case_when(winner_name == 'Juan Martin del Potro' ~ loser_name,
                               winner_name != 'Juan Martin del Potro' ~ winner_name),
          result =case_when(winner_name == 'Juan Martin del Potro' ~ 'Won',
                            winner_name != 'Juan Martin del Potro' ~ 'Lost')) %>%
   select(tourney_id,  tourney_date, tourney_name, tourney_level, surface, round, result) %>%
   mutate(tourney_result = case_when(round == 'F' & result == 'Won' ~ 'W',   #W en cas de victoire finale
                                     result == 'Lost' ~ round)) %>%   #rang du tour de la defaite sinon
   filter(!is.na(tourney_result)) %>%
   select(-round, -result) %>%
   distinct() %>%
   arrange(tourney_date)-> tournoi_delpo
 
 # 2 - Graphiques des competitions -----------------------------------------
 
 #Repartition des matchs joues par Delpo en fonction de la surface, sur toute sa carriere
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
                 position = position_stack(vjust=0.75)) +
   ggtitle("Nombre de matchs joués en fonction du terrain.")
 
 #Nombre de matchs gagnes et perdus contre les adversaires de Delpo au cours de sa carriere
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
