 # Projet Portant sur la création d'un DashBoard ---------------------------

 # 0 - Packages ------------------------------------------------------------
 library(readr)
 library(stringr)
 library(purrr)
 library(dplyr)
 library(tidyr)
 library(ggplot2)
 
 # 1 - Importation ---------------------------------------------------------
 
 #On importe le fichier atp_players contenant les identifiants de tous les joueurs
 #ayant participe a au moins 1 match sur le circuit ATP
 players <- read_csv(file = "./tennis_atp/atp_players.csv", col_names = FALSE)
 names(players) <- c("id", "firstname", "lastname", "hand", "birthday", "nat")
 
 # 2 - Preparation des donnees ---------------------------------------------
 
 #Liste des noms des fichiers de tennis_atp
 lst <- list.files(path = "./tennis_atp")
 
 #On filtre pour ne garder que les noms des fichiers de la forme atp_matches_xxxx.csv
 lst_data <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
 
 #On cree le vecteur des noms des tibbles qui seront crees lors de l'importation
 lst_names <- paste('atp', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
 
 #On effectue une boucle avec map pour importer les donnees et stocker les tibbles dans une liste
 lst_tib <- map(.x = lst_data, function (x) read_csv(paste("./tennis_atp/", x, sep = "")))
 
 #On renomme les composantes de la liste des tibbles
 names(lst_tib) <- lst_names
 lst_tib <- list(53)
 years <- 1968:2020
 for (year in seq_along(years))
 {lst_tib[[year]] <- read_csv(file = paste("./tennis_atp/", "atp_matches_", years[year], ".csv", sep = ""))}
 
 #On regroupe tous les tibbles contenus dans la liste lst_tib
 atp <- reduce(.x = lst_tib, .f = bind_rows)
 str(atp)  #on a donc regroupe 176.951 matches de tennis depuis 1968
 
 # 3 - Performances de Del Potro -------------------------------------------
 
 #On commence par extraire son identifiant dans la base des joueurs
 players %>% 
   filter(firstname == 'Juan Martin' & lastname == 'Del Potro') %>%
   select(id) %>% 
   as.numeric() -> id_delpo   #numero 105 223
 
 #On recupere tous les matchs joues par Delpo au cours de sa carriere
 atp %>%
   filter(winner_id == id_delpo | loser_id == id_delpo) -> delpo   #620 matchs en tout
 
 #Matchs par surface
 delpo %>%
   mutate(surface = factor(surface, levels = names(sort(table(surface))), ordered = TRUE)) %>%
   group_by(surface) %>%
   summarize(`Nombre de matchs` = n()) %>%
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
          result =case_when(winner_name == 'Juan Martin del Potro' ~ "Won",
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
          result =case_when(winner_name == 'Juan Martin del Potro' ~ "Won",
                            winner_name != 'Juan Martin del Potro' ~ 'Lost')) %>%
   select(tourney_id,  tourney_date, tourney_name, tourney_level, surface, round, result) %>%
   mutate(tourney_result = case_when(round == 'F' & result == 'Won' ~ 'W',   #W en cas de victoire finale
                                     result == 'Lost' ~ round)) %>%   #rang du tour de la defaite sinon
   filter(!is.na(tourney_result)) %>%
   select(-round, -result) %>%
   distinct() %>%
   arrange(tourney_date)-> tournoi_delpo
 
 # 4 - Graphiques des competitions -----------------------------------------
 
 #Repartition des matchs joues par Delpo en fonction de la surface, sur toute sa carriere
 surfaces %>%
   mutate(textpos = cumsum(`Nombre de matchs`) - `Nombre de matchs`/2) %>%
   ggplot(mapping = aes(x = 1, y = `Nombre de matchs`, fill = Surface)) +
   geom_col(width = 1) + 
   coord_polar(theta = 'y', start = 0) +
   scale_fill_discrete(type = c('Terre battue' = 'chocolate', 'Herbe' = 'chartreuse4', 'Dur' = "deepskyblue4")) + 
   theme_void() +
   geom_text(aes(y = textpos, 
                 label = paste(`Nombre de matchs`, ' (', round_perc(`Nombre de matchs`/sum(`Nombre de matchs`)*100,1), 
                               '%' ,')', sep = '')),
             size=5)
 
 #Nombre de matchs gagnes et perdus contre les adversaires de Delpo au cours de sa carriere
 bilan_opp %>%
   pivot_longer(cols = c(Won, Lost), names_to = "result", values_to = "eff") %>%
   ggplot(mapping = aes(x = reorder(opponent, Total), y = eff, fill = result)) +
   geom_col() +
   scale_fill_discrete(type = c('Lost' = "tomato3", "Won" = "springgreen3")) +
   coord_flip() +
   theme_bw() +
   labs(y = "Nombre de confrontations", x = 'Adversaires') +
   geom_text(mapping = aes(y = -0.1, label = Total), size = 2) +
   labs(fill = "Resultat")
 
 