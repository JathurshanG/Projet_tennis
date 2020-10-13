<<<<<<< HEAD
# Library / Require -----------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)

# Exportation des données -----------------------------------------------------------------------------
#Exporter les bases de données nécessaire via github
#git clone  https://github.com/JeffSackmann/tennis_atp


# Importation de la base joueur -----------------------------------------------------------------------
player <- read_csv("tennis_atp/atp_players.csv",col_names = FALSE, locale = locale(),na="empty")
head(player)
colnames(player) <- c("id","Prenom","Nom","Hand","Birth","Nat")
player$Birth=ymd(player$Birth)

#la variable date contient des valeurs manquantes.
#a=which(is.na(player$Birth))
#na_birth=player[a,]
#11582/54938
#player$Birth=ymd(player$Birth)
##warning message :  125 failed to parse


# Importation des résultats de match (simple) ---------------------------------------------------------
lst <- list.files(path = "tennis_atp")
lst_data <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("tennis_atp/", x, sep = "")))
names(lst_tib) <- lst_names
atp <- reduce(.x = lst_tib, .f = bind_rows)
head(atp)


# Importation des résultats de match (double) ---------------------------------------------------------
lst <- list.files(path = "tennis_atp")
lst_data <- grep(pattern = "^atp_matches_doubles_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_double', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("tennis_atp/", x, sep = "")))
names(lst_tib) <- lst_names
atp_double <- reduce(.x = lst_tib, .f = bind_rows)
head(atp_double)


# Importation base futures ----------------------------------------------------------------------------
lst <- list.files(path = "tennis_atp")
lst_data <- grep(pattern = "^atp_matches_futures_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_futures', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("tennis_atp/", x, sep = ""),col_types = cols(tourney_level = col_character())))
names(lst_tib) <- lst_names
atp_futures <- reduce(.x = lst_tib, .f = bind_rows)
head(atp_futures)


# Importation base qual_chall -------------------------------------------------------------------------
lst <- list.files(path = "tennis_atp")
lst_data <- grep(pattern = "^atp_matches_qual_chall_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_qual_chall_', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("tennis_atp/", x, sep = ""),
                                                    col_types = cols(best_of = col_double(),draw_size = col_double(), 
                                                                     loser_age = col_double(),loser_ht = col_double(), 
                                                                     loser_id = col_double(),  loser_name = col_character(), 
                                                                     loser_seed = col_character(), match_num = col_double(), 
                                                                     round = col_character(), score = col_character(),
                                                                     surface = col_character(),tourney_date = col_double(), 
                                                                     tourney_id = col_character(),tourney_level = col_character(),
                                                                     tourney_name = col_character(), winner_age = col_double(),
                                                                     winner_hand = col_character(), winner_ht = col_double(),
                                                                     winner_id = col_double(), winner_ioc = col_character(),
                                                                     winner_name = col_character(), winner_seed = col_character())))

#les variables présentent des WC dans la base de données, a ce titre, j'ai décider de configurer chacune des variable,
#en décidant de leur symboles
names(lst_tib) <- lst_names
atp_qual_chall <- reduce(.x = lst_tib, .f = bind_rows)
head(atp_qual_chall)


# Importation base classement -------------------------------------------------------------------------
lst <- list.files(path = "tennis_atp")
lst_data <- grep(pattern = "^atp_rankings_[[:digit:]]{2}s.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_rankings_', str_extract(string = lst_data, pattern = "[[:digit:]]{2}"), sep = "") 
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("tennis_atp/", x, sep = "")))
names(lst_tib) <- lst_names
atp_classement <- reduce(.x = lst_tib, .f = bind_rows)
#Importation de la base actuelle.
atp_rankings_current <- read_csv("tennis_atp/atp_rankings_current.csv",
                                 col_names = FALSE, locale = locale(date_names = "fr"))
colnames(atp_rankings_current) <- colnames(atp_classement)

atp_classement %>%
  bind_rows(atp_rankings_current) %>%
  mutate(ranking_date = ymd(ranking_date))->atp_classement
#verif Ok
max(atp_classement$ranking_date)
#la valeur max est le 09 mars 2020, la totalité des variables a été ajouté
=======
 # Projet Portant sur la création d'un DashBoard ---------------------------
>>>>>>> e34d0e6d0681e567d79bcb02991789b81b182072
