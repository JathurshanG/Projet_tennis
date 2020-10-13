###########################################################################
# Projet Portant sur la création d'un DashBoard ---------------------------
###########################################################################
#setwd
getwd()
setwd("C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis")

# Library / Require -------------------------------------------------------

library(tidyverse)
library(dplyr)
library(stats)
library(readr)
library(lubridate)
library(purrr)
library(stringr)
library(data.table)

# Exportation des données -------------------------------------------------
#Exporter les bases de données nécessaire via github
#git clone  https://github.com/JeffSackmann/tennis_atp


# Exportation de la base joueur en question ---------------------------------------------------
player <- read_csv("tennis_atp/atp_players.csv",col_names = FALSE, locale = locale(),na="empty")
head(player)
colnames(player) <- c("id","Prenom","Nom","Hand","Birth","Nat")

#la variable date contient des valeurs manquante.
#a=which(is.na(player$Birth))
#na_birth=player[a,]
#11582/54938
#player$Birth=ymd(player$Birth)
##warning message :  125 failed to parse


# Exportation des Résultat de match (simple) ------------------------------
lst <- list.files(path = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp")
lst_data <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("", x, sep = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp/")))
names(lst_tib) <- lst_names
atp <- reduce(.x = lst_tib, .f = bind_rows)


# Exporter base double ----------------------------------------------------
lst <- list.files(path = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp")
lst_data <- grep(pattern = "^atp_matches_doubles_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_double', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("", x, sep = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp/")))
names(lst_tib) <- lst_names
atp_double <- reduce(.x = lst_tib, .f = bind_rows)



# Exporter base Futures ---------------------------------------------------
lst <- list.files(path = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp")
lst_data <- grep(pattern = "^atp_matches_futures_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_futures', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("", x, sep = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp/"),col_types = cols(tourney_level = col_character())))
#Après un premier essaie à l'imporatation la donnée de 2020, nous posent un problèmes,
#en effet à des variables, tourney_level -> en 2020, a des variables numeriques. 
names(lst_tib) <- lst_names
atp_futures <- reduce(.x = lst_tib, .f = bind_rows)


# Exporter Base Quall_chall -----------------------------------------------
lst <- list.files(path = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp")
lst_data <- grep(pattern = "^atp_matches_qual_chall_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_qual_chall_', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("", x, sep = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp/"),
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
atp_quall_chall <- reduce(.x = lst_tib, .f = bind_rows)


# Exporter base Claseement ----------------------------------------------------
lst <- list.files(path = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp")
lst_data <- grep(pattern = "^atp_rankings_[[:digit:]]{2}s.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_Classement_', str_extract(string = lst_data, pattern = "[[:digit:]]{2}"), sep = "") 
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("", x, sep = "C:/Users/jathu/Desktop/M2-sep/TSASR/Tennis/tennis_atp/")))
names(lst_tib) <- lst_names
atp_classement <- reduce(.x = lst_tib, .f = bind_rows)
#exportation de la base Actuelle.
atp_rankings_current <- read_csv("tennis_atp/atp_rankings_current.csv",
                                 col_names = FALSE, locale = locale(date_names = "fr"))
colnames(atp_classement)->colnames(atp_rankings_current)


atp_classement %>%
  bind_rows(atp_rankings_current) %>%
  mutate(ranking_date = ymd(ranking_date))->atp_classement
#verif Ok
max(atp_classement$ranking_date)
#la valeuer max est le 30 dec 2019, la totalité des variables a été ajouté