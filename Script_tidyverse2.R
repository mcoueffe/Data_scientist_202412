################# Exercices Tidyverse 2

# Import des données
rg <- read_csv("rolandgarros2013.csv")

# ---- Q2
rg %>% filter(Round == 6) %>% 
  select(Player1, Player2)

# ---- Q3
rg %>% summarise(moy_aces = mean(ACE.1 + ACE.2)) %>%
  select(moy_aces)

# ---- Q4
rg %>% group_by(Round) %>% 
  summarise(moy_aces = mean(ACE.1 + ACE.2)) %>%
  select(Round, moy_aces)

# ---- Q5
joueurs1 <- rg %>% select(Joueur = Player1)
joueurs2 <- rg %>% select(Joueur = Player2)

rg_joueurs <- bind_rows(joueurs1, joueurs2) %>% 
  distinct()

# ---- Q6
rg_victoires_j1 <- rg %>% group_by(Joueur = Player1) %>% 
  summarise(nb_victoires = sum(Result))

rg_victoires_j2 <- rg %>% group_by(Joueur = Player2) %>% 
  summarise(nb_victoires = sum(1 - Result))

rg_victoires <- bind_rows(rg_victoires_j1, rg_victoires_j2) %>% 
  group_by(Joueur) %>% 
  summarise(Victoires = sum(nb_victoires)) %>% 
  arrange(-Victoires)

# Autre méthode
RG_victoires <- function(joueur){
  return(rg %>% filter((Player1 == joueur & Result == 1) | (Player2 == joueur& Result == 0)) %>% 
    nrow())
}

rg_joueurs <- rg_joueurs %>% rowwise %>% mutate(Victoires = RG_victoires(Joueur)) %>% 
  arrange(-Victoires)

# --------------------------------

# ---- Q7
oa <- read_csv("openaustralie2013.csv")
head(oa)

# ---- Q8
tennis <- bind_rows(RG = rg, OA = oa, .id = "Tournoi") 
tail(tennis)

tennis %>% group_by(Tournoi) %>% 
  summarise(nb_match = n())

# ---- Q9
tennis_aces <- tennis %>% group_by(Tournoi, Round) %>% 
  summarise(moy_aces = mean(ACE.1 + ACE.2)) %>%
  select(Tournoi, Round, moy_aces)

tennis_aces_long <-  tennis_aces %>% 
  pivot_longer(cols = moy_aces, names_to = "Variable", values_to = "Value")
tennis_aces_long

tennis_aces_wide <-  tennis_aces %>% 
  pivot_wider(names_from =  Round, values_from = moy_aces)
tennis_aces_wide

# ---- Q10
oa_victoires_j1 <- oa %>% group_by(Joueur = Player1) %>% 
  summarise(nb_victoires = sum(Result))

oa_victoires_j2 <- oa %>% group_by(Joueur = Player2) %>% 
  summarise(nb_victoires = sum(1 - Result))

oa_joueurs <- bind_rows(oa_victoires_j1, oa_victoires_j2) %>% 
  group_by(Joueur) %>% 
  summarise(Victoires = sum(nb_victoires)) %>% 
  arrange(-Victoires)
oa_joueurs

# ---- Q11
rg_joueurs %>% left_join(oa_joueurs, by = "Joueur") %>% 
  select(Joueur, Victoires_RG = Victoires.x, Victoires_OA = Victoires.y)

rg_joueurs %>% right_join(oa_joueurs, by = "Joueur") %>% 
  select(Joueur, Victoires_RG = Victoires.x, Victoires_OA = Victoires.y)

rg_joueurs %>% inner_join(oa_joueurs, by = "Joueur") %>% 
  select(Joueur, Victoires_RG = Victoires.x, Victoires_OA = Victoires.y)

rg_joueurs %>% full_join(oa_joueurs, by = "Joueur") %>% 
  select(Joueur, Victoires_RG = Victoires.x, Victoires_OA = Victoires.y)
