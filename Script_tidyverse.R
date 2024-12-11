library(tidyverse)

# -----------------------------------------------------------------
#       Iris

# Sélection de variables
iris <- as_tibble(iris)
iris %>% select(Petal.Width, Species)

# Filtre
tib1 <- iris %>% 
  filter(Species == "versicolor" | Species == "virginica")
head(tib1)
class(tib1)

# Nombre d'iris setosa
iris %>% filter(Species == "setosa") %>% 
  summarise(n = n())

# Moyenne
iris %>% filter(Species == "versicolor") %>% 
  summarise(moyenne = mean(Petal.Width))

# Création d'une variable
iris %>% mutate(Sum.Width = Petal.Width + Petal.Length)

# Moyenne et variance pour chaque espèce
iris %>% group_by(Species) %>% 
  summarise(moyenne = mean(Sepal.Length),
            variance = sd(Sepal.Length))

# -----------------------------------------------------------------
#       Houston flights

library(hflights)
hflights <- as_tibble(hflights)

# Sélection de variables
hflights %>% select(ends_with("Time"))

hflights %>% select(matches("D.*st") | starts_with("Taxi"))

# Création de variable
hflights %>% mutate(ActualGroundTime = ActualElapsedTime - AirTime)

hflights %>% mutate(AverageSpeed = Distance / AirTime) %>% 
  arrange(-AverageSpeed) %>% 
  select(AverageSpeed)

# Filtre
hflights %>% filter(Dest == "JFK") %>% 
  summarise(n = n())

# Résumé
hflights %>% summarise(n = n(),
                       n_dest = n_distinct(Dest),
                       n_carrier = n_distinct(UniqueCarrier))

hflights %>% filter(UniqueCarrier == "AA") %>% 
  summarise(n_vols = n(),
            n_vols_annules = sum(Cancelled),
            moyenne_delay = mean(ArrDelay, na.rm = TRUE))

# Groupes
hflights %>% group_by(UniqueCarrier) %>% 
  summarise(n = n(),
            moy = mean(AirTime, na.rm = TRUE))

# Groupe et tri
hflights %>% group_by(UniqueCarrier) %>% 
  summarise(retard_moy = mean(DepDelay, na.rm = TRUE)) %>% 
  arrange(-retard_moy)
