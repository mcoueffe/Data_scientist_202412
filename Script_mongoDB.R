# ----------- Exercices MONGODB

library(mongolite)
library(tidyverse)

# ---- Q1
m <- mongo("planets")

if(m$count() > 0) m$drop()
m$count()

# ---- Q2
m$import(file("planets.json"))
m$count()

# ---- Q3
tibble(
  m$find(
  query='{
 "rotation_period": "25"
 }',
 fields='{"_id": false, "name": true, "rotation_period": true}'
 ) %>% head()
)

# ---- Q4
tb <- tibble(
  m$find(
    query='{
 "rotation_period": "25"
 }',
    fields='{"_id": false, "name": true, "rotation_period": true, "orbital_period": true,
    "diameter": true}'
  ) %>% head()
)

# ---- Q5
tb %>% arrange(desc(diameter))

# ---- Q6
m$drop()
m$import(file("planets.json"))
df <- tibble(m$find())

df2 <- df %>% select(name, rotation_period, orbital_period, diameter, surface_water, population,
              climate, terrain) %>% 
  mutate(rotation_period = as.numeric(rotation_period),
         orbital_period = as.numeric(orbital_period),
         diameter = as.numeric(diameter),
         surface_water = as.numeric(surface_water),
         population = as.numeric(population),
         climate = str_split(gsub(climate, pattern = " ", replacement = ""), pattern = ","),
         terrain = str_split(gsub(terrain, pattern = " ", replacement = ""), pattern = ","))

# ---- Q7
m$drop()
m$insert(df2)

tibble(
  m$find(
    query='{
 "rotation_period": 25
 }',
    fields='{"_id": false, "name": true, "rotation_period": true, "orbital_period": true,
    "diameter": true}'
  ) %>% head()
)  %>% arrange(-diameter)

# ---- Q8
tibble(
  m$find(
    query='{
 "name": {"$regex": "^T"}
 }',
    fields='{"_id": false, "name": true, "rotation_period": true, "orbital_period": true,
    "diameter": true}'
  ) %>% head()
)

# ---- Q9
tb <- tibble(
  m$find(
    query='{
 "$and": [
 {"diameter": {"$gt": 10000}},
 {"terrain": "mountains"}
 ]
 }',
    fields='{"_id": false, "name": true, "diameter": true, "terrain": true}'
  ) 
)

# ---- Q10
m$remove(
  query='{
 "name": "unknown"
 }'
) 

# ---- Q11
m$aggregate('[
 {
 "$group": {
 "_id": null,
 "count": { "$sum": 1 }
 }
 }
 ]')

# Vérification
tibble(m$find()) %>% summarise(n = n())

# ---- Q12
m$aggregate('[
 {"$match": {"terrain": "glaciers"}},
 {
 "$group": {
 "_id": null,
 "diametre_moy": { "$avg": "$diameter" },
 "population": {"$sum": "$population"}
 }
 }
 ]')

