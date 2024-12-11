# ---------------- Exercices JSON

library(jsonlite)
library(dplyr)

# ---- Iris de Fisher

# ---- Q1
objet <- toJSON(list(name="Test", valeur=42))
objet

# ---- Q2
fromJSON(objet)

# ---- Q3
iris_json <- toJSON(iris, pretty = TRUE)
iris_json

# ---- Q4
toJSON(iris, pretty = TRUE, dataframe = "columns")
toJSON(iris, pretty = TRUE, dataframe = "rows")
toJSON(iris, pretty = TRUE, dataframe = "values")

# ---- Q5
stream_out(iris)

# ---- Q6
stream_out(iris, con = file("iris.json"))

# ---- Q7
stream_in(con = file("iris.json"))

# ---------------------------------------------------------------------------
# ---- Star Wars API

# ---- Q1
planets <- fromJSON("https://swapi-node.vercel.app/api/planets")
planets

# ---- Q2
planets[["results"]]
dim(planets[["results"]]) # 10 planètes

planets[["count"]] # 60 planètes en tout
planets[["next"]]

# ---- Q3
planets_tb <- tibble(planets[["results"]])
for (i in 2:(planets[["pages"]])){
  temp <- fromJSON(paste0("https://swapi-node.vercel.app/api/planets?page=", i))
  temp <- tibble(temp[["results"]])
  planets_tb <- bind_rows(planets_tb, temp)
}
planets_tb

# Option 2
planets_tb <- tibble(planets[["results"]])
page_next <- planets[["next"]]
for (i in 2:(planets[["pages"]])){
  temp <- fromJSON(paste0("https://swapi-node.vercel.app", page_next))
  page_next <- temp[["next"]]
  temp <- tibble(temp[["results"]])
  planets_tb <- bind_rows(planets_tb, temp)
}
planets_tb

# ---- Q4
stream_out(planets_tb, con = file("planets.json"))
