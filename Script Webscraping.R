# ------------ Exercices Web Scraping
library(rvest)
library(tidyverse)

# ------ Bonsaïs

# ---- Q1
url <- "https://umizenbonsai.com/shop/bonsai/coniferes/"
data_html <- read_html(url)
data_html

# ---- Q2
# On survole le code > on retrouve le li (= liste de puces) > 
# on prend une classe qui se répète pour chaque produit
blocs <- data_html %>% html_nodes("li.entry") 

# --- Q3
blocs[[1]] %>% html_text()

titres <- blocs %>% html_node("li.title h2 a")
prix <- blocs %>% html_node("span.woocommerce-Price-amount") 

Scan <- function(num_bloc){
  titre <-  titres[[num_bloc]] %>% html_text()
  lien <- (titres[[num_bloc]] %>% html_attrs())["href"]
  prix <- prix[[num_bloc]] %>% html_text()
  return(list(titre, lien, prix))
}


for (i in 1:length(blocs)){
  print(i)
  print(paste('Le nom du bonsai est ', Scan(i)[[1]]))
  print(paste('Le lien est ', Scan(i)[[2]]))
  print(paste('Le prix est ', Scan(i)[[3]]))
}

# ---- Q4
bonsai_tb <- tibble(nom = NULL, lien = NULL, prix = NULL)
for (i in 1:length(blocs)){
  temp <- tibble(nom = Scan(i)[[1]], lien = Scan(i)[[2]], prix = Scan(i)[[3]])
  bonsai_tb <- bind_rows(bonsai_tb, temp)
}

# ----------------------------------------------------------------------------
# ---------- Exercice Cate Blanchett

url_wikipedia <- "https://fr.wikipedia.org/"
url_blanchett <- "wiki/Cate_Blanchett"
data_html <- paste0(url_wikipedia, url_blanchett) %>% read_html()
film_selector <- "#mw-content-text div ul:nth-of-type(3) li i a"
film_nodes <- data_html %>% html_nodes(film_selector) %>% html_attrs()
films <- tibble()
for(k in seq_along(film_nodes)) {
  film_node <- film_nodes[[k]]
  if("class" %in% names(film_node)) next # Absence de page dédiée
  if(film_node["title"] == "Galadriel") next # Mauvais lien
  films <- rbind(
    films,
    list(titre=film_node["title"], url=film_node["href"])
  )
}

# ---- Q1
Get_URL_IMBD <- function(lien_url){
  data_html <- lien_url %>% read_html()
  blocs <- data_html %>% html_nodes("span.liste-horizontale li a") 
  i <- which((blocs %>% html_text) == "IMDb")
  lien <- (blocs[[i]] %>% html_attrs())["href"]  
  return(lien)
}

films <- films %>% rowwise() %>% mutate(Lien_IMDB = Get_URL_IMBD(paste0(url_wikipedia, url)))

# ---- Q2
films <- films %>% rowwise() %>% mutate(id_IMDB = substr(Lien_IMDB, regexpr("&id=", Lien_IMDB)+4, nchar(Lien_IMDB)),
                               url_imdb_ok = paste0("https://www.imdb.com/title/", id_IMDB, "/fullcredits/"))

# --- Q3
table_acteurs <- tibble(nom_acteur = NULL)
for (i in 1:dim(films)[1]){
  url_imdb <- (films %>% select(url_imdb_ok))[[1]][i]
  data_html <- url_imdb %>% read_html() %>% html_table()
  acteurs <- tibble(nom_acteur = data_html[[3]]$X2)
  table_acteurs <- bind_rows(table_acteurs, acteurs)
}
table_acteurs %>% group_by(nom_acteur) %>% 
  filter(!(nom_acteur %in% c("Cate Blanchett", "Rest of cast listed alphabetically:", ""))) %>%
  summarise(n_apparition = n()) %>% 
  arrange(-n_apparition)
