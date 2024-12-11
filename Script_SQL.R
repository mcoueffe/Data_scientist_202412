library(DBI)
library(RSQLite)
library(dplyr)

# ---- Q1
con <- dbConnect(RSQLite::SQLite(), "star.db")

# ---- Q2
dbListTables(con)
dbListFields(con, dbListTables(con)[1])
dbListFields(con, dbListTables(con)[2])

# ---- Q3
dbGetQuery(con, "SELECT * from Topologie")

# ---- Q4
etat_db <- tbl(con, "Etat")
topologie_db <- tbl(con, "Topologie")

# ---- Q5
topologie_db %>% select(id, nom, id_proche_1) %>% collect()

# ---- Q6
topologie_db2 <- topologie_db %>% select(id, nom, id_proche_1) %>% 
  left_join(topologie_db, by = c("id_proche_1"="id")) %>% 
  select(id, nom = nom.x,  id_proche_1, nom_proche = nom.y )

# ---- Q7
topologie_db3 <- topologie_db %>% select(id, nom, id_proche_1, latitude, longitude) %>% 
  left_join(topologie_db, by = c("id_proche_1"="id")) %>% 
  select(id, nom = nom.x,  id_proche_1, nom_proche = nom.y, latitude = latitude.x, 
         longitude = longitude.x, latitude_proche = latitude.y, 
         longitude_proche = longitude.y) %>% 
  mutate(distance = (longitude - longitude_proche)^2 + (latitude - latitude_proche)^2)  

topologie_db3 %>% 
  select(id, nom,  id_proche_1, nom_proche, distance)

# ---- Q8
p_long = -1.7028661
p_lat = 48.1179151
topologie_db3 %>% mutate(D_longitude = p_long - longitude, 
                         D_latitude = p_lat - latitude,
                         distance_point = D_longitude^2 + D_latitude^2) %>% 
  select(id, nom,  distance_point) %>% 
  arrange(distance_point)

# ---- Q9
dbGetQuery(con, "SELECT id, nom, id_proche_1 from Topologie")

dbGetQuery(con, "SELECT table1.id, table1.nom, table1.id_proche_1 from Topologie AS table1 
           LEFT JOIN Topologie AS table2 ON table1.id_proche_1=table2.id")

dbGetQuery(con, "SELECT table1.id, table1.nom, table1.id_proche_1, table1.latitude,
                table1.longitude, table2.latitude, table2.longitude,
                (table1.latitude - table2.latitude)*(table1.latitude - table2.latitude) + (table1.longitude - table2.longitude)*(table1.longitude - table2.longitude) AS distance
                FROM Topologie AS table1 
           LEFT JOIN Topologie AS table2 ON table1.id_proche_1=table2.id")

dbGetQuery(con, "SELECT table1.id, table1.nom, table1.latitude,
                table1.longitude,
                (-1.7028661 - table1.longitude)*(-1.7028661 - table1.longitude)+(48.1179151-table1.latitude)*(48.1179151-table1.latitude) as distance
                FROM Topologie AS table1
           ORDER BY distance")

# ---- Q10
dbDisconnect(con)

# ----------------------------------------------------------------------------
# ---- Musique

# ---- Q1
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
dbListTables(con)

Playlist_db <- tbl(con, "Playlist")
Track_db <- tbl(con, "Track")
PlaylistTrack_db <- tbl(con, "PlaylistTrack")

# ---- Q2
dbListFields(con, "PlaylistTrack")
dbListFields(con, "Track")
dbListFields(con, "Playlist")

Track_db %>% left_join(PlaylistTrack_db, by = "TrackId") %>% 
  left_join(Playlist_db, by = "PlaylistId") %>% 
  group_by(PlaylistId, Name.y) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# ---- Q3
Album_db <- tbl(con, "Album")

# ---- Q4
Playlist_db %>% filter(Name == "Classical")  %>% 
  left_join(PlaylistTrack_db, by = "PlaylistId") %>%
  left_join(Track_db, by = "TrackId") %>% 
  left_join(Album_db, by = "AlbumId") %>% 
  show_query()

# ---- Q6
dbDisconnect(con)