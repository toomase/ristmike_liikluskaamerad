# Tuvasta punane foorituli inimestele ning salvesta väiksemas formaadis pilt eraldi kausta

library(magick)  # piltide töötlemiseks
library(tidyverse)


# funktsioon, mis teeb järgmist: 
# kui jalakäiatel on punane tuli ja autodel roheline, siis salvesta pilt eraldi kausta
detect_red_light <- function(x){
  
  # lae kaamera pilt
  pic <- image_read(x)
  
  ## tuvasta autode foorituli
  ## selleks lõika välja fooritule ruut ning tuvasta selle värvus
  ## optimaalsete tingimuste tuvastamiseks tuli palju katsetada
  ## teatud valguse oludes ei õnnestu alati õiget foori värvi tuvastada
  car_light_1 <- pic %>%
    image_crop(geometry_area(width = 8, height = 8, x_off= 1175, y_off = 376)) %>%
    as.raster() %>%
    col2rgb() %>%
    t() %>%  # transpose matrix
    as_tibble() %>%
    rename(r = red, g = green, b = blue) %>%
    summarise_all(mean) %>%
    mutate(light = case_when(r > g & r > b ~ "red",
                             TRUE ~ "green")) %>%
    pull(light)
  
  # tuvasta alumine foorituli, mis põleb autodele
  car_light_2 <- pic %>%
    image_crop(geometry_area(width = 8, height = 8, x_off= 1157, y_off = 425)) %>%
    as.raster() %>%
    col2rgb() %>%
    t() %>%  # transpose matrix
    as_tibble() %>%
    rename(r = red, g = green, b = blue) %>%
    summarise_all(mean) %>%
    mutate(light = case_when(g > r & g > b ~ "green",
                             TRUE ~ "red")) %>%
    pull(light)
  
  # tuvasta jalakäiate foorituli
  person_light <- pic %>% 
    image_crop(geometry_area(width = 8, height = 8, x_off= 222, y_off = 80)) %>% 
    as.raster() %>% 
    col2rgb() %>% 
    t() %>%  # transpose matrix
    as_tibble() %>% 
    rename(r = red, g = green, b = blue) %>% 
    summarise_all(mean) %>% 
    mutate(light = case_when(r > g ~ "red",
                             g > r ~ "green",
                             TRUE ~ "unknown"))  %>% 
    pull(light)
  
  # piltide nimeks anna antud hetke timestamp
  nimi <- str_extract(x, "\\d+")
  
  
  # kui jalakäiatel on punane tuli ja autodel roheline, siis salvesta pilt eraldi kausta
  if (person_light == "red" & (car_light_1 == "green" | car_light_2 == "green")){
    
    # salvesta esialgne pilt
    pic %>% 
      image_write(str_c("data/viru_valjak/red_light/", nimi, ".jpg"))
  
  }
  
}

# kui error mõne pildiga, siis NULL väärtus
detect_red_light_possibly <- possibly(detect_red_light, NULL)


# kõigi ristmiku piltide path
file_path <- list.files("data/viru_valjak_raw/", full.names = TRUE)

# tuvasta kõigi piltide kohta foorituli ning salvesta jalakäiate punase tulega pildid eraldi kausta
map(file_path, detect_red_light_possibly)
