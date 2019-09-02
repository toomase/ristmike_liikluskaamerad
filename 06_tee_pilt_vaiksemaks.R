# Lõika ülekäiku raja pildist välja ainult sõidutee osa, mille põhjal tuvastan hiljem, kas mõni inimene on teel

library(magick)  # piltide töötlemiseks
library(tidyverse)


# funktsioon, mis lõikab pildi väiksemaks
crop_image <- function(x){
  
  # lae kaamera pilt
  pic <- image_read(x)
  
  # piltide nimeks anna antud hetke timestamp
  nimi <- str_extract(x, "\\d+")
  
  # eralda pildist ülekäigu raja osa
  pic %>%
    image_crop(geometry_area(width = 680, height = 680, x_off= 0, y_off = 200)) %>% 
    image_write(str_c("data/viru_valjak/red_light_crop/", nimi, ".jpg"))
  
}

# kui error mõne pildiga, siis NULL väärtus
crop_image_possibly <- possibly(crop_image, NULL)


# kõigi punase tulega ristmiku piltide path
# ainult 2019-08-26 tehtud pildid
file_path <- list.files("data/viru_valjak/red_light/", full.names = TRUE) %>% 
  as_tibble() %>% 
  mutate(aeg_nr = as.numeric(str_extract(value, "\\d+")),
         aeg = as.POSIXct(aeg_nr, origin = "1970-01-01"),
         kp = as.Date(aeg)) %>% 
  filter(kp == as.Date("2019-08-26")) %>% 
  pull(value)
  

# tuvasta kõigi piltide kohta foorituli ning salvesta jalakäiate punase tulega pildid eraldi kausta
map(file_path, crop_image_possibly)
