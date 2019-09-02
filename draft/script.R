library(rstudioapi)
library(magick)
library(tidyverse)


# function to take one-by-one images and crop them to correct size and save to a separate folder
recognise_image <- function(x){
  
  existing <- list.files("data/red_light_pic/done", full.names = TRUE) %>% 
    tibble() %>% 
    select(name = 1) %>% 
    mutate(number = as.numeric(str_extract(name, "\\d+"))) %>% 
    select(number)
  
  file <- list.files("data/red_light_pic", full.names = TRUE) %>% 
    tibble() %>% 
    select(name = 1) %>% 
    mutate(number = as.numeric(str_extract(name, "\\d+"))) %>% 
    anti_join(existing, by = "number") %>% 
    head(1)
  
  files_to_remove <- list.files("data/red_light_pic/todo/", full.names = TRUE)
  
  file.remove(files_to_remove)
  
  write_rds(1, str_c("data/red_light_pic/done/", file$number, ".rds"))  
  
  image_read(file$name) %>% 
    image_crop(geometry_area(width = 1100, height = 600, x_off= 0, y_off = 250)) %>%
    image_write(str_c("data/red_light_pic/todo/", file$number, ".jpg"))
  
  source("predict.Rmd", echo = TRUE)
  
  
}


recognise_image()




run_script <- function(x){
  
  restartSession("source('script.R', echo = TRUE)")
  
}


if (hour(Sys.time()) < 24){
  run_script()
}