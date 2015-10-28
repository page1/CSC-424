#Use this to source the data
library(dplyr)

get_omnipod_data <- function(){
  data <- read.csv("Data/OmniPod.csv")
  
  return(data)
}

get_dexcom <- function(){
  data1 <- read.table("data/Dexcom 1.Export.txt", fill = T, header = T)
  data2 <- read.table("data/Dexcom 2.Export.txt", fill = T, header = T)
}

read_fitbit_folder <- function(folder_name){
  file_names <- list.files(folder_name)
  data <- lapply(paste(folder_name, file_names, sep = "/"), read.csv) %>%
    do.call("rbind", .)
  
 return(data)
}

get_floors <- function(){
  data<-read_fitbit_folder ("Data/floors")
  data<-select(data,date,time,value) %>%
    rename(floors=value)
  
  return(data)
}

get_distance <- function(){
  data <- read_fitbit_folder("Data/distance")
  data <- select(data, date, time, value) %>%
    rename(distance = value)
  
    return(data)
}

get_calories <- function(){
  data <- read_fitbit_folder("Data/calories")
  data <- select(data, date, time, value) %>%
    rename(calories = value)
  
  return(data)
}

get_steps <- function(){
  data <- read_fitbit_folder("Data/steps")
  data <- select(data, date, time, value) %>%
    rename(steps = value)
  
  return(data)
}

get_elevation <- function(){
  data <- read_fitbit_folder("Data/elevation")
  data <- select(data, date, time, value) %>%
    rename(elevation = value)
  
  return(data)
}
