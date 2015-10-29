#Use this to do data cleaning and manipulation
library(dplyr)
library(tidyr)

column_names_to_lower_case <- function(data) {
  colnames(data) <- tolower(colnames(data))
  
  return(data)
}

split_units <- function(data){
  data <- data %>%
    mutate(value = ifelse(value == "", " ", value)) %>%
    separate(value, c("value", "units"), sep = " ") %>%
    mutate(units = gsub("\\(|\\)", "", units))
  
  return(data)
}

munge_omni_pod <- function(omni_pod){
  omni_pod <- omni_pod %>%
    column_names_to_lower_case() %>%
    select(-hidden) %>%
    split_units() %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"),
           value = as.numeric(value))
}

join_fitbit_data <- function(floors, distance, calories, steps, elevation){
  data <- floors %>%
    full_join(distance, by = c("date" = "date", "time" = "time")) %>%
    full_join(calories, by = c("date" = "date", "time" = "time")) %>%
    full_join(steps, by = c("date" = "date", "time" = "time")) %>%
    full_join(elevation, by = c("date" = "date", "time" = "time"))
  
  return(data)
}