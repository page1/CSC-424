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

aggregate_omni_pod <- function(omni_pod){
  omni_pod1 <- omni_pod %>%
    filter(!(type %in% c("Bolus Extended", "Basal Insulin"))) %>%
    group_by(date, time, type) %>%
    summarize(value = as.character(sum(as.numeric(value))))
  
  omni_pod2 <- omni_pod %>%
    filter(type %in% c("Bolus Extended", "Basal Insulin")) %>%
    group_by(date, time, type) %>%
    summarize(value = last(value))
  
  return(rbind(omni_pod1, omni_pod2))
}

flatten_omni_pod <- function(omni_pod){
  omni_pod <- omni_pod %>%
    unite(date_time, date, time, sep = "~") %>%
    spread(type, value) %>%
    separate(date_time, c("date", "time"), sep = "~")
  
  return(omni_pod)
}

munge_omni_pod <- function(omni_pod){
  Sys.setlocale('LC_ALL','C')
  omp <- omni_pod %>%
    column_names_to_lower_case() %>%
    select(-hidden) %>%
    filter(!(type %in% c("Pump Alarm", "State Of Health", "Notes"))) %>%
    split_units() %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"),
           value = as.numeric(value)) %>%
    mutate(extend_amount = ifelse(grepl("Bolus-Extended Meal Bolus - ", description, fixed = T), value, 0),
           extend_time = ifelse(grepl("Bolus-Extended Meal Bolus - ", description, fixed = T), gsub(" minutes.", "", gsub("Bolus-Extended Meal Bolus - ", "", description)), 0),
           value = ifelse(extend_time != 0, paste(extend_amount, extend_time, sep = "~"), value),
           type = ifelse(extend_time != 0, "Bolus Extended", type)) %>%
    aggregate_omni_pod() %>%
    flatten_omni_pod() %>%
    column_names_to_lower_case()
  
  colnames(omp) <- gsub(" |\\(|\\)", "_", colnames(omp))
  
  omp <- omp %>%
    mutate(bolus_extended = gsub(" Reverse Corrected.", "", bolus_extended),
           bolus_extended = ifelse(is.na(bolus_extended), "~", bolus_extended)) %>%
    separate(bolus_extended, c("bolus_extended_units", "bolus_extended_for_minutes"), sep = "~") %>%
    mutate(bolus_extended_units =  ifelse(bolus_extended_units == "", NA, bolus_extended_units),
           bolus_extended_for_minutes =  ifelse(bolus_extended_for_minutes == "", NA, bolus_extended_for_minutes)) %>%
    rename(basal_insulin_hourly_rate = basal_insulin,
           bolus_insulin_units = bolus_insulin,
           meal_approx_carbs = meal,
           daily_insulin_summary_units = insulin_summary) %>%
    select(-glucose__control_)
    
  return(omp)
}

join_fitbit_data <- function(floors, distance, calories, steps, elevation){
  data <- floors %>%
    full_join(distance, by = c("date" = "date", "time" = "time")) %>%
    full_join(calories, by = c("date" = "date", "time" = "time")) %>%
    full_join(steps, by = c("date" = "date", "time" = "time")) %>%
    full_join(elevation, by = c("date" = "date", "time" = "time"))
  
  return(data)
}