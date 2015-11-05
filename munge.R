#Use this to do data cleaning and manipulation
library(dplyr)
library(tidyr)
library(lubridate)

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
    group_by(datetime, type) %>%
    summarize(value = as.character(sum(as.numeric(value))))
  
  omni_pod2 <- omni_pod %>%
    filter(type %in% c("Bolus Extended", "Basal Insulin")) %>%
    group_by(datetime, type) %>%
    summarize(value = last(value))
  
  return(rbind(omni_pod1, omni_pod2))
}

flatten_omni_pod <- function(omni_pod){
  omni_pod <- omni_pod %>%
    spread(type, value)
  
  return(omni_pod)
}

munge_omni_pod <- function(omni_pod_raw){
  Sys.setlocale('LC_ALL','C')
  omp <- omni_pod_raw %>%
    column_names_to_lower_case() %>%
    select(-hidden) %>%
    filter(!(type %in% c("Pump Alarm", "State Of Health", "Notes"))) %>%
    mutate(datetime = parse_date_time(paste(date, time), c("mdy hm", "mdy")) + ifelse(grepl("PM", time), dhours(12), dhours(0))) %>%
    split_units() %>%
    mutate(value = as.numeric(value)) %>%
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
    mutate(bolus_extended_units = ifelse(bolus_extended_units == "", NA, bolus_extended_units),
           bolus_extended_for_minutes = ifelse(bolus_extended_for_minutes == "", NA, bolus_extended_for_minutes)) %>%
    rename(basal_insulin_hourly_rate = basal_insulin,
           bolus_insulin_units = bolus_insulin,
           meal_approx_carbs = meal,
           daily_insulin_summary_units = insulin_summary) %>%
    mutate_each(funs(as.numeric), -datetime) %>%
    mutate(bolus_extended_for_minutes = dminutes(bolus_extended_for_minutes)) %>%
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

munge_fitbit <- function(fitbit){
  fitbit <- fitbit %>%
    mutate(datetime = ymd_hms(paste(date, time)),
           date = NULL,
           time = NULL)
  
  return(fitbit)
}

munge_dexcom <- function(dexcom_raw){
  dexcom <- dexcom_raw %>%
    column_names_to_lower_case() %>%
    select(-contains("internaltime"))
  
  predicted_bg <- select(dexcom, contains("glucose")) %>%
    filter(!is.na(glucosevalue))
  colnames(predicted_bg) <- gsub("glucose", "", colnames(predicted_bg))
  predicted_bg <- predicted_bg %>%
    rename(datetime = displaytime, predicted_bg = value) %>%
    mutate(datetime = mdy_hm(datetime),
           predicted_bg = as.numeric(ifelse(predicted_bg == "Low", 40, predicted_bg)))
  
  calibrated_bg <- select(dexcom, contains("meter")) %>%
    filter(!is.na(metervalue))
  colnames(calibrated_bg) <- gsub("meter", "", colnames(calibrated_bg))
  calibrated_bg <- calibrated_bg %>% 
    rename(datetime = displaytime, calibrated_bg = value) %>%
    mutate(datetime = mdy_hm(datetime),
           calibrated_bg = as.numeric(calibrated_bg))
  
  return(list(predicted_bg = predicted_bg,
              calibrated_bg = calibrated_bg))
}

join_fitbit_with_dexcom_predictions <- function(fitbit, dexcom) {
  predicted_bg <- dexcom$predicted_bg 
  predicted_bg <- predicted_bg %>%
    mutate(datetime = as.POSIXct(floor(as.numeric(datetime) / (5 * 60)) * 5 * 60, origin = "1970-01-01",tz = "GMT")) %>%
    group_by(datetime) %>%
    summarize(predicted_bg = mean(predicted_bg))
  
  fitbit_aggregate <- fitbit %>%
    mutate(datetime = as.POSIXct(ceil(as.numeric(datetime) / (5 * 60)) * 5 * 60, origin = "1970-01-01",tz = "GMT")) %>%
    group_by(datetime) %>%
    summarize_each(funs(sum))
  
  data <- predicted_bg %>%
    inner_join(fitbit_aggregate, by = c("datetime" = "datetime"))
  
  return(data)
}


# in work
insulin_on_board <- function(fitbit_dexcom, omnipod){
  lol <- omni_pod %>%
    mutate(extended_till = datetime + bolus_extended_for_minutes) %>%
    select(extended_till, bolus_extended_units) %>%
    filter(!is.na(bolus_extended_units)) %>%
    rename(datetime = extended_till,
           bolus_insulin_units = bolus_extended_units)
  
  boom <- rbind_all(list(omni_pod, lol)) %>%
    select(-contains("extended"), -contains("summary")) %>%
    filter(!is.na(basal_insulin_hourly_rate) |
           !is.na(bolus_insulin_units) |
           !is.na(glucose) |
           !is.na(meal_approx_carbs))
  
  ddd <- boom %>%
    mutate(datetime = as.POSIXct(floor(as.numeric(datetime) / (5 * 60)) * 5 * 60, origin = "1970-01-01",tz = "GMT")) %>%
    arrange(datetime) %>%
    group_by(datetime) %>%
    summarize(basal_insulin_hourly_rate = last(basal_insulin_hourly_rate), #Note: might introduce null
              bolus_insulin_units = sum(bolus_insulin_units, na.rm = T),
              glucose = mean(glucose, na.rm = T),
              meal_approx_carbs = sum(meal_approx_carbs, na.rm = T))
  
}