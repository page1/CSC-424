#Use this to do data cleaning and manipulation

column_names_to_lower_case <- function(data) {
  colnames(data) <- tolower(colnames(data))
  
  return(data)
}

join_fitbit_data <- function(floors, distance, calories, steps, elevation){
  data <- floors %>%
    full_join(distance, by = c("date" = "date", "time" = "time")) %>%
    full_join(calories, by = c("date" = "date", "time" = "time")) %>%
    full_join(steps, by = c("date" = "date", "time" = "time")) %>%
    full_join(elevation, by = c("date" = "date", "time" = "time"))
  
  return(data)
}