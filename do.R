# Use this to run things from the top level
source("get.R")
source("munge.R")
source("analyze.R")

library(ggplot2)
library(dplyr)

# Get Data
omni_pod <- get_omnipod_data()
floors <- get_floors()
distance <- get_distance()
calories <- get_calories()
steps <- get_steps()
elevation <- get_elevation()

# Mutate Data
omni_pod <- column_names_to_lower_case(omni_pod)

fitbit <- join_fitbit_data(floors, distance, calories, steps, elevation)

# Analyse Data

plot_omnipod_type_frequency(omni_pod)



