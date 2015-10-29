# Use this to run things from the top level
source("get.R")
source("munge.R")
source("analyze.R")

library(dplyr)

# Get Data
omni_pod <- get_omnipod_data()
floors <- get_floors()
distance <- get_distance()
calories <- get_calories()
steps <- get_steps()
elevation <- get_elevation()

# Mutate Data
omni_pod <- munge_omni_pod(omni_pod)

fitbit <- join_fitbit_data(floors, distance, calories, steps, elevation)

# Analyse Data

plot_omnipod_type_frequency(omni_pod)
hist_bolus(omni_pod)
hist_fitbit(fitbit)
corrplot_fitbit(fitbit)



