# Use this to run things from the top level
source("get.R")
source("munge.R")
source("analyze.R")

library(dplyr)

# Get Data
omni_pod_raw <- get_omnipod_data()
dexcom_raw <- get_dexcom()
floors_raw <- get_floors()
distance_raw <- get_distance()
calories_raw <- get_calories()
steps_raw <- get_steps()
elevation_raw <- get_elevation()

# Mutate Data
dexcom <- munge_dexcom(dexcom_raw)
omni_pod <- munge_omni_pod(omni_pod_raw)

fitbit <- join_fitbit_data(floors_raw, distance_raw, calories_raw, steps_raw, elevation_raw)
fitbit <- munge_fitbit(fitbit)

fitbit_dexcom <- join_fitbit_with_dexcom_predictions(fitbit, dexcom)



# Analyse Data

plot_omnipod_type_frequency(omni_pod)
hist_bolus(omni_pod)
hist_fitbit(fitbit)
corrplot_fitbit(fitbit)



