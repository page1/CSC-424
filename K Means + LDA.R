library(MASS)
library(dplyr)
library(lubridate)
library(stats)
library(fpc)
library(cluster)
library(parallel)
library(ggplot2)
library(ggdendro)

data <- read.csv("data_set.csv", stringsAsFactors = F) %>%
  select(-X) %>%
  mutate(datetime = ymd_hms(datetime))

complete_cases <- select(data, bolus_insulin_units, glucose, meal_approx_carbs, total_insulin_burndown, acting_carbs, calories_sum_past_1hour, calories_sum_next_1hour)
complete_cases <- complete_cases[complete.cases(complete_cases), ]

#distance_matrix <- dist(complete_cases)

clusters <- mclapply(1:100, function(x) kmeans(complete_cases, x))

silhouettePlot = function(dataset, clusterData, name)
{
  dissE = daisy(dataset) 
  dE2 = dissE^2
  sk2 = silhouette(clusterData, dE2)
  plot(sk2)
  
  return(mean(sk2[,3]))
}

width_dist <- mclapply(2:length(clusters), function(index){
  file = paste('my_nice_plot', index, '.pdf', sep = '')
  print(file)
  pdf(file)
  mean_silhouette_width <- silhouettePlot(complete_cases, clusters[[index]]$cluster)
  print(length(mean_silhouette_width))
  dev.off()
  
  return(data.frame(index = index, mean_silhouette_width = mean_silhouette_width))
}, mc.cores = 16) %>% do.call("rbind", .)

plot(width_dist$index, width_dist$mean_silhouette_width, main = "Silhouette Avg Width by Cluster Count", xlab = "Clusters", ylab = "Width")
#p <- identify(width_dist$index, width_dist$mean_silhouette_width, plot=T)

complete_cases$cluster <- as.factor(clusters[[4]]$cluster)

lda_model <- lda(cluster ~ ., complete_cases)
lda_model

lda_applied <- as.data.frame(as.matrix(complete_cases[,which(colnames(complete_cases) != "cluster")]) %*% as.matrix(lda_model$scaling))
lda_applied$cluster <- complete_cases$cluster

ggplot(aes(x = LD1, y = LD2, colour = cluster), data = lda_applied) + 
  geom_point() +
  ggtitle("LDA of 4 Cluster Meal Time Data")