#Use this to plot and analyze 

plot_omnipod_type_frequency <- function(data){
  type_frequency <- data %>%
    group_by(type) %>%
    summarize(type_count = n())
  
  plot <- ggplot(aes(x = type, y = type_count), data = type_frequency) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
    xlab("Record Type") +
    ylab("Record Type Frequency") +
    ggtitle("Omnipod Record Type Frequency")
  
  print(plot)
}