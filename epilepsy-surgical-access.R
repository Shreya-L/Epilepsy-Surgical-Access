library(ggplot2)
library(usmap)
library(tidyverse)

plot_usmap(regions = "states") + 
  labs(title = "Level 4 Epilepsy Centers by State", 
       subtitle = "This is a map depicting epilepsy surgery access based on the prevalence of epilepsy \n per state compared to where Level 4 centers are located") + 
  theme(panel.background = element_rect(color = "gray", fill = "lightblue"))


plot_usmap(include = c("OH"), regions = "counties") +
  labs(title = "Epilepsy by county vs ADI") + 
  theme(panel.background = element_rect(color= "gray", fill = "lightblue"))
