library(ggplot2)
library(usmap)
library(maptools)
library(tidyverse)
library(RColorBrewer)


## read in level 4 epilepsy center information

level4 <- read.csv("Epilepsy_centers_distribution.csv", header = T)
centers <- usmap_transform(select(level4, long, lat,region))
epilepsyprev <- read.csv("cdc_epilepsy_prevalence.csv", header = T)
epilepsyprev$pop_2015 <- as.double(epilepsyprev$pop_2015)
uspop2019 <- read.csv("state_pop_2019.csv", header = T)
colnames(uspop2019)[2] <- "state" 

## map looking at general us population (shading of states) vs distribution of level 4 epilepsy centers
plot_usmap(regions = "states", labels = TRUE, data = uspop2019, values = "pop_2019") + 
  scale_fill_continuous(low="lightpink", high = "darkred", name = "Estimated U.S. \nPopulation (2019)", label=scales::comma)+
  theme(legend.position = c(.87,.2)) +
  theme(text=element_text(size=14))+
  geom_point(data = centers, aes(x = long.1, y = lat.1), color = "black", size = 4)
ggsave("Epilepsy-Access-Map-US-Pop.png", width = 29, height = 19, units = "cm")



## map looking at 2015 active epilepsy prevalence defined as anyone with a seizure in the last year or actively
## still on medication
  
  plot_usmap(regions = "states", labels = TRUE, data = epilepsyprev, values = "pop_2015", label_color = "black") + 
    labs(title = "Level 4 Epilepsy Centers by State") + 
    theme(plot.title = element_text(size=25, face = "bold.italic", hjust = 0.5))+
    scale_fill_continuous(low = "lightblue", high="darkgreen",name = "Estimated Active \nEpilepsy Cases 2015", label = scales::comma) +
    theme(legend.position = c(0.87,0.2))+
    theme(text = element_text(size = 14)) +
    geom_point(data = centers, aes(x = long.1, y = lat.1), color = "darkblue", size = 4)
  ggsave("Epilepsy-Access-Map-Epilepsy-Prev.png", width = 29, height = 19, units = "cm")
  
  
## map of just Ohio only  
  plot_usmap(include = c("OH"), regions = "counties") +
    labs(title = "Epilepsy by county vs ADI") + 
    geom_point(data = centers, aes(x = long.1, y = lat.1), color = "red", size = 0.3)
  theme(panel.background = element_rect(color= "gray", fill = "lightblue"))
  
