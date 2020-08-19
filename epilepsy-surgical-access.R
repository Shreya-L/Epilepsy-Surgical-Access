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
  labs(title = "Level 4 Epilepsy Centers by State", 
       subtitle = "This is a map of level 4 epilepsy centers with the shading of states representing population density based on \n the 2019 US Census") + 
  scale_fill_continuous(low="lightpink", high = "darkred", name = "Estimated U.S. Population (2019)", label=scales::comma)+
  theme(legend.position = c(.99,.4)) +
  theme(text=element_text(size=15))+
  geom_point(data = centers, aes(x = long.1, y = lat.1), color = "black", size = 4)


## map looking at 2015 active epilepsy prevalence defined as anyone with a seizure in the last year or actively
## still on medication

plot_usmap(regions = "states", labels = TRUE, data = epilepsyprev, values = "pop_2015", label_color = "black") + 
  labs(title = "Level 4 Epilepsy Centers by State", 
       subtitle = "This is a map depicting epilepsy surgery access based on the prevalence of active epilepsy cases \n per state and the location of Level 4 epilepsy centers ") + 
  scale_fill_continuous(low = "lightblue", high="darkgreen",name = "Estimated Active Epilepsy \nCases 2015", label = scales::comma) +
  theme(legend.position = c(.99,.4))+
  theme(text = element_text(size = 15)) +
  geom_point(data = centers, aes(x = long.1, y = lat.1), color = "darkblue", size = 4)
ggsave("Epilepsy-Access-Map.png", width = 40, height = 20, units = "cm")




plot_usmap(include = c("OH"), regions = "counties") +
  labs(title = "Epilepsy by county vs ADI") + 
  geom_point(data = centers, aes(x = long.1, y = lat.1), color = "red", size = 0.3)
  theme(panel.background = element_rect(color= "gray", fill = "lightblue"))

