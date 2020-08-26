library(ggplot2)
library(usmap)
library(maptools)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(sociome)
library(tidycensus)


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
  
  plot_usmap(regions = "states", labels = T, data = epilepsyprev, values = "pop_2015", label_color = "black") + 
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

  
## sociome map attempt, need to read in a census API key first
  ## state level data 
  adi_data <- get_adi(geography = "state")
  adi_data <- adi_data[-52,]
  adi_data$fips <- fips(adi_data$NAME)
  
  ## county level data 
  adi_data2 <- get_adi(geography = "county",  year = 2017)
  ## ggplot objects for us_maps need a fips column in order to map 
  colnames(adi_data2)[which(colnames(adi_data2)=="GEOID")] <- "fips"
  adi_data2 %>% separate(NAME, c("county","state"), sep = "([.,:])") -> adi_data2
  adi_data2$state <- trimws(adi_data2$state, which = "both")
  adi_data2 <- adi_data2[-(which(adi_data2$state == "Puerto Rico")),]
  
  adi_data %>% ggplot() + geom_sf(aes(fill = ADI))
  
  plot_usmap(regions = "states", labels = TRUE, data = adi_data, values = "ADI", label_color = "black") + 
    theme(plot.title = element_text(size=25, face = "bold.italic", hjust = 0.5))+
    scale_fill_viridis_c(direction = -1, name = "State Area \nDeprivation Index \n2017")+
    theme(legend.position = c(0.87,0.2))+
    theme(text = element_text(size = 14)) +
    geom_point(data = centers, aes(x = long.1, y = lat.1), shape = 21,  colour = "black", fill = "white", size = 4, stroke = 2)+
    ggsave("Epilepsy-Access-Map-ADI-state.png", width = 29, height = 19, units = "cm")
  
  
  plot_usmap(regions = "county", labels = F, data = adi_data2, values = "ADI", label_color = "black") + 
    theme(plot.title = element_text(size=25, face = "bold.italic", hjust = 0.5))+
    scale_fill_viridis_c(direction = -1, name = "County Area \nDeprivation Index \n2017")+
    #scale_fill_continuous(low = "lightblue", high="darkgreen",name = "County Area Deprivation Index 2017", label = scales::comma) +
    theme(legend.position = c(0.87,0.2))+
    theme(text = element_text(size = 14)) +
    geom_point(data = centers, aes(x = long.1, y = lat.1), shape = 21,  colour = "black", fill = "white", size = 4, stroke = 2)+
  ggsave("Epilepsy-Access-Map-ADI-county.png", width = 29, height = 19, units = "cm")
  
  
  