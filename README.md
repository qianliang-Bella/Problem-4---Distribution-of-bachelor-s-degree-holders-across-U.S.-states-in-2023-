# Problem-4---Distribution-of-bachelor-s-degree-holders-across-U.S.-states-in-2023-
I obtained my data from Google Public Data Explorer. This figure illustrates the distribution of bachelor’s degree holders across U.S. states in 2023, highlighting which states have the highest and lowest educated populations. It provides insight into regional differences in educational attainment and can guide policy or investment decisions.
# Map for the disctribution of Population of Bachelor Degrees in US 2023
setwd("C:/Users/6fu/Desktop/R/Final")
Bachelor <- read.csv("Bachelor.csv")
library(ggplot2)
library(maps)
library(dplyr)
library(sf)
states_map <- map_data("state")
state=Bachelor$Entity.properties.name
bachelors_pop=Bachelor$Variable.observation.value
bachelors_data <- data.frame(
  state = Bachelor$Entity.properties.name,
  bachelors_pop = Bachelor$Variable.observation.value
)
bachelors_data <- bachelors_data %>%
  mutate(state = tolower(state))
map_data_combined <- states_map %>%
  left_join(bachelors_data, by = c("region" = "state"))
a= ggplot(map_data_combined, aes(x = long, y = lat, group=group, fill = bachelors_pop))+
geom_polygon(color = "white") + coord_fixed(1.6)
b=a+geom_text(aes(x = -100, y = 40, label = "2023"), 
            color = "blue", size = 5, fontface = "bold")+
  labs(title = "Population of Bachelor Degrees in US 2023",
       x='long',
       y='Lat',
  )+ scale_fill_gradient(
    name = "Population",
    low = "lightpink", high = "purple", na.value = "grey90",
    labels = function(x) paste0(round(x/1e6,1), "M"))
b+annotate(geom = "text",
           x = -120, y = 38,
           label = "California", size=2.5,fontface='bold')+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 13, face = "bold", color = "blue"),
    legend.position = "bottom")#Final Graph
