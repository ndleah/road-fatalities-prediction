# Library and data preparation ----

library(tidyverse)
library(janitor)
library(magrittr)
library(here)
library(scales)

car_accidents <- read_csv(here("data/car_accident.csv"))

car_accidents <- clean_names(car_accidents)
car_accidents <- rename(car_accidents, accident_date = accidentdate)
car_accidents <- rename(car_accidents, accident_time = accidenttime)

# Understanding data ----
glimpse(car_accidents)
names(car_accidents)

# Accident proportion of severity by light condition ----

car_accidents$severity <- as.factor(car_accidents$severity)
car_accidents %>%
  subset(light_condition_desc!="Unknown" & light_condition_desc!="Dark Street lights unknown") %>%
  group_by(severity, light_condition_desc) %>%
  summarize(total_accidents=n_distinct(accident_no)) %>%
  mutate(freq = percent(total_accidents / sum(total_accidents))) %>%
  ggplot(aes(x=light_condition_desc, y=freq, fill = severity)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip()+
  ggtitle("Light Condition by Severity") +
  xlab("Light Condition") + ylab("Accident Proportion") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank())


# Correlation Visibility and Fatalities
# Bar plot
plot <- car_accidents %>%
  ggplot(aes(fatal_accident, visibility)) + geom_boxplot()
plot

#Reference to boxplots: https://ggplot2.tidyverse.org/reference/geom_boxplot.html

# Fatalities to Light condition/visibility by each road geometry ----
ggplot(car_accidents) + geom_boxplot(aes(y=visibility, x=fatal_accident)) + facet_wrap(~ road_geometry_desc) 

# Fatalities to visibility by each accident type ----
ggplot(car_accidents) + geom_boxplot(aes(y=visibility, x=fatal_accident)) + facet_wrap(~accident_type_desc)
