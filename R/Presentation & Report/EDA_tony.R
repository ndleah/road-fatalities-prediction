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

#Accidents by speed zone ----
#change into categorical
car_accidents$speed_zone <- as.factor(car_accidents$speed_zone)
counts <- table(car_accidents$speed_zone)
barplot(counts, main="Accidents By Speed Zone",
        xlab="Speed Zone") 

# Fatal accident proportions by speed zone ----
grouped2 <- car_accidents %>%
  group_by(speed_zone) %>%
  summarize(total_accidents_speedzone=n_distinct(accident_no))

grouped <- car_accidents %>%
  group_by(speed_zone, inj_level_desc) %>%
  summarize(total_accidents_speedzone_fatalities=n_distinct(accident_no))

merged_grouped <- left_join(x = grouped, y = grouped2, by = 'speed_zone')

merged_grouped %>% mutate(fatal_proportion = total_accidents_speedzone_fatalities/total_accidents_speedzone) %>%
  subset(inj_level_desc == "Fatality" & speed_zone != "777" & speed_zone != "888" & speed_zone != "999") %>%
  ggplot(aes(x = speed_zone, y = fatal_proportion)) +
  geom_bar(stat="identity", position="dodge") +
  ggtitle("Fatality proportion by speed zone")