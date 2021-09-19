# Library and data preparation ----

library(tidyverse)
library(janitor)
library(magrittr)
library(here)
library(scales)
library(lubridate)
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
  coord_flip() +
  ggtitle("Light Condition by Severity") +
  xlab("Light Condition") + ylab("Accident Proportion") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank()) + 
  ggthemes::theme_tufte()


# Correlation Visibility and Fatalities
# Bar plot
plot <- car_accidents %>%
  ggplot(aes(fatal_accident, visibility)) + geom_boxplot()
plot

#Reference to boxplots: https://ggplot2.tidyverse.org/reference/geom_boxplot.html

# Fatalities to Light condition/visibility by each road geometry ----
car_accidents %>%
  distinct(accident_no, .keep_all = TRUE) %>%
  ggplot() +
  geom_boxplot(aes(y=visibility, x=fatal_accident)) +
  facet_wrap(~ road_geometry_desc) +
  ggtitle("Fatality comparison of Visibility in different Road geometry") +
  xlab("Was it a Fatal Accident?") + ylab("Visibility") +
  ggthemes::theme_tufte()

# Fatalities to visibility by each accident type ----
ggplot(car_accidents) + geom_boxplot(aes(y=visibility, x=fatal_accident)) + facet_wrap(~accident_type_desc)

#Accidents by speed zone ----
#change into categorical
car_accidents$speed_zone <- as.factor(car_accidents$speed_zone)
counts <- table(car_accidents$speed_zone)
barplot(counts, main="Accidents By Speed Zone",
        xlab="Speed Zone") 

#
####### SPEED ZONE and fatal accident ratio ######
#
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
  geom_bar(stat="identity", position="dodge", fill = "#B22222") +
  xlab("Speed Zone") + ylab("Total Accidients with Fatalaty / Total Accidents") +
  ggthemes::theme_tufte()

ggsave(file="speed-zone_fataility-ratio.png", width=8, height=4, dpi=600)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#
########## YEAR - Fatal accidents by year ##########
#
car_accidents$accident_date <- as.Date(car_accidents$accident_date)
typeof(car_accidents$accident_date)

fatality_summary_year <- car_accidents %>%
  subset(inj_level_desc == "Fatality") %>%
  mutate(year = year(accident_date)) %>%
  subset(year < 2020) %>%
  group_by(year) %>%
  summarize(total_accident_fatalities=n_distinct(accident_no))

#line graph
fatality_summary_year %>% 
  ggplot(aes(x = year, y = total_accident_fatalities)) +
  geom_line(color = "#B22222", size = 0.5) +
  geom_point(color = "#B22222", size = 2) +
  xlab("Year") + ylab("Total Accidents with Fatalities") +
  scale_x_continuous(breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020)) +
  ggthemes::theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), panel.grid.major=element_line(color="grey", size=0.25)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 325), breaks = c(0, 50, 100, 150, 200, 250, 300, 350))

ggsave(file="year_fatal-accidents.png", width=8, height=4, dpi=600)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#
########### MONTH - Fatal accidents #######
#
car_accidents$accident_date <- as.Date(car_accidents$accident_date)
typeof(car_accidents$accident_date)

fatal_accidents_summary_month <- car_accidents %>%
  subset(inj_level_desc == "Fatality") %>%
  subset(year(accident_date) < 2020) %>%
  mutate(month = month(accident_date)) %>%
  group_by(month) %>%
  summarize(total_fatal_accidents=n_distinct(accident_no)) %>%
  mutate(month_name = month.name[month])
  
fatal_accidents_summary_month$month_name = factor(fatal_accidents_summary_month$month_name, levels = month.name)

fatal_accidents_summary_month %>%
  ggplot(aes(x = month_name, y = total_fatal_accidents)) +
  geom_bar(stat="identity", position="dodge", fill = "#B22222") + 
  ggtitle("Fatal Accidents by Month (excludes 2020)") +
  xlab("Month") + ylab("Total Accidents with Fatalities") +
  ggthemes::theme_tufte()

ggsave(file="month_fatal-accidents.png", width=8, height=6, dpi=600)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#
############## MONTH - Fatality ratio ################
#
car_accidents$accident_date <- as.Date(car_accidents$accident_date)
typeof(car_accidents$accident_date)

fatal_accidents_summary_month <- car_accidents %>%
  subset(inj_level_desc == "Fatality") %>%
  subset(year(accident_date) < 2020) %>%
  mutate(month = month(accident_date)) %>%
  group_by(month) %>%
  summarize(total_fatal_accidents=n_distinct(accident_no)) %>%
  mutate(month_name = month.name[month])

accidents_summary_month <- car_accidents %>%
  subset(year(accident_date) < 2020) %>%
  mutate(month = month(accident_date)) %>%
  group_by(month) %>%
  summarize(total_accidents=n_distinct(accident_no)) %>%
  mutate(month_name = month.name[month])

fatality_ratio_by_month <- left_join(x = fatal_accidents_summary_month, y = accidents_summary_month, by = c('month', 'month_name')) %>%
  mutate(fatality_ratio = total_fatal_accidents / total_accidents)

fatality_ratio_by_month$month_name = factor(fatality_ratio_by_month$month_name, levels = month.name)

fatality_ratio_by_month %>%
  ggplot(aes(x = month_name, y = fatality_ratio)) +
  geom_bar(stat="identity", position="dodge", fill = "#B22222") + 
  ggtitle("Fatality Ratio by Month (excludes 2020)") +
  xlab("Month") + ylab("Total Accidents with Fatalities / Total Accidents") +
  ggthemes::theme_tufte()

ggsave(file="month_fatality-ratio.png", width=8, height=6, dpi=600)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#bar graph
fatality_summary_year %>% 
  ggplot(aes(x = year, y = total_accident_fatalities)) +
  geom_bar(stat="identity", position="dodge") + 
  ggtitle("Fatal Accidents by Year") +
  xlab("Year") + ylab("Total Accidents with Fatalities") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = fatality_summary_year$year)

# Fatal accident proportion by year

accident_summary_year <- car_accidents %>%
  mutate(year = year(accident_date)) %>%
  group_by(year) %>%
  summarize(total_accidents=n_distinct(accident_no))

accident_summary_year %>% 
  ggplot(aes(x = year, y = total_accidents)) +
  geom_line() + 
  ggtitle("Total Accidents by Year") +
  xlab("Year") + ylab("Total Accidents") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = fatality_summary_year$year) +
  ggthemes::theme_tufte()

# Proportion of fatalities (total fatal accidents / total accidents)
merged_summary <- left_join(x = fatality_summary_year, y = accident_summary_year, by = 'year')

merged_summary %>%
  mutate(fatal_proportion = total_accident_fatalities/total_accidents) %>%
  ggplot(aes(x = year, y = fatal_proportion)) +
  geom_line() + 
  ggtitle("Fatal Accident Ratio by Year") +
  xlab("Year") + ylab("Total Fatal Accident / Total Accidents") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = fatality_summary_year$year) +
  ggthemes::theme_tufte()

# Multiple lines for age group of drivers
fatality_agegroup_summary_year <- car_accidents %>%
  subset(inj_level_desc == "Fatality" & road_user_type_desc == "Drivers" & age_group != "unknown") %>%
  mutate(year = year(accident_date)) %>%
  group_by(year, age_group) %>%
  summarize(total_accident_fatalities=n_distinct(accident_no))

accident_agegroup_summary_year <- car_accidents %>%
  subset(road_user_type_desc == "Drivers" & age_group != "unknown") %>%
  mutate(year = year(accident_date)) %>%
  group_by(year, age_group) %>%
  summarize(total_accidents=n_distinct(accident_no))

fatality_agegroup_summary_year %>% 
  ggplot(aes(x = year, y = total_accident_fatalities, color = age_group)) +
  geom_line() + 
  ggtitle("Total Fatalities by Year") +
  xlab("Year") + ylab("Total Fatalities") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = accident_agegroup_summary_year$year) +
  ggthemes::theme_tufte()

accident_agegroup_summary_year %>% 
  ggplot(aes(x = year, y = total_accidents, color = age_group)) +
  geom_line() + 
  ggtitle("Total Accidents by Year") +
  xlab("Year") + ylab("Total Accidents") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = accident_agegroup_summary_year$year) +
  ggthemes::theme_tufte()

# What a mess!


# ROAD USER TYPE BY YEAR

# fatalities
fatality_roaduser_summary_year <- car_accidents %>%
  subset(inj_level_desc == "Fatality" & road_user_type_desc != "Unknown") %>%
  mutate(year = year(accident_date)) %>%
  group_by(year, road_user_type_desc) %>%
  summarize(total_accident_fatalities=n_distinct(accident_no))

fatality_roaduser_summary_year %>% 
  ggplot(aes(x = year, y = total_accident_fatalities, color = road_user_type_desc)) +
  geom_line() + 
  ggtitle("Total Fatalities by Year for different Road Users") +
  xlab("Year") + ylab("Total Fatalities") +
  scale_x_continuous(breaks = fatality_roaduser_summary_year$year) +
  ggthemes::theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# accidents
accident_roaduser_summary_year <- car_accidents %>%
  subset(road_user_type_desc != "Unknown") %>%
  mutate(year = year(accident_date)) %>%
  group_by(year, road_user_type_desc) %>%
  summarize(total_accidents=n_distinct(accident_no))

accident_roaduser_summary_year %>% 
  ggplot(aes(x = year, y = total_accidents, color = road_user_type_desc)) +
  geom_line() + 
  ggtitle("Total Accidents by Year for different Road Users") +
  xlab("Year") + ylab("Total Accidents") +
  scale_x_continuous(breaks = accident_roaduser_summary_year$year) +
  ggthemes::theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# fatality out of total accidents
merged_summary_roadusers <- left_join(x = fatality_roaduser_summary_year, y = accident_roaduser_summary_year, by = c("year", "road_user_type_desc"))

merged_summary_roadusers %>%
  mutate(fatal_ratio = total_accident_fatalities / total_accidents) %>%
  ggplot(aes(x = year, y = fatal_ratio, color = road_user_type_desc)) +
  geom_line() + 
  ggtitle("Fatality ratio by year for different Road Users") +
  xlab("Year") + ylab("Total Fatalities / Total Accidents") +
  scale_x_continuous(breaks = accident_roaduser_summary_year$year) +
  ggthemes::theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# By month? https://stackoverflow.com/questions/6242955/converting-year-and-month-yyyy-mm-format-to-a-date
fatality_summary_month <- car_accidents %>%
  subset(inj_level_desc == "Fatality") %>%
  mutate(month = floor_date(accident_date, "month")) %>%
  group_by(month) %>%
  summarize(total_accident_fatalities=n_distinct(accident_no))



typeof(fatality_summary_month$month)


fatality_summary_month %>% 
  ggplot(aes(x = month, y = total_accident_fatalities)) +
  geom_line() +
  ggtitle("Fatal Accidents by Month") +
  xlab("Year") + ylab("Total Fatal Accidents with Fatalities") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = fatality_summary_month$year) +
  ggthemes::theme_tufte()

# AGE GROUP and fatality ratio ----
fatality_agegroup_summary <- car_accidents %>%
  subset(inj_level_desc == "Fatality" & road_user_type_desc == "Drivers" & age_group != "unknown" & age_group != "13-15") %>%
  group_by(age_group) %>%
  summarize(total_accident_fatalities=n_distinct(accident_no))

accident_agegroup_summary <- car_accidents %>%
  subset(road_user_type_desc == "Drivers" & age_group != "unknown" & age_group != "13-15") %>%
  group_by(age_group) %>%
  summarize(total_accidents=n_distinct(accident_no))

merged_summary_agegroup <- left_join(x = fatality_agegroup_summary, y = accident_agegroup_summary, by = c("age_group")) %>%
  mutate(total_fatal_ratio = 100 * total_accident_fatalities / total_accidents)

merged_summary_agegroup %>%
  ggplot(aes(x = age_group, y = total_fatal_ratio)) +
  geom_bar(stat = "identity" , fill = "#B22222") +
  xlab("Age Group") + ylab("Total Accidents with Fatalities / Total Accidents") +
  ggthemes::theme_tufte()

ggsave(file="age-group_fatality-ratio.png", width=8, height=6, dpi=600)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~