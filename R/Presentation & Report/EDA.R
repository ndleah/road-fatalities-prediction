library(tidyverse) #data manipulation
library(here)
library(dplyr)
library(ggplot2)

# Load the dataset
accidents <- read_csv(here('data', 'Car_Accident_Data.csv'))

# get column names
colnames(accidents)

# Data Wrangling
# View Columns in batches
df[,1:18]
df[,19:36]
df[,37:51]

# Data overview
dim(accidents)

# Check for NA values
colSums(is.na(accidents))

# Drop NA values
## accidents <- accidents[complete.cases(accidents), ]
## accidents <- na.omit(accidents)

#--------------------------------------------------------------------------------------------
# Data Visualization
#--------------------------------------------------------------------------------------------

accidents$Day_Week_Description <- ordered( # order Day Of Week chronically
  accidents$Day_Week_Description, levels=c("Monday", "Tuesday", "Wednesday", 
                                           "Thursday", "Friday", "Saturday", 
                                           "Sunday")
  )

# Accident by weekdays
accidents %>% 
  group_by(day_of_week = Day_Week_Description) %>% 
  summarize(total_accidents=n_distinct(ACCIDENT_NO)) %>%
  ggplot(aes(x=day_of_week, y=total_accidents)) +
  labs(x = "Days of Week", y = "Total Accidents", title = "Total Accident By Weekdays") +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=total_accidents), vjust=1.6, color="white", size=3.5)+
  theme(plot.title = element_text(size = 25L, 
                                  face = "bold", hjust = 0.5))+
  ggthemes::theme_stata()

# Accident Severity Proportion by Weather Condition
accidents %>% 
  group_by(Atmosph_Cond_Desc,Inj_Level_Desc) %>% 
  summarize(total_accidents=n_distinct(ACCIDENT_NO)) %>%
  mutate(freq = percent(total_accidents / sum(total_accidents))) %>%
  ggplot(aes(x=Inj_Level_Desc, y=freq,fill=Atmosph_Cond_Desc, na.rm = TRUE)) +
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Accident Severity Proportion by Weather") +
  xlab("Accident Severity") + ylab("Accident Proportion")+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())


# Accident Severity by weekdays
accidents %>%
  ggplot() +
  aes(x = Day_Week_Description, fill = Inj_Level_Desc) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(Fatality = "#0D0887", `Not injured` = "#7D06A5", `Other injury` = "#CA4778", 
                               `Serious injury` = "#F69440", Unknown = "#F0F921")) +
  labs(x = "Hours", y = "Total Accidents", title = "Accident Severity Levels By Weekdays", 
       fill = "Severity Level") +
  ggthemes::theme_stata() +
  theme(plot.title = element_text(size = 25L, 
                                  face = "bold", hjust = 0.5))

accidents$ACCIDENT_HOUR <- format(strptime(accidents$ACCIDENTTIME, "%H:%M"), "%H:00")

# Accident by hours
accidents %>% 
  group_by(ACCIDENT_HOUR) %>% 
  summarize(total_accidents=n_distinct(ACCIDENT_NO)) %>%
  ggplot(aes(x=ACCIDENT_HOUR, y=total_accidents)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=total_accidents), vjust=1.6, color="black", size=3)+
  ggtitle("Total Accidents by Hours") +
  xlab("Hours") + ylab("Total Accidents")+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.text.x=element_text(angle = 90, vjust = 0.5))+
  theme(plot.title = element_text(size = 25L, 
                                  face = "bold", hjust = 0.5))

# HeatMap Weekday vs Hours

ggplot(accidents) +
  aes(
    x = Day_Week_Description,
    y = ACCIDENT_HOUR,
    fill = Inj_Level_Desc
  ) +
  geom_tile(size = 1.2) +
  scale_fill_manual(
    values = c(Fatality = "#800026",
               `Not injured` = "#FED976",
               `Other injury` = "#FD8D3C",
               `Serious injury` = "#E31A1C",
               Unknown = "#FFFFFF")
  ) +
  labs(
    x = "Days of Week",
    y = "Hours",
    title = "HeatMap Weekday vs Hours",
    fill = "Severity Level"
  ) +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5)
  )



