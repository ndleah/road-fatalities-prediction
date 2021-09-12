library(tidyverse) #data manipulation
library(here)
library(dplyr) # EDA
library(ggplot2) # for data visualization
library(patchwork) # for joining 2 plots together

# Load the dataset
accidents <- read_csv(here('data', 'car_accident.csv'))

#--------------------------------------------------------------------------------------------
# Data Wrangling
#--------------------------------------------------------------------------------------------


# get column names
colnames(accidents)  
unique(accidents$Inj_Level_Desc)
dim(accidents)
 
# filter only rows that have fatality
fatality_accidents <- accidents %>% filter(Inj_Level_Desc=="Fatality")
dim(fatality_accidents)

# add columns for Accident hours
fatality_accidents$ACCIDENT_HOUR <- format(strptime(fatality_accidents$ACCIDENTTIME, "%H:%M"), "%H:00")
# order Day Of Week chronically
fatality_accidents$Day_Week_Description <- ordered( 
  fatality_accidents$Day_Week_Description, levels=c("Monday", "Tuesday", "Wednesday", 
                                                    "Thursday", "Friday", "Saturday", "Sunday")
)


# Check for NA values
colSums(is.na(accidents))


#--------------------------------------------------------------------------------------------
# Data Visualization
#--------------------------------------------------------------------------------------------

#----------------------------------------------------------
# Visualization 1: Number of Fatality During Day of Weeks
#----------------------------------------------------------
weekdays <- ggplot(fatality_accidents) +
            aes(x = Day_Week_Description) +
            geom_bar(fill = "#B22222") +
            labs(
              x = "Weekdays",
              y = "# of death",
              title = "Number of Fatality During Day of Weeks"
            ) +
            ggthemes::theme_tufte() +
            theme(
              plot.title = element_text(size = 15L,
                                        face = "bold",
                                        hjust = 0.5),
              plot.subtitle = element_text(size = 17L,
                                           hjust = 0.5),
              axis.title.y = element_text(size = 12L),
              axis.title.x = element_text(size = 12L)
            )
weekdays


#----------------------------------------------------------
# Visualization 2: Number of Fatality During Day Hours
#----------------------------------------------------------
hours <-   ggplot(fatality_accidents) +
            aes(x = ACCIDENT_HOUR) +
            geom_bar(fill = "#B22222") +
            labs(
              x = "Hours",
              y = "# of death",
              title = "Number of Fatality During Day Hours"
            ) +
            ggthemes::theme_tufte() +
            theme(
              plot.title = element_text(size = 15L,
                                        face = "bold",
                                        hjust = 0.5),
              plot.subtitle = element_text(size = 17L,
                                           hjust = 0.5),
              axis.title.y = element_text(size = 12L),
              axis.title.x = element_text(size = 12L)
            )
hours


#----------------------------------------------------------
# Visualization 3: Number of Fatality During Weekend Vs. In week Hours
#----------------------------------------------------------
# weekend hours
weekend_hours <- fatality_accidents %>%
                filter(Day_Week_Description %in% c("Saturday", "Sunday")) %>%
                ggplot() +
                aes(x = ACCIDENT_HOUR) +
                geom_bar(fill = "#B22222") +
                labs(
                  x = "Hours",
                  y = "# of death",
                  title = "Number of Fatality During Weekend Hours"
                ) +
                coord_flip() +
                ggthemes::theme_tufte() +
                theme(
                  plot.title = element_text(size = 15L,
                                            face = "bold",
                                            hjust = 0.5),
                  plot.subtitle = element_text(size = 17L,
                                               hjust = 0.5),
                  axis.title.y = element_text(size = 12L),
                  axis.title.x = element_text(size = 12L)
                )
# in-week hours
inweek_hours <- fatality_accidents %>%
                filter(!(Day_Week_Description %in% c("Sunday", "Saturday"))) %>%
                ggplot() +
                aes(x = ACCIDENT_HOUR) +
                geom_bar(fill = "#B22222") +
                labs(
                  x = "Hours",
                  y = "# of death",
                  title = "Number of Fatality During Weekdays Hours"
                ) +
                coord_flip() +
                ggthemes::theme_tufte() +
                theme(
                  plot.title = element_text(size = 15L,
                                            face = "bold",
                                            hjust = 0.5),
                  plot.subtitle = element_text(size = 17L,
                                               hjust = 0.5),
                  axis.title.y = element_text(size = 12L),
                  axis.title.x = element_text(size = 12L)
                )
#plot 2 plots into 1 graphic
inweek_hours + weekend_hours

#----------------------------------------------------------
# Visualization 4: Number of Fatality by Age Group of Drivers
#----------------------------------------------------------
# filter only age groups rows with road users are drivers
driver_ag <- fatality_accidents %>%
            filter(Road_User_Type_Desc %in% 
                     "Drivers") %>% 
            filter(!(Age_Group %in% c("13-15", "16-17", "unknown")))
            
driver_ag %>%  
  ggplot() +
  aes(x = Age_Group) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "Age Group",
    y = "# of death",
    title = "Number of Fatality By Age Groups of Driver"
  ) +
  ggthemes::theme_tufte() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 10L,
                                 hjust = 0.5),
    axis.title.y = element_text(size = 12L),
    axis.title.x = element_text(size = 12L)
  )

#----------------------------------------------------------
# Visualization 5: Speed Zone Vs. Other Environmental Factors
#----------------------------------------------------------
# light conditions
light_conditions <- fatality_accidents %>%
  filter(!(SPEED_ZONE %in% c("075", "999", "888", "777"))) %>%
  filter(!(Light_Condition_Desc %in% 
             c("Dark Street lights unknown", "Unknown"))) %>%
  ggplot() +
  aes(x = Light_Condition_Desc, fill = SPEED_ZONE) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(
    x = "light conditions",
    y = "percentage",
    title = "Speed Zone Vs. Light Conditions"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5),
    legend.position = "None"
  )
# weather
weather <- fatality_accidents %>%
  filter(!(SPEED_ZONE %in% c("075", "999", "888", "777"))) %>%
  filter(!(Atmosph_Cond_Desc %in% 
             "Not known")) %>%
  ggplot() +
  aes(x = Atmosph_Cond_Desc, fill = SPEED_ZONE) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(
    x = "weather conditions",
    y = "percentage",
    title = "Speed Zone Vs. Weather Conditions"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5)
  ) 
#plot 2 plots into 1 graphic
light_conditions + weather