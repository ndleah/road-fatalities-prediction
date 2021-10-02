library(tidyverse) # data manipulation
library(here) # allocate file
library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(lubridate)
library(patchwork) # merge visual plots to one
library(VIM) # tools for the visualization of missing or imputed values
library(naniar)
library(fastDummies) # automatically create dummy variables columns
library(zoo) # assign mean to NaN values
library(sqldf) # using SQL
library(viridis) # best. color. palette. evar.
library(reshape2)
library(ggrepel)
library(forcats)
library(scales)
library(treemapify) #plot treemap visualization

#--------------------------------------------------------------------------------------------
# Load the dataset
#--------------------------------------------------------------------------------------------
df <- read_csv(here('data', 'car_accident.csv'))

## filter necessary columns for analysis
df <- df[-c(1:2,6,8:10,12,18:21,32,34:35,40)]
glimpse(df)
#--------------------------------------------------------------------------------------------
# Data Wrangling
#--------------------------------------------------------------------------------------------
# get column names
colnms <- colnames(df)

# Create a variable that only contain the columns with missing values
dfNA <- df[ , colSums(is.na(df))!=0]

# Check for NA values
missing_data <- summary(aggr(dfNA,prop=TRUE,combined=TRUE, 
                             cex.axis=0.4, sortVars=TRUE))

#--------------------------------------------------------------------------------------------
# Deal with missing data
#--------------------------------------------------------------------------------------------
# categorical variables
df[c(6,16:18,20:22,35:36)] <- df[c(6,16:18,20:22,35:36)]%>% 
  replace(is.na(.), "Unknown")

df[c(15,19)] <- df[c(15,19)] %>% 
  replace(is.na(.), 0)

# numeric variables
df[c(25,28:34)] <- 
  lapply(df[c(25,28:34)], as.numeric) # convert columns to numeric

df[c(25,28:34)] <- 
  na.aggregate(df[c(25,28:34)]) # replace NA values with mean

# filter driver seat and injury level
fatality_accidents <- df %>% filter(SEATING_POSITION=="D", Inj_Level_Desc=="Fatality")

#--------------------------------------------------------------------------------------------
# Data Visualization
#--------------------------------------------------------------------------------------------

#----------------------------------------------------------
# 1. The "When" - Time
#----------------------------------------------------------

####################################################################
## Total Road Fatalities by Year (2006-2020)
####################################################################
# Fatal accident proportion by year
accident_summary_year <- fatality_accidents %>%
  mutate(year = year(ACCIDENTDATE)) %>%
  group_by(year) %>% 
  tally()

ggplot(accident_summary_year) +
  aes(x = year, y = n) +
  geom_line(size = 0.5, colour = "#B22222") +
  geom_point(color = "#B22222", size = 2) +
  geom_label(
    aes(label=n),
    nudge_x = 0.5,
    nudge_y = 6,
    check_overlap = TRUE,
    size = 3.5)+
  labs(x = "year", 
  y = "total fatalities", title = "Total Road Fatalities by Year (2006-2020)") +
  scale_x_continuous(breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020)) +
  scale_y_continuous(expand = c(0, 0), limits = c(100, 280), breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
## Fatal accident proportion by Month (2015-2019)
####################################################################

year_15_20 <- fatality_accidents %>%
  filter(ACCIDENTDATE > as.Date("2014-12-31"))

accident_summary_month <-  year_15_20 %>% 
  mutate(month = months(as.Date(year_15_20$ACCIDENTDATE))) %>%
  group_by(month) %>% 
  tally()

accident_summary_month$month <- ordered( # order month chronically
  accident_summary_month$month, levels=c("January","February","March",
                                         "April","May","June","July",
                                         "August","September","October",
                                         "November","December"))
## bar plot
accident_summary_month %>%
  group_by(month)  %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(fill = "#B22222") +
  ggtitle("Fatalities by Month (2015-2019)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120,140), 
                     breaks = c(0,20,40,60,80,100,120,140)) +
  geom_label(aes(x = month, y = n, label = n)) +
  labs(x = "month", y = "fatalities") +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
## Fatalities by Seasons (2015-2019)
####################################################################

#if else for season labels
#Convert the dates to year/month and add one month (1/12). 
#Then the calendar quarters correspond to the seasons so 
#convert to year/quarter, yq, and label the quarters as shown:
yq <- as.yearqtr(as.yearmon(fatality_accidents$ACCIDENTDATE, "%m/%d/%Y") + 1/12)
fatality_accidents$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("spring", "summer", "fall", "winter"))

accident_summary_season <- fatality_accidents %>%
  mutate(year = year(ACCIDENTDATE), month = months(as.Date(fatality_accidents$ACCIDENTDATE))) %>%
  group_by(Season,year,month) %>% 
  tally()

#boxplot
accident_summary_season %>%
  filter(year >= 2015L & year <= 2019L) %>%
  ggplot() +
  aes(x = Season, y = n) +
  geom_boxplot(shape = "circle", fill = "#FF8C00") +
  labs(title = "Fatalities by Seasons (2015-2019)") +
  ggthemes::theme_tufte() +
  scale_y_continuous(expand = c(0, 0), limits = c(5,35), 
                     breaks = c(5,10,15,20,25,30,35)) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
## Fatalities by Hour and Weekdays (2015-2019)
####################################################################

## add columns for accident hours
fatality_accidents$ACCIDENT_HOUR <- as.character(format(
  strptime(fatality_accidents$ACCIDENTTIME, "%H:%M"), "%H"))

# order Day Of Week chronically
fatality_accidents$Day_Week_Description <- ordered( 
  fatality_accidents$Day_Week_Description, levels=c("Monday", "Tuesday", "Wednesday", 
                                                    "Thursday", "Friday", "Saturday", 
                                                    "Sunday"))
# create new column to specify weekend and weekday 
fatality_accidents$ACCIDENTDATE <- as.Date(fatality_accidents$ACCIDENTDATE)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

fatality_accidents$wDay <- factor(
  (weekdays(fatality_accidents$ACCIDENTDATE) %in% weekdays1), 
  levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

fatal_week_hour <- sqldf(
  "
  SELECT
      Day_Week_Description,
      ACCIDENT_HOUR,
      wDay,
      COUNT(*) as total
      FROM fatality_accidents
    GROUP BY 
      Day_Week_Description,
      ACCIDENT_HOUR,
      wDay"
  )

#heatmap
ggplot(fatal_week_hour) +
  aes(
    x = ACCIDENT_HOUR,
    y = Day_Week_Description,
    fill = `total`
  ) +
  geom_tile(size = 1.2) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(
    x = "hours",
    y = "weekdays",
    title = "Weekday vs. Hourly Road Fatalities (2006-2020)",
    fill = "fatalities"
  ) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
## Fatalities by Weekend Vs. Weekdays Distribution (2006-2020)
####################################################################
#barplot
ggplot(fatal_week_hour) +
  aes(x = ACCIDENT_HOUR, y = total, fill = wDay) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  labs(
    x = "hour",
    y = "fatalities",
    title = "Fatalities by Time (2006-2020)"
  ) +
  ggthemes::theme_tufte() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 15L,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 13L,
                                 hjust = 0.5)
  ) +
  facet_wrap(vars(wDay))


####################################################################
## Fatalities by Day and Night (General) (2006-2020)
####################################################################
morning_hour <- c('00','01','02','03','04','05','06','07','08','09','10','11','12')
afternoon_hour <- c('13','14','15','16','17')
evening_hour <- c('18','19','20')
night_hour <- c('21','22','23')

# create new column specify Day vs Night time
fatality_accidents$day_night<-
  ifelse(fatality_accidents$ACCIDENT_HOUR %in% morning_hour, "Morning",
         ifelse(fatality_accidents$ACCIDENT_HOUR %in% afternoon_hour, "Afternoon",
                ifelse(fatality_accidents$ACCIDENT_HOUR %in% evening_hour, "Evening",
                       ifelse(fatality_accidents$ACCIDENT_HOUR %in% night_hour, "Night",NA))))


#create new variable
day_night <- sqldf(
  "
  SELECT
      day_night,
      COUNT(*) as value
      FROM fatality_accidents
    GROUP BY day_night
      "
)

# calculate percentage
day_night %>%
  arrange(desc(value)) %>%
  mutate(prop = percent(value / sum(value))) -> day_night

# pie chart
ggplot(day_night, aes(x = "", y = value, fill = fct_inorder(day_night))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x =1, nudge_y = 1) +
  labs(
    title = "Fatalities by Daytime (2006-2020)"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "Daytime")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
## Fatalities by Day and Night (Weekend Vs. Weekdays) (2006-2020)
####################################################################
#create new variable
day_night_wDay <- sqldf(
  "
  SELECT
      day_night,
      SUM(CASE WHEN wDay = 'weekend' THEN 1 ELSE 0 END) AS weekend,
      SUM(CASE WHEN wDay = 'weekday' THEN 1 ELSE 0 END) AS weekday
      FROM fatality_accidents
    GROUP BY day_night
      "
)

# Transform the data into the long format
day_night_wDay <- melt(day_night_wDay)

# double pie charts
ggplot(day_night_wDay, aes(x = "", y = value, fill = day_night)) + 
  geom_bar(stat = "identity", width = 1, position = position_fill()) +
  coord_polar(theta = "y") + 
  facet_wrap( ~ variable) +
  scale_fill_brewer(palette = "Reds") +
    theme_classic() +
    theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

#----------------------------------------------------------
# 1. The "Where" - Location
#----------------------------------------------------------

####################################################################
## Top 10 LGA with highest road fatalities (2006-2020)
####################################################################

fatality_accidents %>%
  group_by(LGA_NAME) %>%
  dplyr::summarise(Total = n()) %>%
  top_n(10, Total)  %>% 
  ggplot(aes(area = Total, fill = Total, label = LGA_NAME)) +
  geom_treemap() +
  labs(
    title = "Top 10 LGA with highest road fatalities (2006-2020)"
  ) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "topleft", reflow = T,grow = TRUE) +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
## Fatalities by Road Geometry (2006-2020)
####################################################################
fatal_geo <- sqldf(
  "
  SELECT
      Road_Geometry_Desc,
      COUNT(*) AS value
      FROM fatality_accidents
      where Road_Geometry_Desc != 'Unknown'
    GROUP BY 
      Road_Geometry_Desc
    ORDER BY COUNT(*) DESC
      "
)

# bar plot
ggplot(fatal_geo) +
  aes(x = Road_Geometry_Desc, y = value) +
  geom_bar(stat='identity', fill = "#FF8C00") +
  geom_label(
    aes(x = Road_Geometry_Desc, y = value, label=value))+
  labs(
    x = "road geometry",
    y = "fatalities",
    title = "Fatalities by Road Geometry (2006-2020)"
  ) +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))


#----------------------------------------------------------
# 1. The "Why" - Other factors
#----------------------------------------------------------
####################################################################
## Fatalities by Speed (2006-2020)
####################################################################
#create new variable
fatal_speed <- sqldf(
  "
  SELECT
      SPEED_ZONE,
      Age_Group,
      COUNT(*) AS value
      FROM fatality_accidents
      WHERE SPEED_ZONE NOT IN ('030','075','888','999') 
    GROUP BY 
      SPEED_ZONE,
      Age_Group
      "
)

# bar plot
ggplot(fatal_speed) +
  aes(x = SPEED_ZONE, y = value) +
  geom_bar(stat='identity',fill="#B22222") +
  labs(
    x = "speed",
    y = "fatalities",
    title = "Fatalities by Speed (2006-2020)"
  ) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
## Age Groups Who Drives At 100km/h (2006-2020)
####################################################################
fatal_speed %>%
  filter(SPEED_ZONE %in% "100") %>%
  filter(!(Age_Group %in% c("13-15", "16-17", "5-12", 
                            "unknown"))) %>%
  ggplot() +
  aes(x = Age_Group, y = value) +
  geom_col(size = 0.5, color = "#B22222", fill = "white") +
  labs(
    x = "age groups",
    y = "fatalities",
    title = "Age Groups Who Drives At 100km/h (2006-2020)"
  ) +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
# Pie chart Road User Type
####################################################################
#create new variable
arranged <- sqldf(
  "
  SELECT
      Road_User_Type_Desc,
      COUNT(*) as value
    FROM fatality_accidents
    WHERE Road_User_Type_Desc != 'Unknown'
    GROUP BY Road_User_Type_Desc
      "
)

# calculate percentage
arranged %>%
  arrange(desc(value)) %>%
  mutate(prop = percent(value / sum(value))) -> arranged

# pie chart
ggplot(arranged, aes(x = "", y = value, fill = fct_inorder(Road_User_Type_Desc))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x =1, nudge_y = 1) +
  labs(
    title = "Fatalities by Road Users (2006-2020)"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "Road Users")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

####################################################################
# Pie chart Accident Type
####################################################################
#create new variable
accident_type <- sqldf(
  "
  SELECT
      Accident_Type_Desc,
      COUNT(*) as value
    FROM fatality_accidents
    WHERE Accident_Type_Desc != 'Unknown'
    GROUP BY Accident_Type_Desc
      "
)

# calculate percentage
accident_type %>%
  arrange(desc(value)) %>%
  mutate(prop = percent(value / sum(value))) -> accident_type

# pie chart
ggplot(accident_type, aes(x = "", y = value, fill = fct_inorder(Accident_Type_Desc))) + 
  geom_bar(stat = "identity", width = 1) +
  geom_col(color = "black", width = 1) +
  coord_polar("y", start = 0) + 
  labs(
    title = "Fatalities by Accident Types (2006-2020)"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_classic() +
  guides(fill = guide_legend(title = "Accident Types")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))

#----------------------------------------------------------
# Visualization 4: Number of Fatality by Age Group of Drivers
#----------------------------------------------------------
# filter only age groups rows with road users are drivers
driver_ag <- fatality_accidents %>%
            filter(Road_User_Type_Desc %in% 
                     "Drivers") %>% 
            filter(!(Age_Group %in% c("13-15", "unknown")))
            
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

