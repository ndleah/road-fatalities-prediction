library(tidyverse)
library(here)
library(caret)
library(RColorBrewer)
library(correlationfunnel)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(mltools)
library(gridExtra)
library(glue)
library(grid)
library(corrplot)
library(summarytools)

df <- read_csv(here('data', 'Car_Accident_Data.csv'))
dfSummary(df,
          style        = "grid",
          method = 'render',
          graph.magnif = 0.85,
          tmp.img.dir = "/tmp")

# Create road name concat
df$ROAD_NAME_FULL <- paste(df$ROAD_NAME, df$ROAD_TYPE, sep = ' ')
df$ROAD_NAME_INT_FULL <- paste(df$ROAD_NAME_INT, df$ROAD_TYPE_INT, sep = ' ')


df <- transform(df, SPEED_ZONE = as.numeric(SPEED_ZONE))

# Histogram numerical cols and bar up categorcal

df <- df %>%
  filter(Road_User_Type_Desc == 'Drivers')

numerical_cols <- c('NO_OF_VEHICLES', 'NO_PERSONS','SPEED_ZONE', 'VEHICLE_YEAR_MANUF',
                    'NO_OF_CYLINDERS', 'TOTAL_NO_OCCUPANTS', 'LIGHT_CONDITION')
                    
numerical_continuous <- c('CloudCover', 'WindSpeed', 'Temperature', 'DewPoint', 
                        'RelativeHumidity', 'Precipitation')


cat_cols <- c('SEX', 'SEATING_POSITION', 'Accident_Type_Desc', 'Road_Geometry_Desc',
              'ROAD_TYPE','Road_Surface_Type_Desc', 'Surface_Cond_Desc', 
              'Atmosph_Cond_Desc', 'Light_Condition_Desc', 'Conditions', 
              'Age_Group', 'Day_Week_Description')


numerical_continuous_df <- df[numerical_continuous]

pdf(file = 'eda_unbalanaced.pdf')
for (col in colnames(numerical_continuous_df)){
  var <- lapply(col, as.symbol)
  plotted <- df %>%
    ggplot(aes_string(x = col, color = 'FATAL_ACCIDENT')) +
    geom_histogram(fill = "white", bins = 30)
  print(plotted)
  }

numerical_df <- df[numerical_cols]
for (col in colnames(numerical_df)){
  var <- lapply(col, as.symbol)
  plotted <- df %>%
    group_by(.dots = var) %>%
    mutate(FATAL_ACCIDENT =  case_when(
          FATAL_ACCIDENT == 'Y' ~ 1,
          FATAL_ACCIDENT == 'N' ~ 0)
        ) %>%
    summarise(total_counts = n(),
              Total_Fatalaties = sum(FATAL_ACCIDENT),
              Fatalties_Ratio = Total_Fatalaties / total_counts) %>%
  ggplot(aes_string(x = col, y = 'total_counts', fill = 'Fatalties_Ratio')) +
  geom_bar(stat = 'identity')+
  coord_flip()

  print(plotted)
}

categorical_df <- df[cat_cols]
for (col in colnames(categorical_df)){
  var <- lapply(col, as.symbol)
  plotted <- df %>%
    group_by(.dots = var) %>%
    mutate(FATAL_ACCIDENT =  case_when(
      FATAL_ACCIDENT == 'Y' ~ 1,
      FATAL_ACCIDENT == 'N' ~ 0)
    ) %>%
    summarise(total_counts = n(),
              Total_Fatalaties = sum(FATAL_ACCIDENT),
              Fatalties_Ratio = Total_Fatalaties / total_counts) %>%
    ggplot(aes_string(x = col, y = 'total_counts', fill = 'Fatalties_Ratio')) +
    geom_bar(stat = 'identity') +
    coord_flip()

  print(plotted)
}
dev.off()
plotted

