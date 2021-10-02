---
title: "STDS-AT2-EDA"
author: "Kasun Caldera"
date: "9/9/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Load libraries
```{r, cache=TRUE}
library(tidyverse)
library(ggplot2)
library(scales)
library(ggthemes)
library(esquisse)
library(lubridate)
library(DataExplorer)
```

#Set working directory
```{r, cache=TRUE}
setwd("D:/01 Subjects/Statistical Thinking for Data Science/AT2")
getwd()
accidents <- read_csv('Car_Accident_Data.csv')
```

```{r}
sapply(accidents, class)
```
#Change date format and adding new columns
```{r}
#Change data format
accidents$ACCIDENTDATE<- as.Date(accidents$ACCIDENTDATE, format = "%d-%m-%y")
str(accidents$ACCIDENTDATE)
#Create a Year column
accidents$YEAR<- format(as.Date(accidents$ACCIDENTDATE), "%Y")
str(accidents$YEAR)
#Create a Month column
accidents$MONTH<- format(as.Date(accidents$ACCIDENTDATE), "%m")
str(accidents$MONTH)
# time slot
accidents$TIMESLOT <-as.numeric(substr(accidents$ACCIDENTTIME,0,2))
```
#check for missing values
```{r fig.height=10, fig.width=10}
plot_missing(accidents)
```


#Number of Fatal Accidents by Day of the Week
```{r, cache=TRUE}
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = Day_Week_Description) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "Day",
    title = "Number of Fatal Accidents by Day of the Week"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )
```
#Number of accidents by Hour of the Day
```{r}
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = TIMESLOT) +
  geom_histogram(bins = 24L, fill = "#B22222") +
  labs(
    x = "Hour",
    title = "Number of Fatal Accidents by Hour of the Day"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )
```
#Number of Fatal accidents by Victim Type
```{r}
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = Road_User_Type_Desc) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "Road User",
    title = "Number of Fatal accidents by Victim Type"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )

```

#Number of Fatal accidents by Victim Type - pie chart
```{r}
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = "", y= Inj_Level_Desc, fill = Road_User_Type_Desc) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0)
```
#Number of Fatal accidents by Incident Type
```{r}
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = Accident_Type_Desc) +
  geom_bar(fill = "#B22222") +
  labs(
    x = " ",
    title = "Number of Fatal accidents by Incident Type"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )
```

#Number of Fatal accidents by Victims' Age Group
```{r}
accidents %>%
 filter(!(Age_Group %in% "unknown")) %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = Age_Group) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "Age Group",
    title = "Number of Fatal accidents by Victims' Age Group"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )
```
#Number of Fatal accidents by Drivers' Age Group
```{r}
accidents %>%
 filter(!(Age_Group %in% "unknown")) %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 filter(Road_User_Type_Desc %in% 
 "Drivers") %>%
 ggplot() +
  aes(x = Age_Group) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "Age Group",
    title = "Number of Fatal accidents by Drivers' Age Group"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )
```

#Number of Fatal accidents by Speed Zone
```{r}
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 filter(SPEED_ZONE >= 30L & SPEED_ZONE <= 110L) %>%
 ggplot() +
  aes(x = SPEED_ZONE) +
  geom_histogram(bins = 10L, fill = "#B22222") +
  labs(
    x = "Speed Zone",
    title = "Number of Fatal Accidents by Speed Zone"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )
```

#Number of Fatal accidents by Road Surface Condition
```{r}
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = Surface_Cond_Desc) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "Road Surface Condition",
    title = "Number of Fatal Accidents by Road Surface Condition"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )
```

#Number of Fatal accidents by Weather Conditions
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = Conditions) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "Weather Conditions",
    title = "Number of Fatal Accidents by Weather Conditions"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )

#Number of Fatal Accidents by Light Conditions
```{r}
accidents %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = Light_Condition_Desc) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "LightConditions",
    title = "Number of Fatal Accidents by Light Conditions"
  ) +
  theme_minimal() +
    coord_flip() +
  theme(
    plot.title = element_text(size = 16L,
    face = "bold",
    hjust = 0.5)
  )
```
# Victim Type
```{r}
accidents %>%
 filter(FATAL_ACCIDENT %in% "Y") %>%
 filter(ACCIDENTDATE >= "2006-01-01" & ACCIDENTDATE <= 
 "2019-12-31") %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 filter(!(Road_User_Type_Desc %in% "Unknown")) %>%
 ggplot() +
  aes(x = YEAR, fill = Road_User_Type_Desc) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(title = "Fatal Accidents by Victim Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15L,
    face = "bold",
    hjust = 0.5)
  )
```
```{r}
accidents %>%
 filter(FATAL_ACCIDENT %in% "Y") %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 ggplot() +
  aes(x = MONTH) +
  geom_bar(fill = "#112446") +
  theme_minimal()

```
# Light Conditions
```{r}
accidents %>%
 filter(FATAL_ACCIDENT %in% "Y") %>%
 filter(Inj_Level_Desc %in% "Fatality") %>%
 filter(!is.na(Conditions)) %>%
 ggplot() +
  aes(x = MONTH, fill = Light_Condition_Desc) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
```
#Density Plots
```{r fig.height=10, fig.width=10}
plot_density(accidents)
```
#Bar Charts
```{r fig.height=10, fig.width=10}
plot_bar(accidents)
```


