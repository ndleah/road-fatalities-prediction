library(tidyverse)
library(ggplot2)
library(scales)
library(here)

file_path <- here("data")
setwd(file_path)
getwd()
df <- read_csv('Car_Accident_Data.csv')

# EDA -----------

# View Columns in batches
df[,1:18]
df[,19:36]
df[,37:51]

names(df)


#--------------------------------------------------------------------------------------------
# Set a particular theme for the Report
#--------------------------------------------------------------------------------------------

report_theme <- function () 
{ # Pick and choose how you want your charts to look like Axis text, legends, fonts etc 
  
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold", color = "#222222"), 
                 plot.subtitle = ggplot2::element_text(size = 15), #margin = ggplot2::margin(9, 0, 9, 0)), 
                 plot.caption = ggplot2::element_blank(), 
                 #   legend.position = "top", 
                 #   legend.text.align = 0, 
                 
                 #   legend.background = ggplot2::element_blank(), 
                 #    legend.title = ggplot2::element_blank(), 
                 #   legend.key = ggplot2::element_blank(), 
                 #   legend.text = ggplot2::element_text(size = 6, color = "#222222"), 
                 legend.position = "right",
                 #legend.justification = c("left", "top"),
                 
                 axis.title = ggplot2::element_blank(), 
                 axis.text = ggplot2::element_text(size = 10, color = "#222222"), 
                 axis.text.x = ggplot2::element_text(margin = margin(5,b=8),size=10,colour= '#4B4B4B'), 
                 axis.text.y = ggplot2::element_text(size = 10,colour = '#4B4B4B'), 
                 #  axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#FFFFFF"), 
                 panel.grid.major.x = ggplot2::element_blank(), 
                 panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 0, hjust = 0))
}

#------------------------------------------------------------------------------------------
# Basic Chart Function 
#------------------------------------------------------------------------------------------
categorical_chart <- function (data, axis_x_or_y, bar_fill){
  df %>% 
    distinct(ACCIDENT_NO,.keep_all = TRUE) %>% 
    group_by({{axis_x_or_y}}) %>% 
    count({{bar_fill}}) %>% 
    ggplot(aes(x=n,y= fct_reorder({{axis_x_or_y}},n),fill= {{bar_fill}})) +
    geom_bar(position='stack', stat="identity",na.rm = TRUE, width = 0.5) +
    scale_fill_brewer(palette = "Set2") 
}
#------------------------------------------------------------------------------------------
# Basic Bar Charts using categorical_chart Function
#------------------------------------------------------------------------------------------


# Accident Type
categorical_chart(df,Accident_Type_Desc, FATAL_ACCIDENT) +
  scale_x_continuous(labels = comma,n.breaks = 6) +
  report_theme() + 
  ggtitle('Accident_Type_Desc')
  
# Age Group
categorical_chart(df,Age_Group , FATAL_ACCIDENT) +
  scale_x_continuous(labels = comma,n.breaks = 6) +
  report_theme() + 
  ggtitle('Age_Group')

# Road User Description
categorical_chart(df,Road_User_Type_Desc , FATAL_ACCIDENT) +
  scale_x_continuous(labels = comma,n.breaks = 6) +
  report_theme() + 
  ggtitle('Road_User_Type_Desc')


# Seating Position
categorical_chart(df,SEATING_POSITION  , FATAL_ACCIDENT) +
  scale_x_continuous(labels = comma,n.breaks = 6) +
  report_theme() + 
  ggtitle('SEATING_POSITION')


# DCA_Description

df %>% 
  distinct(ACCIDENT_NO,.keep_all = TRUE) %>% 
  group_by(DCA_Description) %>% 
  count(FATAL_ACCIDENT) %>%
  filter(FATAL_ACCIDENT =="Y", n>=30) %>% 
  mutate(DCA_Description = factor(DCA_Description)) %>% 
  ggplot(aes(x=n,y= fct_reorder(DCA_Description,n),fill= FATAL_ACCIDENT)) +
  geom_col(position='stack',width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(labels = comma,n.breaks = 6) +
  report_theme() + 
  ggtitle('Accident_Type_Desc')


# VEHICLE_MAKE
df %>% 
  distinct(ACCIDENT_NO,.keep_all = TRUE) %>% 
  group_by(VEHICLE_MAKE) %>% 
  count(FATAL_ACCIDENT) %>%
  filter(FATAL_ACCIDENT =="Y", n>=30) %>% 
  mutate(VEHICLE_MAKE = factor(VEHICLE_MAKE)) %>% 
  ggplot(aes(x=n,y= fct_reorder(VEHICLE_MAKE,n),fill= FATAL_ACCIDENT)) +
  geom_col(position='stack', width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(labels = comma,n.breaks = 6) +
  report_theme() + 
  ggtitle('Cars with most fatal Accidents')



#----------------------------------------------------------------------
# Super Basic Line chart with text
#----------------------------------------------------------------------

p<- df %>% 
  distinct(ACCIDENT_NO,.keep_all = TRUE) %>% 
  filter(FATAL_ACCIDENT=='Y', ) %>% 
  mutate(Date = as.numeric(format(ACCIDENTDATE, "%Y"))) %>% 
  group_by(Date, SEX) %>%
  summarise(total = sum(NO_PERSONS_KILLED )) %>% 
  arrange(Date) %>% 
  ggplot(aes(Date, total, group = SEX, color = SEX)) +
  geom_line(size = 1.2, alpha = 1.5) +
  geom_text(aes(label = sprintf("%1.0f",total)), vjust = -1) +
  scale_color_viridis_d(direction = -1) +
  labs(title = "Fatal Accidents by gender") + 
  report_theme()


# Automate line chart 
library(gganimate)
q = p + transition_reveal(Date)

animate(q, height = 500, width = 800, fps = 20, duration = 16) # takes a minute or so to load


#----------------------------------------------------------------------
# Basic Scatter chart 
#----------------------------------------------------------------------
library(lubridate)

df %>% 
  distinct(ACCIDENT_NO,.keep_all = TRUE) %>% 
  filter(FATAL_ACCIDENT=='Y') %>% 
  select(ACCIDENTDATE, Temperature   , NO_PERSONS_KILLED) %>% 
  mutate(Date = format(ACCIDENTDATE, "%m-%Y"), 
         year = year(ACCIDENTDATE),
         mnth = month(ACCIDENTDATE)) %>% 
    na.omit() %>% 
  group_by(Date,year,mnth) %>%
  summarise(NO_PERSONS_KILLED = sum(NO_PERSONS_KILLED ), 
            Temperature    = mean(Temperature  , na.rm= TRUE )) %>% 
  arrange(year, mnth) %>% 
  ggplot(aes(x= NO_PERSONS_KILLED ,y =  Temperature ,color=factor(mnth))) +
  geom_point(size = 2, alpha = 1.5) +
  ggtitle('blahblah') + 
  report_theme()



