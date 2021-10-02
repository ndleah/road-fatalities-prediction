#EDA for missing data

library(tidyverse)
library(DataExplorer)

# make sure to set variable 'df' with some data if not already there
# df <-

# Bar graph visualisation----
missing.values <- df %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', , fill = "#B22222") +
  labs(x='variable', y="number of missing values", 
  title='Number of missing values') +
  ggthemes::theme_tufte() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))


# Percentage visualisation ----
missing.values <- df %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <- (missing.values  %>% filter(isna == T) %>%     
             arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), 
                    labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", 
       x = 'Variable', y = "% of missing values")

percentage.plot

# Using the DataExplorer library ----
# Missing data ----
plot_missing(df)

# References ----
# #https://towardsdatascience.com/missing-value-visualization-with-tidyverse-in-r-a9b0fefd2246