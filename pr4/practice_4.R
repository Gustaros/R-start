library('readxl')
library(dplyr)

#task 1
df_xl <- read_excel(
  'GAZ.xlsx',
  col_types = c('date', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text')
)

colnames(df_xl) <- c('date', 'p', 'temp', 'gas', 'condensate', 'water', 'ID', 'tree', 'group') 

#task 2
df_xl <-na.omit(df_xl)

#task 3
df_xl$kalvins <- df_xl[,3] + 273

df_xl <- df_xl[,-3]      

#task 4
#df_xl[,c(7,8,9)] <- as.factor(df_xl[,c(7,8,9)])

#task 5
df_xl$gas_con <- df_xl[3] / df_xl[4]
df_xl$gas_water <- df_xl[3] / df_xl[5]
df_xl$water_con <- df_xl[5] / df_xl[4] 

#task 6
df_xl_2018 <- subset(df_xl, substring(date, 1,4) == '2018')

#task 7
df_xl_wth_111 <- subset(df_xl, ID == '111')

#task 8

unique_ids = df_xl %>%
  group_by(ID) %>%
  summarize(max_water = max(water)) %>%
  filter(max_water < 2) %>%
  pull(ID)

unique_ids

#task 9

daily = df_xl %>%
  group_by(ID) %>%
  filter(all(water + gas + condensate >= 1000)) %>%
  pull(ID)

unique(daily)

#task 10
df_xl_2018$sum <- df_xl_2018$gas + df_xl_2018$condensate + df_xl_2018$water
print(subset(df_xl_2018, sum == max(sum))$tree)

#task 11
print(subset(df_xl_2018, water == max(water))$tree)

#task 12


mean_gas_water <- df_xl %>%
  subset(gas_water$gas != Inf) %>%
  group_by(tree) %>%
  summarise(mean_gas_water = mean(gas_water$gas, na.rm = TRUE))

print(mean_gas_water)

max_mean_row <- mean_gas_water %>%
  filter(mean_gas_water == max(mean_gas_water, na.rm = TRUE))

print(max_mean_row)

