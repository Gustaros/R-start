


# part 1 task 1 -----------------------------------------------------------

library(ggplot2)
demography = read.csv("demography.csv")


# part 1 task 2 -----------------------------------------------------------


demography$young_share = demography$young_total / demography$popul_total
demography$trud_share = demography$wa_total / demography$popul_total
demography$old_share = demography$ret_total / demography$popul_total

# part 1 task 3 -----------------------------------------------------------

ggplot(demography, aes(x = trud_share)) +
  geom_histogram(binwidth = 0.01, color = "blue", fill = "lightblue", alpha = 0.5) +
  geom_rug(sides = "b", color = "grey") +
  geom_rug(sides = "t", color = "grey") +
  geom_vline(xintercept = median(demography$trud_share), color = "red") +
  labs(x = "Трудоспособны (%)", y = "Доля") +
  scale_x_continuous(labels = scales::percent)

# part 1 task 4 -----------------------------------------------------------


ggplot(demography, aes(x = trud_share, fill = region)) +
  geom_density(alpha = 0.5) +
  labs(x = "Трудоспособны (%)", y = "Доля") +
  scale_fill_manual(values = rainbow(length(unique(demography$region))))


ggplot(demography, aes(x = region, y = trud_share, fill = region)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = .2) +
  labs(y = "Трудоспособны (%)")

# part 1 task 5 -----------------------------------------------------------


ggplot(demography, aes(x = young_share, y = old_share)) +
  geom_point(color = "purple", shape = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Молодые (%)", y = "Пожилые (%)", title = paste("корреляция: ", round(cor(demography$young_share, demography$old_share, use = "complete.obs"), 2)))

# утверждение верно

# part 1 task 6 -----------------------------------------------------------


demography$male_total = demography$wa_male + demography$ret_male + demography$young_male
demography$male_share = demography$male_total / demography$popul_total 
demography$male = ifelse(demography$male_share > 0.5, 1, 0)



# part 1 task 7 -----------------------------------------------------------


ggplot(demography, aes(x = young_share, y = old_share, size = male_share, color = factor(male))) +
  geom_point(alpha = 0.7) +
  labs(x = "Молодые (%)", y = "Пожилые (%)", size = "Доля мужчин (%)", color = "Преобладают нет|да") +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(values = c("darkgreen", "orange"))



ggplot(demography, aes(x = region, fill = region)) +
  geom_bar(position = "dodge") +
  labs(x = "Регион", y = "Районов") +
  scale_fill_manual(values = c("orange", "darkgreen"))



# part 2 task 1 -----------------------------------------------------------

View(mtcars)

ggplot(mtcars, aes(x = hp, y = wt, size = cyl, color = factor(am == 1))) +
  geom_point(alpha = 0.7) +
  labs(x = "Лошадиные силы", y = "Вес", size = "Cylinders", color = "Коробка передач") +
  scale_color_manual(values = c("red", "green"), labels = c("Механика", "Автоматика")) +
  ggtitle("Зависимости автомобильных компонентотв")


# part 2 task 2 -----------------------------------------------------------


ggplot(mtcars, aes(x = hp, fill = factor(am))) +
  geom_histogram(bins = 6, color = "black", fill = "brown") +
  labs(x = "Horsepower") +
  scale_fill_manual(values = c("brown", "black"), guide = FALSE) +
  facet_wrap(~ factor(am, labels = c("Automatic", "Mechanic")), nrow = 1) +
  ggtitle("Gross horsepower")+
  theme_bw()

# part 2 task 3 -----------------------------------------------------------

View(sleep)

ggplot(sleep, aes(x = group, y = extra)) +
  geom_boxplot(fill = c("orange", "darkgreen")) +
  labs(x = "группа", y = "значение extra", title = "Усики👁〰👁️",
       fill = "Transmission Type") +
  scale_fill_manual(values = c("blue", "green")) +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))


# part 3 ------------------------------------------------------------------


covid_data = read.csv("final_data.csv")

colnames(covid_data) = gsub('NA_','', colnames(covid_data))



ggplot(covid_data, aes(x = covid_data$date,y = Armenia))+
  geom_point(col='orange', size=1)+
  labs(x='Время с начала пандемии', title = 'Динамика роста заболеваемости в Армении', subtitle = paste("Данные от", min(rownames(covid_data))))
ggplot(covid_data, aes(x = covid_data$date,y = Belgium))+
  geom_point(size=1, col='darkred')+
  labs(x='Время с начала пандемии', title = 'Динамика роста заболеваемости в Бельгии', subtitle = paste("Данные от", min(rownames(covid_data))))
ggplot(covid_data, aes(x = covid_data$date,y = Brazil))+
  geom_point(col='blue', size=1)+
  labs(x='Время с начала пандемии', title = 'Динамика роста заболеваемости в Бразилии', subtitle = paste("Данные от", min(rownames(covid_data))))





ggplot(data = covid_data, aes(Belarus)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "darkgreen", fill = "red") +
  geom_density(fill="green", alpha = 0.1)+
  labs(x='Беларусь', y='Плотность')
ggplot(data = covid_data, aes(Latvia)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "white", fill = "darkred") +
  geom_density(fill="black", alpha = 0.1)+
  labs(x='Латвия', y='Плотность')
ggplot(data = covid_data, aes(Russia)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "blue", fill = "white") +
  geom_density(fill="red", alpha = 0.1)+
  labs(x='Россия', y='Плотность')

