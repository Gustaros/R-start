

# Part 1 ------------------------------------------------------------------

# task 1

info = list(c("Jane", "Michael", "Mary", "George"), c(8, 6, 28, 45), c(0, 1, 0, 1))

print(info[[1]][2])
print(info[[3]])

info = list(names=c("Jane", "Michael", "Mary", "George"), age=c(8, 6, 28, 45), gender=c(0, 1, 0, 1))

print(info$names)

info$drinks = c('juice', 'tea', 'rum', 'coffee')

print(info$drinks)

# task 2

s = "a,b,c,d"
t = unlist(strsplit(s, ','))
print(t)

index = "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
I = unlist(strsplit(index, ';'))
I = as.numeric(sub(",",".", I))
print(I)


# Part 2 ------------------------------------------------------------------

#install.packages("randomNames")
#install.packages("dplyr")

library(dplyr)
library(randomNames)
set.seed(1234)

names = randomNames(100, which.names="first", ethnicity = 4)
ages = sample(16:75, 100, replace = TRUE)
views = c("right", "left", "moderate", "indifferent")
polit = sample(views, 100, replace = TRUE)

data = data.frame(names, ages, polit)
id = 1:100

data = dplyr::mutate(data, id)
from_25_to_30 = dplyr::filter(data, ages >= 25 & ages <=30)
from_25_to_30_proportion = round((dim(from_25_to_30)[1] / dim(data)[1]) * 100, digit = 1)
from_25_to_30_proportion

polit_views = as.factor(polit)
print(polit_views)
data = dplyr::mutate(data, polit_views = polit_views)




# Part 3 ------------------------------------------------------------------

#install.packages('car')

##Description
#The Ornstein data frame has 248 rows and 4 columns. The observations are the 248 largest Canadian firms with publicly available information in the mid-1970s. The names of the firms were not available.

data = carData::Ornstein
data[1,]

filled = sum(complete.cases(data))
filled

if(sum(!complete.cases(data)) > 0) {
  print("there are some empties")
  print(data[!complete.cases(data), ])
} else {
  print("all filled")
}

s_1 = dplyr::filter(data, assets >= 10000 & assets <= 20000)
s_2 = dplyr::filter(data, interlocks <= 30)
s_3 = data %>% dplyr::filter(sector == "TRN") %>% dplyr::filter(nation == "CAN")
s_3

log_assets = log(data$assets)

data = dplyr::mutate(data, log_assets) 

## ???   6   ???

library(foreign)
write.dta(data, "Firms.dta")



# Part 3.1 ? --------------------------------------------------------------

library(readr)
library(tidyr)
library(dplyr)

cov19 = read_csv("time_series_covid19_confirmed_global.csv")

dim(cov19)
colnames(cov19)[0:5]
sapply(cov19, class)[0:5]


cov19 = unite(cov19, region, c('Province/State', 'Country/Region'))

library(matrixStats)
cov19_stats = data.frame(region=cov19[,1], Lat=cov19[,2], Long=cov19[,3], sumx=rowSums(cov19[c(4:ncol(cov19))]), meanx=rowMeans(cov19[c(4:ncol(cov19))]), sdx=rowSds(as.matrix(cov19[,c(4:ncol(cov19))]), na.rm=TRUE))

cov19_t= t(as.matrix(cov19[, 4:ncol(cov19)]))

colnames(cov19_t) = cov19$region

rownames(cov19_t) = gsub('/','-', rownames(cov19_t))
rownames(cov19_t) = format(as.Date(rownames(cov19_t), '%m-%d-%y'), "%Y-%m-%d")

cov19_t = as.data.frame(cov19_t)
cov19_t$date = as.Date(rownames(cov19_t), '%Y-%m-%d')
cov19_t = as.data.frame(cov19_t)
cov19_t = cov19_t[, c('date', colnames(cov19_t)[-1])]
rownames(cov19_t) = NULL


#install.packages("writexl")

library(writexl)

dir.create("data_output", showWarnings = FALSE)

write.table(cov19_t, file = "data_output/final_data.txt")
write.csv(cov19_t, file = "data_output/final_data.csv")
write_xlsx(cov19_t, path = "data_output/final_data.xlsx")
