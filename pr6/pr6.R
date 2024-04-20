

# task 1 ------------------------------------------------------------------

#download.file('https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/ECG_yurchenkov.txt', destfile = 'ECG.txt')

# починить кодировку внутри r тяжко
# буду использовать файл полученный внешними средствами


# task 2 ------------------------------------------------------------------


d_ecg = readLines('ecg.txt')

for (i in 1:length(d_ecg)) {
  if (d_ecg[i] == '0	-0,02875	-0,024375	0,3125	-1,44196	1,475') {
    break
  }
  print(d_ecg[i])
}


# task 3 ------------------------------------------------------------------



for (i in c(1:length(d_ecg))){
  if (d_ecg[i] == 'Время(мс)	0	1	2	3	4')
    break
  else {
    d_ecg = d_ecg[-i]
  }
}

meta = read.csv('ecg.txt', sep='\t', skip = 46)

colnames(meta) = c('time', 'v1', 'v2', 'v3', 'v4', 'v5')
for (i in 1:ncol(meta)){
  meta[,i] = gsub(',', '.', meta[,i])
  meta[,i] = as.numeric(meta[,i])
}
meta = na.omit(meta)


# task 4 ------------------------------------------------------------------

stages = function(x){
  sum(x == 0)
}

stages(meta[,1])


# task 5 ------------------------------------------------------------------

check_time = function(x) {
  result = c()
  for (i in 2:length(x)) {
    if (abs(x[i] - x[i - 1]) != 4) {
      result = c(result, x[i - 1])
    }
  }
  result = c(result, x[length(x)])
  return(result)
}

# время каждого этапа

ms = check_time(meta[,1])
ms

# конец этапов с 00:00:00

cumsum(ms)


table(cut(x = as.integer(meta[,1]), breaks = seq(1, length(meta), (length(meta) - 1)/ncol(meta))))



# task 6 ------------------------------------------------------------------


library(matrixStats)

colStats = function(x) {
  cbind(
    colSds = colSds(as.matrix(x)),
    colVars = colVars(as.matrix(x)),
    colSums = colSums(as.matrix(x)),
    colMeans = colMeans(as.matrix(x)),
    colMedians = colMedians(as.matrix(x))
  )
}
#статистика
colStats(meta)
summary(meta)

split_data = split(meta, cumsum(meta[,1] == '0'))

for (i in split_data){
  print(summary(i))
}

#графики

hist(meta$v1, breaks = 100, col = 'red')
hist(meta$v2, breaks = 100, col = 'yellow')
hist(meta$v3, breaks = 100, col = 'blue')
hist(meta$v4, breaks = 100, col = 'darkgreen')
hist(meta$v5, breaks = 100, col = 'pink')


hist(split_data[[1]]$v5, breaks = 100, col = 'orange')

# и тд


# task 7 ------------------------------------------------------------------

library(ggplot2)

ggplot(meta[1:10000,], aes(time, v5)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 90))


source("D:/R-start/pr5/met.R")


v1 = c(meta$v1) + 1

alt_arimf = Alter_Johns(out_of_trend(v1[1:5000],2,'Arifm'))
alt_geom = Alter_Johns(out_of_trend(v1[1:5000], 25, "Geom"))
alt_garm = Alter_Johns(out_of_trend(v1[1:5000], 100, "Garm"))
p1 = plot(alt_arimf, type = 'l', col='darkorange')
p2 = plot(alt_geom, type = 'l', col = 'darkgreen')
p3 = plot(alt_garm, type = 'l', col = 'darkred')
