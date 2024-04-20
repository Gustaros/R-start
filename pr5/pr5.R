

# Part 2 stage 1 ------------------------------------------------------------------



if ("quantmod" %in% rownames(installed.packages()) == FALSE) {
  install.packages("quantmod") }
library(quantmod)
if ("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr") }
library(stringr)
downloadable_stocks = c("TSLA", "^IXIC")
quantmod::getSymbols(Symbols = downloadable_stocks,
                     src = "yahoo",
                     from = as.Date.character("1900-01-01"))
df = data.frame(get(downloadable_stocks[1]))
downloadable_stocks = stringr::str_remove(downloadable_stocks,"[:punct:\\^]")
rm(list = downloadable_stocks)



# Part2 stage 2 -----------------------------------------------------------


# ?ссылка в сдо?
# https://online-edu.mirea.ru/mod/url/view.php?id=374585
# =- редирект на https://github.com/qwerty29544/IMSSLAER/blob/master/R/MariaRemark.R

out_of_trend = function(x, dt, method='Arifm'){
  stopifnot(length(x) > 3 & dt <= (ceiling(length(x)/2)-1) & is.numeric(x) & is.numeric(dt))
  stopifnot(method %in% c('Arifm', 'Geom', 'Garm'))
  i = (1+dt):(length(x)-dt)
  y = switch(method,
              Arifm = log((x[i-dt] + x[i+dt])/(2*x[i])),
              Geom = log((x[i-dt] * x[i+dt])/(x[i]^2)),
              Garm = log((2 * x[i-dt] * x[i+dt])/(x[i] * (x[i-dt] + x[i + dt]))))
  return(y)
}



# Part 2 stage 3 ----------------------------------------------------------

t = seq(0, 10, 0.1)
x = 2* t + 3 + sin(2*t)
mean(x)
xn = out_of_trend(x, 2, 'Arifm')
mean(xn)



# Part 2 stage 4 ----------------------------------------------------------



Alter_Johns = function(y){
  for (i in y){
    stopifnot(is.numeric(i))
  }
  at = numeric(length(y)-1)
  for (i in (1:(length(y)-1))){
    at[i] = sum((1/(length(y)-i)) * abs(y[(1+i) : length(y)] - y[1:(length(y)-i)]))
  }
  return (at)
}


# part 2 stage 5 ----------------------------------------------------------

Alter_Johns(xn)
plot(Alter_Johns(xn), type = 'l', col='darkblue', lwd=3)



# part 2 stage 6 ----------------------------------------------------------



alt_arimf = Alter_Johns(out_of_trend(df$TSLA.Adjusted,20,'Arifm'))
alt_geom = Alter_Johns(out_of_trend(df$TSLA.Adjusted, 50, "Geom"))
alt_garm = Alter_Johns(out_of_trend(df$TSLA.Adjusted, 80, "Garm"))
p1 = plot(alt_arimf, type = 'l', col='darkorange')
p2 = plot(alt_geom, type = 'l', col = 'darkgreen')
p3 = plot(alt_garm, type = 'l', col =  'darkred')








# Part 3 ------------------------------------------------------------------


SIM = function(A, u0, f, n_iter = 10e5, eps=10e-7){
  stopifnot(n_iter>=0 & class(n_iter)=='numeric')
  stopifnot(eps>=0 & is.numeric(eps))
  stopifnot(is.numeric(A))
  stopifnot(class(f) == 'numeric')
  stopifnot(length(A)>2, sqrt(length(A)) %% 1 == 0)
  maxaf = max(A, f)
  A = A/maxaf
  f = f/maxaf
  B_matrix = diag(rep(1,ncol(A))) - A
  u1 = as.vector(B_matrix%*%u0 + f)
  for (i in c(1:n_iter)){
    if (max(abs(u1 - u0)) >= eps){
      u0 = u1
      u1 = B_matrix%*%u0 + f
    }
    else break
  }
  return (as.vector(u1))
}


A = diag(c(0.3, 0.4, 0.5), nrow = 3, ncol = 3)
f = rnorm(3)
u = rnorm(3)
result = SIM(A = A, u0 = u, f = f)
print(result)


