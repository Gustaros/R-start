

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