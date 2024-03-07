sin(pi)

100-1

fun_1 <- function(x,y,n=2){
  x*y**n
}

print('test')

fun_1(2,2)


# ctrl + shift + r (create label) -----------------------------------------

x <- 68

x = '123'

print(x)

date()



# Vectors -----------------------------------------------------------------

var_integer = 78

is.vector(var_integer)
length(var_integer)
var_integer[1]
is.integer(var_integer)
var_integer[10] = 56.12 
is.numeric(var_integer)
var_integer


# set vector --------------------------------------------------------------

vec_1 = 4:10
10:4
-4:4
4.02:9


# последовательности -------------------------------------------------------

seq()
?seq

seq(-1,5,0.25)

f = function(x) {sin(x^5) + x}
n = 100

grid = seq(-1,1, length.out = n+1)

centers = (grid[2:length(grid)] + grid[1:(length(grid))])
sum(f(centers) * (grid[2] - grid[1]))


# combine -----------------------------------------------------------------

vec_c = c(5,6.1,0.9,-3)
vec_c

c(vec_1,vec_c)


# operations witn vectors -------------------------------------------------


1:5 + 2:6

1:5 + 2:8

rep(1:5,2)

2:11 < 6
vec_1[vec_1 < 6 & vec_1 > 4]

sum(vec_1)

prod(vec_1)
sd(vec_1)
median(vec_1)
quantile(vec_1,0.2)
