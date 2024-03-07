

# Part 1 Task 1 -----------------------------------------------------------

matr = matrix(3, 3, 4)
matr[1,3] <- 4
matr[2,1] <- 1
matr[3,2] <- NA
matr[3,4] <- 1


# Part 1 Task 2 -----------------------------------------------------------


a <- c(1, 3, 4, 9, NA)
b <- c(5, 6, 7, 0, 2)
c <- c(9, 10, 13, 1, 20)
vec_cols = cbind(a,b,c)
vec_rows = rbind(a,b,c)
rownames(vec_cols) <- paste0("row", 1:5)
colnames(vec_rows) <- paste0("col", 1:5)


# Part 1 Task 3 -----------------------------------------------------------


names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)
diff_el = cbind(ages, gender)
rownames(diff_el) <- names
square_age <- ages ^ 2
diff_el <- cbind(diff_el, square_age)


# Part 1 Task 4 -----------------------------------------------------------


info = list(names,ages, gender)
info[[1]][2]
print(info[3])
names(info) <- c("name", "age", "gender")
print(info["name"])
drinks <- c("juice", "tea", "rum", "coffee")
info$drinks <- drinks
info_John = c("John", 2, 1, "milk")
for (i in 1:length(info_John)) {
  info[[i]] <- c(info[[i]], info_John[i])
}



# Part 1 Task 5 -----------------------------------------------------------


index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
num_vec <- gsub(",", ".", index)
num_vec <- as.numeric(unlist(strsplit(num_vec, ";")))




# Part 2 Task 1 -----------------------------------------------------------


A <- diag(c(4,9), 2, 2)
rownames(A) <- paste0("eq", 1:2)
colnames(A) <- paste0("x", 1:2)


# Part 2 Task 2 -----------------------------------------------------------


t <- eigen(A)
print(t[[1]])


# Part 2 Task 3 -----------------------------------------------------------


I <- diag(x=1, 2, 2)
B <- I - A
print(B)


# Part 2 Task 4 -----------------------------------------------------------


f <- c(4, 2)
u <- c(0.2, -0.3)
dim(f) <- c(2, 1)
dim(u) <- c(2, 1)


# Part 2 Task 5 -----------------------------------------------------------


R_A <- solve(A)
u_result <- R_A %*% f



# Part 2 Task 6 -----------------------------------------------------------

u1 = B%*%u+f
u2 = B%*%u1+f
u3 = B%*%u2+f
u4 = B%*%u3+f
u5 = B%*%u4+f
u6 = B%*%u5+f
u7 = B%*%u6+f


# Part 2 Task 7 -----------------------------------------------------------

res1 = abs(u7 - u_result)
print(res1)


# Part 2 Task 8 -----------------------------------------------------------


A <- A/max(A)
print(A)
f <- f/max(A)
print(f)


# Part 2 Task 9 -----------------------------------------------------------


# 2
t <- eigen(A)
print(t[[1]])

# 3
I <- diag(x=1, 2, 2)
B <- I - A
print(B)

# 4
f <- c(4, 2)
u <- c(0.2, -0.3)
dim(f) <- c(2, 1)
dim(u) <- c(2, 1)

# 5
R_A <- solve(A)
u_result <- R_A %*% f


# 6
u1 = B%*%u+f
u2 = B%*%u1+f
u3 = B%*%u2+f
u4 = B%*%u3+f
u5 = B%*%u4+f
u6 = B%*%u5+f
u7 = B%*%u6+f

# 7

res2 = abs(u7 - u_result)
print(res2)


# Part 2 Task 10 ----------------------------------------------------------

print(abs(res1-res2))




# Part 3 ------------------------------------------------------------------

step <- 1
dekart_begin <- -5 
dekart_end <- 5

x <- seq(from = dekart_begin, to = dekart_end, by = step)
y <- x

surface_matrix <- outer(X=x,
                        Y=y,
                        FUN=function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)


# Part 3 Task 1 -----------------------------------------------------------


diag_sum <- 0
for (i in 1:dim(surface_matrix)[1]) {
  diag_sum <- diag_sum + surface_matrix[i, i]
}

data = c(c("number of matrix elements: ", length(surface_matrix)),
         c("number of rows: ", dim(surface_matrix)[1]),
         c("number of cols: ", dim(surface_matrix)[2]),
         c("sum of main diag elements: ", diag_sum),
         c("sum of main diag elements: ", diag_sum),
         c("sum of middle row elements: ", sum(surface_matrix[6,])), 
         c("sum of middle column elements: ", sum(surface_matrix[,6])),
         c("row sums: ", rowSums(surface_matrix)),
         c("col sums: ", colSums(surface_matrix)))

write(data, 'summary.txt')



# Part 3 Task 2 -----------------------------------------------------------



# dekart_begin <- as.numeric(readline(prompt = "dekart_begin="))
# dekart_end <- as.numeric(readline(prompt = "dekart_end="))
# step <- as.numeric(readline(prompt = "step="))

x <- seq(from = dekart_begin, to = dekart_end, by = step)
y <- x

surface_matrix <- outer(X=x, Y=y, FUN=function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)


data = c(c("number of matrix elements: ", length(surface_matrix)),
         c("number of rows: ", dim(surface_matrix)[1]),
         c("number of cols: ", dim(surface_matrix)[2]),
         c("sum of main diag elements: ", diag_sum),
         c("sum of main diag elements: ", diag_sum),
         c("row sums: ", rowSums(surface_matrix)),
         c("col sums: ", colSums(surface_matrix)))

write(x = data, file = 'summary2.txt')



# Part 3 Task 3 -----------------------------------------------------------


info <- as.numeric(readLines(con = "input.txt", n = 5, encoding = "UTF-8"))

l = info[2] - info[1]
steps = info[3:5]

intervals = l / steps

x <- c(seq(from = dekart_begin, to = dekart_end, by = intervals[1]), 
       seq(from = dekart_begin, to = dekart_end, by = intervals[2]), 
       seq(from = dekart_begin, to = dekart_end, by = intervals[3]))
y <- x

surface_matrix <- outer(X=x, Y=y, FUN=function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)


data = c(c("number of matrix elements: ", length(surface_matrix)),
         c("number of rows: ", dim(surface_matrix)[1]),
         c("number of cols: ", dim(surface_matrix)[2]),
         c("sum of main diag elements: ", diag_sum),
         c("sum of main diag elements: ", diag_sum),
         c("row sums: ", rowSums(surface_matrix)),
         c("col sums: ", colSums(surface_matrix)))

write(x = data, file = 'summary3.txt')




# Part 4 Task 1 -----------------------------------------------------------

cars_matrix <- as.matrix(cars)
cars_speed <- cbind(1, cars_matrix[,1])


# Part 4 Task 2 -----------------------------------------------------------

cars_dist <- cars_matrix[,2]


# Part 4 Task 3 -----------------------------------------------------------

alpha <- solve(t(cars_speed) %*% cars_speed) %*% t(cars_speed) * cars_dist


# Part 4 Task 4 -----------------------------------------------------------

vector <- as.vector(alpha)


# Part 4 Task 5 -----------------------------------------------------------

alpha_c <- alpha[1]
alpha_x <- alpha[2]
cat("alpha_c =", alpha_c)
cat("alpha_x =", alpha_x)


# Part 4 Task 6 -----------------------------------------------------------

cars_speed_lm <- cars_matrix[,1]


# Part 4 Task 7 -----------------------------------------------------------

cars_dist_lm <- alpha_c + cars_speed_lm * alpha_x


# Part 4 Task 8 -----------------------------------------------------------

dist_residuals <- cars_dist_lm - cars_dist


# Part 4 Task 9 -----------------------------------------------------------

m = mean(dist_residuals)
s = sd(dist_residuals)


# Part 4 Task 10 ----------------------------------------------------------

print(cars_dist_lm)


# Part 4 Task 11 ----------------------------------------------------------

cat("mean: ",m, "\n","st",s)
