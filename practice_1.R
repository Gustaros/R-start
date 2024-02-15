
# Part 1 Task1 ------------------------------------------------------------

x <- 2
y <- 4
x <- x + y
y <- x - y
x <- x - y
print(x)
print(y)


# Part1 Task 2 ------------------------------------------------------------

x <- 3.5
y <- "2,6"
z <- 1.78
h <- TRUE

# 1)
class(x)
class(y)
class(z)
class(h)

# 2)
h <- as.integer(h)
class(h)

# 3)
y <- as.numeric(gsub(",", ".", y))
class(y)

# 4)
x <- as.character(x)
class(x)


# Part 1 Task 3 -----------------------------------------------------------

dohod <- 1573
dohod <- log(dohod)
print(dohod)



# Part 1 Task 4 -----------------------------------------------------------

variant_number <- 17
writeLines(as.character(variant_number), "variant.txt")
variant <- as.numeric(readLines("variant.txt"))
result <- 2 * variant - 1
print(result)



# Part 2 Task 1 -----------------------------------------------------------

g <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)

print(g[1])
print(g[length(g)])
print(g[3:5])
print(g[g == 2])
print(g[g > 4])
print(g[which(g %% 3 == 0 & !is.na(g))])
print(g[g > 4 & g %% 3 == 0 & !is.na(g)])
print(g[g < 1 | g > 5])
print(which(g == 0))
print(which(g >= 2 & g <= 8))
print(sort(g[order(g != 2, is.na(g), g)]))




# Part 2 Task 2 -----------------------------------------------------------

vec <- c(1, 2, 3, 4, 5)
vec[length(vec)] <- NA
print(vec)



# Part 2 Task 3 -----------------------------------------------------------

vec <- c(1, NA, 3, NA, 5)
print(which(is.na(vec)))



# Part 2 Task 4 -----------------------------------------------------------

vec <- c(1, NA, 3, NA, 5)
count_na <- sum(is.na(vec))
print(count_na)



# Part 2 Task 5 -----------------------------------------------------------

respondent_ids <- 1:100
print(respondent_ids)



# Part 2 Task 6 -----------------------------------------------------------

#???????

countries <- c("France", "France", "France", "France", "France", 
               "Italy", "Italy", "Italy", "Italy", "Italy",
               "Spain", "Spain", "Spain", "Spain", "Spain")
print(countries)
years <- c(2019, 2020, 2020, 2018, 2017,
           2019, 2020, 2020, 2018, 2017,
           2019, 2020, 2020, 2018, 2017)
print(years)



# Part 2 Task 7 -----------------------------------------------------------

income <- c(10000, 32000, 28000, 150000, 65000, 1573)

average_income <- sum(income) / length(income)

income_class <- ifelse(income < average_income, 0, 1)
print(income_class)



# Part 2 Task 8 -----------------------------------------------------------

N <- 17
P <- 1.74
x <- runif(N)
write.table(x, file = "coords.txt")

norm_result <- sum(abs(x)^P)^(1/P)
write.table(norm_result, file = "result.txt")



# Part 2 Task 9 -----------------------------------------------------------

N <- 17
x <- runif(N)
write.table(x, file = "coords.txt", col.names = FALSE)

x <- as.numeric(readLines("coords.txt")$V1)

first_diff <- diff(x, differences = 1)
second_diff <- diff(first_diff, differences = 1)

write.table(cbind(first_diff, second_diff), file = "diff_vectors.txt", col.names = FALSE)
