# Questions 5 and 6 ####

# R has a built in function, pbirthday
# Check ?pbirthday for documentation

message("Preliminary results for demonstration")
pbirthday(n = 2, classes = 365)

1 / 365

1 - (365/365)*(364/365)

# Custom function to calculate probability there are no matches
# with d days and n people
p_no_match <- function(d, n) {
  prod(d:(d - n + 1)) / d^n  
}

1 - p_no_match(365,2)

message("\n\n----- 2 People in Room -----")
# Simulation approach
# Generate a sample of 2 days and check if they match
set.seed(6644)
(birthdays <- sort(sample(365, size = 25, replace = TRUE)))

message(paste("Any duplicated birthdays? ", any(duplicated(birthdays))))

# Do this many times to approximate the probability calculated above
check_sample <- function(d, n) {
  birthdays <- sample(d, size = n, replace = TRUE)
  any(duplicated(birthdays))
}

results <- replicate(1e5, check_sample(365, 2))
message(paste("Probability from simulation: ", mean(results)))
message(paste("Probabiltiy from prbirthday: ", pbirthday(n = 2, classes = 365)))

# See how far off we were
diff <- mean(results) - pbirthday(n = 2, classes = 365)
message(paste("Difference between simulation and exact: ", diff))

# Now try for 3 people in the same room
message("\n\n----- 3 People in Room -----")
message(paste("Probability from pbirthday: ", pbirthday(n = 3, classes = 365)))

message(paste("Probability from doing 1 minus method: ", 1 - (365/365)*(364/365)*(363/365)))

results <- replicate(1e5, check_sample(365, 3))
message(paste("Probability from simulation: ", mean(results)))

# See how far off we were
diff <- mean(results) - pbirthday(n = 3, classes = 365)
message(paste("Difference between simulation and exact: ", diff))

# Using permutation formula
perm <- function(n, k) {
  prod(n:(n-k+1))
}

my_pbirthday <- function(n, classes) {
  1 - perm(classes, n) / classes^n
}

message(paste("Probability from doing 1 minus method in a custom function: ", my_pbirthday(n = 3, classes = 365)))

# Questions 7 and 8 ####
# If you don't have the following packages, then install them by uncommenting
# the following two lines.
# install.packages("ggplot2")
# install.packages("ggforce")
library(ggplot2)
library(ggforce)

# Generate many uniform xs and ys between 0 and 1
x <- runif(1e3)
y <- runif(1e3)

# Logical indicating whether or not the point "landed" inside the circle
inside_circle <- (x - 0.5)^2 + (y - 0.5)^2 < 0.5^2

# Graph to show the points and whether or not they landed inside the circle
ggplot(data.frame(x = x, y = y, inside_circle = inside_circle)) +
  geom_point(aes(x = x, y = y, color = inside_circle)) +
  geom_circle(aes(x0 = 0.5, y0 = 0.5, r = 0.5)) +
  scale_color_manual(values = c("firebrick", "forestgreen")) +
  theme_bw()

# Find proportion by dividing number inside circle divided by total number
sum(inside_circle == TRUE) / length(inside_circle)
# Alternative is to take the mean of the logical vectors (TRUE = 1, FALSE = 0)
mean(inside_circle)

# Approximation of pi is proportion times 4
mean(inside_circle) * 4

# Function that runs one simulation of throwing n_points and calculates
# the approximation for pi by taking 4 times the proportion of darts
# that land inside the circle
get_pi <- function(n_points) {
  x <- runif(n_points)
  y <- runif(n_points)
  
  inside_circle <- (x - 0.5)^2 + (y - 0.5)^2 < 0.5^2
  
  mean(inside_circle) * 4
}

# See what happens if select 500 points
get_pi(500)

# Run the simulation of 500 points 1000 times
pis <- replicate(1e4, get_pi(500))

# Look at the distribution of pi approximations
summary(pis)
hist(pis, 100)
quantile(pis, probs = c(0.025, 0.5, 0.975))


# Run the simulation with many, many more points
pis <- replicate(1e4, get_pi(1e4))
summary(pis)
hist(pis, 100)
quantile(pis, probs = c(0.025, 0.5, 0.975))



# Question 10
# Create an empty vector to hold 16 of the x values
x_i <- numeric(16)
# Set the seed to 1
x_0 <- 1
# This is a slightly different generator than the one in the problem
# This one might be even MORE awful than the one in the assignment!
for (i in 1:16) {
  if (i == 1) {
    x_i[i] <- (3 * x_0 + 1) %% 8
  } else {
    x_i[i] <- (3 * x_i[i - 1] + 1) %% 8
  }
}

x_i
sum(duplicated(x_i))

# Question 11
x_i <- numeric(1e4)
# Set the seed to 1
x_0 <- 6644
for (i in 1:1e3) {
  if (i == 1) {
    x_i[i] <- (16807 * x_0) %% (2^31 - 1)
  } else {
    x_i[i] <- (16807 * x_i[i - 1] + 1) %% (2^31 - 1)
  }
}



# Examine first 25 generated
x_i[2:26]
x_i[2:26] / (2^31 - 1)
# Check to see if any are duplicated
sum(duplicated(x_i))

(16807 * x_0 ) %% (2^31 - 1)
x_i[1]

# Question 12 ####
set.seed(1)
n <- 5e6
U <- runif(n)
x_1 <- -(1/5) * log(U)
x_2 <- -(1/5) * log(1 - U)
x_3 <- rexp(n, rate = 5)

mean(x_1)
mean(x_2)
mean(x_3)

var(x_1)
var(x_2)
var(x_3)

summary(x_1)
summary(x_2)
summary(x_3)

par(mfrow = c(3, 1))
hist(x_1, 100, main = "U")
hist(x_2, 100, main = "1 - U")
hist(x_3, 100, main = "R Generated")

