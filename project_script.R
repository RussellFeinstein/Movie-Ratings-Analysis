# Set working directory and import dataset
setwd("D:\\CodingClasses\\CS555\\Project")

data <- read.csv("Movies.csv", header=TRUE)

# Take 1000 samples
data.1000 <- data[sample(nrow(data), 1000),]

data.1000

# Perform Multiple Linear Regression
x1 <- data.1000$vote_count
x2 <- data.1000$popularity
y <- data.1000$vote_average
m <- lm(y~x1+x2)
summary(m)

# Get F-statistic Value
qf(.95, df1=2, df2=997)

# t critical value
qt(1-.025, df=997)

# Checking for collinearity
cor(x1, x2)


# Perform Multiple Linear Regression
# Again without sampling
x3 <- data$vote_count
x4 <- data$popularity
y2 <- data$vote_average
m2 <- lm(y2~x3+x4)
summary(m2)

# F-statistic
qf(.95, df1=2, df2=9977)

# t critical value
qt(1-.025, df=9977)

hist(resid(m))
hist(resid(m2))

plot(fitted(m), resid(m), axes=TRUE, frame.plot=TRUE,
     xlab="Fitted Values", ylab="Residuals",
     main="Residual Plot of Linear Regression on the Effect of
     Movie Popularity and Vote Count on Vote Average")
abline(h=0)
