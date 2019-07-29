library(datasets)
library(bootstrap)
library(ggplot2)

data()
airquality
ecdf.temp <- ecdf(airquality$Temp)
plot(ecdf.temp)

alpha <- 0.05
n <- length(airquality$Temp)
eps <- sqrt(log(2/alpha)/(2*n))
grid <- seq(0, 100, length.out = 1000)
lines(grid, pmin(ecdf.temp(grid)+eps, 1), col = "red")
lines(grid, pmax(ecdf.temp(grid)-eps, 0), col = "red")



# empirical cdf of airquality
ggplot(airquality, aes(x = Temp)) + stat_ecdf(geom = "step")


# Playing around with ecdf
ggplot(airquality, aes(x = Temp)) + stat_ecdf(geom = "step") + 
  geom_line(aes(x = Temp, y = ecdf.temp(Temp)+ eps, col = "red")) + 
  geom_line(aes(x = Temp, y = ecdf.temp(Temp)- eps, col = "red"))


# Bootstrap Median
med <- median(airquality$Temp)
med
temp_result <- bootstrap(airquality$Temp, 1000, median)
med.boot <- temp_result$thetastar
se.boot <- sd(temp_result$thetastar)
se.boot
mean(med.boot)

ci_med_95 <- c(med - 2*se.boot, med + 2*se.boot)
ci_med_95
piv_95 <- 2*med - quantile(med.boot, probs = c(0.975, 0.025))
piv_95
quant_95 <- quantile(med.boot, probs = c(0.025, 0.975))
quant_95

# Bootstrap the mean
mean_temp <- mean(airquality$Temp)
mean_temp
sd_temp <- sd(airquality$Temp)
sd_temp
temp_mean_result <- bootstrap(airquality$Temp, 1000, mean)
mean.boot <- temp_mean_result$thetastar
mean(mean.boot)
se_boot <- sd(temp_mean_result$thetastar)
ci_mean_95 <- c(mean_temp - 2*se_boot, mean_temp + 2*se_boot)
ci_mean_95
piv_95_mean <- 2*mean_temp - quantile(mean.boot, probs = c(0.975, 0.025))
piv_95_mean
quant_95_mean <- quantile(mean.boot, probs = c(0.025, 0.975))
quant_95_mean

# MLE for airquality temp
hist(airquality$Temp, probability = TRUE, main = "Histogram of Temperature",
     xlab = "Temperature")
abline(v = mean_temp, col = "red")
lines(seq(50,100, length.out = 1000), dnorm(seq(50,100, length.out = 1000), 
                                          mean =mean_temp, sd = sd(airquality$Temp)),
      col = "blue")






# linear modeling
library(dplyr)
head(airquality)
air <- mutate(airquality, may = if_else(Month == 5, 1, 0))
air <- mutate(air, jun = if_else(Month == 6, 1, 0))
air <- mutate(air, jul = if_else(Month == 7, 1, 0))
air <- mutate(air, aug = if_else(Month == 8, 1, 0))


mo <- lm(Temp ~ Ozone + Wind + may + jun + jul + aug, data = air)
summary(mo)

# Bayesian
air2 <- mutate(air, Below_70 = if_else(Temp <=70, 1, 0))
temps_low70 <- sum(air2$Below_70) / length(air2$Below_70)
temps_grtr_70 <- 1-temps_low70

wind_low.temp_low <- sum(air2$Wind <= 10 & air2$Temp <= 70) / sum(air2$Below_70)
wind_high.temp_low <- 1- wind_low.temp_low
wind_high.temp_high <- sum(air2$Wind > 10 & air2$Temp > 70) / (n - sum(air2$Below_70))
wind_low.temp_high <- sum(air2$Wind <= 10 & air2$Temp > 70) / (n - sum(air2$Below_70))

p1 <- wind_low.temp_high
p2 <- wind_low.temp_low
p1-p2
p1_Y <- sum(air2$Wind > 10 & air2$Temp > 70)
p2_Y <- sum(air2$Wind <= 10 & air2$Temp <= 70)
p1_n <- (n - sum(air2$Below_70))
p2_n <- sum(air2$Below_70)
p1_alpha <- p1_Y + 1
p2_alpha <- p2_Y + 1
p1_beta <- p1_n - p1_Y + 1
p2_beta <- p2_n - p2_Y + 1

posterior_mean1 <- (p1_Y + p1_alpha) / (p1_n + p1_alpha + p1_beta)
posterior_mean2 <- (p2_Y + p2_alpha) / (p2_n + p2_alpha + p2_beta)
posteriordiff <- posterior_mean1 - posterior_mean2

# Comparison of the mle vs. differences in posterior mean

# Kruskal-Wallis Rank sum test
kruskal.test(Temp ~ Month, data = airquality)

# ANOVA
summary(aov(Temp~Month, data = airquality))
boxplot(Temp~Month, data = airquality, xlab = "Months", 
        ylab = "Temperature", main = "Temperature over Months")

model <- lm(Temp ~ Month, data = airquality)
summary(model)
