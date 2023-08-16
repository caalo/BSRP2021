setwd("Documents/Broad_Institute_BSRP/2021/")
library(dplyr)
#demo 1
parameter = mean(iris$Sepal.Length)
total_trials = 1000
all_statistics = c()
for(trial in 1:total_trials) {
  iris_sample = sample_n(iris, 100)
  statistic = mean(iris_sample$Sepal.Length)
  all_statistics = c(all_statistics, statistic) 
}

hist(all_statistics, breaks=30, xlim = c(5.3, 6.5))
abline(v = parameter, col="blue")
abline(v = parameter + .1, col="red")
abline(v = parameter - .1, col="red")

sum(abs(all_statistics - parameter) < .1) / total_trials


#demo 2
set.seed(100)
genes = read.csv('chap04e1HumanGeneLengths.csv')
#population histogram
hist(genes$geneLength, breaks = 500, xlim = c(0, 15000), main = "", xlab = "Gene length")
abline(v = mean(genes$geneLength), col = "blue", lwd = 2)
#probability histogram ???
h = hist(genes$geneLength, plot=F)
h$counts = h$counts/sum(h$counts)
plot(h,  density = 50, xlim = c(0, 15000), main = "", xlab = "Gene length")

genes_sample = sample_n(genes, 1000)
hist(genes_sample$geneLength, breaks = 400, xlim = c(0, 15000), main = "", xlab = "Gene length")
abline(v = mean(genes_sample$geneLength), col = "blue", lwd = 2)




total_trials = 500
all_statistics = c()
parameter = mean(genes$geneLength)
for(trial in 1:total_trials) {
  genes_sample = sample_n(genes, 2000)
  statistic = mean(genes_sample$geneLength)
  all_statistics = c(all_statistics, statistic) 
}

hist(all_statistics, breaks = 80, xlim = c(2400, 2800), main = "", xlab = "Mean gene length")
abline(v = parameter, col = "red", lwd = 2)
abline(v = parameter - 50, col = "yellow", lwd = 2)
abline(v = parameter + 50, col = "yellow", lwd = 2)

sum(abs(all_statistics - parameter) < 50) / total_trials

statistic = 2640
hist(rnorm(1000, mean=2620, sd=10), 100, xlab = "Null hypothesis sampling distribution",  main = "H_O = 2620bp", xlim = c(2600, 2700))
abline(v = statistic, col = "blue", lwd = 2)


#
set.seed(12354)
temperature = read.csv("human_temperature.txt")
dim(temperature)
h_0 = rnorm(n = 10000, mean = 98.6)

h = hist(h_0, plot=F)
h$counts = h$counts/sum(h$counts)
plot(h, xlab = "Sampling distribution for H_0 = 98.6", ylab = "Probability", main = "")
abline(v = mean(temperature$temp - 1), col = "blue", lwd = 2)
abline(v = 98.6 + abs(98.6 - mean(temperature$temp - 1)), col = "brown", lwd = 2)
