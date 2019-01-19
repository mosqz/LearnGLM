# Code for Exercises in Chapter 2
## Q2
# Can't find the data in the provided folders
girl <- c(.4777, .4875, .4859, .4754, .4874, .4864, .4813, .4787, .4895, .4797, .4876, .4859,
           .4857, .4907, .5010, .4903, .4860, .4911, .4871, .4725, .4822, .4870, .4823, .4973)
# actual standard deviation
actual_sd <- sd(girl)
sprintf('Actual sd is: %f', actual_sd)
# theoretical standard deviation
# assume the girl proportion each month is an i.i.d. from a distribution with constant probability. 
# this constant probability is the mean of those 24 values
mean_prop <- mean(girl)
# the theoretical standard deviation could be calculated by:
thre_sd <- sqrt(mean_prop*(1-mean_prop)/3900)
sprintf('Theoretical sd is: %f', thre_sd)
# is actual sd significantly different from theoretical sd?
# the actuall variance followes a distribution with theoretical variance as the expected value and proportional to chi-square distribution with 23 degrees of freedom
# note that for chi-square distribution with m degree of freedom, its mean value is m. 
# transform the actual variance into scale of chi-square
actual_var_trans <- actual_sd^2 * 23 / thre_sd^2
# is this value significantly smaller than degree of freedom 23?
chisquare_lower5 <- qchisq(.05, df=23)
sprintf('Transformed actual variance is: %f', actual_var_trans)
sprintf('Lower 5 percents of chi-square is: %f', chisquare_lower5)

## Q3
# simulate 1000 values of x
x <- c()
for(i in 0:1000){
  x[i] <- sum(runif(20))
}
# plot the hist
hist(x, density=20, breaks=20, prob=TRUE, xlab="x-variable", main="normal curve over histogram")
# the expected normal distribution have a mean of 10, and variance of 1/12*20=5/3
# overlay the density curve
curve(dnorm(x, mean=10, sd=sqrt(5/3)), col="darkblue", lwd=2, add=TRUE, yaxt="n")

## Q4
# simulate 1000 values of difference
men_height <- rnorm(1000, mean=69.1, sd=2.9)
women_height <- rnorm(1000, mean=63.7, sd=2.7)
men_women_diff <- men_height - women_height
# plot hist
hist(men_women_diff, prob=TRUE)
sprintf('Mean of simulated difference is: %f', mean(men_women_diff))
sprintf('Sd of simulated difference is: %f', sd(men_women_diff))
# the exact value
sprintf('Mean of exact difference is: %f', 69.1-63.7)
sprintf('Sd of exact difference is: %f', sqrt(2.9^2+2.7^2))

## Q5
sprintf('Mean of average height is: %f', (69.1+63.7)/2)
sprintf('Sd of average height is: %f', sqrt(2.9^2+2.7^2+2*0.3*2.9*2.7)/2)