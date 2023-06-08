# Define population (Hieght of sri lankans)
p_meanHight <- 1.6
p_sd <- 2
# create samples
sampleSize <- 10
sample <- rnorm(sampleSize,p_meanHight,p_sd)
sampleMean <- mean(sample)

# create an array using c function
sample_means <- c()

# create 100 samples from above population using rnorm function 
#and create an array which will contain the mean of each sample
number_of_samples <- 100
for(i in 1:number_of_samples){
  sample <- rnorm(sampleSize,p_meanHight,p_sd)
  sample_means[i] <- mean(sample)
}

# this prints the variable value
print(sample_means)

# draw histogram
print(hist(sample_means, main="p_mean = 1.6 p_sd = 2 sample_size = 10 number_of_samples = 100"))

# Now we need to randomly generate bootstrap samples from the original sample 
# which is defined in line 6
bootstrap_sample_means <- c()
for(i in 1:number_of_samples){
  bootstrap_sample_means[i] <- mean(sample(sample, sampleSize, replace = TRUE))
}
 
print(hist(bootstrap_sample_means, main="bootstrap sampling"))

# Calculate the standard deviation of sample means and bootstrap sample means
sd_sample_means <- sd(sample_means)
sd_bootstrap_sample_means <- sd(bootstrap_sample_means)
print(sd_sample_means)
print(sd_bootstrap_sample_means)

# calculate standard error(se) for samples and bootstrap samples
se_samples <- sd_sample_means/sqrt(number_of_samples)
se_bootstrap_samples <- sd_bootstrap_sample_means/sqrt(number_of_samples)
print(se_samples)
print(se_bootstrap_samples)

# margin of error with 95% confidence interval =  2xstandrad error
z <- qnorm(0.975) # 0.975 corresponds to the 97.5th percentile of the standard normal distribution for a 95% CI
margin_of_error_samples <- z*se_samples/sqrt(number_of_samples)
margin_of_error_bootstrap_samples <- z*se_bootstrap_samples/sqrt(number_of_samples)

print(margin_of_error_samples)
print(margin_of_error_bootstrap_samples)
