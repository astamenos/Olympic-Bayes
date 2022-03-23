# Import libraries and data
library(ggplot2)
library(dplyr)
medals <- read.csv('Medals.csv')
set.seed(21222)

# AGGREGATE ANALYSIS
# Formatting the data
aggregates <- medals %>% summarise(Y0 = sum(MEDALS.WON.DURING.PREVIOUS.OLYMPICS),
                                   Y1 = sum(MEDALS.WON.DURING.HOST.YEAR),
                                   N0 = sum(PARTICIPATING.ATHLETES.DURING.PREVIOUS.OLYMPICS),
                                   N1 = sum(PARTICIPATING.ATHLETES.DURING.HOST.YEAR))
colnames(medals) <- c('country', 'year', 'Y0', 'Y1', 'N0', 'N1')
S <- 10^6
N0 <- aggregates$N0
Y0 <- aggregates$Y0
N1 <- aggregates$N1
Y1 <- aggregates$Y1
epsilon <- 0.1

# Sampling from the posterior distributions
lambda <- seq(0.01, 0.3, 0.0001)
posterior_0 <- dgamma(lambda, Y0 + epsilon, N0 + epsilon)
posterior_1 <- dgamma(lambda, Y1 + epsilon, N1 + epsilon)
df <- data.frame(lambda, posterior_0, posterior_1)

# Visualizations of posterior distributions
p <- ggplot(data = df, aes(x = lambda)) 
p <- p + ggtitle('Medal-Winning Rate Posteriors') + labs(x = expression(lambda),
                                                         y = 'density',
                                                         color = 'Legend',
                                                         linetype = 'Legend')
p <- p + geom_line(aes(y = posterior_0, 
                       color = 'Previous Year Posterior', 
                       linetype = 'Previous Year Posterior')) 
p <- p + geom_line(aes(y = posterior_1,
                       color = 'Host Year Posterior',
                       linetype = 'Host Year Posterior'))

# HYPOTHESIS TEST
epsilon <- c(0.01, 0.1, 1, 10, 100)
prob_null <- c()

# MC samples from the posteriors
for(e in epsilon) {
  lambda_0 <- rgamma(S, Y0 + e, N0 + e)
  lambda_1 <- rgamma(S, Y1 + e, N1 + e)
  
  # Approximate probability that theta2 > theta1
  prob_null <- c(prob_null, mean(lambda_1 > lambda_0))
}

# PREDICTION
# Loading the data and setting parameters for prior
medals <- medals %>% mutate(pct_change_N = (N1 - N0)/N0)
median_pct_change_N <- quantile(medals$pct_change_N, 0.5)
a <- b <- 0.1
N0_FR <- 398
Y0_FR <- 33

# Estimating the number of athletes from France
estimated_N1_FR <- N0_FR * (1 + median_pct_change_N)
lambda_star <- rgamma(S, median(medals$Y1) + a, median(medals$N1) + b)
ppd <- data.frame(Y1_star = rpois(S, estimated_N1_FR * lambda_star))
plot_ppd <- ggplot(ppd, aes(Y1_star)) + ggtitle('Posterior Predictive Distribution') + labs(x = expression(Y^"*"))

plot_ppd <- plot_ppd + geom_histogram(color = 'black',
                                      fill = 'darkred',
                                      alpha = 0.5,
                                      bins = 25)

# Posterior median and 95% credible interval
quantiles <- quantile(ppd$Y1_star, c(0.025, 0.5, 0.975))

# COUNTRY-SPECIFIC ANALYSIS
country_specific <- medals %>% group_by(country) %>%
  summarise(Y0 = sum(Y0), 
            Y1 = sum(Y1),
            N0 = sum(N0),
            N1 = sum(N1))

posterior_summaries <- data.frame(country = c(), CI95_low = c(), MAP = c(), CI95_high = c())
e <- 0.1

for(j in 1:15){
  lambda_0 <- rgamma(S, country_specific$Y0[j] + e, country_specific$N0[j] + e)
  lambda_1 <- rgamma(S, country_specific$Y1[j] + e, country_specific$N1[j] + e)
  r <- lambda_1/lambda_0
  CI95_low = quantile(r, 0.025)
  med = quantile(r, 0.5)
  CI95_high = quantile(r, 0.975)
  posterior_summaries <- rbind(posterior_summaries, 
                               data.frame(country = country_specific$country[j],
                                          CI95_low = round(CI95_low, 2),
                                          med = round(med, 2),
                                          CI95_high = round(CI95_high, 2)))
}
