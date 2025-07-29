setwd("~/Documents/PLSC_341/Danielwangplsc341finalproject")
library(dplyr)
library(tidyverse)
library(randomizr)
library(readr)
library(estimatr)
library(DeclareDesign)
library(tidyverse)
library(ggplot2)
dat <- read_csv('banana_data_complete.csv')


#Exporatory Basic Analysis
#Plot of data
hist(dat$Y[dat$D == 0], main = "Control Group Ripening Times", ylab = "Number of Bananas", xlab = "Days", col = 'yellow')

hist(dat$Y[dat$D == 1], main = "Treatment Group Ripening Times", ylab = "Number of Bananas", xlab = "Days", col = 'yellow')

#Let's start off with finding difference in means between our treatment and control group:

#First, our mean of the control group is 13.6666, and the mean of the treatment group is 14.2667.
dat |> 
  summarize(DIM = mean(Y[D==1])-mean(Y[D==0]))
#With a Difference in Means of -0.6, on average, those in the treatment group ripened approximately 0.6 days faster than those in the control group.
#Analysis:

   #Part 1: Randomization Inference
sims<- 10000
estimates <- rep(NA, sims)
for(i in 1:sims){
  dat_new <- dat |>
    mutate(
      D_new = complete_ra(N= nrow(dat), m = 10),
      Y_new = Y*D_new + (1-D_new) * Y
    )
  estimates[i] <- with(dat_new, mean(Y_new[D_new == 1]) - mean(Y_new[D_new == 0]))
}
(p_value <- mean((estimates) >= -0.6))
p_value
hist(estimates, xlab = 'Sample Difference in Means', col = 'yellow', main = "Randomization Inference Histogram")
abline(v = -0.6, col = 'red')


#Because our p-value of 0.71 is greater than 0.05, we do not have statistically significant evidence to reject the H0 that there is no difference between putting aluminum foil on banana stems versus not putting aluminum foil on banana stems.

  #Part 2: T-Test
ttest_result <- t.test(dat$Y[dat$D == 1], dat$Y[dat$D == 0])

mean_diff <- ttest_result$estimate[1] - ttest_result$estimate[2]
ci <- ttest_result$conf.int

#Because our p-value from our t-test is 0.57, which is greater than all normal significant values(e.g.0.05, 0.10, 0.01), we do not have statistically significant evidence to reject H0 at any significance level.

  #Part 3: Power Analysis(Pre-Experiment)

design_monster <- 
  declare_model(N = 30) +  # Sample size of 30
  declare_potential_outcomes(
    Y_Z_0 = rnorm(N, mean = 15, sd = 1),  # Control: Normally distributed outcome
    Y_Z_1 = rnorm(N, mean = 16, sd = 1)   # Treatment: Mean shift of +5
  ) + 
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +  # Define ATE
  declare_assignment(Z = complete_ra(N = 30, prob = 0.5)) +  # Random assignment (p=0.5)
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +  # Observed outcome
  declare_estimator(Y ~ Z, model = difference_in_means, inquiry = "ATE")  # Difference-in-means estimator
simulations <- simulate_design(design_monster)
simulations |>
  summarise(power = mean(p.value <= 0.05))

#The power of my design, if assumptions hold, at n=30 is 0.75, which is good.


#Confidence Intervals; all on one plot, using ggplot2 library

summary_df <- dat %>%
  group_by(D) %>%
  summarise(
    estimate = mean(Y),
    n = n(),
    se = sd(Y)/ sqrt(n),
    lower = estimate - qt(0.975, df = n - 1) * se,
    upper = estimate + qt(0.975, df = n - 1) * se
  )

summary_df$label <- ifelse(summary_df$D == 1, "Treatment", "Control")
summary_df$type <- "Group Mean"

df <- data.frame(
  label = "Mean Difference",
  estimate = mean_diff,
  lower = ci[1],
  upper = ci[2],
  type = "Difference"
)

plot_df <- rbind(
  summary_df[, c("label", "estimate", "lower", "upper", "type")],
  df
)
ggplot(plot_df, aes(x = label, y = estimate, color = type)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  labs( y = "Confidence Intervals") +
  scale_color_manual(values = c("Group Mean" = "black", "Difference" = "red"))


#OSLR
dat
model1 <- tidy(lm_robust(Y ~ D + Sticker + Height, data = dat)) |>
  select(-df,-outcome,-statistic)
model1






