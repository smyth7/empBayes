# Chpt 3

library(dplyr) 
library(tidyr) 
library(Lahman)
library(ggplot2)
library(stats4)
library(knitr)

# Filter out pitchers
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>% group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>% mutate(average = H / AB)

# Include names along with the player IDs
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>% unite(name, nameFirst, nameLast, sep = " ") %>% inner_join(career, by = "playerID") %>% dplyr::select(-playerID)


career %>%
  arrange(desc(average)) %>%
  head(5) %>%
  knitr::kable(booktabs = TRUE)

career %>%
  filter(AB >= 500) %>%
  ggplot(aes(average)) +
  geom_histogram(binwidth = .005)

# estimate prior - blog

# just like the graph, we have to filter for the players we actually
# have a decent estimate of
career_filtered <- career %>%
  filter(AB >= 500)

m <- MASS::fitdistr(career_filtered$average, dbeta,
                    start = list(shape1 = 1, shape2 = 10))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

career_filtered %>%
  filter(AB > 500) %>%
  ggplot() +
  geom_histogram(aes(average, y = ..density..), binwidth = .005) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  xlab("Batting average")

# estimate prior - book

career_filtered <- career %>%
  filter(AB > 500)

# log-likelihood function
ll <- function(alpha, beta) {
  x <- career_filtered$H
  total <- career_filtered$AB
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}

# maximum likelihood estimation
m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B",
         lower = c(0.0001, .1))
ab <- coef(m)

alpha0 <- ab[1]
beta0 <- ab[2]

profile(m)

career_filtered %>%
  filter(AB > 500) %>%
  ggplot() +
  geom_histogram(aes(average, y = ..density..), binwidth = .005) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  xlab("Batting average")

# Update - conjugate prior

career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

options(digits = 3)
career_eb %>%
  arrange(desc(eb_estimate)) %>%
  head(5) %>%
  kable(booktabs = TRUE)
options(digits = 1)

# note it's not necessary to filter out the low at-bats - emp Bayes does it.


# Plot shows shrinkage to average. 
ggplot(career_eb, aes(average, eb_estimate, color = AB)) +
  geom_hline(yintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  geom_point() +
  geom_abline(color = "red") +
  scale_colour_gradient(trans = "log", breaks = 10 ^ (1:5)) +
  xlab("Batting average") +
  ylab("Empirical Bayes batting average")
# 
# The horizontal dashed red line marks $y=\frac{\alpha_0}{\alpha_0 + \beta_0}=r sprintf("%.3f", alpha0 / (alpha0 + beta0))$. That's what we would guess someone's batting average was if we had no evidence at all (if both $H$ and $AB$ were 0). Notice that points above that line tend to move down towards it, while points below it move up.
# 
# The diagonal red line marks $x=y$. Points that lie close to it are the ones that didn't get shrunk at all by empirical Bayes. Notice that they're the ones with the highest number of at-bats (the brightest blue): they have enough evidence that we're willing to believe the naive batting average estimate.
# 
# This process is often (including in the rest of this book) called shrinkage: the process of moving all our estimates towards the average. How much it moves these estimates depends on how much evidence we have: if we have very little evidence (4 hits out of 10) we move it a lot, if we have a lot of evidence (300 hits out of 1000) we move it only a little. That's shrinkage in a nutshell: Extraordinary outliers require extraordinary evidence.