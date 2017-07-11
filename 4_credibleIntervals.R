library(dplyr)
library(tidyr)
library(Lahman)
library(broom)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

# values estimated by maximum likelihood in Chapter 3
alpha0 <- 101.4
beta0 <- 287.3

career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))
# 
# 4.2 Posterior distribution
# Consider that what we’re really doing with empirical Bayes estimation is computing two new values for each player: α1 and β1. These are the posterior shape parameters for each player’s distribution, after the prior (which was estimated from the whole dataset) has been updated based on each player’s evidence. They are computed as α1 = α0 + H and β1 = β0 + AB − H.

career_eb <- career_eb %>% mutate(alpha1 = alpha0 + H,beta1 = beta0 + AB - H)

# Since we have these two parameters for each player’s beta distribu- tion, we can visualize the density of the posterior distribution for each, using the dbeta function in R. I’ll pick a few of my favorites from the 1998 Yankee lineup (Figure 4.1).


yankee_1998 <- c("brosisc01", "jeterde01", "knoblch01", "martiti02",
                 "posadjo01", "strawda01", "willibe02")

yankee_1998_career <- career_eb %>%
  filter(playerID %in% yankee_1998)

library(tidyr)
library(ggplot2)

yankee_beta <- yankee_1998_career %>%
  crossing(x = seq(.18, .33, .0002)) %>%
  ungroup() %>%
  mutate(density = dbeta(x, alpha1, beta1))

ggplot(yankee_beta, aes(x, density, color = name)) +
  geom_line() +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black") +
  labs(x = "Batting average",
       color = "Player")

# Each of these curves is our probability distribution of what the player's batting average could be, after updating based on that player's performance. The empirical Bayes estimate that we reported in Chapter @ref(empirical-bayes) is simply the peak of each[^meanmode], but this distribution is what we're really estimating.
# 
# [^meanmode]: Technically, the peak (or mode) of the beta distributions is $\frac{\alpha-1}{\alpha+\beta-2}$, not the empirical Bayes estimate (or mean) of $\frac{\alpha}{\alpha+\beta}$, but those two become indistinguishable for large $\alpha$ and $\beta$.


# A credible interval for one player

jeter <- yankee_beta %>%
  filter(name == "Derek Jeter")

jeter_pred <- jeter %>%
  mutate(cumulative = pbeta(x, alpha1, beta1)) %>%
  filter(cumulative > .025, cumulative < .975)

jeter_low <- qbeta(.025, jeter$alpha1[1], jeter$beta1[1])
jeter_high <- qbeta(.975, jeter$alpha1[1], jeter$beta1[1])

jeter %>%
  ggplot(aes(x, density)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = density), data = jeter_pred,
              alpha = .25, fill = "red") +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black") +
  geom_errorbarh(aes(xmin = jeter_low, xmax = jeter_high, y = 0), height = 3.5, color = "red") +
  xlim(.18, .34)

yankee_1998_career <- yankee_1998_career %>%
  mutate(low  = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))

yankee_1998_career %>%
  dplyr::select(-alpha1, -beta1, -eb_estimate) %>%
  knitr::kable()

yankee_1998_career %>%
  mutate(name = reorder(name, eb_estimate)) %>%
  ggplot(aes(eb_estimate, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  xlab("Estimated batting average (w/ 95% interval)") +
  ylab("Player")



# Credible versus CI ------------------------------------------------------

career_eb <- career_eb %>%
  mutate(low = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))

set.seed(2015)

some <- career_eb %>%
  sample_n(20) %>%
  mutate(name = paste0(name, " (", H, "/", AB, ")"))

frequentist <- some %>%
  group_by(playerID, name, AB) %>%
  do(tidy(binom.test(.$H, .$AB))) %>%
  ungroup() %>%
  dplyr::select(playerID, name, estimate, low = conf.low, high = conf.high) %>%
  mutate(method = "Confidence")

bayesian <- some %>%
  dplyr::select(playerID, name, AB, estimate = eb_estimate,
                low = low, high = high) %>%
  mutate(method = "Credible")

combined <- bind_rows(frequentist, bayesian)

combined %>%
  mutate(name = reorder(name, -AB, na.rm = TRUE)) %>%
  ggplot(aes(estimate, name, color = method, group = method)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  xlab("Estimated batting average") +
  ylab("Player") +
  labs(color = "")
