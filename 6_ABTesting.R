library(dplyr) 
library(tidyr) 
library(Lahman)
library(tidyr)
library(ggplot2)

# Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb) 
pitchers <- Pitching %>%
group_by(playerID) %>% summarize(gamesPitched = sum(G)) %>% filter(gamesPitched > 3)
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>% group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>% mutate(average = H / AB)
# Add player names
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>% unite(name, nameFirst, nameLast, sep = " ") %>% inner_join(career, by = "playerID")
# values estimated by maximum likelihood in Chapter 3
alpha0 <- 101.4
beta0 <- 287.3
# For each player, update the beta prior based on the evidence # to get posterior parameters alpha1 and beta1
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>% mutate(alpha1 = H + alpha0,
                                                                        beta1 = AB - H + beta0) %>% arrange(desc(eb_estimate))


# save two players as separate objects too for later: ---------------------

aaron <- career_eb %>% filter(name == "Hank Aaron") 
piazza <- career_eb %>% filter(name == "Mike Piazza")
two_players <- bind_rows(aaron, piazza) 
two_players



theme_set(theme_bw())

two_players %>%
  crossing(x = seq(.28, .33, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x, density, color = name)) +
  geom_line() +
  labs(x = "Batting average", color = "")

# 
# We’d need to know the probability one beta distribution is greater than another. This question is not trivial to answer, and I’m going to illustrate four routes that are common lines of attack in a Bayesian problem:
# • Simulation of posterior draws 
# • Numerical integration
# • Closed-form solution
# • Closed-form approximation


# Simulation of posterior draws -------------------------------------------

piazza_simulation <- rbeta(1e6, piazza$alpha1, piazza$beta1) 
aaron_simulation <- rbeta(1e6, aaron$alpha1, aaron$beta1)
sim <- mean(piazza_simulation > aaron_simulation) 
sim
## [1] 0.59.4 # percentage chance that > is true


# Integration -------------------------------------------------------------



x <- seq(.29, .318, .0002)
crossing(piazza_x = x, aaron_x = x) %>%
  mutate(piazza_density = dbeta(piazza_x, piazza$alpha1, piazza$beta1),
         aaron_density = dbeta(aaron_x, aaron$alpha1, aaron$beta1),
         joint = piazza_density * aaron_density) %>%
  ggplot(aes(piazza_x, aaron_x, fill = joint)) +
  geom_tile() +
  geom_abline() +
  scale_fill_gradient2(low = "white", high = "red") +
  labs(x = "Piazza batting average",
       y = "Aaron batting average",
       fill = "Joint density") +
  theme(legend.position = "none")

d <- .00002
limits <- seq(.29, .33, d)
sum(outer(limits, limits, function(x, y) {
  (x > y) *
    dbeta(x, piazza$alpha1, piazza$beta1) *
    dbeta(y, aaron$alpha1, aaron$beta1) *
    d ^ 2
}))
# 0.5933


#Like simulation, this is a bit on the "brute force" side. (And unlike simulation, the approach becomes intractable in problems that have many dimensions, as opposed to the two dimensions here).

# Closed-form solution ----------------------------------------------------

h <- function(alpha_a, beta_a, alpha_b, beta_b) {
  j <- seq.int(0, round(alpha_b) - 1)
  log_vals <- (lbeta(alpha_a + j, beta_a + beta_b) - log(beta_b + j) - lbeta(1 + j, beta_b) - lbeta(alpha_a, beta_a)) 
  1 - sum(exp(log_vals))
}
h(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)
# 0.59


# Closed-form approximation - normal --------------------------------------

# Large alpha and beta -> use normal dist. 

two_players %>%
  mutate(mu = alpha1 / (alpha1 + beta1),
         var = alpha1 * beta1 / ((alpha1 + beta1) ^ 2 * (alpha1 + beta1 + 1))) %>%
  crossing(x = seq(.28, .33, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1),
         normal = dnorm(x, mu, sqrt(var))) %>%
  ggplot(aes(x, density, group = name)) +
  geom_line(aes(color = name)) +
  geom_line(lty = 2)

#The probability one normal variable is greater than another is very easy to calculate- much easier than the beta!
  
  h_approx <- function(alpha_a, beta_a, alpha_b, beta_b) {
    u1 <- alpha_a / (alpha_a + beta_a)
    u2 <- alpha_b / (alpha_b + beta_b)
    var1 <- (alpha_a * beta_a) /
      ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1))
    var2 <- (alpha_b * beta_b) /
      ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
    pnorm(0, u2 - u1, sqrt(var1 + var2))
  }

h_approx(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)
# 0.59
# 
# The disadvantage is that for low $\alpha$ or low $\beta$, the normal approximation to the beta is going to fit rather poorly. While the simulation and integration approaches were inexact, this one will be systematically biased: in some cases it will always give too high an answer, and in some cases too low. But when we have priors $\alpha_0=r alpha0$ and $\beta_0=r beta0$, as we do here, our parameters are never going to be low, so we're safe using it.


# Confidence and credible intervals ---------------------------------------

#"contingency table".

two_players %>%
  transmute(Player = name, Hits = H, Misses = AB - H) %>%
  knitr::kable(booktabs = TRUE)

#One of the most common ways to approach these contingency table problems is with Pearson's chi-squared test, implemented in R as prop.test.

prop.test(two_players$H, two_players$AB)


#Now we’ll use empirical Bayes to compute the credible interval about the di erence in these two players. We could do this with simulation or integration, but we’ll use our normal approximation approach since it’s the most e cient (we’ll also compute our posterior probability while we’re at it).
credible_interval_approx <- function(a, b, c, d) {
  u1 <- a / (a + b)
  u2 <- c / (c + d)
  var1 <- a * b / ((a + b) ^ 2 * (a + b + 1))
  var2 <- c * d / ((c + d) ^ 2 * (c + d + 1))
  mu_diff <- u2 - u1
  sd_diff <- sqrt(var1 + var2)
  data_frame(posterior = pnorm(0, mu_diff, sd_diff), estimate = mu_diff,
             conf.low = qnorm(.025, mu_diff, sd_diff), conf.high = qnorm(.975, mu_diff, sd_diff))
}
credible_interval_approx(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)


# It’s not particularly exciting for this Piazza/Aaron comparison (notice it’s very close to the con dence interval we calculated with prop.test). So let’s select 20 random players, and compare each of them to Mike Piazza: how many players can we say are better than Piazza? We’ll also calculate the con dence interval using prop.test, and compare them:

set.seed(2016)

intervals <- career_eb %>%
  filter(AB > 10) %>%
  sample_n(20) %>%
  group_by(name, H, AB) %>%
  do(credible_interval_approx(piazza$alpha1, piazza$beta1, .$alpha1, .$beta1)) %>%
  ungroup() %>%
  mutate(name = reorder(paste0(name, " (", H, " / ", AB, ")"), -estimate))
f <- function(H, AB) broom::tidy(prop.test(c(H, piazza$H), c(AB, piazza$AB)))
prop_tests <- purrr::map2_df(intervals$H, intervals$AB, f) %>%
  mutate(estimate = estimate1 - estimate2,
         name = intervals$name)

all_intervals <- bind_rows(
  mutate(intervals, type = "Credible"),
  mutate(prop_tests, type = "Confidence")
)

all_intervals %>%
  mutate(name = reorder(name, -AB, na.rm = TRUE)) %>%
  ggplot(aes(x = estimate, y = name, color = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  xlab("Piazza average - player average") +
  ylab("Player")
