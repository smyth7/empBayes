library(dplyr) 
library(tidyr) 
library(Lahman)
library(ggplot2)
library(scales)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>% group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>% mutate(average = H / AB)

career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>% unite(name, nameFirst, nameLast, sep = " ") %>% inner_join(career, by = "playerID")

# values estimated by maximum likelihood in Chapter 3
alpha0 <- 101.4
beta0 <- 287.3
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0),
         alpha1 = H + alpha0,
         beta1 = AB - H + beta0)


career_eb %>%
  filter(name == "Hank Aaron") %>%
  do(data_frame(x = seq(.27, .33, .0002),
                density = dbeta(x, .$alpha1, .$beta1))) %>%
  ggplot(aes(x, density)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = density * (x < .3)),
              alpha = .1, fill = "red") +
  geom_vline(color = "red", lty = 2, xintercept = .3) +
  labs(x = "Batting average")



# Posterior Error Probability PEP = Local FDR -----------------------------------------------------------
career_eb %>%
  filter(name == "Hank Aaron")

pbeta(.3, 3850, 8818)

# Complement: Posterior Inclusion Probability, or PIP. (Note that PIP = 1 − PEP) 

career_eb <- career_eb %>%
  mutate(PEP = pbeta(.3, alpha1, beta1))

ggplot(career_eb, aes(PEP)) +
  geom_histogram(binwidth = .05) +
  xlab("Posterior Error Probability (PEP)")


# Note that the PEP is closely related to the estimated batting average, as shown in Figure @ref(fig:pepaverage). Notice that crossover point: to have a PEP less than 50%, you need to have a shrunken batting average greater than .300. That's because the shrunken estimate is the center of our posterior beta distribution (the "over/under" point). If a player's shrunken estimate is above .300, it's more likely than not that their true average is as well. And the players we're not sure about (PEP $\approx$ .5) have batting averages very close to .300.

career_eb %>%
  ggplot(aes(eb_estimate, PEP, color = AB)) +
  geom_point(size = 1) +
  xlab("(Shrunken) batting average estimate") +
  ylab("Posterior Error Probability (PEP)") +
  geom_vline(color = "red", lty = 2, xintercept = .3) +
  scale_colour_gradient(trans = "log", breaks = 10 ^ (1:5))

# 
# Notice also the relationship between the number of at-bats (the amount of evidence) and the PEP. If a player's shrunken batting average is .28, but he hasn't batted many times, it is still possible his true batting average is above .3 (the credible interval is wide). However, if a player with a score of .28 has a high AB (light blue), the credible interval becomes thinner, we become confident that the true probability of hitting is under .3, and the PEP goes up to 1.


# 5% FDR ------------------------------------------------------------------

top_players <- career_eb %>% arrange(PEP) %>% head(100)
sum(top_players$PEP)
mean(top_players$PEP)


# Q values = cummulative mean ----------------------------------------------------------------

career_eb <- career_eb %>% arrange(PEP) %>% mutate(qvalue = cummean(PEP))

#The q-value is convenient because we can say “to control the FDR at X%, collect only hypotheses where q < X”.

hall_of_fame <- career_eb %>% filter(qvalue < .05)

strict_hall_of_fame <- career_eb %>% filter(qvalue < .01)

career_eb %>%
  filter(qvalue < .3) %>%
  ggplot(aes(qvalue, rank(PEP))) +
  geom_line() +
  scale_x_continuous(labels = percent_format()) +
  xlab("q-value threshold") +
  ylab("Number of players included at this threshold")
