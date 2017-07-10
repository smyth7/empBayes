# Chpt 3

library(dplyr) 
library(tidyr) 
library(Lahman)
library(ggplot2)

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

# estimate prior

library(stats4)

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

