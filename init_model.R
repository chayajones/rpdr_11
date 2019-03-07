library(tidyverse)
library(knitr)
library(googlesheets)
library(rstan)
knitr::dep_prev()

gs_auth()
rpdr_data <- "16z2UvQB6CQqc1jrqlpTBn-gOkelklB586qRJ47XMWIM" %>%  gs_key

all_episodes <- rpdr_data %>% gs_read("all_episodes")
all_contestants <- rpdr_data %>% gs_read("all_contestants")
all_rankings <- rpdr_data %>% gs_read("all_rankings")

#--------------------- add next episode to predict ---------------------#
all_episodes_next <- add_row(all_episodes, season_number = 11, episode_number = 2,
                             episode_airdate = NA, episode_title = NA,
                             episode_type = "Competition", episode_maxi_challenge_type = NA) %>% 
  arrange(season_number, episode_number) %>% rowid_to_column("t")
season_11_contestants <- (all_contestants %>% filter(season_number == 11))$contestant_id
eliminated_contestants <- (all_rankings %>% filter(is.element(episode_placement,c('ELIM','Eliminated'))))$contestant_id
next_contestants <- setdiff(season_11_contestants, eliminated_contestants)
all_rankings_next <- add_row(all_rankings, season_number = 11, episode_number = 2, 
                             contestant_id = next_contestants, episode_placement = NA)
#-----------------------------------------------------------------------#

wrangled <- all_rankings_next %>%
  left_join(all_contestants, by = c("season_number","contestant_id")) %>%
  left_join(all_episodes_next, by=c("season_number", "episode_number")) %>%
  mutate(placement = case_when(is.element(episode_placement,c('WIN','Winner')) ~ 1,
                               is.element(episode_placement,c('ELIM','Eliminated'))  ~ -1,
                               TRUE ~ 0)) %>%
  group_by(t) %>% mutate(num_winners = sum(placement == 1), 
                         num_losers = sum(placement == -1)) %>% 
  arrange(desc(placement), .by_group = TRUE) %>% ungroup() %>% # within episode: winner first, loser last
  filter(is.element(episode_type,c('Competition','Finale'))) %>%
  filter((num_winners == 1 & num_losers == 1) | t == max(t)) %>% # use data on typical episodes
  filter(!is.element(episode_placement,c('Guest','Miss C')) | t == max(t)) %>% # use data on typical contestants
  group_by(contestant_id) %>% mutate(past_wins = cumsum(placement == 1) - (placement == 1)) %>%
  ungroup() %>%
  mutate(z.past_wins = (past_wins - mean(past_wins))/(2*sd(past_wins))) %>%
  mutate(z.age = (age - mean(age))/(2*sd(age))) %>%
  select(season_number, episode_number, t, contestant_id, contestant_name, # identifiers
         z.age, z.past_wins, # x variables
         placement, num_winners, num_losers) # episode outcomes

# renumber episodes skipping the atypical ones:
wrangled$t <- as.numeric(as.factor(wrangled$t))
next_t = max(wrangled$t)

# data {
#   int N; // number of contestant-episode observations
#   int I; // number of contestants
#   int T; // number of episodes
#   int<lower=1,upper=I> ii[N]; // contestant for each observation
#   int num_contestants[T]; // number of contestants for each episode
#   vector[N] age;
#   vector[N] past_wins;
# }
# parameters {
#   real beta_age;
#   real beta_past_wins;
#   vector[I] alpha_contestant_raw;
#   real<lower=0> sigma;
# }
# transformed parameters {
#   vector[I] alpha_contestant = alpha_contestant_raw * sigma;
# }
# model {
#   vector[N] eta = beta_age * age + beta_past_wins * past_wins + alpha_contestant[ii];
#   beta_age ~ student_t(6,0,2.5);
#   beta_past_wins ~ student_t(6,0,2.5);
#   alpha_contestant_raw ~ normal(0,1);
#   sigma ~ normal(0,1);
#   { int pos;
#     pos = 1;
#     for (t in 1:T) {
#       vector[num_contestants[t]] eta_t = segment(eta, pos, num_contestants[t]);
#       target += eta_t[1] - log_sum_exp(eta_t);
#       target += -1*eta_t[rows(eta_t)] - log_sum_exp(-1*eta_t[2:rows(eta_t)]); // remove winner (listed first)
#       pos = pos + num_contestants[t];
#     }}
# }

fit_model <- function(df) {
  standata <- list(
    N = nrow(df),
    I = max(df$contestant_id),
    T = max(df$t),
    ii = df$contestant_id,
    num_contestants = (df %>% group_by(t) %>% summarise(n = n()))$n,
    age = df$z.age,
    past_wins = df$z.past_wins
  )
  sampling(object = model, data = standata, chains = 4, iter = 2000, control = list(adapt_delta = 0.99))
}
data_so_far <- wrangled %>% filter(t < next_t)
fit <- fit_model(df = data_so_far)

print(fit, pars = c('beta_age','beta_past_wins','sigma'))

stan_plot(fit, pars = c('beta_age','beta_past_wins','sigma')) + geom_vline(xintercept=0, size = 2)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
L = 30 # always use at least L past episodes of data
correct = c()
random_correct = c()
# this will take a long time!
for (t_current in (L+1):(next_t - 1)) {
  data_current <- wrangled %>% filter(t < t_current) # leave-future-out
  fit_current <- fit_model(df = data_current)
  newdata <- wrangled %>% filter(t == t_current)
  s <- as.data.frame(fit_current) # simulations from the posterior
  eta_s <- matrix(NA, nrow = nrow(s), ncol = nrow(newdata))
  for (n in 1:nrow(newdata)) {
    i = newdata$contestant_id[n]
    if (is.element(i,data_current$contestant_id)) { # we have seen this contestant before
      alpha_contestant_s <- s[,c(paste('alpha_contestant[',i,']',sep=''))]
    } else { # this is a new contestant
      alpha_contestant_s <- rnorm(n = nrow(s), mean = 0, sd = s$sigma)
    }
    eta_s[,n] <- s$beta_age * newdata$z.age[n] + s$beta_past_wins * newdata$z.past_wins[n] + alpha_contestant_s
  }
  winner_s <- apply(eta_s, MARGIN = 1, FUN = which.max)
  pred_winner <- Mode(winner_s)
  loser_s <- apply(eta_s, MARGIN = 1, FUN = which.min)
  pred_loser <- Mode(loser_s)
  correct <- c(correct, (newdata$placement[pred_winner] == 1) & 
                 (newdata$placement[pred_loser] == -1))
  random_correct = c(random_correct, (1/nrow(newdata))*(1/(nrow(newdata)-1)))
}


winners <- wrangled %>% filter(placement != -1)

losers <- wrangled %>% filter(placement != 1)

losers$placement <- losers$placement *-1

winfit <- glm(placement ~ z.age + z.past_wins + plus_size + drag_fam_competed + puerto_rican + new_york_city, family = binomial(link = "logit"), 
    data = winners)

summary(winfit)

losefit <- glm(placement ~ z.age + z.past_wins + plus_size + drag_fam_competed + puerto_rican + new_york_city, family = binomial(link = "logit"), 
               data = losers)

summary(losefit)

winfit2 <- glm(placement ~ z.age + z.past_wins + plus_size + puerto_rican, family = binomial(link = "logit"), 
               data = winners)

summary(winfit2)

losefit2 <- glm(placement ~ z.age + z.past_wins + plus_size + puerto_rican, family = binomial(link = "logit"), 
                data = losers)

summary(losefit2)

anova(winfit, test = "Chisq")
anova(winfit2, test = "Chisq")
anova(losefit, test = "Chisq")
anova(losefit2, test = "Chisq")

winfit3 <- glm(placement ~ z.age + z.past_wins + plus_size + drag_fam_competed + puerto_rican, family = binomial(link = "logit"), 
              data = winners)

summary(winfit3)

anova(winfit3, test = "Chisq")

losefit3 <- glm(placement ~ z.age + z.past_wins + plus_size + drag_fam_competed + puerto_rican, family = binomial(link = "logit"), 
               data = losers)

summary(losefit3)

anova(losefit3, test = "Chisq") # this model performs better for losers than for winners 


winfit4 <- glm(placement ~ z.age + z.past_wins + plus_size + drag_fam_competed + drag_house_ind + puerto_rican + new_york_city + z.past_lipsync + z.past_top + z.past_bottom, family = binomial(link = "logit"), 
    data = winners)

summary(winfit4)
anova(winfit4, test = "Chisq")

losefit4 <- glm(placement ~ z.age + z.past_wins + plus_size + drag_fam_competed + drag_house_ind + puerto_rican + new_york_city + z.past_lipsync + z.past_top + z.past_bottom, family = binomial(link = "logit"), 
    data = losers)

summary(losefit4)
anova(losefit4, test = "Chisq")

# definitely include: z.past_wins, z.past_lipsync, z.past_top, z.past_bottom - also include z.age (?), puerto_rican, drag_house_ind
# this may be more predictive of losers than winners

winfit5 <- glm(placement ~ z.past_wins + drag_house_ind + puerto_rican + z.past_lipsync + z.past_top + z.past_bottom, family = binomial(link = "logit"), 
    data = winners)

summary(winfit5)
anova(winfit5, test = "Chisq")

losefit5 <- glm(placement ~ z.past_wins + drag_house_ind + puerto_rican + z.past_lipsync + z.past_top + z.past_bottom, family = binomial(link = "logit"), 
    data = losers)

summary(losefit5)
anova(losefit5, test = "Chisq")


winfit6 <- glm(placement ~ z.age + drag_house_ind + puerto_rican + z.past_wins + z.past_lipsync + z.past_top + z.past_bottom, family = binomial(link = "logit"), 
    data = winners)

summary(winfit6)
anova(winfit6, test = "Chisq")

losefit6 <- glm(placement ~ z.age + drag_house_ind + puerto_rican + z.past_wins + z.past_lipsync + z.past_top + z.past_bottom, family = binomial(link = "logit"),
    data = losers)

summary(losefit6)
anova(losefit6, test = "Chisq")

winfit7 <- glm(placement ~ z.age + plus_size + drag_house_ind + puerto_rican + z.past_wins + z.past_lipsync + z.past_top + z.past_bottom, family = binomial(link = "logit"), 
    data = winners)

summary(winfit7)
anova(winfit7, test = "Chisq")

losefit7 <- glm(placement ~ z.age + plus_size + drag_house_ind + puerto_rican + z.past_wins + z.past_lipsync + z.past_top + z.past_bottom, family = binomial(link = "logit"),
    data = losers)

summary(losefit7)
anova(losefit7, test = "Chisq")

winfit8 <- glm(placement ~ z.age + plus_size + drag_house_ind + puerto_rican + z.past_lipsync + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit8)
anova(winfit8, test = "Chisq")

losefit8 <- glm(placement ~ z.age + plus_size + drag_house_ind + puerto_rican + z.past_lipsync + z.past_top, family = binomial(link = "logit"),
    data = losers)

summary(losefit8)
anova(losefit8, test = "Chisq")

winfit9 <- glm(placement ~ z.age + plus_size + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit9)
anova(winfit9, test = "Chisq")

losefit9 <- glm(placement ~ z.age + plus_size + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"),
    data = losers)

summary(losefit9)
anova(losefit9, test = "Chisq")

winfit10 <- glm(placement ~ z.age + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit10)
anova(winfit10, test = "Chisq")

losefit10 <- glm(placement ~ z.age + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"),
    data = losers)

summary(losefit10)
anova(losefit10, test = "Chisq")

winfit11 <- glm(placement ~ z.age + ethnicity + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit11)
anova(winfit11, test = "Chisq")

losefit11 <- glm(placement ~ z.age + ethnicity + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"),
    data = losers)

summary(losefit11)
anova(losefit11, test = "Chisq")


winfit12 <- glm(placement ~ z.age + asian + black + latino + white + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit12)
anova(winfit12, test = "Chisq")

losefit12 <- glm(placement ~ z.age + asian + black + latino + white + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"),
    data = losers)

summary(losefit12)
anova(losefit12, test = "Chisq")


winfit13 <- glm(placement ~ z.age + black * new_york_city + latino * drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit13)
anova(winfit13, test = "Chisq")


library(BayesVarSel

gibbs_fit <- GibbsBvs(placement ~ z.age + asian + black + latino + white + drag_house_ind + puerto_rican + plus_size + 
    drag_fam_competed + new_york_city + z.past_wins + z.past_top + z.past_bottom + z.past_lipsync, data = wrangled

bayes_fit <- Bvs(placement ~ z.age + asian + black + latino + white + drag_house_ind + puerto_rican + plus_size + 
    drag_fam_competed + new_york_city + z.past_wins + z.past_top + z.past_bottom + z.past_lipsync, data = wrangled)

bayes_fit.predict <- predict(bayes_fit, newdata=wrangled, n.sim = 10000)

winfit14 <- glm(placement ~ z.age + asian + black + latino + white + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit14)
anova(winfit14, test = "Chisq")

losefit14 <- glm(placement ~ z.age + asian + black + latino + white + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"),
    data = losers)

summary(losefit14)
anova(losefit14, test = "Chisq")


x_cor <- data.frame(placement = wrangled$placement, age = wrangled$z.age, 
          asian = wrangled$asian, black = wrangled$black, 
          latino = wrangled$latino, white = wrangled$white, 
          drag_house = wrangled$drag_house_ind, puerto_rican = wrangled$puerto_rican,
          plus_size = wrangled$plus_size, drag_fam = wrangled$drag_fam_competed, 
          nyc = wrangled$new_york_city, wins = wrangled$z.past_wins,
          top = wrangled$z.past_top, bottom = wrangled$z.past_bottom,
          lipsync = wrangled$z.past_lipsync)

cor(x_cor)


x_cor_win <- data.frame(placement = winners$placement, age = winners$z.age, 
          asian = winners$asian, black = winners$black, 
          latino = winners$latino, white = winners$white, 
          drag_house = winners$drag_house_ind, puerto_rican = winners$puerto_rican,
          plus_size = winners$plus_size, drag_fam = winners$drag_fam_competed, 
          nyc = winners$new_york_city, wins = winners$z.past_wins,
          top = winners$z.past_top, bottom = winners$z.past_bottom,
          lipsync = winners$z.past_lipsync)

cor(x_cor_win)

x_cor_lose <- data.frame(placement = losers$placement, age = losers$z.age, 
          asian = losers$asian, black = losers$black, 
          latino = losers$latino, white = losers$white, 
          drag_house = losers$drag_house_ind, puerto_rican = losers$puerto_rican,
          plus_size = losers$plus_size, drag_fam = losers$drag_fam_competed, 
          nyc = losers$new_york_city, wins = losers$z.past_wins,
          top = losers$z.past_top, bottom = losers$z.past_bottom,
          lipsync = losers$z.past_lipsync)

cor(x_cor_lose)

# based on correlation matrix, top correlations are: asian, latino, drag_house, puerto_rican, wins, top, bottom, lipsync 
# of the uppers and lowers, top & bottom are most correlated

winfit15 <- glm(placement ~ asian + latino + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit15)
anova(winfit15, test = "Chisq")

losefit15 <- glm(placement ~ asian + latino + drag_house_ind + puerto_rican + z.past_bottom + z.past_top, family = binomial(link = "logit"),
    data = losers)

summary(losefit15)
anova(losefit15, test = "Chisq")

winfit16 <- glm(placement ~ asian + latino + drag_house_ind + z.past_bottom + z.past_top, family = binomial(link = "logit"), 
    data = winners)

summary(winfit16)
anova(winfit16, test = "Chisq")

losefit16 <- glm(placement ~ asian + latino + drag_house_ind + z.past_bottom + z.past_top, family = binomial(link = "logit"),
    data = losers)

summary(losefit16)
anova(losefit16, test = "Chisq")




winfit17 <- glm(placement ~ asian + latino + drag_house_ind + z.past_bottom + z.past_top + z.past_safe, family = binomial(link = "logit"), 
    data = winners)

summary(winfit17)
anova(winfit17, test = "Chisq")

losefit17 <- glm(placement ~ asian + latino + drag_house_ind + z.past_bottom + z.past_top + z.past_safe, family = binomial(link = "logit"),
    data = losers)

summary(losefit17)
anova(losefit17, test = "Chisq")


# next things to consider adding: cumsum of being in the bottom 2 (lip sync w/o getting out) (BTM2), cumsum of being in the top w/o winning (HIGH), 
# cumsum of being in the bottom (bottom 3) w/o losing (BTM2 or LOW), cumsum of being in the top or winning (HIGH or WIN)
# cumsum of being safe ??
# drag house indicator, trans/nonbinary?
# think about how/whether to add race

# for future exploration - consider adding social media info?