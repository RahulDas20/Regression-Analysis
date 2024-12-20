library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()



#Scatter plot between Home run and Run
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#Scatter plot between Stolen bases and wins
Teams %>% filter(yearID %in% 1961 : 2001) %>%
  mutate(SB_per_game = SB/G,R_per_game = R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#Scatter plot between Bases on Balls and Runs
Teams %>% filter(yearID %in% 1961 : 2001) %>%
  mutate(BB_per_game = BB/G,R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)


#Scatter plot between runs_per_game and at_bats
Teams %>% filter( yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

## This is my Branch!!
## Nobody will enter here!!


#Triples per game vs Doubles per game
Teams %>% filter( yearID %in% 1961:2001) %>%
  mutate(TR_per_game = X3B/G, DR_per_game = X2B/G) %>%
  ggplot(aes(TR_per_game, DR_per_game)) +
  geom_point(alpha = 0.5)