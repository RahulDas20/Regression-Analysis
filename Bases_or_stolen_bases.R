library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()



#Scatter plot between Homerun and Run
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
