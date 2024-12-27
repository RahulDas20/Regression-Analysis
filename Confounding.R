library(tidyverse)
library(Lahman)
get_slope <- function(x,y) {
  cor(x,y) * sd(y) / sd(x)
}


#BB_slope gets the slope between bases on balls and runs

bb_slope <- Teams |> 
  filter(yearID %in% 1962:2001) |>
  mutate(BB_per_game = BB/G, R_per_game = R/G) |>
  summarize(slope = get_slope(BB_per_game, R_per_game))

bb_slope

#regression line between singles and runs

singles_slope <- Teams |> 
  filter(yearID %in% 1962:2001) |>
  mutate(singles_per_game = (H-HR-X3B-X2B)/G, R_per_game = R/G) |>
  summarize(slope = get_slope(singles_per_game, R_per_game))

singles_slope

#the singles value is much lower than the bases on balls slope

#the correlation between home run, bases on balls and singles

Teams |>
  filter(yearID %in% 1062:2001) |>
  mutate(Singles = (H-HR-X2B-X3B)/G,BB=BB/G,HR=HR/G) |>
  summarize(cor(BB,HR), cor(Singles,HR), cor(BB,Singles))

#bases on balls have a strong correlation with home run because pitchers avoid the HR hittel
#so we can say that BB are confounded with HRs.

#Understanding confounding through stratification
dat <- Teams |> 
  filter(yearID %in% 1962:2001) |>
  mutate(HR_strata = round(HR/G,1),
         BB_per_game = BB/G,
         R_per_game = R/G) |>
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

#the dat is stratified by HR
#now we make scatter plot for each strata

dat |> 
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)


dat |> 
  group_by(HR_strata) |>
  summarise(slope = get_slope(BB_per_game,R_per_game))

#from this we can see that HR-strata makes a difference between different Home run groups
#let's check that if different BB strata makes a difference slope of homerun and run slope


dat2 <- Teams |> 
  filter(yearID %in% 1962:2001) |>
  mutate(BB_strata = round(BB/G,1),
         HR_per_game = HR/G,
         R_per_game = R/G) |>
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)
dat2 |>
  group_by(BB_strata) |>
  summarise(slope = get_slope(HR_per_game, R_per_game))

#this is consistent with the fact that BB is fact cause some runs
hr_slope <- Teams |> filter(yearID %in% 1961:2001) |>
  mutate(HR_per_game = HR/G, R_per_game = R/G) |>
  summarise(slope = get_slope(HR_per_game, R_per_game))

hr_slope
