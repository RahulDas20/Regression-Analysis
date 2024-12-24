#create the data set
library(tidyverse)
library(dslabs)
library(dplyr)
install.packages("HistData")
library(HistData)
library(ggplot2)
data("GaltonFamilies")

set.seed(1983)

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights


#means and standard deviation of father and sons height
galton_heights %>% summarise(mean(father), sd(father), mean(son), sd(son))

#scatter plot of father and sons height
galton_heights %>%
  ggplot(aes(father,son)) +
  geom_point(alpha = 0.5)

#father and son correlation
galton_heights %>% summarize(cor(father,son))



#compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son)) %>% pull(r)
R

#Monte carlo simulation to show the distribution of the corellation
B <- 10000
N <- 25
R <- replicate(B, {
  slice_sample(galton_heights, n = N, replace = TRUE) %>%
    summarize( r = cor(father, son)) %>%  pull(r)
})

data_frame(R) %>% ggplot(aes(R))  + geom_histogram(binwidth = 0.05, color = "black")


#expected value is the population corellation
mean(R)

#standard error is high relative to its size
sd(R)

#qq plot to evaluate whether N is large enough
data_frame(R) %>% ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


library(Lahman)

my_team <- Teams %>% filter(yearID %in% 1961:2001)

#calculate the wins per game
win_rate <- my_team$W/my_team$G

#calculate the error per game
error_per_game <- my_team$E / my_team$G

#calculate the doubles per game
double_per_game <- my_team$X2B/ my_team$G

#calculate the tripples per game
tripples_per_game <- my_team$X3B / my_team$G

#calculate the correlation
cor(double_per_game, tripples_per_game)
