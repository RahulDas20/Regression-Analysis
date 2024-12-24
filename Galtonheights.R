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


## stratification 

sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)


#conditional average
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarise(avg = mean(son)) %>%
  pull(avg)
conditional_avg

#stratification of heights
galton_heights |> mutate(father_strate = factor(round(father))) |> 
  ggplot(aes(father_strate, son)) +
  geom_boxplot() +
  geom_point()

#conditional avarage of strata
galton_heights |> mutate(father_strata = factor(round(father))) %>%
  group_by(father_strata) %>%
  summarise(avg = mean(son)) %>%
  ggplot(aes(father_strata, avg)) +
  geom_point()


#center of each boxplot
galton_heights %>% 
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son_conditional_avarage = mean(son)) %>%
  ggplot(aes(father,son_conditional_avarage)) +
  geom_point()

#add regression line to standard data
r <- galton_heights %>% summarise(r = cor(father, son)) %>% pull(r)
#we take r as the correlation coefficient of father and son height

galton_heights %>%
  mutate( father = scale(father),son = scale(son)) %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  ggplot(aes(father, son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)


#add regression line to the original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
#r is the correlation coefficient
r <- cor(galton_heights$father, galton_heights$son)


#slope will be the correlation coefficient multiplied by(standard deviation of y / standard deviation of x)
#intercept will be mean of y - (slope * mean of x)
m <- r * s_y/s_x
b <- mu_y - m * mu_x

galton_heights %>% 
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)


#plot in standard units and see that intercept 
galton_heights %>% 
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)
