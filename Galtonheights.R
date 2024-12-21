#create the data set
library(tidyverse)
library(dslabs)
install.packages("HistData")
library(HistData)
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
