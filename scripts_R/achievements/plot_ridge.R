rm(list = ls(all=TRUE))

library(tidyverse)
library(ggplot2)
library(mgcv)
library(broom)
library(tidyr)
library(ridge)
library(ggpubr)

source('scripts_R/helpers/plotting.R')
source('scripts_R/helpers/utils.R')

coefs = read_csv('data/proc/regression_ridge_coefs.csv') %>% 
  filter(attribute != '(Intercept)') %>%
  mutate(slope_max = slope + 1.96 * std_error) %>%
  mutate(slope_min = slope - 1.96 * std_error) 


# split
df = coefs %>% filter(pop_group == 'small')
lev = df %>% arrange(slope) %>% pull(attribute)
df = df %>% mutate(attribute = factor(attribute, lev))
g1 = ggplot(df, aes(x = attribute, y = slope, color = p < 0.05)) +
  facet_wrap(~pop_group, labeller = labeller(pop_group = lab_pop)) +
  geom_point() +
  #geom_text(aes(label = text), y = 1, color = 'black') +
  geom_linerange(aes(ymin = slope_min, ymax = slope_max)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = NULL, y = 'Scaled slope [-]') + 
  scale_x_discrete(labels = lab_vars) +
  coord_flip(ylim = c(-0.6, 0.6)) +
  theme_ruben()

df = coefs %>% filter(pop_group == 'large')
lev = df %>% arrange(slope) %>% pull(attribute)
df = df %>% mutate(attribute = factor(attribute, lev))
g2 = ggplot(df, aes(x = attribute, y = slope, color = p < 0.05)) +
  facet_wrap(~pop_group, labeller = labeller(pop_group = lab_pop)) +
  geom_point() +
  #geom_text(aes(label = text), y = 1, color = 'black') +
  geom_linerange(aes(ymin = slope_min, ymax = slope_max)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = NULL, y = 'Scaled slope [-]') + 
  scale_x_discrete(labels = lab_vars) +
  coord_flip(ylim = c(-0.6, 0.6)) +
  theme_ruben()

g = ggarrange(g1, g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = 'top')
ggsave('figs/achievements/regression/reg_ridge.png', g, width = 30, height = 15, units = 'cm')
