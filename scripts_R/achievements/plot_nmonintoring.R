rm(list = ls(all=TRUE))

library(tidyverse)
library(ggplot2)
library(mgcv)
library(broom)
library(tidyr)
library(ridge)
library(shapr)

source('scripts_R/helpers/plotting.R')
source('scripts_R/helpers/utils.R')


# data -------------------------------------------------------------------------
df = read_csv('data/proc/regression.csv', na = 'NULL') %>%
  select(pop_group, n_mon) 
n_tot = df %>%
  group_by(pop_group) %>%
  summarize(n_tot = n())
stats = df %>%
  group_by(pop_group, n_mon) %>%
  summarize(n = n()) %>%
  left_join(n_tot) %>%
  mutate(n_r = 100 * n / n_tot) 

g = ggplot(stats, aes(x = n_mon, y = n_r, fill = pop_group)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(x = 'Number of monitoring reports per LA', y = 'Percentage of LAs [%]', fill = NULL) +
  scale_fill_manual(values = pal_pop, labels = lab_pop) +
  theme_ruben() +
  theme(legend.position = c(.8, .8))
ggsave('figs/achievements/n_mon_LA.png', g, width = 15, height = 15, units = 'cm')

# group
stats = df %>%
  mutate(n_mon = if_else(n_mon > 3, 4, n_mon)) %>%
  mutate(n_mon = factor(n_mon)) %>%
  group_by(pop_group, n_mon) %>%
  summarize(n = n()) %>%
  left_join(n_tot) %>%
  mutate(n_r = 100 * n / n_tot)

g = ggplot(stats, aes(x = n_mon, y = n_r, fill = pop_group)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(x = 'Number of monitoring reports per LA', y = 'Percentage of LAs [%]', fill = NULL) +
  scale_fill_manual(values = pal_pop, labels = lab_pop) +
  scale_x_discrete(labels = c('1', '2', '3', '> 3')) +
  theme_ruben() +
  theme(legend.position = c(.8, .8))
ggsave('figs/achievements/n_mon_LA_red.png', g, width = 10, height = 15, units = 'cm')
