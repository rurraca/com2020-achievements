rm(list = ls())

library(tidyverse)
library(magrittr)
library(ggalt)
library(xtable)
library(sf)
library(GGally)
library(egg)
library(ggpubr)

source('scripts_R/config.R')
source('scripts_R/helpers/utils.R')
source('scripts_R/helpers/utils_com.R')
source('scripts_R/helpers/utils_cont_x.R')
source('scripts_R/helpers/plotting.R')

# read ---------------------------------------------------------------------------------------------
main = read_csv('data/proc/main.csv')
last_qc = read_csv('data/proc/last_qc.csv')

df = last_qc %>% 
  left_join(main %>% select(action_plan_id, pop_group))


# RESULTS (QC values) ------------------------------------------------------------------------------
gg_hist = function(df, x_breaks, x_bin, x_lab, bw, lines = NA, legend = TRUE) {
  g0 = ggplot(df, aes(x = pop_group, y = x, fill = pop_group)) +
    geom_boxplot(size = .2, outlier.size = .2, outlier.color = 'white') + 
    scale_y_continuous(breaks = x_breaks) +
    scale_fill_manual(values = pal_pop) +
    labs(x = '', y = NULL) +
    guides(fill = 'none') +
    coord_flip(ylim = c(min(x_breaks), max(x_breaks))) +
    theme_ruben() +
    theme(legend.position = 'top', axis.line = element_blank(), axis.text = element_blank(), 
      axis.ticks = element_blank()) 
  g1 = ggplot(df, aes(x = x, fill = pop_group, color = pop_group)) +
    #geom_histogram(binwidth =  x_bin, aes(y = ..density..), alpha = .7, position = 'identity') + 
    geom_density(alpha = .5, bw =  bw) +
    labs(x = x_lab, y = 'Density', fill = NULL ,color = NULL) +
    geom_vline(xintercept = c(0), linetype = 2, color = 'black', size = .2) +
    scale_x_continuous(breaks = x_breaks) +
    scale_fill_manual(values = pal_pop, labels = lab_pop) +
    scale_color_manual(values = pal_pop, labels = lab_pop) +
    coord_cartesian(xlim = c(min(x_breaks), max(x_breaks))) +
    theme_ruben() +
    theme(legend.position = c(.8, .9))
  if (!is.na(lines)) {
    g1 = g1 + geom_vline(xintercept = lines, linetype = 2, color = 'red', size = .2) 
  }
  if (legend == FALSE) {
    g1 = g1 + theme(legend.position = 'none')
  }
  g = ggpubr::ggarrange(g0, g1,  nrow = 2, ncol = 1, heights = c(.15, 1))
}


#out = df %>% stats_cont_x(var='red_ppc', test='mw')
g_red_ppc = df %>%
  rename(x = red_ppc) %>%
  gg_hist(x_breaks =  seq(-6, 9, 3), x_bin = .5, x_lab = expression("Total emissions reduction [tCO"["2"] * "e/cap]")
, bw = 0.6, legend = FALSE)

#out = df %>% stats_cont_x(var='red_per', test='mw')
g_red_per = df %>%
  rename(x = red_per) %>%
  gg_hist(x_breaks =   seq(-100, 100, 25), x_bin = 4, x_lab = 'Total emissions reduction [%]', bw = 6, lines = 20, legend = FALSE)

#out = df %>% stats_cont_x(var='progress', test='ks')
g_progress = df %>%
  rename(x = progress) %>%
  gg_hist(x_breaks =   seq(-300, 450, 100), x_bin = 20, x_lab = ex_progress, bw = 25, lines = 100)

g = ggpubr::ggarrange(g_red_ppc, g_red_per, g_progress, nrow = 1, ncol = 3, labels = c('a', 'b', 'c'))
ggsave('figs/achievements/results_pop.png', g, width = 30, height = 9, units = 'cm')
