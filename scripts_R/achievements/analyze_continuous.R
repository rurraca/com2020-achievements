rm(list = ls())

library(tidyverse)
library(magrittr)
library(egg)
library(ggpubr)
library(ggrepel)
library(ggcorrplot)
library(ggridges)
library(ggpointdensity)
library(broom)
library(MASS, include.only = 'rlm')

source('scripts_R/config.R')
source('scripts_R/helpers/utils.R')
source('scripts_R/helpers/utils_com.R')
source('scripts_R/helpers/plotting.R')
source('scripts_R/helpers/utils_disc.R')
source('scripts_R/helpers/utils_cont.R')

num = read_csv('data/proc/main.csv') %>% 
  left_join(read_csv('data/proc/last_qc.csv')) %>%
  left_join(read_csv('data/proc/budget_qc.csv') %>% select(action_plan_id, budget_total_ppc)) 


# correlation --------------------------------------------------------------------------------------
vars = c(
  'red_per', 'bei_ppc', 'population',  'co2_target', 'budget_total_ppc', 
  'y_bei', 'y_start', 'y_submission', 'y_mei', 'y_plan'
  )

# all
corr = num %>%
  select(all_of(vars)) %>%
  cor(use = 'pairwise')
g = ggcorrplot(corr, lab = TRUE, p.mat = cor_pmat(corr), ggtheme = ggplot2::theme_dark(), show.legend = FALSE)
ggsave('figs/cont/correlation.png', g, width = 17, height = 15, units='cm')

# by population
corr_s = num %>%
  filter(pop_group == 'small') %>%
  select(all_of(vars)) %>%
  select(-population, -y_plan) %>%
  rename(
    `Emission reduction [%]` = red_per, `Baseline emissions [tCO2e/cap]` = bei_ppc, `Reduction target [%]` = co2_target,
    `Budget [EUR/cap/year]` = budget_total_ppc, `Baseline year` = y_bei, `SEAP start year` = y_start, 
    `SEAP submission year` = y_submission, `MEI year` = y_mei) %>%
  cor(use = 'pairwise', method = 'spearman')
corr_l = num %>%
  filter(pop_group == 'large') %>%
  select(all_of(vars)) %>%
  select(-population, -y_plan) %>%
  rename(
    `Total emissions reduction [%]` = red_per, `Baseline emissions [tCO2e/cap]` = bei_ppc, `Reduction target [%]` = co2_target,
    `Budget [EUR/cap/year]` = budget_total_ppc, `Baseline year` = y_bei, `SEAP start year` = y_start, 
    `SEAP submission year` = y_submission, `MEI year` = y_mei) %>%
  cor(use = 'pairwise', method = 'spearman')
g1 = ggcorrplot(corr_s, lab = TRUE, p.mat = cor_pmat(corr_s), ggtheme = ggplot2::theme_dark(), show.legend = FALSE) +
  labs(title = 'Pop. < 50,000')
g2 = ggcorrplot(corr_l, lab = TRUE, p.mat = cor_pmat(corr_l), ggtheme = ggplot2::theme_dark(), show.legend = FALSE) +
  labs(title = 'Pop. > 50,000')
g = egg::ggarrange(g1, g2, nrow=1, ncol=2)
ggsave('figs/achievements/cont/correlation_pop.png', g, width = 30, height = 15, units='cm')


# scatterplots -------------------------------------------------------------------------------------
plot_scatter = function(df, x, x_breaks, x_lab, binwidth, x_text, hist = TRUE, text_y = FALSE, strip = FALSE) {
  df = df %>%
    mutate(x = get(x)) %>%
    mutate(pop_group = factor(pop_group, levels = lev_pop))

  library(data.table)
  dt = data.table(df)
  stats = dt[, cor.test(x, red_per, method = 'spearman')[-2], by = pop_group]
  stats = tibble(stats) %>%
    select(pop_group, rho = estimate, p.value) %>%
    mutate(p = sprintf('%.3f', p.value)) %>%
    mutate(p_sig = case_when(
      p >= 0.05 ~ '',
      p < 0.05 & p >= 0.01 ~ '*',
      p < 0.01 & p >= 0.001 ~ '**',
      p < 0.001 ~ '***'
    )) 
  detach('package:data.table')
    
  
  model = df %>%
    group_by(pop_group) %>%
    do(m = tidy(lm(red_per ~ x, data = .))) %>%
    mutate(pvalue = NA, slope = NA, intercept = NA) 
  for (i in 1:nrow(model)) {
    model$pvalue[i] = model$m[[i]]$p.value[2]
    model$slope[i] = model$m[[i]]$estimate[2]
    model$intercept[i] = model$m[[i]]$estimate[1]
  }
  stats = stats %>%
    left_join(model)
  if(x != 'y_plan') {
    stats_sig = stats %>% filter(pvalue < 0.05)
  } else {
    stats_sig = stats
  }

  
  g0 = ggplot(df, aes(x = x, fill = pop_group)) +
    geom_boxplot(size = .3, outlier.size =0, outlier.color = 'white') +
    scale_fill_manual(values = pal_pop, guide = 'none') +
    labs(x = NULL, y = NULL) +
    theme_ruben(box = FALSE) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  g1 = ggplot(df, aes(x = x, fill = pop_group)) +
    scale_x_continuous(breaks = x_breaks) +
    scale_fill_manual(values = pal_pop, guide = 'none') +
    scale_color_manual(values = pal_pop, guide = 'none') +
    labs(x = NULL, y = NULL) +
    theme_ruben(box = FALSE) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  if (hist) {
    g1 = g1 + geom_histogram(aes(y = ..density..), 
                             binwidth = binwidth, alpha = .5, position = 'identity') 
  } else {  # density plot
    g1 = g1 + geom_density(aes(color = pop_group), alpha = .5, bw = binwidth)
  }
  
  g2 =ggplot(df, aes(x = x, y = red_per)) +
    facet_wrap(~pop_group, nrow = 2, labeller = labeller(pop_group = lab_pop), 
               strip.position = 'right') +
    geom_pointdensity(adjust = 2, size = .8) + 
    geom_smooth(data = df %>% filter(pop_group %in% stats_sig$pop_group),
                method = 'rlm', color = 'red', span = 2, size = .5) +
    geom_text(data = stats, aes(x = x_text, y = -94, label = sprintf('rho==%.2f*"%s"', rho, p_sig)), 
      size = 3, hjust = 1, parse = T) +
    scale_color_viridis(guide = 'none') +
    scale_x_continuous(breaks = x_breaks) +
    labs(x = x_lab, y = NULL) +
    coord_cartesian(expand = TRUE, ylim = c(-100, 100)) +
    theme_ruben(box = TRUE) +
    theme(panel.grid.major = element_line(color = 'grey', size = .1)) 
  if (text_y == FALSE) {
    g2 = g2 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  if (strip == FALSE) {
    g2 = g2 + theme(strip.text.y = element_blank()) 
  }
  g = egg::ggarrange(g0, g1, g2, nrow = 3, ncol = 1, heights = c(.05, .2, 1)) 
  #g = g + theme(plot.background = element_rect(fill = 'white', color = 'white'))

  return(g)
}



# variables ----------------------------------------------------------------------------------------
num = num %>%
  filter(co2_target < 100) %>%
  filter(bei_ppc < 20) %>%
  filter(mei_ppc < 20)

# main variables -----------------------------------------------------------------------------------
var = 'bei_ppc'
breaks = seq(0, 40, 5)
lab = expression("Baseline emissions [tCO"["2"] * "e/cap]")
g_bei = num %>%
  plot_scatter(x = var, x_breaks = breaks, x_lab = lab, x_text = 18, hist = FALSE, binwidth = .6, text_y = TRUE)

var = 'mei_ppc'
breaks = seq(0, 40, 5)
lab = ex_mei_ppc
g_mei = num %>%
  plot_scatter(x = var, x_breaks = breaks, x_lab = lab, x_text = 18, hist = FALSE, binwidth = .5)

var = 'co2_target'
breaks = seq(20, 100, 20)
lab = 'Reduction target [%]'
g_co2 = num %>%
  plot_scatter(x = var, x_breaks = breaks, x_lab = lab, x_text = 65, binwidth = 1.5,  hist = FALSE)

x = 'budget_total_ppc'
x_breaks = seq(0, 1000, 200)
x_lab = ex_budget
g_budget = num %>% 
  plot_scatter(x = x, x_breaks = x_breaks, x_lab = x_lab, x_text = 900, binwidth = 20, hist = FALSE)


# Years --------------------------------------------------------------------------------------------
var = 'y_bei'
breaks = seq(1990, 2020, 5)
bins = seq(1990, 2020, 5)
lab = 'Baseline year'
g_y_bei = num %>%
  plot_scatter(x = var, x_breaks = breaks, x_lab = lab, x_text = 2015, binwidth = 1, text_y = FALSE)

var = 'y_start'
breaks = seq(1990, 2020, 5)
bins = seq(1990, 2020, 5)
lab = 'SEAP start year'
g_y_start = num %>%
  plot_scatter(x = var, x_breaks = breaks, x_text = 2017, x_lab = lab, binwidth = 1)

var = 'y_submission'
breaks = seq(2005, 2020, 5)
bins = seq(2008, 2020, 1)
lab = 'SEAP submission year'
g_y_sub = num %>%
  plot_scatter(x = var, x_breaks = breaks, x_lab = lab, x_text = 2019, binwidth = 1)

var = 'y_mei'
breaks = seq(2005, 2020, 5)
bins = seq(2005, 2020, 5)
lab = 'MEI year'
g_y_mei = num %>%
  plot_scatter(x = var, x_breaks = breaks,  x_lab = lab, x_text = 2020, binwidth = 1)

var = 'y_com'
breaks = seq(0, 10, 2)
bins = seq(0, 10, 2)
lab = 'MEI submission - SEAP submission'
g_y_com = num %>%
  plot_scatter(x = var, x_breaks = breaks, x_lab = lab, x_text = 25, binwidth = 1)

var = 'y_plan'
breaks = seq(0, 30, 5)
bins = seq(0, 30, 5)
lab = 'MEI year - SEAP start year'
g_y_plan = num %>%
  plot_scatter(x = var, x_breaks = breaks, x_lab = lab, binwidth = 1, x_text = 27, strip = TRUE)

# PANNEL 
g = ggpubr::ggarrange(
  g_bei, 
  g_co2, 
  g_y_bei, 
  g_y_sub,
  g_y_mei, 
  g_y_plan, 
  nrow = 1, ncol = 6, widths = c(1, .8, .8, .5, .5, .9))
g = annotate_figure(g, left = text_grob('Total emissions reduction [%]', rot = 90, size = 9))
ggsave('figs/achievements/cont/scatter_cont.png', g, width = 35, height = 15, units = 'cm')
