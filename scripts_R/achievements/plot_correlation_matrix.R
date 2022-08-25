rm(list = ls())

library(tidyverse)
library(magrittr)
library(ggpubr)

source('scripts_R/config.R')
source('scripts_R/helpers/plotting.R')
source('scripts_R/helpers/utils.R')

df = read_csv('data/proc/correlation_matrix.csv')

# all  -------------------------------------------------------------------------
lev_vars = c(
  'red_per',
  'red_residential', 'red_transport', 'red_industry', #'red_municipal', 'red_other',
  'co2_target', 'bei_ppc', 'budget_total_ppc',
  'y_bei', 'y_start', 'y_submission', 'y_mei', 'y_plan', 'y_diff', #'n_mon',
  'overlapper', 'group_approach_eval',
  'staff_local', 'staff_supporter', 'staff_coordinator', 'staff_external',
  'stk_local', 'stk_external', 'stk_other',
  'budget_private', 'budget_public', 'budget_notallocated' #budget_local
)

aux = df %>%
  filter(var1 %in% lev_vars, var2 %in% lev_vars) %>%
  mutate(var1 = factor(var1, rev(lev_vars))) %>%
  mutate(var2 = factor(var2, lev_vars)) %>%
  mutate(pop_group = factor(pop_group, lev_pop))

g = ggplot(aux, aes(x = var1, y = var2, fill=cor)) +
  facet_wrap(~pop_group, nrow = 2, labeller = labeller(pop_group = lab_pop)) +
  geom_raster() +
  geom_text(aes(label = sprintf('%.2f', cor)), size = 2) +
  scale_fill_gradientn(colors = p_seismic, limits = c(-1, 1)) +
  scale_x_discrete(labels = lab_vars) +
  scale_y_discrete(labels = lab_vars) +
  labs(x = NULL, y = NULL, fill = '') + 
  guides(fill = guide_colorbar(barheight = unit(8, 'cm'))) +
  coord_fixed(expand = FALSE) + 
  theme_heatmap() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
ggsave('figs/achievements/correlation/cor_matrix_all.png', g, width = 25, height = 35, units = 'cm')

# model ------------------------------------------------------------------------
lev_vars = c(
  #'red_per',
  'red_transport', 'red_industry', #'red_municipal', 'red_other',
  'co2_target', 'bei_ppc',
  'y_submission', 'y_mei',
  'overlapper', 'group_approach_eval',
  'staff_local', #'staff_external', # 'staff_supporter', 'staff_coordinator', 'staff_external',
  'budget_external',  'budget_notallocated' # 'budget_private', 'budget_public', 
)

aux = df %>%
  filter(var1 %in% lev_vars, var2 %in% lev_vars) %>%
  mutate(var1 = factor(var1, rev(lev_vars))) %>%
  mutate(var2 = factor(var2, lev_vars)) %>%
  mutate(pop_group = factor(pop_group, lev_pop))
g = ggplot(aux, aes(x = var1, y = var2, fill=cor)) +
  facet_wrap(~pop_group, nrow = 2, labeller = labeller(pop_group = lab_pop)) +
  geom_raster() +
  geom_text(aes(label = sprintf('%.2f', cor)), size = 2) +
  scale_fill_gradientn(colors = p_seismic, limits = c(-1, 1)) +
  scale_x_discrete(labels = lab_vars) +
  scale_y_discrete(labels = lab_vars) +
  labs(x = NULL, y = NULL, fill = '') + 
  guides(fill = guide_colorbar(barheight = unit(8, 'cm'))) +
  coord_fixed(expand = FALSE) + 
  theme_heatmap() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
ggsave('figs/achievements/correlation/cor_matrix_model.png', g, width = 16, height = 25, units = 'cm')
