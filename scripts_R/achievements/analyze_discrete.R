rm(list=ls())

library(tidyverse)
library(magrittr)
library(readxl)
library(egg)
library(ggpubr)

source('scripts_R/config.R')
source('scripts_R/helpers/utils.R')
source('scripts_R/helpers/utils_com.R')
source('scripts_R/helpers/plotting.R')
source('scripts_R/helpers/utils_disc.R')
source('scripts_R/helpers/utils_cont.R')

# read ---------------------------------------------------------------------------------------------
main = read_csv('data/proc/main.csv')
test = 'mw'  # or ks

plot_disc = function(df, labels, title) {
  pal_sel = c('FALSE' = '#b3ecff', 'TRUE' = '#0099cc')
  aux = df %>%
    mutate(pop_group=factor(pop_group, levels = lev_pop))
  
  g = ggplot(aux, aes(x=x, y=red_per, fill=selected)) +
    facet_wrap(~pop_group, nrow=2, strip.position = 'right', labeller = labeller(pop_group=lab_pop)) +
    geom_boxplot(size=.15, outlier.size = .2, outlier.color = 'white') +
    geom_text(data=out2 %>% filter(p < 0.05), 
              aes(x=x, label=sprintf('d = %.2f \n %s', d, p_sig)),
              inherit.aes = FALSE, y = 80, size = 3) +
    scale_fill_manual(values=pal_sel) +
    scale_y_continuous(breaks=seq(-100,100,25)) +
    scale_x_discrete(labels=labels) +
    labs(x=NULL,y='Total emissions reduction [%]', title=title, fill=NULL) +
    coord_cartesian(ylim=c(-35,90)) +
    theme_ruben() +
    theme(axis.text.x = element_text(angle=20, hjust=1), panel.grid.major.y = element_line(size=.5),
      plot.title = element_text(hjust=.5, size=10, face='bold'))
  return (g)
}

main_tot = main %>%
  group_by(pop_group) %>%
  summarize(n_tot=n()) %>%
  ungroup()

# overlappers & CTC ------------------------------------------------------------

title = 'General'
labels = c('overlapper' = 'Overlapper', 'group_approach_eval' = 'CTC approach', 'overlapper2' = 'Overlapper')
df = read_csv('data/proc/ctc-overlapper.csv') %>%
  right_join(main %>% select(signatory_id, pop_group, red_per), by = 'signatory_id') %>%
  select(overlapper2, group_approach_eval, red_per, pop_group) %>%
  gather(x, selected, overlapper2, group_approach_eval)
out = df %>%
  stats_cont(group='selected', var='red_per', key='x', total=main_tot, test=test)
out2 = out %>%
  proc_cont(filename='discrete/general')

g_over = df %>%
  plot_disc(labels=labels, title=title)

# STAFF --------------------------------------------------------------------------------------------
levels = lev_staff
labels = lab_staff
x = 'allocated_staff_type'
title = 'Staff'
df = read_csv(sprintf('%s/raw/str_staff.csv', PATH_DATA)) %>%
  filter(action_plan_id %in% main$action_plan_id) %>%
  #filter(is.na(monitoring_report_id)) %>%
  mutate(monitoring_report_id = if_else(is.na(monitoring_report_id), 0, monitoring_report_id)) %>%
  group_by(action_plan_id) %>%
  filter(monitoring_report_id == max(monitoring_report_id)) %>%
  ungroup() %>%
  left_join(main) %>%
  mutate(x=get(x)) %>%
  select(x, selected, red_per, pop_group) %>%
  mutate(x=factor(x, levels=levels)) %>%
  filter(x != 'Other')
out = df %>%
  stats_cont(group='selected', var='red_per', key='x', total=main_tot, test=test)
out2 = out %>%
  proc_cont(filename='discrete/staff')
g_staff = df %>%
  plot_disc(labels=labels, title=title)

# STAKEHOLDERS  ------------------------------------------------------------------------------------
levels = lev_stk_type
labels = lab_stk_type
x = 'stakeholder_type'
title = 'Stakeholders'
df = read_csv(sprintf('%s/raw/str_stk_inv.csv', PATH_DATA)) %>%
  filter(action_plan_id %in% main$action_plan_id) %>%
  left_join(main) %>%
  mutate(x=get(x)) %>%
  select(x, selected, red_per, pop_group) %>%
  mutate(x=factor(x, levels=levels))
out = df %>%
  stats_cont(group='selected', var='red_per', key='x', total=main_tot, test=test)
out2 = out %>%
  proc_cont(filename='discrete/stakeholder')
g_stk = df %>%
  plot_disc(labels=labels, title=title)

# BUDGET -------------------------------------------------------------------------------------------
levels = c('Local_authoritys_own_resources', 'external', 'not_allocated') #lev_budget_src
labels = c('Local_authoritys_own_resources' = 'Local authority', 'external' = 'External', 'not_allocated' = 'Not allocated')
x = 'budget_financing_source'
title = 'Financing'
df = read_csv(sprintf('%s/raw/str_budget_src.csv', PATH_DATA)) %>%
  filter(action_plan_id %in% main$action_plan_id) %>%
  left_join(main) %>%
  mutate(x=get(x)) %>%
  select(x, selected, red_per, pop_group) %>%
  mutate(x=factor(x, levels=levels)) %>%
  drop_na(x)
out = df %>%
  stats_cont(group='selected', var='red_per', key='x', total=main_tot, test=test)
out2 = out %>%
  proc_cont(filename='discrete/budget')
g_budget = df %>%
  plot_disc(labels=labels, title=title)

# pannel
g = ggpubr::ggarrange(
  g_over + theme(strip.text.y = element_blank(), legend.position = "none"),
  g_staff + labs(y=NULL) + theme(strip.text.y = element_blank(), legend.position = "none"),
  g_budget + labs(y=NULL) + theme(strip.text.y = element_blank(), legend.position = "none"),
  g_stk + labs(y=NULL) + theme(),
  nrow = 1, ncol = 4, widths = c(.65, 1, 1, .85),
  common.legend = TRUE, legend = 'bottom'
)

ggsave('figs/disc/attributes_discrete.png', g, width=25, height=15, units='cm')

# FINAL TABLE
t1 = read_csv('out/discrete/general.csv')
t2 = read_csv('out/discrete/staff.csv')
t3 = read_csv('out/discrete/stakeholder.csv')
t4 = read_csv('out/discrete/budget.csv')
t = bind_rows(t1, t2, t3, t4) %>%
  mutate(p=sprintf('%.3f %s',  p, p_sig)) %>%
  select(-p_sig) %>%
  write_csv('out/discrete/all.csv', na = '')

