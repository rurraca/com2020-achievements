rm(list = ls())

library(tidyverse)
library(magrittr)

source('scripts_R/config.R')
source('scripts_R/helpers/utils.R')


# read -------------------------------------------------------------------------
main = read_csv('data/proc/main.csv')  
last_qc = read_csv('data/proc/last_qc.csv') %>%
  left_join(main %>% select(action_plan_id, pop_group, population))
all = bind_rows(
  last_qc, 
  last_qc %>% mutate(pop_group = 'all')
)
all_s = bind_rows(
  read_csv('data/proc/last_qc_sec_common.csv') %>% left_join(main %>% select(action_plan_id, pop_group)), 
  read_csv('data/proc/last_qc_sec_common.csv') %>% mutate(pop_group = 'all')
)


stats = all %>%
  group_by(pop_group) %>%
  summarize(
    n = n(),
    pop_tot = round(sum(population) / 1e6, 2),
    bei_tot = round(sum(bei_ppc * pop_bei) / 1e6, 2),
    red_tot = round(sum(red_ppc * pop_bei) / 1e6, 2),
    #bei_avg = sprintf('%.2f [%.2f, %.2f]', mean(bei_ppc), quantile(bei_ppc, .25), quantile(bei_ppc, .75)),
    bei_avg = sprintf('%.2f', mean(bei_ppc)),
    y_bei_avg = sprintf('%.1f', mean(y_bei)),
    y_mei_avg = sprintf('%.1f', mean(y_mei)),
    y_start_avg = sprintf('%.1f', mean(y_start)),
    y_submission_avg = sprintf('%.1f', mean(y_submission)),
    y_plan_avg = sprintf('%.1f', mean(y_plan)),
    y_com_avg = sprintf('%.1f', mean(y_com)),
    target_avg = sprintf('%.2f', mean(co2_target)),
    red_ppc_avg = sprintf('%.2f', mean(red_ppc)),
    red_per_avg = sprintf('%.2f', mean(red_per)),
    progress_avg = sprintf('%.2f', mean(progress)),
    progress_20 = sprintf('%.2f', 100 * (sum(progress > 100) / n())),
    progress_neg = sprintf('%.2f', 100 * (sum(red_per < 0) / n())),
    red_20_100 = sprintf('%.2f', 100 * (sum(red_per > 20) / n())),
    red_0_20 = sprintf('%.2f', 100 * (sum(red_per < 20 & red_per > 0) / n()))
  ) %>%
  mutate(red_per_tot = round(100 * red_tot / bei_tot, 2)) %>%
  gather(key, value, -pop_group) %>%
  spread(pop_group, value) %>%
  select(key, small, large, all)

stats_s = all_s %>%
  left_join(last_qc %>% distinct(action_plan_id, pop_bei)) %>%
  group_by(pop_group, sector_c) %>%
  summarize(
    red_tot = round(sum(red_ppc_s * pop_bei) / 1e6, 2), 
    red_ppc = sprintf('%.2f', mean(red_ppc_s)),
    red_per = sprintf('%.2f', mean(red_per_s))
    ) %>%
  mutate(red_s = sprintf('%s (%s)', red_ppc, red_per)) %>%
  gather(key, value, -pop_group, -sector_c) %>%
  spread(pop_group, value)


lev_sector = c('residential_tertiary', 'municipal_buildings_equipment_facilities',
               'industry', 'transport', 'others')

# table totoals
table = bind_rows(
  stats %>% filter(key %in% c('n', 'pop_tot', 'bei_tot', 'red_tot', 'red_per_tot')),
  stats_s %>% filter(key %in% c('red_tot')) %>% mutate(key = sector_c) 
) %>%
  select(key, small, large, all) %>%
  mutate(key = factor(key, levels = c('n', 'pop_tot', 'bei_tot', 'red_tot', lev_sector, 'red_per_tot'))) %>%
  write_csv('out/summary_total.csv')

# table avgs
levels = c('y_bei_avg', 'bei_avg', 'y_submission_avg', 'y_start_avg', 
           'target_avg', 'y_mei_avg', 'red_ppc_avg', 'red_per_avg', 
           'progress_avg', 'y_plan_avg', 'y_com_avg')
stats %>% 
  select(key, small, large, all) %>%
  filter(key %in% levels) %>%
  mutate(key = factor(key, levels = levels)) %>%
  arrange(key) %>%
  write_csv('out/summary_avg.csv')

stats_s %>%
  filter(key %in% c('red_s')) %>%
  mutate(sector_c = factor(sector_c, levels = lev_sector)) %>%
  #mutate(key = factor(key, levels = c('red_ppc', 'red_per'))) %>%
  arrange(sector_c, key) %>%
  select(key, small, large, all) %>%
  write_csv('out/summary_avg_s.csv')


df = all_s %>% 
  filter(pop_group == 'large', sector_c == 'municipal_buildings_equipment_facilities') %>% 
  left_join(last_qc %>% distinct(action_plan_id, pop_bei, y_bei, y_mei)) %>%
  left_join(main %>% select(signatory_name, action_plan_id)) %>%
  mutate(red_tot = round(red_ppc_s *pop_bei / 1e6, 2)) %>%
  arrange(red_tot) %>%
  select(signatory_name, pop_bei, y_bei, y_mei, red_ppc_s, red_tot)

ggplot(df, aes(y = red_tot)) +
  geom_boxplot() +
  labs(y = 'Total emissions reduction [MtCO2e]')

df %>%
  view()


# ==============================================================================
# progress per group
main_tot = read_csv('data/proc/main.csv')  %>%
  group_by(pop_group) %>%
  summarize(n_tot = n())


lev = seq(-60, 80, 20)
df = read_csv('data/proc/main.csv') %>%
  mutate(group = cut(red_per, breaks = lev)) %>% 
  filter(!is.na(group)) %>%
  group_by(pop_group, group) %>%
  summarize(
    n = n(),
    n_over = 100 * sum(progress > 100) / n(),
    n_ongoing = 100 * sum((progress > 0) & (progress < 100)) / n(),
    n_bad = 100 * sum(progress < 0) / n(),
    progress = median(progress)
  ) %>%
  left_join(main_tot) %>%
  mutate(nr = 100 * n / n_tot) %>%
  arrange(pop_group, desc(group)) %>%
  #mutate(n = sprintf('%d (.%.1f)', n, nr)) %>%
  mutate(nr = sprintf('%.2f', nr)) %>%
  mutate(progress = sprintf('%.1f', progress)) %>%
  mutate(n_over = sprintf('%.1f', n_over)) %>%
  mutate(n_ongoing = sprintf('%.1f', n_ongoing)) %>%
  mutate(n_bad = sprintf('%.1f', n_bad)) %>%
  select(pop_group, group, n, nr, progress, n_over, n_ongoing, n_bad) %T>%
  write_csv('out/table_sup_progress_level.csv')
  
lev = c(-100, 0, 20, 100)
df = read_csv('data/proc/main.csv') %>%
  mutate(group = cut(red_per, breaks = lev)) %>% 
  filter(!is.na(group)) %>%
  group_by(pop_group, group) %>%
  summarize(
    n = n(),
    n_over = 100 * sum(progress > 100) / n(),
    n_ongoing = 100 * sum((progress > 0) & (progress < 100)) / n(),
    n_bad = 100 * sum(progress < 0) / n(),
    progress = median(progress)
  ) %>%
  left_join(main_tot) %>%
  mutate(nr = 100 * n / n_tot) %>%
  arrange(pop_group, desc(group)) %>%
  #mutate(n = sprintf('%d (.%.1f)', n, nr)) %>%
  mutate(nr = sprintf('%.2f', nr)) %>%
  mutate(progress = sprintf('%.1f', progress)) %>%
  mutate(n_over = sprintf('%.1f', n_over)) %>%
  mutate(n_ongoing = sprintf('%.1f', n_ongoing)) %>%
  mutate(n_bad = sprintf('%.1f', n_bad)) %>%
  select(pop_group, group, n, nr, progress, n_over, n_ongoing, n_bad) %T>%
  write_csv('out/table_sup_progress_group.csv')
