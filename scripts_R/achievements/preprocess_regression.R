rm(list = ls())

library(tidyverse)
library(magrittr)

source('scripts_R/config.R')

main = read_csv('data/proc/main.csv')

num = read_csv('data/proc/main.csv') %>% 
  left_join(read_csv('data/proc/last_qc.csv')) %>%
  left_join(read_csv('data/proc/budget_qc.csv') %>% select(action_plan_id, budget_total_ppc)) %>%
  select('action_plan_id', 'red_per', 'bei_ppc', 'budget_total_ppc', 'co2_target',  
         'population',
         'y_bei', 'y_start', 'y_submission', 'y_mei', 'y_plan', 'y_com', 'y_diff', 'n_mon')

num_s = read_csv('data/proc/last_qc_sec_common.csv') %>%
  mutate(share = 100 * red_per_s / red_per) %>%
  mutate(share = if_else(red_per < 0, share * -1, share)) %>%
  select(action_plan_id, sector_c, var = share) %>%
  spread(sector_c, var) %>%
  rename(red_industry = industry, red_residential = residential_tertiary,
         red_transport = transport, red_municipal = municipal_buildings_equipment_facilities,
         red_other = others)

cat1 = read_csv('data/proc/ctc-overlapper.csv') %>%
  select(action_plan_id, overlapper, group_approach_eval) %>%
  mutate(overlapper = as.numeric(overlapper)) %>%
  mutate(group_approach_eval = as.numeric(group_approach_eval))

cat2 = read_csv(sprintf('%s/raw/str_staff.csv', PATH_DATA)) %>%
  filter(action_plan_id %in% main$action_plan_id) %>%
  mutate(monitoring_report_id = if_else(is.na(monitoring_report_id), 0, monitoring_report_id)) %>%
  group_by(action_plan_id) %>%
  filter(monitoring_report_id == max(monitoring_report_id))%>%
  select(action_plan_id, allocated_staff_type, selected)  %>%
  filter(allocated_staff_type != 'Other') %>%
  mutate(selected = as.numeric(selected)) %>%
  spread(allocated_staff_type, selected) %>%
  rename('staff_external'= 'External consultant', 'staff_local' = 'Local authority',
         'staff_coordinator' = 'Covenant coordinator', 'staff_supporter' = 'Covenant supporter')

cat3 = read_csv(sprintf('%s/raw/str_stk_inv.csv', PATH_DATA)) %>%
  filter(action_plan_id %in% main$action_plan_id) %>%
  select(action_plan_id, stakeholder_type, selected)  %>%
  mutate(selected = as.numeric(selected)) %>%
  spread(stakeholder_type, selected) %>%
  rename('stk_local'= "Local authority's staff", 'stk_external' = 'External stakeholders at local level',
         'stk_other' = 'Stakeholders at other levels of governance')

cat4 = read_csv(sprintf('%s/raw/str_budget_src.csv', PATH_DATA)) %>%
  filter(action_plan_id %in% main$action_plan_id)  %>%
  select(action_plan_id, budget_financing_source, selected)  %>%
  filter(budget_financing_source != 'external') %>%
  mutate(selected = as.numeric(selected)) %>%
  spread(budget_financing_source, selected)  %>%
  rename('budget_local'= "Local_authoritys_own_resources", 'budget_private' = 'private',
         'budget_public' = 'public', 'budget_notallocated'= 'not_allocated') %>%
  mutate(budget_external = as.integer(budget_public | budget_private))

all = main %>% 
  select(action_plan_id, pop_group) %>%
  left_join(num) %>%
  left_join(num_s) %>%
  left_join(cat1) %>%
  left_join(cat2) %>%
  left_join(cat3) %>%
  left_join(cat4) %>%
  select(-action_plan_id) %>%
  #mutate_at(all_of(num_vars), ~(scale(.) %>% as.vector()))
  write_csv('data/proc/regression.csv', na = 'NULL')

meta = tibble(
 colname = colnames(all),
 type = 'numeric'
)
ordinal =  c('y_bei', 'y_start', 'y_submission', 'y_mei', 'y_plan', 'y_com')
binary = c(
  'overlapper', 'group_approach_eval',
  'staff_local', 'staff_external', 'staff_coordinator', 'staff_supporter',
  'stk_local', 'stk_external', 'stk_other',
  'budget_local', 'budget_public', 'budget_private', 'budget_notallocated'
)

meta = meta %>%
  mutate(type = if_else(colname %in% ordinal, 'ordinal', type)) %>%
  mutate(type = if_else(colname %in% binary, 'binary', type)) %T>%
  write_csv('data/proc/regression_meta.csv', na = 'NULL')

