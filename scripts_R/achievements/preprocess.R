rm(list = ls())

library(tidyverse)
library(magrittr)
library(ggpubr)

source('scripts_R/config.R')
source('scripts_R/helpers/utils_com.R')

pop_threshold = 50000
main = read_csv(sprintf('%s/real/main.csv', PATH_DATA)) 


# monitoring data (only QC one ) -----------------------------------------------
all_qc = read_csv(sprintf('%s/real_qc/mei.csv', PATH_DATA)) 
n_mon = all_qc %>% 
  group_by(action_plan_id) %>%
  summarize(n_mon = n()) %>%
  ungroup()

last_qc = all_qc %>%
  group_by(action_plan_id) %>%
  filter(y_mei == max(y_mei)) %>%
  ungroup() %>%
  select(action_plan_id, emission_factor_type, emission_reporting_unit, 
    id_bei, y_bei, y_bei_created, pop_bei, bei_ppc,
    id_mei, y_mei, y_mei_created, pop_mei, mei_ppc,
    co2_target, y_submission, y_start, red_ppc, red_per, progress
  ) %>%
  mutate(
    y_com = y_mei_created - y_submission,
    y_plan = y_mei - y_start,
    y_diff = y_mei - y_bei
  ) %>%
  left_join(n_mon) %T>%
  write_csv('data/proc/last_qc.csv')

# population QC
sig = read_csv(sprintf('%s/valid_qc/sig.csv', PATH_DATA), na='null') %>%
  filter(signatory_id %in% main$signatory_id) %T>%
  write_csv('data/proc/sig_qc.csv')

# budget QC
budget = read_csv(sprintf('%s/raw/str_budget.csv', PATH_DATA)) %>%
  right_join(main) %>%
  left_join(sig %>% select(signatory_id, population)) %>%
  mutate(years = budget_year_end - budget_year_start) %>%
  mutate(budget_total_ppc = budget_total / population / years) %>%
  filter(budget_total_ppc < 1000) %>%
  filter(budget_total > 10)  %T>% 
  write_csv('data/proc/budget_qc.csv')


# ancillary data
# CTC info
status = readxl::read_excel("data/raw/2020_seap_db.xlsx", sheet=1)
# overlapper
sig_2030 = read_csv(sprintf('%s/raw/sig_2030.csv', PATH_DATA), na='null') 
status_2030 = readxl::read_excel("data/raw/2030_secap_db.xlsx", sheet=1) %>%
  filter(commitment == '2030', analysis_status %in% c('accepted', 'under_evaluation')) #, 'pre_contacted', 'under_evaluation', 'ready_to_analyze'))

df = main %>%
  left_join(status %>% select(signatory_id, group_approach_eval)) %>%
  mutate(overlapper = if_else(signatory_id %in% sig_2030$signatory_id, TRUE, FALSE)) %>%
  mutate(overlapper2 = if_else(signatory_id %in% status_2030$signatory_id, TRUE, FALSE)) %>%
  left_join(sig %>% select(signatory_id, population)) %T>%
  write_csv('data/proc/ctc-overlapper.csv')


# main for this study ------------------------------------------------------------------------------
main = main %>%
  select(signatory_name, signatory_id, action_plan_id, monitoring = mei_plan) %>%
  right_join(last_qc %>% select(action_plan_id, red_per, progress, red_ppc, co2_target)) %>% 
  left_join(sig %>% select(signatory_id, population, country_code)) %>%
  mutate(pop_group = if_else(population < pop_threshold, 'small', 'large')) %>%
  mutate(pop_group = factor(pop_group, levels = c('small', 'large')))


# reductions per sector -----------------------------------------------------------------------
# red_ppc_s - total GHG reduction [PPC]
# red_per_s - total GHG reduction [%]
last_qc_s = read_csv(sprintf('%s/real_qc/mei_subsector.csv', PATH_DATA)) %>%
  mutate(red_per_s = if_else(is.na(red_per_s), 0, red_per_s)) %>%
  filter(id_mei %in% last_qc$id_mei) %>%
  write_csv('data/proc/last_qc_sec.csv')

# aggregated in actions sectors
last_qc_s = read_csv('data/proc/last_qc_sec.csv') %>%
  left_join(main %>% select(action_plan_id, pop_group), by = 'action_plan_id') %>% 
  mutate(sector_c = case_when(
    subsector %in% c('municipal_buildings') ~ 'municipal_buildings_equipment_facilities',
    subsector %in% c('residential_buildings', 'tertiary_buildings', 'buildings_equipment_facilities_non_allocated' ) ~ 'residential_tertiary',
    subsector %in% c('industry') ~ 'industry',
    subsector %in% c('public_transport', 'private_transport', 'municipal_fleet', 'transport_non_allocated') ~ 'transport',
    subsector %in% c('other_non_allocated', 'water_management', 'waste_management', 
                     'non_energy_related_sector', 'agriculture_forestry_fisheries') ~ 'others')) %>%
  group_by(action_plan_id, sector_c) %>%
  summarize(
    bei_ppc_s = sum(bei_ppc_s, na.rm = TRUE),
    mei_ppc_s = sum(mei_ppc_s, na.rm = TRUE),
    red_ppc_s = sum(red_ppc_s, na.rm = TRUE),
    red_per_s = sum(red_per_s, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(last_qc %>% select(action_plan_id, co2_target, red_ppc, red_per, progress, bei_ppc, mei_ppc)) %T>%
  write_csv('data/proc/last_qc_sec_common.csv')


# actions per sector -------------------------------------------------------------------------------
actions = read_csv(sprintf('%s/valid_qc/ac_sector.csv', PATH_DATA)) %>%
  mutate(red_ppc_s_plan = as.numeric(co2_reduction_har_ppc))   %>%
  write_csv('data/proc/ac_sec.csv')
# common
actions = read_csv('data/proc/ac_sec.csv') %>%
  filter(!(sector %in% c('local_heat_cold_production', 'local_electricity_production'))) %>%
  mutate(sector_c = case_when(
    sector %in% c('municipal_buildings_equipment_facilities') ~ 'municipal_buildings_equipment_facilities',
    sector %in% c('residential_buildings', 'tertiary_buildings_equipment_facilities' ) ~ 'residential_tertiary',
    sector %in% c('industry') ~ 'industry',
    sector %in% c('transport') ~ 'transport',
    sector %in% c('others') ~ 'others')) %>%
  group_by(action_plan_id, sector_c) %>%
  summarize(red_ppc_s_plan = sum(red_ppc_s_plan, na.rm = TRUE)) %>%
  ungroup() %T>%
  write_csv('data/proc/ac_sec_common.csv')


# write main -------------------------------------------------------------------
main %>%
  write_csv('data/proc/main.csv')


