"""
MEI >= 2: 808
MEI in the start year = 314
non linear: 48
"""

library(tidyverse)


source('scripts_R/config.R')
source('scripts_R/helpers/utils_com.R')
source('scripts_R/helpers/plotting.R')

main = read_csv('data/proc/main.csv')

df = read_csv(sprintf('%s/real_qc/mei.csv', PATH_DATA))  %>%
  group_by(action_plan_id) %>%
  filter(n() >= 2) %>%
  select(action_plan_id, y_bei, y_mei, bei_ppc, mei_ppc, y_start)
bei = df %>%
  distinct(action_plan_id, y_bei, bei_ppc) %>%
  rename(y_ei = y_bei, ei_ppc = bei_ppc)
ei = df %>%
  select(action_plan_id, y_ei = y_mei, ei_ppc = mei_ppc) %>%
  bind_rows(bei) %>%
  left_join(main %>% select(action_plan_id, signatory_name)) %>%
  left_join(df %>% distinct(action_plan_id, y_start))
aux =  ei %>%
  arrange(y_ei) %>%
  group_by(action_plan_id) %>%
  filter(y_start %in% y_ei) %>%
  ungroup() %>%
  filter(y_ei >= y_start) %>%
  group_by(action_plan_id) %>%
  filter(row_number() == 1| row_number() == n()) # first and last

g = ggplot(ei %>% filter(action_plan_id %in% aux$action_plan_id), aes(x = y_ei, y = ei_ppc)) +
  facet_wrap(~action_plan_id, ncol = 15) +
  geom_line() +
  geom_line(data = aux, color = 'red') +
  #geom_line(data = aux2, color = 'blue') +
  labs(x = NULL, y = 'Total emissions [tCO2e/cap]') +
  theme_ruben(box = TRUE)
ggsave('figs/achievements/multiple_meis.png', g, width = 30, height = 40, units = 'cm')


# non-linear -------------------------------------------------------------------
ids = c(
  237, 523, 691, 693, 748, 884, 914, 1243, 1297, 1657, 1681, 
  2053, 2061, 2183, 2317, 2360, 2806,
  3107, 3132, 3135, 3217, 3336, 3454, 3455, 3456, 3468, 3470, 3471, 3504, 3553, 3593, 3614, 3658, 3692, 3798, 3847, 3912, 
  4018, 4037, 4128, 4954, 
  5006, 5162, 5201, 5536, 5807, 5837, 
  27446)

g = ggplot(ei %>% filter(action_plan_id %in% ids), aes(x = y_ei, y = ei_ppc)) +
  facet_wrap(~signatory_name, ncol = 6) +
  geom_line() +
  geom_line(data = aux %>% filter(action_plan_id %in% ids), color = 'red', linetype = 'longdash', size = .5) +
  #geom_line(data = aux2, color = 'blue') +
  labs(x = NULL, y = 'Total emissions [tCO2e/cap]') +
  theme_ruben(box = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('figs/achievements/multiple_meis_nonlinear.png', g, width = 25, height = 40, units = 'cm')


