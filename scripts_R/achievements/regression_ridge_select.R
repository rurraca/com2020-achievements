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




lab_vars = c(
  'red_residential' = 'residential',
  'red_transport' = 'transport',
  'red_industry' = 'industry',
  'y_bei' = 'baseline year', 
  'y_start' = 'SEAP start year', 
  'y_submission' = 'submission year', 
  'y_mei' = 'MEI year', 
  'y_plan' = 'MEI year - SEAP start year', 
  'y_diff' = 'MEI year - baseline year', 
  'staff_local' = 'local', 
  'staff_supporter' = 'CoM supporter', 
  'staff_coordinator' = 'CoM coordinator', 
  'staff_external' = 'external'
)


# variables --------------------------------------------------------------------
var_y = 'red_per'
vars_num_cont = c(
  'co2_target', 'bei_ppc'
)
vars_bin = c(
  'overlapper', 'group_approach_eval',
  #'staff_external', 'staff_local', #'staff_supporter', #'staff_coordinator', 
  'budget_external', 'budget_notallocated' # 'budget_local'
)

vars_years = list()
vars_years[[1]] = c('y_plan')
vars_years[[2]] = c('y_diff')
vars_years[[3]] = c('y_bei', 'y_mei')
vars_years[[4]] = c('y_start', 'y_mei')
vars_years[[5]] = c('y_submission', 'y_mei')
#lab_years = c('1' = 'y_plan', '2' = 'y_diff', '3' = 'y_bei + y_mei', '4' = 'y_start + y_mei', '5' = 'y_submission + y_mei')
lev_years = seq(1, 5, 1)


vars_sector = list()
vars_sector[[1]] = c('red_residential')
vars_sector[[2]] = c('red_transport')
vars_sector[[3]] = c('red_industry')
vars_sector[[4]] = c('red_transport', 'red_industry')
#lab_sector = c('1' = 'red_residential', '2' = 'red_transport', '3' = 'red_industry', '4' = 'red_transport + red_industry')
lev_sector = seq(1, 4, 1)

vars_staff = list()
vars_staff[[1]] = c('staff_external')
vars_staff[[2]] = c('staff_local')
vars_staff[[3]] = c('staff_supporter')
vars_staff[[4]] = c('staff_coordinator')
lev_staff = seq(1, 4, 1)

stats = expand.grid(pop_group = lev_pop, var_years = lev_years, var_sector = lev_sector, var_staff = lev_staff) %>%
  as_tibble() 


# data -------------------------------------------------------------------------
df = read_csv('data/proc/regression.csv', na = 'NULL') 

create_names = function(x) {
  names = lab_vars[x[1]]
  if (length(x) > 1) {
    for (i in (2:length(x))) {
      names = str_c(names, ' + ', lab_vars[x[i]])
    }
  }
  return(names)
}

# test scenarios ---------------------------------------------------------------
out = tibble()
for (i in 1:nrow(stats)) {
  print(i)
  row = stats %>% slice(i)
  pop = row %>% pull(pop_group)
  id_sector = row %>% pull(var_sector)
  id_year = row %>% pull(var_years)
  id_staff = row %>% pull(var_staff)
  
  vars_i =  c(vars_years[[id_year]], vars_sector[[id_sector]], vars_staff[[id_staff]])
  vars_num = c(vars_num_cont, vars_i)
  vars_x = c(vars_num, vars_bin)
  
  df_p = df %>%
    filter(pop_group == pop) %>%
    select(vars_x, var_y) %>% 
    drop_na() 
  
  # scale
  y = df_p %>% pull(var_y)
  y_scaled = y %>% scale()
  df_p_scaled = df_p %>% 
    mutate_at(all_of(vars_num), ~(scale(.) %>% as.vector())) %>%
    mutate_at(var_y, ~(scale(.) %>% as.vector())) 
  
  # RIDGE --------------------------------------------------------------------
  model = linearRidge(red_per~., data = df_p_scaled, lambda = 0.1, scaling = 'none')
  
  # rmse
  yp_scaled = predict(object=model, newdata=df_p_scaled %>% select(vars_x))
  yp = yp_scaled * attr(y_scaled, 'scaled:scale') + attr(y_scaled, 'scaled:center')
  rmse_y = sqrt(mean((yp - y)^2, na.rm = TRUE))
  mae_y = mean(abs(yp-y), na.rm = TRUE)
  results = tibble(
    'pop_group' = pop,
    'vars_years' = create_names(vars_years[[id_year]]),
    'vars_sectors' = create_names(vars_sector[[id_sector]]),
    'vars_staff' = create_names(vars_staff[[id_staff]]),
    'mae' = mae_y,
    'rmse' = rmse_y,
  )
  out = bind_rows(out, results)

}

out %>% 
  arrange(pop_group, mae) %>%
  View()

out %T>% 
  write_csv('data/proc/regression_ridge_stats.csv')

out %>%
  arrange(pop_group, mae) %>%
  mutate(mae = sprintf('%.2f', mae)) %>%
  mutate(rmse = sprintf('%.2f', rmse)) %>%
  select(pop_group, vars_sectors, vars_years, vars_staff, mae) %T>% 
  write_csv('out/regression_ridge_stats.csv')

