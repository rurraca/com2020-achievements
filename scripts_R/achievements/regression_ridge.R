rm(list = ls(all=TRUE))

library(tidyverse)
library(ggplot2)
library(mgcv)
library(broom)
library(tidyr)
library(ridge)
library(shapr)
library(glmnet)

source('scripts_R/helpers/plotting.R')
source('scripts_R/helpers/utils.R')

# variables --------------------------------------------------------------------
var_y = 'red_per'
vars_num_cont = c(
  'red_transport', 'red_industry', #'red_municipal', 'red_other',
  'co2_target', 'bei_ppc',
  'y_submission', 'y_mei'
)
vars_bin = c(
  'overlapper', 'group_approach_eval',
  'staff_local', #'staff_external', #'staff_supporter', #'staff_local', 'staff_supporter', 'staff_coordinator', 
  'budget_external', 'budget_notallocated' # 'budget_local'
)

vars_num = vars_num_cont
vars_x = c(vars_num, vars_bin)

df = read_csv('data/proc/regression.csv', na = 'NULL') 

coefs = tibble()
for (pop in lev_pop) {
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
  
  # optimize lambda
  cv = cv.glmnet(df_p_scaled %>% select(vars_x) %>% as.matrix(), y_scaled, alpha = 0)
  print(cv$lambda.min)
  
  model = linearRidge(red_per~., data = df_p_scaled, lambda = cv$lambda.min, scaling = 'none')
  
  out = summary(model)$summaries$summary1$coefficients
  coefs_i = tibble(
    attribute = rownames(out),
    slope = out[, 'Estimate'],
    std_error = out[, 'Std. Error'],
    t_value = out[, 't value'],
    p = out[, 'Pr(>|t|)']
  ) %>%
    mutate(pop_group = pop)
  coefs = bind_rows(coefs, coefs_i)
}

# save
coefs %>% write_csv('data/proc/regression_ridge_coefs.csv')

