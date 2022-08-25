library(rcompanion)

stats_cont = function(df, group, var, total, key = NULL, test = 'mw') {
  
  if (is.null(key)) {
    df['key'] = 'aux' 
    df = df %>% select(group = !!group, var = !!var, key, pop_group) 
  } else {
    df = df %>% select(group = !!group, var = !!var, key = !!key, pop_group) 
  }
  
  out = df %>%
    group_by(pop_group, key, group) %>%
    summarize(n = n(), avg = mean(var), med = median(var)) %>%
    ungroup() %>%
    left_join(total) %>%
    mutate(n_r = 100 * n / n_tot) %>%
    arrange(key) %>%
    mutate(p = NA_real_) %>%
    mutate(d = NA_real_) %>%
    mutate(d_min = NA_real_) %>%
    mutate(d_max = NA_real_) 
  
  lev_key = unique(out$key)
  lev_pop = unique(out$pop_group)
  for (i in lev_pop) {
    for (j in lev_key) {
      df_l = df %>% 
        filter(key == j, pop_group == i) %>%
        mutate(group = factor(group, levels = c(TRUE, FALSE))) %>%
        arrange(group)
      
      if ((df_l %>% pull(group) %>% unique() %>% length()) == 2) {
        if (test == 'mw') {
          mw = wilcox.test(
            x = df_l %>% filter(group == TRUE) %>% pull(var),
            y = df_l %>% filter(group == FALSE) %>% pull(var)
          )
          out = out %>%
            mutate(p = if_else((key == j & pop_group == i), mw$p.value, p)) 
        } else if (test == 'ks') {
          ks = ks.test(
            x = df_l %>% filter(group == TRUE) %>% pull(var),
            y = df_l %>% filter(group == FALSE) %>% pull(var)
          )
          out = out %>%
            mutate(p = if_else((key == j & pop_group == i), ks$p.value, p)) 
        }
        # effect size 
        cliff = cliffDelta(var ~ group, data = df_l, ci = TRUE, type = 'perc')
        out = out %>%
          mutate(d = if_else((key == j & pop_group == i),  cliff$Cliff.delta, d)) %>%
          mutate(d_min = if_else((key == j & pop_group == i), cliff$lower.ci, d_min)) %>%
          mutate(d_max = if_else((key == j & pop_group == i), cliff$upper.ci, d_max))
      }
    }
  }
  
  out = out %>%
    mutate(p = round(p, 3)) %>%
    mutate(p_sig = case_when(
      p >= 0.05 ~ '',
      p < 0.05 & p >= 0.01 ~ '*',
      p < 0.01 & p >= 0.001 ~ '**',
      p < 0.001 ~ '***'
    )) %>%
    mutate_at(c('n_r','avg', 'med', 'd', 'd_min', 'd_max'), round, digits = 2) %>%
    rename(x = key)
  
  #if (is.null(key)) {
  #  out = out %>% select(-key)
  #}
  return(out)
}


proc_cont = function(raw, filename) {
  
  proc = raw %>%
    mutate(d_ci = sprintf('%.2f [%.2f, %.2f]', d, d_min, d_max)) %>%
    select(x, pop_group, group, n_r, avg, med, p, d_ci, p_sig, d, d_min, d_max) %>%
    pivot_wider(names_from = group, values_from = c(n_r, avg, med)) 
  
  proc %>% 
    select(x, pop_group, n_r_FALSE, med_FALSE, n_r_TRUE, med_TRUE, p, d_ci, p_sig) %T>%
    write_csv(sprintf('out/%s.csv', filename)) 
  
  return(proc)
    
}
