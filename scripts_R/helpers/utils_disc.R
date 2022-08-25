stats_disc = function(df, group, key, total) {

  out = df %>% 
    select(group = !!group, key = !!key) %>% 
    group_by(group, key) %>%
    summarize(n_true = n()) %>%
    ungroup() %>%
    left_join(total %>% rename(group = !!group)) %>%
    mutate(n_false = n_tot - n_true) %>%
    mutate(n_true_r = 100 * (n_true / n_tot)) %>%
    mutate(p = NA_real_, odds = NA_real_, odds_min = NA_real_, odds_max = NA_real_) %>%
    mutate(group = factor(group, levels = c(TRUE, FALSE))) %>%
    arrange(key, group)
  
  levels = unique(out$key)
  for (l in levels) {
    fisher = out %>% 
      filter(key == l) %>%
      select(n_true, n_false) %>%
      as_tibble() %>% 
      fisher.test() 
    out = out %>% 
      mutate(p = if_else(key == l, fisher$p.value, p)) %>%
      mutate(odds = if_else(key == l, fisher$estimate, odds)) %>%
      mutate(odds_min = if_else(key == l, fisher$conf.int[1], odds_min)) %>%
      mutate(odds_max = if_else(key == l, fisher$conf.int[2], odds_max)) 
  }
  out = out %>%
    mutate(p = round(p, 3)) %>%
    mutate(n_true_r = round(n_true_r, 1)) %>%
    mutate_at(c('odds', 'odds_min', 'odds_max'), round, digits = 2) %>%
    mutate(p_sig = case_when(
      p >= 0.05 ~ '',
      p < 0.05 & p >= 0.01 ~ '*',
      p < 0.01 & p >= 0.001 ~ '**',
      p < 0.001 ~ '***'
    )) %>%
    mutate(group = factor(group, levels = c('FALSE', 'TRUE'))) %>%
    arrange(key, group)
}

stats_disc_row = function(stats_col) {
  
  stats_row = stats_col %>%
      group_by(key) %>%
      mutate(n_tot_true = sum(n_true))%>%
      mutate(n_tot_false = sum(n_false)) %>%
      ungroup() %>%
      filter(group == TRUE) %>%
      select(-n_tot, -n_true_r, -group) %>%
      mutate(n_true_r = 100 * n_true / n_tot_true) %>%
      mutate(n_false_r = 100 *n_false / n_tot_false) %>%
      #mutate(odds_true = per_true / (100 - per_true)) %>%
      #mutate(odds_false = per_false / (100 - per_false)) %>%
      gather(group, n_true_r, n_true_r, n_false_r) %>%
      arrange(key) %>%
      mutate(group = factor(group, levels = c('n_false_r', 'n_true_r'))) %>%
      mutate(group = recode(group, n_false_r = "FALSE", n_true_r = "TRUE")) 
  return (stats_row)
}
  

proc_disc = function(raw, filename, levels) {
  
    proc = raw %>%
      mutate(n = str_c(n_true, ' (', round(n_true_r, 1), ' %)')) %>%
      mutate(odds_ci = str_c(odds, ' [', odds_min, ', ', odds_max, ']')) %>%
      mutate(odds_ci2 = str_c('OR \n', odds, ' \n', p_sig)) %>%
      select(key, group, n, p, p_sig, odds, odds_ci, odds_min, odds_max, odds_ci2) %>%
      spread(group, n) %>%
      select(key, `TRUE`, `FALSE`, p, odds, odds_ci, odds_min, odds_max, p_sig, odds_ci2) %>%
      left_join(raw %>% group_by(key) %>% summarize(y_lab = max(n_true_r)) %>% ungroup()) %>%
      mutate(x_lab = seq(1, length.out = length(levels))) %>%
      mutate(x_lab = if_else(p_sig == '', NA_real_, x_lab)) 
    
    proc %>%
      select(key, `FALSE`, `TRUE`, p, odds_ci, p_sig) %>%
      write_csv(sprintf('out/%s.csv', filename)) 

  return(proc)
}
