rm(list = ls())

library(tidyverse)
library(magrittr)
library(egg)
library(ggpubr)
library(ggpointdensity)

source('scripts_R/config.R')
source('scripts_R/helpers/utils.R')
source('scripts_R/helpers/utils_com.R')
source('scripts_R/helpers/plotting.R')

main = read_csv('data/proc/main.csv')
mei_s = read_csv('data/proc/last_qc_sec_common.csv') %>%
  left_join(main %>% select(action_plan_id, pop_group), by = 'action_plan_id') %>%
  mutate(pop_group = factor(pop_group, levels = lev_pop)) %>%
  rename(subsector = sector_c)

# ABSOLUTE IMPACT (including zeros)
stats = mei_s %>%
  group_by(subsector) %>%
  summarize(
    n_fill = 100 * sum(abs(red_ppc_s) > 0) / n(),
    red_ppc_med = median(red_ppc_s),
    red_ppc_avg = mean(red_ppc_s),
    red_per_med = median(red_per_s),
    red_per_avg = mean(red_per_s)
  ) %>%
  arrange(desc(n_fill))
# sectors arranged by number of cities reporting per sector 
lev_subsec = stats$subsector
stats = stats %>% 
  mutate(subsector = factor(subsector, levels = lev_subsec)) 
mei_s = mei_s %>%
  mutate(subsector = factor(subsector, levels = rev(lev_subsec))) 

# RELATIVE IMPACT (excluding sectors = 0 in both MEI and BEI)
mei_s_nz = mei_s %>%
  filter(!(bei_ppc_s == 0 & mei_ppc_s == 0))  %>% 
  mutate(subsector = factor(subsector, levels = rev(lev_subsec)))
stats_nz = mei_s_nz %>%
  group_by(subsector) %>%
  summarize(
    n_fill = 100 * sum(abs(red_ppc_s) > 0) / n(),
    red_ppc_med = median(red_ppc_s),
    red_ppc_avg = mean(red_ppc_s),
    red_per_med = median(red_per_s),
    red_per_avg = mean(red_per_s)
  ) %>%
  mutate(subsector = factor(subsector, levels = rev(lev_subsec)))

aux = bind_rows(
  mei_s %>% select(subsector, pop_group, red_per_s) %>% mutate(type = 'all'),
  mei_s_nz %>% select(subsector, pop_group, red_per_s) %>% mutate(type = 'nz'),
)

g1 = ggplot(mei_s, aes(x = subsector, y = red_per_s, fill = pop_group)) +
  geom_hline(yintercept = 0, size = .3) +
  geom_boxplot(outlier.size = 0, outlier.color = 'white', size = .3) +
  geom_text(data = stats, aes(label = sprintf("%.1f%%", n_fill), x = subsector),
            y = -25, size = 2.5, inherit.aes = FALSE) + 
  stat_summary(aes(group = pop_group), fun=mean, geom="point", shape=23, size=1, color="red", fill = 'red', position=position_dodge(width=0.75)) +
  scale_fill_manual(values = pal_pop, labels = lab_pop) +
  scale_x_discrete(labels = lab_sec_c) + 
  scale_y_continuous(breaks = seq(-20, 40, 10)) +
  coord_flip(ylim = c(-30, 40)) +
  labs(x = NULL, y = "Sector emissions reduction [%]", fill = NULL) + 
  theme_ruben() +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank()) +
  theme(plot.margin = margin(0.1, 0.1, .1, 1, unit = 'cm')) 
g2 = ggplot(mei_s_nz, aes(x = subsector, y = red_per_s, fill = pop_group)) +
  geom_hline(yintercept = 0, size = .3) +
  geom_boxplot(outlier.size = 0, outlier.color = 'white', size = .3) +
  stat_summary(aes(group = pop_group), fun=mean, geom="point", shape=23, size=1, color="red", fill = 'red', position=position_dodge(width=0.75)) +
  scale_fill_manual(values = pal_pop, labels = lab_pop) +
  scale_x_discrete(labels = lab_sec_c) + 
  scale_y_continuous(breaks = seq(-20, 40, 10)) +
  coord_flip(ylim = c(-30, 40)) +
  labs(x = NULL, y = "Sector emissions reduction [%]") + 
  theme_ruben() +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank()) +
  theme( plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = 'cm')) 
gA = ggpubr::ggarrange(g1, g2, nrow = 1, ncol = 2, widths = c(1 , .6), common.legend = TRUE, 
                       legend = 'right', labels = c('a', 'b'))

# scatter ------------------------------------------------------------------------------------------
# only most important sectors
library(data.table)
dt = data.table(mei_s_nz)
stats = dt[, cor.test(red_per, red_per_s, method = 'spearman')[-2], by = .(subsector, pop_group)]
stats = tibble(stats) %>%
  select(subsector, pop_group, rho = estimate, p.value) %>%
  mutate(p = sprintf('%.3f', p.value)) %>%
  mutate(p_sig = case_when(
    p >= 0.05 ~ '',
    p < 0.05 & p >= 0.01 ~ '*',
    p < 0.01 & p >= 0.001 ~ '**',
    p < 0.001 ~ '***'
  )) 
detach('package:data.table')

#stats = mei_s_nz %>% 
#  group_by(subsector, pop_group) %>%
#  summarise(r = cor(red_per, red_per_s, method = 'spearman'))   


model = mei_s_nz %>%
  group_by(subsector, pop_group) %>%
  do(m = tidy(lm(red_per ~ red_per_s, data = .))) %>%
  mutate(pvalue = NA, slope = NA, intercept = NA) 
for (i in 1:nrow(model)) {
  model$pvalue[i] = model$m[[i]]$p.value[2]
  model$slope[i] = model$m[[i]]$estimate[2]
  model$intercept[i] = model$m[[i]]$estimate[1]
}
stats = stats %>%
  left_join(model)
stats_sig = stats %>% filter(pvalue < 0.05)

avg1 =  mei_s %>%  group_by(pop_group, subsector) %>%  summarize(avg = mean(red_per_s))
avg2 =  mei_s_nz %>%  group_by(pop_group, subsector) %>%  summarize(avg_nz = mean(red_per_s))
avg = avg1 %>% left_join(avg2)# %>% filter(subsector %in% subsec2)
df_plot = mei_s_nz %>% 
  mutate(subsector = factor(subsector, lev_subsec)) %>%
  #filter(subsector %in% subsec2)  %>%
  mutate(pop_group = factor(pop_group, lev_pop))
g0 = ggplot(df_plot, aes(x = red_per_s, fill = pop_group, color = pop_group)) +
  facet_wrap(~subsector, nrow = 1, labeller = labeller(subsector = lab_con_subsec)) +
  geom_density(alpha = .5, bw = 4) +
  scale_fill_manual(values = pal_pop, guide = FALSE) +
  scale_color_manual(values = pal_pop, guide = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_ruben(box = FALSE) +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  theme(plot.margin = margin(c(0, .8, 0, 1.4), unit = 'cm'))
g1 = ggplot(df_plot, aes(x = red_per_s, y = red_per)) +
  facet_grid(pop_group~subsector, labeller =labeller(subsector = lab_sec_c, pop_group = lab_pop)) +
  geom_vline(xintercept = 0, size = .3) +
  #geom_vline(xintercept = 20, size = .3, color = 'red') +
  #geom_smooth(data = df_plot %>% right_join(stats_sig), method = 'lm') +
  geom_hline(yintercept = 0, size = .3) +
  geom_pointdensity(adjust = 7, size = 1) +
  geom_text(data = stats, aes(label = sprintf('rho == %.2f*"%s"', rho, p_sig)), x = -95, y = 80, size = 2.5, hjust = 0, parse = T) +
  geom_text(data = avg, aes(label = sprintf('avg. = %.1f%%', avg_nz)), x = -95, y = 97, size = 2.5, hjust = 0) +
  geom_abline(slope = 1, intercept = 0, size = .2, linetype = 2) +
  #geom_smooth(method = 'rlm', color = 'red', span = 2, size = .5) +
  scale_color_viridis(guide = FALSE) +
  labs(x = 'Sector emissions reduction [%]', y = 'Total emissions reduction [%]') +
  coord_fixed(xlim = c(-100, 100), ylim = c(-100 , 100)) +
  theme_ruben(box = TRUE) 

#gB = ggpubr::ggarrange(g0,g1, nrow = 2, ncol = 1, heights = c(.5, 2))
gB = g1
#ggpubr::ggarrange(gA, gB, nrow = 2, ncol = 1, labels = c('', 'c'))


# share ------------------------------------------------------------------------
lev_red = seq(-80, 80, 20)
min_group = 5
lab = tribble(
  ~group, ~pos,
  '(-80,-60]', 5,
  '(-60,-40]', 5,   
  '(-40,-20]', 5,
  '(-20,0]', 5,
  '(0,20]', 15,     
  '(20,40]', 35,      
  '(40,60]', 55,    
  '(60,80]', 75     
)


# at least 5 cities per group !
n_tot =  main %>%
  bind_rows(main %>% mutate(pop_group = 'all')) %>%
  group_by(pop_group) %>%
  summarize(n_tot = n())
n_group = mei_s %>%
  bind_rows(mei_s %>% mutate(pop_group = 'all')) %>%
  mutate(group = cut(red_per, breaks = lev_red)) %>% 
  drop_na(group) %>%
  distinct(pop_group, group, action_plan_id) %>%
  group_by(pop_group, group) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  left_join(n_tot) %>%
  mutate(nr = 100 * n / n_tot) %>%
  filter(nr > 1) %>%
  left_join(lab) %>%
  arrange(group)
stats = mei_s %>% 
  bind_rows(mei_s %>% mutate(pop_group = 'all')) %>%
  mutate(group = cut(red_per, breaks = lev_red)) %>%
  drop_na(group) %>%
  group_by(pop_group, group, subsector) %>%
  summarize(
    red_per_s = mean(red_per_s),
    sign = mean(red_per) / abs(mean(red_per)),
    progress = median(progress),
  ) %>%
  ungroup() %>%
  right_join(n_group) %>%
  mutate(subsector = factor(subsector, levels = rev(lev_subsec))) %>%
  mutate(group = factor(group, levels = lab$group))
aux = mei_s %>% 
  bind_rows(mei_s %>% mutate(pop_group = 'all')) %>%
  mutate(group = cut(red_per, breaks = lev_red)) %>%
  drop_na(group) %>%
  group_by(pop_group, group) %>%
  summarize(
    progress = median(progress)
  ) %>%
  spread(group, progress)


gC = ggplot(stats %>% filter(pop_group %in% lev_pop), 
            aes(x = group, y = red_per_s, fill = subsector)) +
  facet_wrap(~pop_group, nrow = 1, labeller = labeller(pop_group = lab_pop)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0, size = .5) +
  geom_text(data = n_group %>%filter(pop_group %in% lev_pop), aes(label = sprintf('%.1f %%', nr), y = pos, x = group),
            size = 2.5, inherit.aes = FALSE, hjust = 0) +
  coord_flip() +
  labs(x = 'Total emissions reduction [%]', y = 'Sector emissions reduction [%]') +
  labs(fill = NULL) +
  scale_fill_manual(values = pal_sec_c, labels = lab_sec_c)+
  guides(fill = guide_legend(ncol = 1)) +
  theme_ruben() +
  theme(legend.position = 'right') +
  theme( plot.margin = margin(0.1, 1, 0.1, 2, unit = 'cm')) 

g = ggpubr::ggarrange(gA, gB, gC, nrow = 3, ncol = 1, labels = c('', 'c', 'd'), 
                      heights = c(.45, 1, .5))
ggsave('figs/achievements/sector/pannel.png', g, width = 27, height = 23, units = 'cm')
