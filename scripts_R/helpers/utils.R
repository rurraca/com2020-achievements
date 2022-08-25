# population groups

lev_pop = c('large', 'small')
lab_pop = c('small' ='Pop. < 50 000', 'large' = 'Pop. > 50 000')
pal_pop = c('small' = '#c99bca', 'large' = "#a2ddcb")


# common sectors

lev_sec_c = c('municipal_buildings_equipment_facilities', 'residential_tertiary', 'industry', 'transport', 'others')
lab_sec_c = c(
  'municipal_buildings_equipment_facilities' = 'Municipal buildings',
  'residential_tertiary' = 'Residential & tertiary buildings',
  'industry' = 'Industry',
  'transport' = 'Transport',
  'others' = 'Others'
)
pal_sec_c = c(
  'municipal_buildings_equipment_facilities' = '#e5ae38',
  'residential_tertiary' = '#30a2da',
  'industry' = '#fc4f30',
  'transport' = '#6d904f',
  'others' = '#8b8b8b'
)




lab_vars = c(
  'red_per' = 'GHG reduction: total',
  'red_residential' = 'GHG reduction share: residential',
  'red_transport' = 'GHG reduction share: transport',
  'red_industry' = 'GHG reduction share: industry',
  'red_municipal' = 'GHG reduction share: municipal', 
  'red_other' = 'GHG reduction share: other',
  'co2_target' = 'GHG reduction target', 
  'bei_ppc' = 'GHG baseline emissions', 
  'budget_total_ppc' = 'annual budget',
  'y_bei' = 'baseline year', 
  'y_start' = 'SEAP start year', 
  'y_submission' = 'year submission plan', 
  'y_mei' = 'MEI year', 
  'y_plan' = 'MEI year - SEAP start year', 
  'y_diff' = 'MEI year - baseline year', 
  'n_mon' = 'number of MEIs',
  'overlapper' = 'overlapper', 
  'group_approach_eval' = 'CTC approach evaluation',
  'staff_local' = 'staff: local', 
  'staff_supporter' = 'staff: CoM supporter', 
  'staff_coordinator' = 'staff: CoM coordinator', 
  'staff_external' = 'staff: external',
  'stk_local' = 'stakeholder: local', 
  'stk_external' = 'stakeholder: external', 
  'stk_other' = 'stakeholder: other',
  'budget_local' = 'budget: local', 
  'budget_private' = 'budget: private', 
  'budget_public' = 'budget: public', 
  'budget_external' = 'budget: external', 
  'budget_notallocated' = 'budget: non-allocated'
)
