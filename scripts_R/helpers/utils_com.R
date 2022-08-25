# expressions -------------------------------------------------------------------------------------
ex_co2_target = expression('CO'['2']*' target [%]')
ex_ghg_target = expression('GHG reduction target [%]')
ex_ghg_target_2020 = expression('2020 GHG reduction target [%]')
ex_co2_target_2020 = expression('CO'['2']*' target 2020 [%]')
ex_ghg_emission = expression('GHG emissions [tCO2q]')
ex_ghg_emission_ppc = expression('GHG emissions per capita [tCO'['2']*'eq/cap]')
ex_ghg_emission_ppc2 = expression('GHG emissions per capita \n [tCO'['2']*'-eq/cap]')
ex_ghg_reduction_ppc = expression('GHG reduction per capita [tCO'['2']*'-eq/cap]')
ex_energy_supply_ppc = expression('Energy supply per capita [MWh/cap]')

ex_bei_ppc = expression('BEI [tCO'['2']*'eq/cap]')
ex_mei_ppc = expression('MEI [tCO'['2']*'eq/cap]')

ex_red_ppc = expression('GHG reduction [tCO'['2']*'e/cap]')

ex_red_per = 'GHG reduction [% BEI]'
ex_red_per_s = 'GHG reduction [% total reduction]'

ex_progress = 'SEAP progress [%]'
ex_progress_s = 'Sector progress [%]'

ex_budget = 'Budget [EUR/year/cap]'
ex_pop = 'Population [Inhabitants]'




# factors ------------------------------------------------------------------------------------------

# energy consumption - items
lev_con = c(
  'municipal_buildings_equipment_facilities', 'public_lighting', 'municipal_other',
  'industry_ets', 'industry_non_ets', 
  'institutional_buildings', 'tertiary_other',
  'residential_buildings',
  'buildings_equipment_facilities_non_allocated',
  'municipal_fleet_road', 'municipal_fleet_other',
  'public_transport_road', 'public_transport_rail', 'public_transport_water', 'public_transport_other',
  'private_transport_road', 'private_transport_water', 'private_transport_rail', 'private_transport_aviation', 'private_transport_other',
  'transport_non_allocated',
  'agriculture_forestry_fisheries',
  'other_non_allocated',
  'waste_management_disposal', 'waste_management_treatment', 'waste_management_incineration', 'waste_management_other',
  'water_management',
  'non_energy_related_sector'
  )

# energy consumption - sectors
lev_con_sec = c('buildings_equipment_facilities', 'transport', 'other_energy', 'non_energy')
lab_con_sec = c(
  'buildings_equipment_facilities' = 'Buildings', 
  'transport' = 'Transport', 
  'other_energy' = 'Other energy', 
  'non_energy' = 'Non-energy')
pal_con_sec = c(
  'buildings_equipment_facilities' = '#298fca', 
  'transport' = '#ffc45d', 
  'other_energy' = '#5ca08e', 
  'non_energy' = '#eb5a46')

# energy consumption - subsectors
lev_con_subsec = c(
  'municipal_buildings', 'tertiary_buildings', 'residential_buildings', 'industry',
  'buildings_equipment_facilities_non_allocated',
  'municipal_fleet', 'public_transport', 'private_transport', 'transport_non_allocated', 
  'agriculture_forestry_fisheries', 'other_non_allocated',
  'water_management', 'waste_management', 'non_energy_related_sector')
lab_con_subsec = c(
  'municipal_buildings' = 'Municipal buildings',
  'tertiary_buildings' = 'Tertiary buildings', 
  'residential_buildings' = 'Residential buildings', 
  'industry' = 'Industry',
  'buildings_equipment_facilities_non_allocated' = 'Buildings non allocated',
  'municipal_fleet' = 'Municipal fleet', 
  'public_transport' = 'Public transport', 
  'private_transport' = 'Private transport', 
  'transport_non_allocated' = 'Transport non allocated', 
  'agriculture_forestry_fisheries' = 'Agriculture', 
  'other_non_allocated' = 'Other non allocated',
  'water_management' = 'Wastewater management', 
  'waste_management' = 'Waste treatment',
  'non_energy_related_sector' = 'Non energy')
pal_con_subsec = c(
  'municipal_buildings' = '#094c72',
  'tertiary_buildings' = '#055a8c', 
  'residential_buildings' = '#026aa7', 
  'industry' = '#0079bf',
  'buildings_equipment_facilities_non_allocated' = '#298fca',
  
  'municipal_fleet' = '#efa536', 
  'public_transport' = '#ffc45d', 
  'private_transport' = '#f6e486', 
  'transport_non_allocated' = '#ffff9c', 
  
  'agriculture_forestry_fisheries' = '#006a4e', 
  'other_non_allocated' = '#5ca08e',
  
  'water_management' = '#933b27', 
  'waste_management' = '#cf513d',
  'non_energy_related_sector' = '#ef7564'
  )

pal_ac_sec = c(
  'municipal_buildings_equipment_facilities' =  '#094c72',
  'tertiary_buildings_equipment_facilities' = '#055a8c', 
  'residential_buildings' = '#026aa7', 
  'industry' = '#0079bf',
  'transport' =  '#ffc45d',
  'local_electricity_production' = '#b366ff',
  'local_heat_cold_production' = '#8000ff', 
  'waste' = '#cf513d',
  'others' = 'grey'
)




# energy supply - sectors
lev_sup_sec = c(
  "municipal_purchases_of_certified_green_electricity" ,
  "local/distributed_electricity_production",
  "local/distributed_electricity_production_(renewable_energy_only)" ,
  "local_heat/cold_production" 
)
lab_sup_sec = c(
  "municipal_purchases_of_certified_green_electricity" = 'Green electricity',
  "local/distributed_electricity_production" = 'Electricity production',
  "local/distributed_electricity_production_(renewable_energy_only)" = 'Renewable production',
  "local_heat/cold_production" = 'Heat/cold production'                              
)

# energy supply - subsectors
lev_sup_subsec = c(
  "certified_green_electricity_purchased",
  "certified_green_electricity_sold",
  "wind",
  "hydroelectric",
  'photovoltaics',
  'geothermal',
  'renewable_electricity_non_allocated',
  'combined_heat_and_power',
  'other',
  'district_heating_(heat-only)'
)
lab_sup_subsec = c(
  "municipal_purchases_of_certified_green_electricity" = 'Certified green electricity',
  "local/distributed_electricity_production" = 'Local/distributed electricity production',
  "local/distributed_electricity_production_(renewable_energy_only)" = 'Local/distributed electricity production (renewable energy only)',
  "local_heat/cold_production" = 'Local heat/cold production'                              
)

# actions - subsectors
lev_ac_sec = c(
  'municipal_buildings_equipment_facilities', 
  'tertiary_buildings_equipment_facilities',
  'residential_buildings', 
  'industry', 
  'transport', 
  'local_electricity_production',
  'local_heat_cold_production', 
  'waste', 
  'others'
)
lab_ac_sec = c(
  'municipal_buildings_equipment_facilities' =  'Municipal buildings',
  'tertiary_buildings_equipment_facilities' = 'Tertiary buildings',
  'residential_buildings' = 'Residential buildings', 
  'industry' = 'Industry', 
  'transport' = 'Transport', 
  'local_electricity_production' = 'Local Electricity Production',
  'local_heat_cold_production' = 'Local Heat/Cold Production', 
  'waste' = 'Waste', 
  'others' = 'Others'
)

pal_ac_sec = c(
  'municipal_buildings_equipment_facilities' =  'Municipal buildings',
  'tertiary_buildings_equipment_facilities' = 'Tertiary buildings',
  'residential_buildings' = 'Residential buildings', 
  'industry' = 'Industry', 
  'transport' = 'Transport', 
  'local_electricity_production' = 'Local Electricity Production',
  'local_heat_cold_production' = 'Local Heat/Cold Production', 
  'waste' = 'Waste', 
  'others' = 'Others'
)

lev_admin = c("mono-sectoral", "multi-sectoral", "multi-level")
lab_admin = c(
  "mono-sectoral" = 'Mono-sectoral', 
  "multi-sectoral" = 'Multi-sectoral', 
  "multi-level" = 'Multi-level'
)

lev_staff = c("Local authority", "Covenant coordinator", "Covenant supporter", "External consultant",
  "Other"  )
lab_staff = c(
  "Local authority" = 'Local authority', 
  "Covenant coordinator" = 'CoM coordinator', 
  "Covenant supporter" = 'CoM supporter', 
  "External consultant" = 'External consultant',
  "Other" = 'Other'
  )

lev_stk_type = c(
  "Local authority's staff", 
  "External stakeholders at local level", 
  "Stakeholders at other levels of governance"
  )
lab_stk_type = c(
  "Local authority's staff" = "Local authority", 
  "External stakeholders at local level" = 'External local level', 
  "Stakeholders at other levels of governance" = 'Other levels'
)

lev_budget_src = c("Local_authoritys_own_resources", "public", "private", "not_allocated")
lab_budget_src = c(
  "Local_authoritys_own_resources" = 'Local authority', 
  "public" = 'External: public', 
  "private" = 'External: private', 
  "not_allocated" = 'Not allocated'
  )

lev_co2_units = c('CO2', 'CO2_equivalent')
lab_co2_units = c('CO2' = 'CO2', 'CO2_equivalent' = 'CO2-eq')

lev_factor_type = c('IPCC', 'LCA')

lev_ac_sec = c(
  'municipal_buildings_equipment_facilities', 'tertiary_buildings_equipment_facilities',
  'residential_buildings', 'industry', 'transport', 'local_electricity_production',
  'local_heat_cold_production', 'waste', 'others'
  )
lab_ac_sec = c(
  'municipal_buildings_equipment_facilities' =  'Municipal buildings',
  'tertiary_buildings_equipment_facilities' = 'Tertiary buildings',
  'residential_buildings' = 'Residential buildings', 
  'industry' = 'Industry', 
  'transport' = 'Transport', 
  'local_electricity_production' = 'Local Electricity Production',
  'local_heat_cold_production' = 'Local Heat/Cold Production', 
  'waste' = 'Waste', 
  'others' = 'Others'
)



