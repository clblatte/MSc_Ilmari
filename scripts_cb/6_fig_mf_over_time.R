#####
#
# Start how to explore the FES effect of scenarios and its trade-offs
# Some code example from a different project...
# 2022-04-06
#
#####


# Calculate indicator values of scenarios over time (average over landscape, XX/ha)
# Normalize the values in relation to min/max observed in data; over all scenarios, years, and CC trajectories



### load libraries
library(dplyr)
library(tidyr)
library(ggplot2)



# --------------------
# PATHs
# --------------------

# get working direction
path <- getwd()



# ----------
# get yearly landscape values for indicators: here as sum() - for MSc we might use the mean()
# ----------

# all indicators except the ones addressing a regime share (SA, CCF_3, CCF_4, and "BAUwGTR", "BAUwT_GTR") - this is coming below.
# use all indicators that we have used in the optimization for the EU strategy: see https://github.com/clblatte/MSc_IlmariOpt/blob/master/MSc_ForestStrategy.ipynb 


# Indicators addressed in policy optimization (any further? check with optimization notebook, see link above)
# 
ind <- c("Harvested_V", "Biomass", "HSI_MOOSE", "HAZEL_GROUSE", 
         "CAPERCAILLIE", "V_deadwood", "prc_V_deciduous", "N_where_D_gt_40", "CARBON_SINK", "Recreation", "Scenic", 
         "BILBERRY", "COWBERRY", "ALL_MARKETED_MUSHROOMS") 

ind_timely_values <- df.solution_alldata[] %>%
  
  # here the area weighted sum() over each year was calculated, we might take the "mean()" instead
  group_by(year, policy, scenario) %>%
  summarise(sum_Harvested_V = sum(Harvested_V * represented_area_by_NFIplot * stand_share),
            Sum_Biomass = sum(Biomass * represented_area_by_NFIplot * stand_share),
            Sum_BILBERRY = sum(BILBERRY * represented_area_by_NFIplot * stand_share),
            Sum_COWBERRY = sum(COWBERRY * represented_area_by_NFIplot * stand_share),
            Sum_ALL_MARKETED_MUSHROOMS = sum(ALL_MARKETED_MUSHROOMS * represented_area_by_NFIplot * stand_share),
            Sum_HSI_MOOSE = sum(HSI_MOOSE * represented_area_by_NFIplot * stand_share),
            Sum_HAZEL_GROUSE = sum(HAZEL_GROUSE * represented_area_by_NFIplot * stand_share),
            Sum_CAPERCAILLIE = sum(CAPERCAILLIE * represented_area_by_NFIplot * stand_share),
            Sum_Deadwood_V = sum(V_deadwood * represented_area_by_NFIplot * stand_share),
            Sum_prc_V_deciduous = sum(prc_V_deciduous * represented_area_by_NFIplot * stand_share),
            Sum_N_where_D_gt_40 = sum(N_where_D_gt_40 * represented_area_by_NFIplot * stand_share ),
            Sum_CARBON_SINK = sum(CARBON_SINK * represented_area_by_NFIplot * stand_share),
            Sum_Recreation = sum(Recreation * represented_area_by_NFIplot * stand_share),
            Sum_Scenic = sum(Scenic * represented_area_by_NFIplot * stand_share))

# make df format from wide to long
# The new column is here called "objective", could also be called "indicator" --> think of to adjust this also below
ind_timely_values <- ind_timely_values  %>% tidyr::gather("objective", "value", 4:length(ind_timely_values))



# -----------
# Get optimal solution for indicators measuring an regime (SA, or CCF_3, CCF_4, and "BAUwGTR", "BAUwT_GTR") share of management regimes for the protection target
# -----------

# similar it can be done for regime SA

REGIMES = c("CCF_3", "CCF_4", "BAUwGTR", "BAUwT_GTR")
  
df.regimeInd <- df.solution_alldata %>% 
    # take only the needed columns
    select(policy, scenario, id, year, regime,represented_area_by_NFIplot, stand_share) %>% 
    # calculation the area share per solution (there can be two opt regimes per stand)
    mutate(solution_area_share = round(stand_share * represented_area_by_NFIplot)) %>% 
    
    # calculate the percentage share for each regime according to the area
    # First: area share by regime
    group_by(scenario, policy, regime, year) %>% 
    summarise(tot_area = sum(solution_area_share)) %>% 
    
    # add column with total area
    group_by(scenario, policy, year) %>% 
    mutate(sum_tot_area = sum(tot_area)) %>% 
    
    # calculate prc share
    mutate(prc_share = round((tot_area / sum_tot_area), digits = 4) ) %>%  # having it on scale between 0-1 to match later with normalized FES indicators
    
    ungroup() %>% 
    
    # filter the regimes and sum up
    filter(regime %in% REGIMES) %>% 
    group_by(policy, scenario, year) %>% 
    summarise(value = sum(prc_share)) %>% 
    ungroup() %>% #
    
    mutate(objective = "Ratio_protection_regimes") # or later for SA: "Ratio_SA_forests"
    
 

#### df for the regime indicator need to be combined with the other indicators (ind_timely_values)


   
# ----------
# group objective by ecosystem services - GROUP IT HERE ACCORDING YOUR POLICY TABLE, which indicators have been used to address the FES !!
# ----------

# add column indicating the ES
ind_timely_values <- ind_timely_values %>% 
  mutate(es = case_when(
    objective %in% c("Sum_Harvested_V")  ~ "Wood",
    objective %in% c("Sum_Biomass")  ~ "Bioenergy",
    objective %in% c("Sum_BILBERRY","Sum_COWBERRY", "Sum_ALL_MARKETED_MUSHROOMS" )  ~ "Nonwood",
    objective %in% c("Sum_HSI_MOOSE", "Sum_HAZEL_GROUSE", "Sum_CAPERCAILLIE")  ~ "Game",
    objective %in% c("Sum_Deadwood_V", "Sum_prc_V_deciduous", "Sum_N_where_D_gt_40", "Ratio_CCF_forests") ~ "Biodiversity",
    
    objective %in% c("Sum_CARBON_SINK")  ~ "Climate",
    
    objective %in% c("Sum_Recreation", "Sum_Scenic")  ~ "Recreation" #,
    #objective %in% c("Ratio_ACC_forests") ~ "Resilience",
    #objective %in% c("Ratio_CCF_onPeat") ~ "Water"
    )) 




####################################################################################################
# Normalize landscape indicators between observed ranges (min/max) in data
####################################################################################################



head(ind_timely_values)

# Normalize function
normalizeFct <- function(x) {(x - min(x)) / (max(x) - min(x))}

# Except the once related to management regime share -> they are already on 0-1 scale!
regime_indicator <- c("Ratio_protection_regimes", "Ratio_SA_forests")

unique(ind_timely_values$objective)

df.norm_timely2 <- ind_timely_values %>%
  group_by(objective) %>%
  # except regime indicators
  mutate(norm = ifelse(!objective %in% regime_indicator, normalizeFct(value), value)) # %>%
  # or if all indicators should be normalized, including the ones addressing a regime share
  # mutate(norm_all = normalizeFct(value))

head(df.norm_timely2)





