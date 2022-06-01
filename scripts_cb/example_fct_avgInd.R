#
# Example for function to calculate area weighted average of indicators 
# That how I have done it once - probably there are also better ways ;-)
# CB: 2022-05-24



#
# Eg. function to calculate area weighted averages for indicators...
#


# specify the indicator columns, except the ones addressing regime shares (see below) !!! 
# See policy table of FIN, e.g.: share of SA, conservation regimes, CCF on Peat
# Those need to be calculated seperately
indicator_col <- c("Harvested_V", "Biomass", "i_Vm3", 
                   "BILBERRY", "COWBERRY", "ALL_MARKETED_MUSHROOMS",
                   "HSI_MOOSE", "CAPERCAILLIE", "HAZEL_GROUSE", 
                   "V_deadwood", "prc_V_deciduous", "N_where_D_gt_40",
                   "CARBON_SINK", "Recreation", "Scenic"
                   ,"CARBON_STORAGE")


# -----------
# Calculated area weighted averages per indicator
# -----------

# Restructure the data - put it in a fuction since it needs to be done twice
FctAreaWeightedAverage <- function(data, indicator_col){
  
  # # to test the part within the function
  #data = df.solution_alldata_gv1
  
  df.es <- data %>% 
    # select only columns that are needed / is faster
    # basic columns
    select(id, year, regime, policy, scenario, globiom, 
           # in FIN, 19 provinces, in each province one NFI plot represents a different forest area
           represented_area_by_NFIplot, 
           # comes originally from df.solution - a stand can be assigned partly to two optimal regimes (50% BAU, 50% CCF)
           stand_share,
           # columns for indicators
           indicator_col) %>% 
    #
    # multiply the represented forest area with stand share assigned to an optimal regime
    #
    mutate(represented_area_stand_share = represented_area_by_NFIplot * stand_share) %>% 
    #
    # calculate the total forest area: new column used for calculate the area weighted average
    #
    group_by(scenario, policy, year) %>% 
    mutate(tot_area = sum(represented_area_stand_share)) %>% 
    ungroup() %>% 
    #
    # multiply indicators by area factor to get "area weighted indicator"
    #
    mutate_at(indicator_col, ~ .* (represented_area_stand_share/tot_area) ) %>%
    #
    # calculate sum over the country to get "area weighted averages per year"
    #
    group_by(policy, scenario, globiom, year) %>%
    summarise_at(indicator_col, sum) 
  
  return(df.es)
  
}


# Parameter, defining to which ES an indicator belongs
indicator_es <- read.csv(paste0(path, "params/indicator_es_Copy.txt"),
                         header = TRUE,
                         sep = ",")

df.es_gv1 <- FctAreaWeightedAverage(df.solution_alldata_gv1, all_of(indicator_col)) %>% 
  # from wide to long format
  gather("indicator","areaWeightedAverage", 5:20)




##########################################################################################
##########################################################################################




#
# Eg. Function to calculate the % share of management for different regimes
#

FctPrcRegimeGroup <- function(data, REGIMES){
  
  # # Test
  # data = df.solution_alldata
  # REGIMES = c("CCF_3", "CCF_4", "BAUwGTR"))
  
  df.regimeInd <- data %>% 
    select(policy, scenario, id, year, regime,represented_area_by_NFIplot, stand_share) %>% 
    # calculation the area share per solution (there can be two opt regimes per stand)
    mutate(solution_area_share = round(stand_share * represented_area_by_NFIplot, digits=0)) %>% 
    
    # calculate the percentage share for each regime according to the area
    # First: area share by regime
    group_by(scenario, policy, regime, year) %>% 
    summarise(tot_area = sum(solution_area_share)) %>% 
    
    # add column with total area
    group_by(scenario, policy, year) %>% 
    mutate(sum_tot_area = sum(tot_area)) %>% 
    
    # calculate prc share
    #mutate(prc_share = round((tot_area / sum_tot_area)*100, digits = 1) ) %>% # for FES indicator effects expressed as percentage difference (0-100)
    mutate(prc_share = round((tot_area / sum_tot_area), digits = 4) ) %>%  # for FES indicators that were "normalized" (0-1)
    
    ungroup() %>% 
    
    # filter the regimes and sum up
    filter(regime %in% REGIMES) %>% 
    group_by(policy, scenario, year) %>% 
    summarise(value = sum(prc_share)) %>% 
    ungroup()
  
  return(df.regimeInd)
  
}

# call the function for the joint data
# "Ratio_CCF_forests"
opt_ccf_forest <-  FctPrcRegimeGroup(df.solution_alldata, c("CCF_3", "CCF_4", "BAUwGTR, BAUwT_GTR")) %>%  # specify here the regimes used to address the extensify management
  mutate(objective = "Ratio_CCF_forests") 


### Same could also be done for SA regime









