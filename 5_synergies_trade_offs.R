####
#
# Synergies and trade offs
# 2022-05
#
####


### load libraries
library(dplyr)
library(ggplot2)
library(tidyr)



# -----------
# Calculate area weighted averages per indicator
# -----------


# specify the indicator columns, except the ones addressing regime shares
indicators <- c("Harvested_V", "Biomass", "V_deadwood", "prc_V_deciduous", "ALL_MARKETED_MUSHROOMS", "BILBERRY", "COWBERRY", "Recreation", "Scenic", "CARBON_STORAGE_Update")


# Restructure the data - put it in a fuction since it needs to be done twice
FctAreaWeightedAverage <- function(data, indicators){
  
  # to test the part within the function
  # data = df.solution_alldata
  
  df.es <- data %>% 
    # select only columns that are needed
    # basic columns
    select(id, year, regime, policy, scenario, 
           # area 
           represented_area_by_NFIplot, stand_share,
           # columns for indicators
           indicators) %>% 
    
    
    # multiply the represented forest area with stand share assigned to an optimal regime
    mutate(represented_area_stand_share = represented_area_by_NFIplot * stand_share) %>% 
    
    # calculate the total forest area: new column used for calculate the area weighted average
    group_by(scenario, policy, year) %>% 
    mutate(tot_area = sum(represented_area_stand_share)) %>% 
    ungroup() %>% 
    
    # multiply indicators by area factor to get "area weighted indicator"
    mutate_at(indicators, ~ .* (represented_area_stand_share/tot_area) ) %>%
    
    # calculate sum over the country to get "area weighted averages per year"
    group_by(policy, scenario, year) %>%
    summarise_at(indicators, sum) 
  
  return(df.es)
  
}


# Use function to calculate area weighted averages for selected ecosystem services
df.es <- FctAreaWeightedAverage(df.solution_alldata, all_of(indicators)) %>% 
  # from wide to long format
  gather("indicator","areaWeightedAverage", 5:14)


# ----------
# Normalization of values - synergies/trade-off analysis
# ----------

# Normalize function - normalized values for ecosystem services
normalizeFct <- function(x) {(x - min(x))/(max(x)/min(x))}

# New dataframe for analysis - use function to get normalized values
df.norm <- df.es %>%
  group_by(indicator) %>%
  mutate(norm = normalizeFct(areaWeightedAverage)) %>%
  ungroup()

# Normalization analysis - graph each ecosystem service in conjunction with each other

plot_norm <- df.norm %>%
  ggplot(aes(x=year, y=norm))

