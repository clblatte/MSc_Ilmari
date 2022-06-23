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
indicator_col <- c("Harvested_V", "Biomass", "V_deadwood", "prc_V_deciduous", "ALL_MARKETED_MUSHROOMS", "BILBERRY", "COWBERRY", "Recreation", "Scenic", "CARBON_STORAGE_Update")


## CB: I removed the function... since it is here actually not needed - area weighted averages only calculated once

df.es <- df.solution_alldata %>% 
    # select only columns that are needed
    # basic columns
    select(id, year, regime, policy, scenario, 
           # area 
           represented_area_by_NFIplot, stand_share,
           # columns for indicators
           indicator_col) %>% 
    
    
    # multiply the represented forest area with stand share assigned to an optimal regime
    mutate(represented_area_stand_share = represented_area_by_NFIplot * stand_share) %>% 
    
    # calculate the total forest area: new column used for calculate the area weighted average
    group_by(scenario, policy, year) %>% 
    mutate(tot_area = sum(represented_area_stand_share)) %>% 
    ungroup() %>% 
    
    # multiply indicators by area factor to get "area weighted indicator"
    mutate_at(indicator_col, ~ .* (represented_area_stand_share/tot_area) ) %>%
    
    # calculate sum over the country to get "area weighted averages per year"
    group_by(policy, scenario, year) %>%
    summarise_at(indicator_col, sum) %>% 
    ungroup() %>% 
    as.data.frame()
  
  
 


# Use function to calculate area weighted averages for selected ecosystem services

## CB: you identified the wrong columns that need to gathered from wide to long (5:13); I changed it a bit that you see what I mean

# df.es <- FctAreaWeightedAverage(df.solution_alldata, all_of(indicators)) %>% 
#   # from wide to long format
#   gather("indicator","areaWeightedAverage", 5:13)

head(df.es)
df.es <- df.es %>% 
# after applying the function, and looking at the df, we see that indicators are in column 4:10, this needs to replace the values 5:13, 
# This was coming from my earlier example I guess
  tidyr::gather("indicator","areaWeightedAverage", 4:13)
# now we can look at it again
head(df.es)


# ----------
# Normalization of values - synergies/trade-off analysis
# ----------

# Normalize function - normalized values for ecosystem services

## CB. THIS WAS THE WRONG FUNCTION !!! That is the reason why it was not giving correct values
# normalizeFct <- function(x) {(x - min(x))/(max(x)/min(x))} 
normalizeFct <- function(x) {(x - min(x))/(max(x) - min(x))} # minus instead divide !!

# New dataframe for analysis - use function to get normalized values
df.norm <- df.es %>%
  group_by(indicator) %>%
  mutate(norm = normalizeFct(areaWeightedAverage)) %>%
  ungroup()

summary(df.norm$norm)



# Normalization analysis - graph each ecosystem service in conjunction with each other

# filter for 3 time steps
df.norm <- df.norm %>%
  filter(year %in% c("2021", "2056", "2116"))

# Stuck here
#
# This results in a dataframe with many rows of NA and I was unable to combine the rows into a format that could be used in the next step to make the graphs
df.norm2021 <- df.norm %>%
  filter(year %in% c("2021"))%>%
  select(-Harvested_V, -areaWeightedAverage)%>%
  spread(indicator, norm)

# Next I'll make spider charts for each regime at each of the three time steps. This would result in three sets of graphs (one for each time step), containing a spider chart for each regime (two climate scenarios in each graph) with all indicators in every graph.

# I was using instructions from the link above
# https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/
# install.packages("fmsb")


library(fmsb)

  
  
