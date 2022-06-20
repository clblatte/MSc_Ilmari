#####
#
# Load and format the data coming from the optimization tool
# (solution_..., solutionAlldata on the virtual machine)
# 
# 2022-01-31
#
#####



# get working direction
path <- getwd()


### load libraries
library(dplyr)
library(tidyr)
library(openxlsx)




# --------------------
# read params
# --------------------

# Info to group the regimes according to 6 management classes as in Blattert et al. 2022 
regimeclass <- read.csv(paste0(path, "/params/regimeclass.txt"), sep = ";"  ,header = TRUE, stringsAsFactors = TRUE)



# --------------------
# read solution data (contains optimal regime share for each stand)
# --------------------

# List all .csv files in folder opt_data with "solution_" in the name
# and combine the data to one file
solution <- list.files(paste0(path, "/opt_data/"), pattern = "solution_")
solution_names <- gsub(".csv", "", solution)

# create a empty object to which files will be added
df.solution.t <- NULL

# read individual files in a "for loop" - in case we have later multiple scenarios 
for(name in solution_names) {
  
  # read each solution_ file
  df <- read.csv(paste0(path, "/opt_data/", name, ".csv"), sep = ","  ,header = TRUE)
  
  # add a new column that contains the name of the file
  df$name <- name 
  
  # bind the data to the empty opbject
  df.solution.t <- rbind(df.solution.t, df)
  rm(df)

}

# modify the data further
df.solution <- df.solution.t %>%  
  # split the column name to gain additional infos in new columns (split by "_")
  separate(name, c("solution","policy", "scenario", "info"), sep = "_") %>% 
  # remove unnecessary columns
  select(- c("solution", "info")) %>% 
  # rename columns
  rename(NoStands = X, # consecutive number given by opt tool
         id = X0, # unique stand ID, 
         regime = X1, # optimal management regime for each stand
         stand_share = X2)  # stand share the optimal regime is allocated, genereally = 1, but can also be allocated to two/three regimes, needs to be considered!
  

head(df.solution)

# remove temporary files                           
rm(df.solution.t) 
rm(solution, solution_names)
  


  
  
# --------------------
# read solution data (contains timely developemen of indicator uder the optimal solution)
# --------------------  

# same process like above
solution_alldata <- list.files(paste0(path, "/opt_data/"), pattern = "solutionAlldata_")
solution_alldata_names <- gsub(".csv", "", solution_alldata)
  
df.solution_alldata.t <- NULL
  
for(name in solution_alldata_names) {
  
  df <- read.csv(paste0(path, "/opt_data/", name, ".csv"), sep = ","  , header = TRUE)
    
  df$name <- name 
    
  df.solution_alldata.t <- rbind(df.solution_alldata.t, df)
  rm(df)
    
}  
  
# modify the combined data set further
df.solution_alldata <- df.solution_alldata.t  %>% 
  # rename deadwood otherwise it will be removed as well by the over next line
  rename(V_deadwood = V_total_deadwood) %>% 
  # remove the columns created in the opt tool "Relative_ ...", "Total_ ..."
  select(!contains("Relative_")) %>%
  select(!contains("Total_")) %>% 
  select(- c("CCF_forests", "SA_forests", "Broadleave_forests")) %>% 
  # remove also column (climate) scenario; so far I try to attach the climate scenario based on the file name
  select(- c("scenario")) %>% 
  # split the data name column
  separate(name, c("solution","policy", "scenario", "info"), sep = "_") %>% 
  # remove unnecessary columns 
  select(- c("solution", "info"))   

rm(df.solution_alldata.t)  

  
  
# --------------------
# merge regime classification
# --------------------

df.solution <- df.solution %>% 
  # merge the regime classes
  left_join(regimeclass, by = "regime") 

df.solution_alldata <- df.solution_alldata %>% 
  # merge the regime classes
  left_join(regimeclass, by = "regime") 
  


# --------------------
# Add represented forest area to solution_ data (needed to calculte the % share of each regime)
# --------------------

# get the represented area form the alldata 
areabyid <- df.solution_alldata[,c("id","represented_area_by_NFIplot")] %>% 
  dplyr::distinct()  

# add the represented area by ID to solution, which does not contain this info
df.solution <- df.solution %>% 
  left_join(areabyid, by = "id" ) %>% 
  # considere if multipe optimal regimes were assigned to eachs stand
  mutate(solution_area_share = round(stand_share * represented_area_by_NFIplot, digits=0)) %>% 
  ungroup()

rm(areabyid) #remove temporary file



# --------------------
# Add stand_share & solution_area_share to solutionAlldata_ (needed, e.g. for calculating later the areaWeightedAverage of indicators)
# --------------------

solution_area_share <- df.solution  %>% 
  select(id, regime,policy,scenario, stand_share, solution_area_share)
  
# attach it
df.solution_alldata <- df.solution_alldata %>% 
  left_join(solution_area_share, by = c("id", "regime", "policy", "scenario"))

rm(solution_area_share)


# --------------------
# Remove some unused columns
# --------------------

df.solution <- df.solution %>%  select(-NoStands)



# --------------------
# combine ref scenario SA with optimized scenarios
# --------------------

df.solution_alldata <- df.solution_alldata %>% select(-Old_forests) %>% 
  rbind(df.refall)

df.solution <- df.solution %>% rbind(df.refallsolution)




