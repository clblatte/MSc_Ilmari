#####
#
# Data for MSc Thesis Ilmari Kivineva
# Represents a sup-sample of data used in project MultiForest - region Central Finland
# 
# 2022-01-17
#
#####


# get working direction
path <- getwd()


library(dplyr)


# simulated data of whole FIN
simulated <- read.csv(unz("U:/2_ForestValue/rslt_inoutmetsaan_RCP0_V13.zip", "rslt_inoutmetsaan_RCP0_V13.csv"), header = TRUE, sep =";")

names(simulated)
summary(simulated)
unique(simulated$region)

# take only data for Central FIN
# indicated by column "region"; #13 represents Keski Suomi (https://stat.luke.fi/en/finnish-statistical-yearbook-forestry-2021-2021_en, page 10)
sample <- simulated %>% filter(region %in% 13)

# how many stands
length(unique(sample$id)) # 3579 stands
head(sample) 

# write data to csv file - used for optimization later
write.table(sample, paste0(path,"/scripts_cb/sample_central_fin.csv" ), sep = ";", row.names = F, col.names = TRUE)  



# additional data including for example XY coordinates of stands (if used for plotting)
simulated_xy <- read.csv(unz("U:/2_ForestValue/rslt_inoutmetsaan_RCP0_V13_XY.zip", "rslt_inoutmetsaan_RCP0_V13_XY.csv"), header = TRUE, sep =";")

head(simulated_xy) 

# take only data for Central FIN
sample_xy <- simulated_xy %>% filter(region %in% 13)

# how many stands
length(unique(sample_xy$id)) # 3579 stands

# write data to csv file - used for optimization later
write.table(sample_xy, paste0(path,"/scripts_cb/sample_central_fin_xy.csv" ), sep = ";", row.names = F, col.names = TRUE)  



