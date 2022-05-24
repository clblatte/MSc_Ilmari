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
simulated <- read.csv(unz("U:/2_ForestValue/rslt_inoutmetsaan_RCP45_V13.zip", "rslt_inoutmetsaan_RCP45_V13.csv"), header = TRUE, sep =";")

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
write.table(sample, paste0(path,"/scripts_cb/sample_central_fin_rcp45.csv" ), sep = ";", row.names = F, col.names = TRUE)  



# additional data including for example XY coordinates of stands (if used for plotting)
simulated_xy <- read.csv(unz("U:/2_ForestValue/rslt_inoutmetsaan_RCP45_V13_XY.zip", "rslt_inoutmetsaan_RCP45_V13_XY.csv"), header = TRUE, sep =";")

head(simulated_xy) 

# take only data for Central FIN
sample_xy <- simulated_xy %>% filter(region %in% 13)

# how many stands
length(unique(sample_xy$id)) # 3579 stands

# write data to csv file - used for optimization later
write.table(sample_xy, paste0(path,"/scripts_cb/sample_central_fin_rcp45_xy.csv" ), sep = ";", row.names = F, col.names = TRUE)  





################################################


test_xy <- read.csv(paste0(path, "/scripts_cb/sample_central_fin_rcp0_xy.csv"), sep = ";"  ,header = TRUE, stringsAsFactors = TRUE)
names(test_xy)


test <- read.csv(paste0(path, "/scripts_cb/sample_central_fin_rcp0.csv"), sep = ";"  ,header = TRUE, stringsAsFactors = TRUE)
names(test)

testt <- test %>% select(id, year, regime, MAIN_SP, AGE_ba, protection) %>% 
  filter(AGE_ba >= 90) %>% 
  filter(regime %in% "SA") %>% 
  filter(year %in% 2021) %>% 
  filter(protection != "strict")

length(unique(testt$id)) / length(unique(test$id)) * 100

unique(test$regime)


length(unique(test$id))

regimes <- test[, c("regime","id")] %>% 
  # get the number off stands per region
  dplyr::distinct() %>% 
  group_by(regime) %>% 
  count(regime)  



#####################################################


# 5.5 % SA and 4.5% objective with old forest

test1 <- read.csv(paste0(path, "/scripts_cb/solution_EUFS_rcp0_test1.csv"), sep = ","  ,header = TRUE, stringsAsFactors = TRUE)
head(test1)
test1 <- test1 %>% rename(no = X,
                          id = X0,
                          regime = X1,
                          share = X2) %>% 
  filter(regime %in% "SA")
  

# 10% SA and objective to max Age in SA, 20% protection + objective to max deadwood
test2 <- read.csv(paste0(path, "/scripts_cb/solution_EUFS_rcp0_test2.csv"), sep = ","  ,header = TRUE, stringsAsFactors = TRUE)
test2 <- test2 %>% rename(no = X,
                          id = X0,
                          regime = X1,
                          share = X2) %>% 
  filter(regime %in% "SA")

joint <- test1 %>% left_join(test2, by =("id")) %>% 
  filter(regime.y %in% NA)

nrow(joint[joint$regime.y %in% NA,])

