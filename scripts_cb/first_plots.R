####
#
# Plot some indicators of Central Finland
# 2022-01-17
#
####


library(dplyr)
library(ggplot2)


# get directory
path <- getwd()


# read data
df <- read.csv(paste0(path, "/scripts_cb/sample_central_fin.csv"), header = TRUE, sep= ";")

head(df)
summary(df)


# quick plot age of the first year
unique(df$year)
unique(df$regime)

df %>% filter(regime %in% "initial_state") %>% 
  ggplot(aes(x=AGE_ba)) + 
  geom_density()
