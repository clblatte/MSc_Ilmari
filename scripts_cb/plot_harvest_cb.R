####
#
# Economical analysis - timber harvest
# 2022-03-10
#
####


### load libraries
library(dplyr)
library(ggplot2)
library(tidyr)


head(df.solution_alldata)


plot <- df.solution_alldata %>%
  group_by(policy, scenario ,year) %>%
  
  mutate(areaWeighted_ind = solution_area_share * Harvested_V) %>%
  # divided by 1'000'000, since this are million values 
  summarise(total_harvest = sum(areaWeighted_ind)/1000000)  %>%
  
  ggplot(aes(year, total_harvest)) +
  geom_line( aes(color = policy, linetype = scenario)) + #
  geom_hline(yintercept=6.1, linetype="dotted", 
             color = "black", size=1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position="right",
        axis.title.x=element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey"),
        panel.grid = element_blank()) +
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_color_brewer(palette="Dark2") +
  scale_x_continuous(breaks=c(seq(2016,2116, by = 10) ))  +
  expand_limits(x = c(2016, 2116)) +
  ylab(expression(Mm^{3}~year^{-1})) +
  labs("total harvest")
plot



