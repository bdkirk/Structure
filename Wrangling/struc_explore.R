##Incorporating structural aspects of planted tree species to look for trends across dispersal mode, succession and life form of arriving seed species.

#Started on 25 Jan 18

##Load relevant libraries
library(ggplot2); library(plyr); library(dplyr)

##Set working directory
setwd("~/M.S. Thesis/Data/GitHubProjects/Structure/Data/Raw Data")

## Add in rawdata set which includes plot level structural data and abudance and diversity metrices.
#rawdata file just has abundance, richness, and species diversity along with plot
seeds <- read.csv("rawdata.csv", header = TRUE)

setwd("~/M.S. Thesis/Data/GitHubProjects/Structure/Data/Tidy Data")
#structure_tidy file is from struc_wrangling where average dbh and ht were found across years
structure <- read.csv("structure_tidy.csv", header= TRUE)

#This binds the two datasets based on plot a column that is shared by both.
structure2 <- left_join(seeds, structure, by="plot")

# Start regressions to look for trends

# 1) Abudance and leaf area index (lai), we are predicting that higher lai reduces abudance of seeds that come in.  Conversely, abundance could increase if dispersers are being attracted to come in.

ggplot(structure2, aes(lai, abundance, color=treatment))+
  geom_point(aes(shape=treatment)) +
  geom_smooth(method = "lm", fill=NA) +
  ylim(0, 40000)

# 2) Abundance and dbh_avg
ggplot(structure2, aes(dbh_avg, abundance, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0,40000)

# 3) Abundance and ht_avg
ggplot(structure2, aes(ht_avg, abundance, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0,40000)

# 4) Abudance and tree density (# of stems)
ggplot(structure2, aes(tdensity, abundance, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0,40000)

# 5) Abundance and atmass (aboveground tree mass)
ggplot(structure2, aes(atmass, abundance, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0,40000)

# 6) Richness and lai
ggplot(structure2, aes(lai, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)

# 7) Richness and dbh_avg
ggplot(structure2, aes(dbh_avg, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)

# 8) Richness and ht_avg
ggplot(structure2, aes(ht_avg, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)

# 9) Richness and tdensity
ggplot(structure2, aes(tdensity, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)

# 10) Richness and atmass
ggplot(structure2, aes(atmass, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)

# 11) Diversity and lai
ggplot(structure2, aes(lai, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)

# 12) Diversity and height
ggplot(structure2, aes(ht_avg, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)

# 13) Diversity and tdensity
ggplot(structure2, aes(tdensity, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)

#14) Diversity and atmass
ggplot(structure2, aes(atmass, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)

#15) Diversity and dbh_avg
ggplot(structure2, aes(dbh_avg, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)


##################################################
###Determine which variables are related to each other

#load libraries
library(ClustOfVar); library(stats)

#Change working directory
setwd("~/M.S. Thesis/Data/GitHubProjects/Structure/Data/Tidy data")

# load in tidy data file created from wrangling that includes average dbh and ht from 2013 and 2014.

#if not read in already use first line of code, if already ran previous code can just use the same file and rename it
#svar <- read.csv("structure_tidy.csv", header= TRUE)
svar <- structure 

#pull out subset of columns to use in clustering analysis (remove environmental data that specifies a plot)
svar_2 <- svar[,4:12]

#Then remove duplicate variables (ht and ht2 which were used to make averages)
svar_3 <- svar_2
svar_3$dbh <- NULL
svar_3$dbh2 <- NULL
svar_3$ht <- NULL
svar_3$ht2 <- NULL

#Now can use the clustering approach to first look at whether there are more than two clustering together or not.
scluster <- hclustvar(svar_3)
plot(scluster)

# use cor with just an environmental dataset, perhaps look at 0.7 and above. Can consider using JMP using graph builder. 
scorrelation <- cor(svar_3)

write.csv(scorrelation, "struc_cor.csv", row.names = FALSE)



###Notes on using clustering and correlation from meeting with Katie Rey 2/5/18
#using clustering will allow you to pick out if more than two variablels are related. Uses a dendrogram to show how variables are clustered. long lines mean big separation .  code is hclust(t(data)) then next line is plot(hclust).  This package clusters based on rows rather than columns. clust(var). long vertical lines are less similar.
#use hclustvar() function rather than the hclust function.