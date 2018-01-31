##Incorporating structural aspects of planted tree species to look for trends across dispersal mode, succession and life form of arriving seed species.

#Started on 25 Jan 18

##Load relevant libraries
library(ggplot2); library(plyr)

##Set working directory
setwd("~/M.S. Thesis/Data/GitHubProjects/Structure/Data/Raw Data")

## Add in rawdata set which includes plot level structural data and abudance and diversity metrices.

structure <- read.csv("rawdata.csv", header = TRUE)


# Start regressions to look for trends

# 1) Abudance and leaf area index (lai), we are predicting that higher lai reduces abudance of seeds that come in.  Conversely, abundance could increase if dispersers are being attracted to come in.

ggplot(structure, aes(lai, abundance, color=treatment))+
  geom_point(aes(shape=treatment)) +
  geom_smooth(method = "lm", fill=NA) +
  ylim(0, 40000)


# 2) Abundance and dbh
ggplot(structure, aes(dbh, abundance, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0,40000)

# 3) Abundance and ht
ggplot(structure, aes(ht, abundance, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0,40000)


# 4) Abudance and tree density (# of stems)
ggplot(structure, aes(tdensity, abundance, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0,40000)

# 5) Abundance and atmass (aboveground tree mass)
ggplot(structure, aes(atmass, abundance, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0,40000)


# 6) Richness and lai
ggplot(structure, aes(lai, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)


# 7) Richness and dbh
ggplot(structure, aes(dbh, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)


# 8) Richness and ht
ggplot(structure, aes(ht, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)

# 9) Richness and tdensity
ggplot(structure, aes(tdensity, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)

# 10) Richness and atmass
ggplot(structure, aes(atmass, richness, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(20, 55)

# 11) Diversity and lai
ggplot(structure, aes(lai, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)

# 12) Diversity and height
ggplot(structure, aes(ht, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)

# 13) Diversity and tdensity
ggplot(structure, aes(tdensity, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)

#14) Diversity and atmass
ggplot(structure, aes(atmass, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)

#15) Diversity and dbh
ggplot(structure, aes(dbh, diversity, color=treatment))+
  geom_point(aes(shape=treatment))+
  geom_smooth(method = "lm", fill=NA)+
  ylim(0, 3.5)
