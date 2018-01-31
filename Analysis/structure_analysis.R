# Started on 31 Jan 18
# will run through A) data visualization and then B) create model and do C) analysis

# set working directory
setwd("~/M.S. Thesis/Data/GitHubProjects/Structure/Data/Tidy Data")

# load libraries
library(ggplot2); library(car); library(lsmeans); library(stats); library(lme4); library(dplyr)

# import data
struc <- read.csv("structure_tidy.csv")

###Katie Rey Data Visualization####

#Find outliers
ggplot(struc, aes(treatment, dbh_avg))+
  geom_point()

ggplot(struc, aes(treatment, lai))+
  geom_point()
#more variance in pema and viko

ggplot(struc, aes(treatment, ht_avg))+
  geom_point()
#pema is much lower than ht of vogu and hial

ggplot(struc, aes(treatment, biomass))+
  geom_point()
#vogu seems to be very different from the rest

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

struc %>% 
  filter(!is.na(dbh_avg)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(block~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(struc, aes(block, dbh_avg))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

ggplot(struc, aes(block, ht_avg))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

ggplot(struc, aes(block, lai))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

ggplot(struc, aes(block, biomass))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

ggplot(struc, aes(block, density))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

## Check class of data
str(struc)
struc$block <- as.factor(struc$block)

### check data for normality ####

qqnorm(struc$dbh_avg)
qqline(struc$dbh_avg, col= 'red')
#assume normality for dbh_avg

qqnorm(struc$ht_avg)
qqline(struc$ht_avg, col = 'red')
#assume normality for ht_avg

qqnorm(struc$lai)
qqline(struc$lai, col= 'red')
#less normality but still close to line

qqnorm(struc$density)
qqline(struc$density, col = 'red')
#steep line

qqnorm(struc$biomass)
qqline(struc$biomass, col = 'red')
#normal data

dbh_fit <- aov(dbh_avg~ treatment + block, data = struc)
vif(dbh_fit)
summary(dbh_fit)
TukeyHSD(dbh_fit)
#treatment not significant

ht_fit <- aov(ht_avg~ treatment + block, data = struc)
vif(ht_fit)
summary(ht_fit)
TukeyHSD(ht_fit)
#treatment significant


lai_fit <- aov(lai~ treatment + block, data = struc)
vif(lai_fit)
summary(lai_fit)
#Treatment not significant

biomass_fit <- aov(biomass~ treatment + block, data = struc)
summary(biomass_fit)
#Not significant



density_fit <- aov(density~ treatment + block, data = struc)



ggplot(struc, aes(lai, dbh_avg))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(struc, aes(lai, density))+
  geom_point()+
  geom_smooth(method = "lm")
