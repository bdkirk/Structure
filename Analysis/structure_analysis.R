# Started on 31 Jan 18
# will run through A) data visualization and then B) create model and do C) analysis

#After talking with Katie Rey, data visualization is a good check of the data before the model is fitted but you should look at residuals after the model is done to make sure it will work.

# set working directory
setwd("~/M.S. Thesis/Data/GitHubProjects/Structure/Data/Tidy Data")

# load libraries
library(ggplot2); library(car); library(lsmeans); library(stats); library(lme4); library(dplyr); library(tidyverse); library(devtools)

# import data
struc <- read.csv("structure_tidy.csv")

###Katie Rey Data Visualization####

#Find outliers

### dbh_avg
ggplot(struc, aes(treatment, dbh_avg))+
  geom_point(aes(color=block))
# one plot of PEMA has relatively smaller trees. 
ggplot(struc, aes(block, dbh_avg))+
  geom_point(aes(shape=treatment))
#block 4 appears to have greatest variance but block 2 also has large variance.

### LAI
ggplot(struc, aes(treatment, lai))+
  geom_point(aes(color=block))
#looking at across blocks as well. 
ggplot(struc, aes(block, lai))+
  geom_point(aes(shape=treatment))
#more variance in pema and viko

### height
ggplot(struc, aes(treatment, ht_avg))+
  geom_point(aes(color=block))
#pema is much lower than ht of vogu and hial. Viko has some variance.

ggplot(struc, aes(block, ht_avg))+
  geom_point(aes(shape=treatment))
#block 2 and block 4 vary in height

###t_biomass
ggplot(struc, aes(treatment, t_biomass))+
  geom_point(aes(color= block))
#vogu seems to be very different from the rest
ggplot(struc, aes(block, t_biomass))+
  geom_point(aes(shape=treatment))
#large variation in blocks 2 and 4

###all_biomass
ggplot(struc, aes(treatment, all_biomass))+
  geom_point(aes(color= block))
#vogu seems to be very different from the rest
ggplot(struc, aes(block, all_biomass))+
  geom_point(aes(shape=treatment))
#large variation in blocks 2 and 4


###density
ggplot(struc, aes(treatment, density))+
  geom_point(aes(color= block))
#hial has a very different density, VIKO has greatest variance

ggplot(struc, aes(block, density))+
  geom_point(aes(shape=treatment))
#variance high across all blocks with less variance beween block 2.

#find out if there is any missing data:
##Not as useful because we are missing data for VOGU 1 because this plot was struc by lighting.
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

ggplot(struc, aes(block, t_biomass))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

ggplot(struc, aes(block, all_biomass))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

ggplot(struc, aes(block, density))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

## Check class of data before analysis
str(struc)
struc$block <- as.factor(struc$block)

#add in package that stat dept is developing to look at residuals in a panel format.
#devtools::install_github("goodekat/ggResidpanel")

library(ggResidpanel)

#create models and subsequently check residuals.

### 1) dbh
# a) model
dbh_fit <- aov(dbh_avg~ treatment + block, data = struc)
vif(dbh_fit)
summary(dbh_fit)
TukeyHSD(dbh_fit)
#treatment not significant; more of a difference found between treatment than block

#b) residuals
dbh_resid <- resid_panel(resid(dbh_fit), fitted(dbh_fit), bins = 20)
dbh_resid

### 2) Height
#a) model
ht_fit <- aov(ht_avg~ treatment + block, data = struc)
vif(ht_fit)
summary(ht_fit)
TukeyHSD(ht_fit)
#treatment significant

#b) residuals
ht_resid <- resid_panel(resid(ht_fit), fitted(ht_fit), bins = 20)
ht_resid

### 3) LAI
#a) model
lai_fit <- aov(lai~ treatment + block, data = struc)
vif(lai_fit)
summary(lai_fit)
#Treatment not significant

#b) residuals
lai_resid <- resid_panel(resid(lai_fit), fitted(lai_fit), bins = 20)
lai_resid

### 4)Tree Biomass (see Russell et al. 2017 or 2010)
#a) model
t_biomass_fit <- aov(t_biomass~ treatment + block, data = struc)
summary(t_biomass_fit)
#Not significant

#b) residuals
t_biomass_resid <- resid_panel(resid(t_biomass_fit), fitted(t_biomass_fit), bins = 20)
t_biomass_resid

### 4)All Biomass (see Russell et al. 2010)
#a) model
all_biomass_fit <- aov(all_biomass~ treatment + block, data = struc)
summary(all_biomass_fit)
#Not significant

#b) residuals
all_biomass_resid <- resid_panel(resid(all_biomass_fit), fitted(all_biomass_fit), bins = 20)
all_biomass_resid


### 5)Density
#a) model
density_fit <- aov(density~ treatment + block, data = struc)
summary(density_fit)
#treatment significant, p = 0.012

#b)residuals
density_resid <- resid_panel(resid(density_fit), fitted(density_fit), bins = 20)
density_resid


