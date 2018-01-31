# Data Wrangling for Structure data.  Need to combine data for 2013 and 2014 for DBH and Ht
# Started on 31 Jan 18

# Set working directory
setwd("~/M.S. Thesis/Data/GitHubProjects/Structure/Data/Raw Data")

# Load libraries
library(readxl); library(dplyr)

# Add in data
structure <- read_excel("ECOS Tree structure data_2013-2014.xlsx", sheet = 2, col_names = TRUE)

# look at class of data
str(structure)

#edit classes of data as needed
structure$plot <- as.factor(structure$plot)
structure$treatment <- as.factor(structure$treatment)
structure$block <- as.factor(structure$block)

# find average between ht and dbh across years and add this to dataset
structure2 <- mutate(structure, dbh_avg = ((dbh+dbh2)/2), ht_avg = ((ht+ht2)/2))

# change working directory to create a csv to use in analysis
setwd("~/M.S. Thesis/Data/GitHubProjects/Structure/Data/Tidy Data")

# save file
write.csv(structure2, "structure_tidy.csv", row.names = FALSE)
