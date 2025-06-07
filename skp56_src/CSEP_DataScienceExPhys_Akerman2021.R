#------------------------------------------------------#
# Meta-data -----------------------------
#------------------------------------------------------#

# Title: CSEP Presentation: Working From OSF and Batch Data Handling
# Author: Ashley Akerman
# Contact: ashleypaulakerman@gmail.com

#------------------------------------------------------#
# Clean environment -----------------------------
#------------------------------------------------------#

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list = ls())
# Remove scientific notation (helpful for p values!)
options(scipen = 999)

#------------------------------------------------------#
# Libraries -----------------------------
#------------------------------------------------------#

# Save a vector of required packages for this script 
packages_required <- c("tidyverse",
                       "readxl",
                       "devtools",
                       "performance",
                       "osfr",
                       "httr",
                       "signal")

## What packages are already installed?
# Return booleans of whether each index in the pacakges_required is already installed
installed_packages <- packages_required %in% rownames(installed.packages())

## If any are not installed, then install them

# If statement asks whether any of the installed_packages are FALSE (i.e., not included in the already installed list)
# If there are any equal to (==) FALSE, then it moves to the function specified in the curly brackets {}
# Then, it will run the 'install.packages' function only on the packages which are not (!) already installed
if(any(installed_packages == FALSE)){
  
  install.packages(packages_required[!installed_packages])
  
}

## Load all packages
# Lapply loops over the specificed packages_required and runs the function 'library' to load the required pacakges
# The invisible function just keeps the console clean for this action
invisible(lapply(packages_required, library, character.only = TRUE))

#------------------------------------------------------#
# Scope of this talk -----------------------------
#------------------------------------------------------#

# I hope to introduce you to three separate, yet related ways in which using R and open science practices can make your research life easier.
# This is based on problems I have regularly encountered in the past, and based on conversations with supervised students and collaborators, it highlights the benefits of computational approaches to research
# The talk will be somewhat limited, but hopefully these examples will get your creative juices going and you can see how the possibilities are endless (cliche!)

# All of the content will be available, and please feel free to contact me with any questions (see email in meta-data at the top)

# 1. Generate reproducible data and save it to OSF (Note: Using OSF rather than Github to avoid discussions regarding push, pull, and merge)
# 2. Iterate through multiple files, or multiple tabs in one excel/csv sheet, and generate a master tab, and summary statistics
# 3. Add summary statistics to a word document using rmarkdown to send to a conference (or full paper!)

#------------------------------------------------------#
# Working with the data -----------------------------
#------------------------------------------------------#

## Reading data from OSF -----------------------------

# For detailed documentation of the 'osfr' package, see: https://github.com/ropensci/osfr
# And: https://cran.r-project.org/web/packages/osfr/vignettes/getting_started.html
# For more information related to rOpenSci Project, see: http://ropensci.org

# A (more) simple method for downloading csv files from OSF is by using the httr packages
# See: https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
# See: https://github.com/r-lib/httr

# NOTE: The OSF link must be public to work with it via R and osfr package

# Open the data in the browser to check
# This is useful if you were reviewing a paper and wanted to immediately open and download a local copy of the data to your R environment
osf_project <- osfr::osf_retrieve_node("RS8KZ")

# Set workign directory, this is where you will save the files to
setwd("/Users/ashleyakerman/Dropbox/My Mac (Ashley’s MacBook Pro)/Documents/AcademicWork/OtherProjects/CSEP2021/")

# take the OSF link and download the file to the local environment (set working directory above)
# The list files argument needs to have the maximum number set to Inf (infinite) to retrun all files
osf_project %>%
  osfr::osf_ls_files(n_max = Inf) %>%
  osfr::osf_download()

## Reading from the local environment -------------------

# What are the csv files in the local environment (see working directory)
# These have just been saved from the OSF link above
csv_files <- list.files(pattern = "*.csv")

# Take the list of csv files from above, and read them all into R (purrr::map function)
# This will download 100 csv files into a list
# The reduce function will then bind them all together into a dataframe
large_df <- csv_files %>% 
  
  purrr::map(function(x) {
    
    read.csv(x)
    
  }) %>%
  
  purrr::reduce(cbind)

# Check the structure
str(large_df)

# Re structure this dataframe to turn it to long format (pivot_longer)
# Keep the X columns in, these are the rowvalues we can use to denote a sample

# The column names contain the participant (P) and the session number (S)
# It will display with each 1st sample of each session for a given participant, so need to reorder to get sorted by participant, session, then second sample
# The names_pattern uses regular expresisons to denote that the names will come from the specified area
# i.e., after the P, which could contain any values which occur before the underscore, and likewise after the S
large_df <- large_df %>%
  tidyr::pivot_longer(cols = !contains("X"),
                      names_to = c("participant", "session"),
                      names_pattern = c("P(.*)_S(.*)"),
                      values_to = "heart_rate")  %>%
  dplyr::rename(seconds = "X") %>%
  dplyr::arrange(participant, session, seconds) %>%
  dplyr::mutate(participant = as.factor(participant),
                session = as.factor(session))

# Check
head(large_df)
tail(large_df)

#------------------------------------------------------#
# Analysing the data -----------------------------
#------------------------------------------------------#

# Imagine we are only interested in the baseline and end
# We can take the mean or the median of the last X minutes
# If you remember from the data generation code, we have 5 minutes of rest, 30 minutes of exercise, and 5 minutes of recovery
# Since the data is saved in second intervals, we can subset out the first 300 samples (i.e., 5 mins * 60 seconds/min), and the last 300 samples.

# The length of the data (i.e., number of samples) should be equal to (5 * 60) + (30 * 60) + (5 * 60)
# First, check whether this is true
max(large_df$seconds) == (5 * 60) + (30 * 60) + (5 * 60)

# Now, subset the data beyond the first 5 minutes and removing the last 5 minutes
large_df <- large_df %>%
  dplyr::filter(seconds > 300 & seconds <= 2100)

# Check the structure
str(large_df)
nrow(large_df)

# Plot the data
# This is a large plot with each participant and their 10 sessions illustrated (i.e., it will take time to render)
large_df %>%
  ggplot2::ggplot(aes(x = seconds, y = heart_rate, colour = session)) +
  ggplot2::facet_wrap(~participant) +
  ggplot2::geom_point(na.rm = TRUE, alpha = 0.2) +
  ggplot2::theme_bw()

# We don't need the data at this sampling rate, so let's focus on what we think is most important
# It is the baseline and end of exercise, so let's take the mean of every 3 minutes (180 seconds) to get 10 new time points (but this could be anything)
# There are many many ways to do this, but one was is to use a grouping column (gl) to group together every 180 values
# This process will drop the seconds column, but we can make a new column for stages (i.e., 3 minute stages)
# There will be 10 stages per participant, per session, so this should change the length of the new dataframe to 1000 rows long
# i.e., 10 participants * 10 sessions * 10 stages
# Then we can remove everything but the 1st and 10th stage, and drop the other levels of the stage factor
# Should also re-level the session and participant factor so that it doesn't have the order at 1, 10, 2, 3....etc

large_df_sub <- large_df %>% 
  dplyr::group_by(participant, session, grp = as.integer(gl(n(), 180, n()))) %>%
  dplyr::summarise(across(heart_rate, mean), .groups = 'drop') %>%
  dplyr::rename(stage = "grp") %>%
  dplyr::mutate(stage = rep(1:10, times = length(levels(large_df$participant)) * length(levels(large_df$session))) %>%
                  as.factor()) %>%
  dplyr::filter(stage == 1 | stage == 10) %>%
  dplyr::mutate(session = factor(session, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))) %>%
  dplyr::mutate(participant = factor(participant, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))) %>%
  droplevels()

# Check
str(large_df_sub)

# Now visualise the subset data
large_df_sub %>%
  ggplot2::ggplot(aes(x = stage, y = heart_rate, colour = session)) +
  ggplot2::facet_wrap(~participant) +
  ggplot2::geom_point(na.rm = TRUE, alpha = 0.8) +
  ggplot2::geom_line(aes(group = session)) +
  ggplot2::theme_bw()
