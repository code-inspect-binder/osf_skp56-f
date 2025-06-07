#------------------------------------------------------#
# Meta-data -----------------------------
#------------------------------------------------------#

# Title: CSEP Presentation: Data Generation
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
# Generate the data to use  ----------------------
#------------------------------------------------------#

# Set reproducible seed
set.seed(20211009)

# Set up a dataframe for a hypothetical exercise training study
# This training study will have 10 training sessions for each individual participant, and 10 individual participants enrolled in the study
# For each training session, a polar HR file is saved, with the name of the participant_id and the session number (out of 10); e.g., P1_S1, P1_S2

# This is likely saved as 100 separate csv/excel files, or as 10 csv/excel files with 10 tabs in each
# To collate these by hand is a time wasting task for sure!
# Use R and computational methods to help and save time
# It will likely reduce transcription errors and retain all of the raw data without any need to touch it

# The hypothetical data will have: 
# Heart Rate for 30 minutes, sampled every 5 seconds

# Exercise duration in minutes
exercise_duration_mins <- 30
# Exercise duration in seconds
exercise_duration_secs <- 30  * 60

# Add resting (5 mins) and recovery (5 mins) HRs to simulate non-exercise data
exercise_duration_secs <- (5 * 60) + exercise_duration_secs + (5 * 60)

# 10 participants with different resting HR
resting_hrs <- rnorm(n = 10, mean = 60, sd = 10)

# 10 participants with different submax HR
submax_hrs <- rnorm(n = 10, mean = 150, sd = 15)

# List for separate outputs
output_list <- list()

# For every index in the resting_hrs and submax_hrs, loop over and generate a HR trajectory
# Then take a random sample and up-sample
# This will be the 1st session, then to simulate a reducing HR for a given stress, take off 5 beats/min plus/minus a random value
for(i in 1:length(resting_hrs)){
  
  # Set up an inter-mediary vector to store data in
  heart_rate_data <- c()
  
  # Each participant will have 10 sessions, so we also want to generate 10 heart rate traces
  for(j in c(1:10)){
    
    # Generate up and down sequence of HR
    # This sequence just defines a vector of values that go from one value to another, and then another to another, etc etc
    # It will be used to sample from, so if you take a random value with the mean defined in this range of oscillating values, then you will have a random oscillating pattern
    # The general trajectory will follow the values defined here
    # Because of the random sampling, there will also be erroneous data at the start and end of the trace, this is kept in to simulate real-life data
    heart_rate_traj <- c(resting_hrs[i]: c(resting_hrs[i] + sample(2:8, 1, replace = FALSE)),
                         resting_hrs[i]: c(resting_hrs[i] + sample(5:15, 1, replace = FALSE)),
                         resting_hrs[i]: c(submax_hrs[i] - sample(5:15, 1, replace = FALSE)),
                         c(submax_hrs[i] - sample(5:15, 1, replace = FALSE)): 
                           c((submax_hrs[i] - 10) - 10),
                         c((submax_hrs[i] - sample(5:15, 1, replace = FALSE)) - sample(5:15, 1, replace = FALSE)): 
                           c(submax_hrs[i] - sample(5:15, 1, replace = FALSE)),
                         c(submax_hrs[i] - sample(5:15, 1, replace = FALSE)): submax_hrs[i],
                         submax_hrs[i]: c(submax_hrs[i] - sample(2:8, 1, replace = FALSE)),
                         submax_hrs[i]: c(submax_hrs[i] - sample(2:8, 1, replace = FALSE)),
                         submax_hrs[i]: c(submax_hrs[i] - sample(10:20, 1, replace = FALSE)),
                         c(submax_hrs[i] - sample(10:30, 1, replace = FALSE)): 
                           c(submax_hrs[i] - sample(30:50, 1, replace = FALSE)))
    
    # Draw samples setting heart_rate_traj above as mean with appropriate constant variance (10 beats/min)
    heart_rate <- rnorm(n = length(heart_rate_traj), 
                        mean = heart_rate_traj, 
                        sd = 10)
    
    # Use signal package to resample at a factor equal to the length of the exercise bout divided by the length of the current data (should be 120 to make it simple rescale)
    heart_rate_secs <- signal::resample(heart_rate, p = exercise_duration_secs/length(heart_rate))
    
    # For each session we want to reduce the heart rate slightly to show a fitness effect
    # So if the session is 1 (i.e., j == 1) then we keep the data as simulated
    # If not, we reduce it by a random value equal to the session number, with a SD of 2
    if(j == 1){
      
      # Bind together the data that already exists, with the data which has just been generated
      heart_rate_data <- cbind(heart_rate_data, 
                               heart_rate_secs)
      
    } else {
      
      # Each trial, the HR will reduce (on average) 
      heart_rate_secs <- heart_rate_secs + rnorm(n = length(heart_rate_secs),
                                                 mean = -j,
                                                 sd = 2)
      
      # Bind together the data that already exists, with the data which has just been generated
      heart_rate_data <- cbind(heart_rate_data, 
                               heart_rate_secs)
      
    }
    
  }

  # Save the  output to the list generated outside the loop
  # Therefore each index will denote a separate participant and there should be 10 columns indicating the sessions
  output_list[[i]] <- as.data.frame(heart_rate_data)
  
}

# Rename the columns in each of the indices of the list
# Iterate through the major and minor indices of the list
for(i in seq_along(1:10)){
  
  for(j in seq_along(1:10)){
    
    colnames(output_list[[i]])[j] <- stringr::str_c("P", i, "_S", j)
    
  }
  
}

# Check the output of the list
str(output_list)

#------------------------------------------------------#
# Writing to local environment ----------------------
#------------------------------------------------------#

# First, set a working directory
setwd("/Users/ashleyakerman/Dropbox/My Mac (Ashley’s MacBook Pro)/Documents/AcademicWork/OtherProjects/CSEP2021")

## Notes:
# Now have a list of hypothetical data
# For the purposes of this talk, we will go through the programmatic approach to collating multiple files
# For this, we will go through all 100 files to show the scale of the approach (all as single csv's as if downloaded from polar)

# Upload each of the single csv files representing each column in the data list
# Use map (purrr) to loop over the indices of the list and produce a csv which is saved to the working directory
# Then use a similar function to save them all to the project link

purrr::map(output_list, function(x) {
  
  # Loop over each index in the list (i.e., the 'x')
  # Then name the csv with the appropriate column name
  for(i in 1:length(x)){
    
    write.csv(x[i], file = stringr::str_c(colnames(x)[i], ".csv"))
    
  }
  
})

#------------------------------------------------------#
# Writing the data to OSF -----------------------------
#------------------------------------------------------#

# Need to setup an OSF personal access token (PAT) to  manage projects or upload files with 'osfr'.
# See: https://docs.ropensci.org/osfr/articles/auth

# After generating the PAT on the Settings tab of OSF website, insert the token name here
osfr::osf_auth("kjWBbaCRyNtrSzUdnVzL3wl6mQM93lm9yDCWyFBxHJkb4zLWDL85ANclk2Luibdmc2ms0O")

# Create project for CSEP2021 conference
# Add a component for the data, and a sub directory for the multi-file data
# Then upload each file to the directory and open in browser
project_link <- osfr::osf_create_project(title = "CSEP 2021: Data Science for the Modern Exercise Physiologist")

# What are the csv files in the local directory?
# This should be all of the generated files from above, so make sure the working directory is correct
# See: getwd() and setwd()
csv_files <- list.files(pattern = "*.csv")

# Now use map function to iterate over each csv file and save to OSF project generated in code above
csv_files %>% 
  purrr::map(function(x) {
    
    # Takes the project link generated and uploads each index of the list of csv files
    project_link %>%
      osfr::osf_upload(x)
    
  })

# Check it worked
osfr::osf_open(project_link)

## NOTE: At this point, when the link opens it will be private, but it needs to be changed to public to access via osfr package or httr package
## NOTE: At this point, it is also worth setting the DOI for the project

#------------------------------------------------------#

## END
