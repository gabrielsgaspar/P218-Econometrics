#!/usr/bin/Rscript

#' This script contains the answers in R for question 1 of Problem Set 2. For
#' the interpretation of the results here, please look at the answer sheet.
#' 
#' Last update: 09/11/2022
#' By: @gabrielsgaspar


################################################################################
#################################### Setup #####################################
################################################################################

# Clean environment change working directory
rm(list=ls())
setwd("/Users/gabrielsgaspar/Library/CloudStorage/OneDrive-LondonBusinessSchool/Teaching/P218-Econometrics/session-3/code")
#setwd("C:\\Users\\gsimoesgaspar\\OneDrive - London Business School\\Teaching\\P218-Econometrics\\session-3\\code")

# Create resources folder if it does not exist already
if (file.exists("../resources")==FALSE){dir.create("../resources")}

# Define a vector of packages that the script will use
pkgs_required <- c("haven", "readxl", "stargazer", "tidyverse")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already install then install it in machine
if (length(pkgs_install) > 0) {
  install.packages(pkgs_install, dependencies=TRUE)
}

# Import necessary packages
invisible(lapply(pkgs_required, library, character.only=TRUE))

# Set seed
set.seed(9)

################################################################################
################################### Part (d) ###################################
################################################################################

# Read data
data <- read_excel("../data/SP500Index.xlsx", col_names=c("date", "sp500"), skip=1)

# Create x_t = log(SP_t / SP_0) variable
data <- data %>% mutate(x = log(sp500/first(sp500)))

# Define time periods
TT <- nrow(data)-1

# Get MLE estimators
delta_MLE <- last(data$x) / TT
sigma_MLE <- sqrt( (1/TT) * sum((data$x - lag(data$x, 1) - delta_MLE)^2, na.rm=TRUE))
