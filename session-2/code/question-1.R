#!/usr/bin/Rscript

#' This script contains the answers in R for question 1 of Problem Set 1. For
#' the interpretation of the results here, please look at the answer sheet.
#' 
#' Last update: 20/10/2022
#' By: @gabrielsgaspar


################################################################################
#################################### Setup #####################################
################################################################################

# Clean environment change working directory
rm(list=ls())
setwd("/Users/gabrielsgaspar/Library/CloudStorage/OneDrive-LondonBusinessSchool/Teaching/P218-Econometrics/session-2/code")
#setwd("C:\\Users\\gsimoesgaspar\\OneDrive - London Business School\\Teaching\\P218-Econometrics\\session-2\\code")

# Create resources folder if it does not exist already
if (file.exists("../resources")==FALSE){dir.create("../resources")}

# Define a vector of packages that the script will use
pkgs_required <- c("tidyverse")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already install then install it in machine
if (length(pkgs_install) > 0) {
  install.packages(pkgs_install, dependencies=TRUE)
}

# Import necessary packages
invisible(lapply(pkgs_required, library, character.only=TRUE))


################################################################################
################################### Part (b) ###################################
################################################################################

# Define function for ACE(x)
ACE  <- function(x) (5-6*x^2)/(3*(x+1))

# Create plot
ggplot() +
geom_density() +
xlim(0, 2) +
geom_function(fun = ACE, colour = "red") + 
labs(x="Rain",
     y="ACE") +
theme_bw() +                        # Use black and white theme
theme(text             = element_text(size=12),
      axis.line        = element_line(colour="black"),
      axis.text        = element_text(size=12),
      plot.title       = element_text(hjust=0.5),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      legend.title     = element_blank())

# Save plot
ggsave("../resources/question-1-b.png")

