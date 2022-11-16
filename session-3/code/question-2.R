#!/usr/bin/Rscript

#' This script contains the answers in R for question 2 of Problem Set 2. For
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
pkgs_required <- c("haven", "stargazer", "tidyverse")
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
################################### Part (a) ###################################
################################################################################

# Import data
data <- read.table("../data/ps1.dat", header=TRUE)

# Rename variable
data <- data %>% rename(educ = ed0)

# Create new variables
data$lwage <- log(data$w0)
data$exper <- data$a0 - (data$educ + 6)

# Regress log wages on dummies
reg_a <- lm(lwage ~ factor(educ) + factor(exper), data=data)

# Use stargazer to output pretty table
stargazer(reg_a,
          type             = "text",
          title            = "Results",
          label            = "tab:question-5-a",
          dep.var.labels   = c("Log(wage)"),
          covariate.labels = c("Intercept", paste0("Education ", 3:20), paste0("Experience ", 1:25)),
          out              = "../resources/question-2-a.tex")


################################################################################
################################### Part (b) ###################################
################################################################################

# Regress log wages on education, experience and experience square
#reg_b <- lm(lwage ~ educ + poly(exper, 2, raw=TRUE), data=data)
reg_b <- lm(lwage ~ educ + exper, data=data)
# Get residual sum of squares for unrestricted model
RSS_U_b <- deviance(reg_a)
RSS_R_b <- deviance(reg_b)

# Set number of restrictions and other parameters
p <- 35         # Number of restrictions
n <- nrow(data) # Number of observations
k <- 39         # Number of variables

# Calculate F test
F_b <- ((RSS_R_b - RSS_U_b)/p) / (RSS_U_b/(n-k))


################################################################################
################################### Part (c) ###################################
################################################################################

# Perform unrestricted and restricted regressions
reg_c_U <- lm(w0 ~ factor(educ) + factor(exper), data=data)
#reg_c_R <- lm(w0 ~ educ + poly(exper, 2, raw=TRUE), data=data)
reg_c_R <- lm(w0 ~ educ + exper, data=data)

# Get residual sum of squares for unrestricted model
RSS_U_c <- deviance(reg_c_U)
RSS_R_c <- deviance(reg_c_R)

# Set number of restrictions and other parameters
p <- 35         # Number of restrictions
n <- nrow(data) # Number of observations
k <- 39         # Number of variables

## Calculate F test
F_c <- ((RSS_R_c - RSS_U_c)/p) / (RSS_U_c/(n-k))

