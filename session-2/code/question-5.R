#!/usr/bin/Rscript

#' This script contains the answers in R for question 5 of Problem Set 1. For
#' the interpretation of the results here, please look at the answer sheet. This
#' script assumes that the data is stored at `../data/PS1.dta` and will crash if
#' the folder or file names do not conform.
#' 
#' Last update: 13/10/2022
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
pkgs_required <- c("haven", "stargazer")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already install then install it in machine
if (length(pkgs_install) > 0) {
  install.packages(pkgs_install, dependencies=TRUE)
}

# Import necessary packages
invisible(lapply(pkgs_required, library, character.only=TRUE))


################################################################################
################################### Part (a) ###################################
################################################################################

# Import data file
data <- read_dta("../data/PS1.dta")

# Create experience variable
data$exp <- data$a0 - (data$ed0 + 6)

# Regress log(wage) on constant, education, experience and experience squared
reg_a <- lm(log(w0) ~ ed0 + poly(exp, 2, raw=TRUE) , data=data)

# Use stargazer to output pretty table
stargazer(reg_a,
          type             = "text",
          title            = "Results",
          label            = "tab:question-5-a",
          dep.var.labels   = c("Log(wage)"),
          covariate.labels = c("Education", "Experience", "Experience\\textsuperscript{2}"),
          out              = "../resources/question-5-a.tex")


################################################################################
################################### Part (b) ###################################
################################################################################

# Get fitted values of regression
fit_vals <- predict(reg_a) # In this case should be the same as fitted()

# Regress log(wage) on constant, education, experience and fitted values
reg_b <- lm(log(w0) ~ ed0 + exp + fit_vals, data=data)

# Use stargazer to output pretty table
stargazer(reg_b,
          type             = "text",
          title            = "Results",
          label            = "tab:question-5-b",
          dep.var.labels   = c("Log(wage)"),
          covariate.labels = c("Education", "Experience", "Fitted Values"),
          out              = "../resources/question-5-b.tex")


################################################################################
################################### Part (c) ###################################
################################################################################

# Regress log(wage) and experience separately on constant education and experience squared
reg_po1   <- lm(log(w0) ~ ed0 + I(exp^2), data=data)
reg_po2   <- lm(exp ~ ed0 + I(exp^2), data=data)

# Get residuals of regression in (a) and regression above
resid_po1 <- resid(reg_po1)
resid_po2 <- resid(reg_po2)

# Regress the residual of (a) on residual of experience regression
reg_c     <- lm(resid_po1 ~ resid_po2)

# Use stargazer to output pretty table
stargazer(reg_c,
          type             = "text",
          title            = "Results",
          label            = "tab:question-5-c",
          dep.var.labels   = c("Residual of Log(wage) on Education and Experience\\textsuperscript{2}"),
          covariate.labels = c("Residual of Experience on Education and Experience\\textsuperscript{2}"),
          out              = "../resources/question-5-c.tex")


################################################################################
################################### Part (d) ###################################
################################################################################

# Regress log(wage) on partialled out experience
reg_d <- lm(log(w0) ~ resid_po2, data=data)

# Use stargazer to output pretty table
stargazer(reg_d,
          type             = "text",
          title            = "Results",
          dep.var.labels   = c("Log(wage)"),
          label            = "tab:question-5-d",
          covariate.labels = c("Residual of Experience on Education and Experience\\textsuperscript{2}"),
          out              = "../resources/question-5-d.tex")