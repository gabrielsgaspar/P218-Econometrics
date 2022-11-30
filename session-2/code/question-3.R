#!/usr/bin/Rscript

#' This script contains the answers in R for question 3 of Problem Set 2. For
#' the interpretation of the results here, please look at the answer sheet.
#' 
#' Last update: 10/11/2022
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
pkgs_required <- c("haven", "latex2exp", "stargazer", "tidyverse")
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

# Set simulation parameters
sample_size <- 10^2
number_sims <- 10^4

# Create matrix to hold data
sims_data <- matrix(c(0, 0), nrow=number_sims, ncol=2)
  
# Use for loop for simulations
for (j in 1:number_sims) {
  
  # Simulate data
  z    <- rchisq(sample_size, df=3)
  ones <- rep(1, sample_size)
  
  X <- matrix(c(ones, z), ncol = 2)
  Y <- ones + 0.5*z + + runif(sample_size, min=-1, max=1)
  
  beta_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  sims_data[j,1] <- beta_OLS[1,1]
  sims_data[j,2] <- beta_OLS[2,1]
  
}
 
# Get average value for the OLS estimator
beta_hat_1 <- mean(sims_data[,1])
beta_hat_2 <- mean(sims_data[,2])


################################################################################
################################### Part (b) ###################################
################################################################################

# Define vector of sample sizes
beta_1 <- 1
beta_2 <- 0.5

# Iterate over multiples of 4
for (q in c(1, 4, 16, 64)) {
  
  # Create matrix to hold data
  sims_data <- matrix(c(0, 0), nrow=number_sims, ncol=2)
  
  # Use for loop for simulations
  for (j in 1:number_sims) {
    
    # Simulate data
    z    <- rchisq(sample_size*q, df=3)
    ones <- rep(1, sample_size*q)
    
    X <- matrix(c(ones, z), ncol = 2)
    Y <- ones + 0.5*z + + runif(sample_size*q, min=-1, max=1)
    
    beta_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y
    
    sims_data[j,1] <- beta_OLS[1,1]
    sims_data[j,2] <- beta_OLS[2,1]
    
  }
  
  # Rename variables
  colnames(sims_data) <- c("beta_OLS_1", "beta_OLS_2")
  
  # Plot histogram for beta 1
  as.data.frame(sims_data) %>% ggplot(aes(x=beta_OLS_1)) +
                               geom_histogram(bins=100, colour="blue") + 
                               geom_vline(aes(xintercept=beta_1),
                                          color="red",
                                          linetype="dashed",
                                          size=0.5) +
                               labs(x     = TeX("$\\hat{\\beta}_1"),
                                    y     = "Count",
                                    title = TeX(paste0("Histogram of $\\hat{\\beta}_1$ with $n = ",sample_size*q,"$"))) +
                               theme_bw() +                        # Use black and white theme
                               theme(text             = element_text(size=12),
                                     axis.line        = element_line(colour="black"),
                                     axis.text        = element_text(size=12),
                                     plot.title       = element_text(hjust=0.5),
                                     panel.grid.minor = element_blank(),
                                     panel.border     = element_blank(),
                                     legend.title     = element_blank())
  
  # Save plot
  ggsave(paste0("../resources/sim-beta1-", sample_size*q, ".png"))
  
  # Plot histogram for beta 2
  as.data.frame(sims_data) %>% ggplot(aes(x=beta_OLS_2)) +
                               geom_histogram(bins=100, colour="blue") + 
                               geom_vline(aes(xintercept=beta_2),
                                          color="red",
                                          linetype="dashed",
                                          size=0.5) +
                               labs(x     = TeX("$\\hat{\\beta}_2"),
                                    y     = "Count",
                                    title = TeX(paste0("Histogram of $\\hat{\\beta}_2$ with $n = ",sample_size*q,"$"))) +
                               theme_bw() +                        # Use black and white theme
                               theme(text             = element_text(size=12),
                                     axis.line        = element_line(colour="black"),
                                     axis.text        = element_text(size=12),
                                     plot.title       = element_text(hjust=0.5),
                                     panel.grid.minor = element_blank(),
                                     panel.border     = element_blank(),
                                     legend.title     = element_blank())
  
  # Save plot
  ggsave(paste0("../resources/sim-beta2-", sample_size*q, ".png"))
}


################################################################################
################################### Part (c) ###################################
################################################################################

# Sort estimator
beta_2_sort <- sort(sims_data[,2])
F_hat       <- as.data.frame(80*(beta_2_sort - beta_2)/10000)

# Rename column
colnames(F_hat) <- c("value")

F_hat %>% ggplot(aes(x=value)) + geom_line()

# Get normal distribution
prob <- as.data.frame(pnorm(seq(-1, 1, .001), mean=0, sd=sqrt(1/18)))

data.frame(x = c(-1, 1)) %>% ggplot(aes(x)) +
                             stat_function(fun  = pnorm,
                                           n    = 100,
                                           args = list(mean = 0,
                                                       sd   = sqrt(1/18))) +
                             labs(x     = TeX("$x$"),
                                  y     = TeX("$Pr(x \\leq X)$"),
                                  title = "Cumulative Distribution Functions") +
                             theme_bw() +                        # Use black and white theme
                             theme(text             = element_text(size=12),
                                   axis.line        = element_line(colour="black"),
                                   axis.text        = element_text(size=12),
                                   plot.title       = element_text(hjust=0.5),
                                   panel.grid.minor = element_blank(),
                                   panel.border     = element_blank(),
                                   legend.title     = element_blank())


################################################################################
################################### Part (d) ###################################
################################################################################

# Create matrix to hold sigma hat
sigma_hat <- matrix(0, nrow=number_sims, ncol=1)

# Use for loop for simulations
for (j in 1:number_sims) {
  
  # Simulate data
  z    <- rchisq(sample_size, df=3)
  ones <- rep(1, sample_size)
  
  X <- matrix(c(ones, z), ncol = 2)
  Y <- ones + 0.5*z + + runif(sample_size, min=-1, max=1)
  
  beta_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  # Get residuals and calculate sigma hat
  residuals    <- Y - X %*% beta_OLS
  RSS          <- t(residuals) %*% residuals
  sigma_hat[j] <- RSS / (sample_size - 2)
  
}

# Calculate mean sigma hat value
sgma_hat_mean <- mean(sigma_hat)
