#!/usr/bin/Rscript

#' This script goes over some basics of R and regressions using the default lm()
#' function and the lm_robust() function from the estimatr package. For more
#' derivation of formulas used in this script, please check the presentation that
#' goes with it in TA session 1.
#' 
#' Last update: 07/10/2022
#' By: @gabrielsgaspar


################################################################################
#################################### Setup #####################################
################################################################################

# Clean environment change working directory
rm(list=ls())
setwd("/Users/gabrielsgaspar/Library/CloudStorage/OneDrive-LondonBusinessSchool/Teaching/")

# Define a vector of packages that the script will use
pkgs_required <- c("AER", "estimatr", "tidyverse", "vtable")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already install then install it in machine
if (length(pkgs_install) > 0) {
  install.packages(pkgs_install, dependencies=TRUE)
}

# Import necessary packages
invisible(lapply(pkgs_required, library, character.only=TRUE))


################################################################################
####################### Basic R Structures & Operations ########################
################################################################################

# Defining variables
var_a <- 10 
var_b <- "this"
var_c <- 3.14

# Print the type/class of the variable - similar to mode(), check it out
class(var_a)
class(var_b)
class(var_c)

# Basic operations
var_a + var_c # sum
var_a - var_c # subtraction
var_a^2       # square it
var_a**2      # also square it
10 %% 3       # modulus

# Other useful operations
exp(var_a)  # exponential
log(var_a)  # log
sqrt(9)     # square root

# Defining functions
square_function <- function(x) {
  
  #' This function squares the argument variable and returns the results.
  #' 
  #' Argument: x   - a numeric variable
  #' Output  : sqx - a numeric variable with the square of the argument x
  #' 
  
  # If variable is numeric then square it
  if (is.numeric(x)==TRUE) {
    sqx <- x^2
  # Otherwise raise error
  } else {
    stop("Looks like your argument is not a numeric variable!")
  }
  # Return result
  return (sqx)
  
}

# Can also define one-line functions
odds  <- function(x) x %% 2 > 0
evens <- function(x) x %% 2 == 0

# Vectors in R
vec_a <- c(1, 2, 3)
vec_b <- c(1, "two", 3)

# Create random vectors
vec_1 <- rpois(n=50, lambda=10)
vec_2 <- runif(n=50, min=0, max=1)
vec_3 <- sample(x=c(1, 3, 5), size = 50, replace = TRUE)
vec_4 <- rnorm(n=3, mean=10, sd=2)

# Create repeated vectors
vec_1s <- rep(1, 10)
vec_0s <- rep(0, 10)

# Help with functions
help(runif)
help(sample)

# Important to always set a seed (this should come at the start of your script!!!)
set.seed(9)

# Matrices using one vector
A <- matrix(c(10, 8, 5, 12)  , ncol = 2, byrow = TRUE)
B <- matrix(c(5, 3, 15, 6)   , ncol = 2, byrow = TRUE)

# Matrices with multiple vectors
x <- c(1:5)
y <- c(11:15)
z <- c(21:25)
m <- matrix(c(x, y, z), ncol = 3)

# Get dimensions
dim(A)
dim(B)

# Some operations
A - B    # element-wise subtraction
A * B    # element-wise multiplication
A %*% B  # matrix multiplication
t(A)     # transpose of a matrix
solve(A) # inverse of a matrix


################################################################################
############################ Load and explore data #############################
################################################################################

# We will use data from California school test scores
data(CASchools)

# Show five observations of the data frame
head(CASchools, 5)  # this command shows the top 5
tail(CASchools, 5)  # and this shows the bottom 5

# Create new variables
CASchools$st_ratio <- CASchools$students / CASchools$teachers
CASchools$score    <- (CASchools$read + CASchools$math)/2

# Can also select columns like this
col_df1 <- CASchools$st_ratio
col_df2 <- CASchools["st_ratio"]
col_vec <- CASchools[["st_ratio"]]

# Print mean and variance of a variable
mean(CASchools$score) # mean
var(CASchools$score)  # variance
sd(CASchools$score)   # standard deviation

# Square root and exponential operations
sqrt(var(CASchools$score)) == sd(CASchools$score)
var(CASchools$score)       == sd(CASchools$score)^2

# Generate a summary table (can also use st() command)
sumtable(CASchools, vars = c("students", "teachers", "calworks", "lunch"))

# Alternatively to see on console
summary(CASchools %>% select(students, teachers, calworks, lunch))

# Create data frames by filtering another using pipe
df2 <- CASchools %>%                            # use forward pipe operator
        select(district, st_ratio, score) %>%   # select the variables you want
          filter(st_ratio > 20) %>%             # insert your filters
            rename(ratio      = st_ratio,       # rename variables
                   test_score = score)

# Get unique variables in a column
unique(CASchools$grades)

# And summary of variable
summary(CASchools$grades)

# Create plot to visualize variables
CASchools %>% ggplot(aes(x=st_ratio, y=score)) +  # Choose x and y variables
              geom_point() +                      # Create data points
              geom_smooth(method=lm) +            # Regress y ~ x and add interval
              labs(x="Student-teacher ratio",
                   y="Test scores",
                   title="Lower student-teacher ratio leads to higher test scores.") +
              theme_bw() +                        # Use black and white theme
              theme(text             = element_text(size=12),
                    axis.line        = element_line(colour="black"),
                    axis.text        = element_text(size=12),
                    plot.title       = element_text(hjust=0.5),
                    panel.grid.minor = element_blank(),
                    panel.border     = element_blank(),
                    legend.title     = element_blank())

# Save plot
ggsave("./st_ratio-score.png")


################################################################################
########################### Univariate Regression ##############################
################################################################################

# Many ways to regress variables, simplest one is
reg_uni <- lm(score ~ st_ratio, data=CASchools)

# Show entire regression table
summary(reg_uni)

# List coefficient values
coeff   <- coefficients(reg_uni)

# What is the expected score for a district with st_ratio = 15 ?
print(coeff["(Intercept)"] + coeff["st_ratio"] * 15)

# Get residuals and fitted values
resids  <- resid(reg_uni)
fit_val <- fitted(reg_uni)

# Let's do some checks
length(resids) == dim(CASchools)[1]   # can also use nrow()
fit_val[1]     == coeff["(Intercept)"] + coeff["st_ratio"] * CASchools[1, "st_ratio"]

# Get some results
SSR     <- sum(resids^2)
TSS     <- sum((CASchools$score - mean(CASchools$score))^2)
ESS     <- sum((fit_val - mean(CASchools$score))^2)

# Bonferroni outlier test
outlierTest(reg_uni) # is the largest residual stat. diff. from others?

# QQ plots
qqPlot(reg_uni, main="QQ Plot")

# Leverage plots
leveragePlots(reg_uni)

# Can apply functions inside regression
lm(log(score) ~ log(st_ratio), data=CASchools) %>% summary()      # Logs
lm(exp(score) ~ exp(st_ratio), data=CASchools) %>% summary()      # Exponential
lm(scale(score) ~ scale(st_ratio), data=CASchools) %>% summary()  # Normalize


################################################################################
############################## Exercise ########################################
################################################################################

# Complete this function to compute the coefficients of the OLS regression
compute_OLS <- function(var_Y, vars_X, intercept=TRUE, data=CASchools) {
  
  #' This function calculates the coefficients of an OLS regression. It outputs
  #' a matrix with the intercept values and standard errors.
  #' Arguments: var_Y     - a string or vector with the name of the variable Y
  #'            vars_X    - a string or vector with the name of the variables X
  #'            intercept - a Boolean (default TRUE) assigning intercept
  #'            data      - a data.frame with the data to be used for regression
  #' Output   : output    - a matrix with the estimates and std.errors as columns
  
  # Get number of observations and variables
  n <- dim(data)[1]
  k <- length(vars_X)
  
  # Check if var_Y is a variable that is available as column of data
  if (var_Y %in% colnames(data)) {
    # Create matrix Y
    Y <- as.matrix(data %>% select(all_of(var_Y)))
  } else {
    # Raise error otherwise
    stop("Argument `var_Y` not a column of argument `data`.")
  }
  
  # Check if X variables are columns of data
  if (all(vars_X %in% colnames(data))) {
    # Check if intercept should be included
    if (intercept==TRUE) {
      # Create vector with column names for output
      rownames <- c("(Intercept)", vars_X)
      # Create column of one and update vars_X vector
      data$ones <- 1
      vars_X    <- c("ones", vars_X)
      # Create matrix X accounting for intercept
      X <- as.matrix(data %>% select(all_of(vars_X)))
    } else {
      # Create matrix X with no intercept
      X <- as.matrix(data %>% select(all_of(vars_X)))
    }
  } else {
    # Raise error otherwise
    stop("Argument `vars_X` not a column of argument `data`.")
  }
  
  # Get OLS estimator
  beta_OLS  <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  # Get residuals
  residuals <- Y - X %*% beta_OLS
  
  # Calculate estimated var-cov matrix and get standard errors
  var_cov <- ((t(residuals) %*% residuals)/(n-k))[1] * solve(t(X) %*% X)
  std_err <- sqrt(diag(var_cov))
  
  # Create matrix with estimates and std. errors
  output           <- matrix(c(beta_OLS, std_err), ncol=2)
  rownames(output) <- rownames
  colnames(output) <- c("Estimate", "Std. Error")
  
  # Return estimator
  return(output)
}

################################################################################
########################## Multivariate Regression #############################
################################################################################

# Similar to before, but let's use a different function
reg_multi  <- lm(score ~ st_ratio + income, data=CASchools)
summary(reg_multi)

# Same leverage plots in multivariate regressions
leveragePlots(reg_multi)

# We can also specify the standard errors with lm_robust()
reg_robust <- lm_robust(score ~ st_ratio + income, data=CASchools, se_type="HC3")
summary(reg_robust)

# For more details read up the lm_robust() and estimatr documentation but note that
hc0 <- lm_robust(score ~ st_ratio + income, data=CASchools, se_type="HC0")$std.error
hc1 <- lm_robust(score ~ st_ratio + income, data=CASchools, se_type="HC1")$std.error
hc2 <- lm_robust(score ~ st_ratio + income, data=CASchools, se_type="HC2")$std.error
hc3 <- lm_robust(score ~ st_ratio + income, data=CASchools, se_type="HC3")$std.error

# HC3 errors are the most conservative std error estimates
hc0 < hc1 # HC0 gives smaller errors than HC1
hc1 < hc2 # HC1 gives smaller errors than HC2
hc2 < hc3 # HC2 gives smaller errors than HC3

# We can can also cluster the errors
reg_cluster <- lm_robust(score ~ st_ratio + income, data=CASchools,
                         se_type="CR0", clusters=district)
summary(reg_cluster)

# The errors are different, go and read the documentation for details but notice that
cr0     <- lm_robust(score ~ st_ratio + income, data=CASchools,
                     se_type="CR0", clusters=district)$std.error
crstata <- lm_robust(score ~ st_ratio + income, data=CASchools,
                     se_type="stata", clusters=district)$std.error
cr2     <- lm_robust(score ~ st_ratio + income, data=CASchools,
                     se_type="CR2", clusters=district)$std.error

# CR2 errors are the most conservative std error estimates
cr0 < crstata # CR0 gives smaller errors than Stata
crstata < cr2 # Stata gives smaller errors than CR2


################################################################################
########################### Polynomial Regression ##############################
################################################################################

# We can also add polynomial terms to our regression (e.g. a second decree polynomial)
reg_pol1  <- lm(score ~ st_ratio + poly(income, 2, raw=TRUE), data=CASchools)
summary(reg_pol1)

# Alternatively, you can write
reg_pol2  <- lm(score ~ st_ratio + income + I(income^2), data=CASchools)
summary(reg_pol2)

# Note that the two above are NOT the same as
reg_pol3  <- lm(score ~ st_ratio + poly(income, 2), data=CASchools)
summary(reg_pol3) # Terms are orthogonalized, so results differ
