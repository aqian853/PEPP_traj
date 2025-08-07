library(mclust)

### Loading GMM Models
gmm2_2_cdss <- readRDS("gmm2_2_CDSS_c.rds")
gmm3_2_cdss <- readRDS("gmm3_2_CDSS_c.rds")
gmm2_2_sans <- readRDS("gmm2_2_sans_c.rds")
gmm3_2_sans <- readRDS("gmm3_2_sans_c.rds")
gmm2_2_sofas <- readRDS("gmm2_2_sofas.rds")
gmm3_2_sofas <- readRDS("gmm3_2_sofas.rds")
gmm2_2_LV1 <- readRDS("gmm2_2_LV1_c.rds")
gmm3_2_LV1 <- readRDS("gmm3_2_LV1_c.rds")

### Loading data subsets and removing NAs
SOFAS <- read.csv("PEPP_sofas_long_clean.csv")
SOFAS_NA_removed <- as.data.frame(SOFAS)
SOFAS_NA_removed <- SOFAS_NA_removed[apply(SOFAS_NA_removed, 1, function(row) all(is.finite(row))), ]

LV1 <- read.csv("PEPP_cdss_LV1_long_clean.csv")
LV1_NA_removed <- as.data.frame(LV1)
LV1_NA_removed <- LV1_NA_removed[apply(LV1_NA_removed, 1, function(row) all(is.finite(row))), ]

CDSS <- read.csv("PEPP_cdss_tot_long_clean.csv")
CDSS_NA_removed <- as.data.frame(CDSS)
CDSS_NA_removed <- CDSS_NA_removed[apply(CDSS_NA_removed, 1, function(row) all(is.finite(row))), ]

SANS <- read.csv("PEPP_sans_long_clean.csv")
SANS_NA_removed <- as.data.frame(SANS)
SANS_NA_removed <- SANS_NA_removed[apply(SANS_NA_removed, 1, function(row) all(is.finite(row))), ]


# Step 1: Fit GMM with 2 classes (null) and 3 classes (alternative)
gmm2_2_sofas <- Mclust(SOFAS_NA_removed, G = 2)
gmm3_2_sofas <- Mclust(SOFAS_NA_removed, G = 3)

# Observed LRT statistic
LRT_obs <- 2 * (gmm3_2_sofas$loglik - gmm2_2_sofas$loglik)
cat("Observed LRT:", LRT_obs, "\n")

# Step 2: Parametric bootstrap
n_boot <- 500
LRT_boot <- numeric(n_boot)
set.seed(123)

for (i in 1:n_boot) {
  sim_data <- mclust::sim(modelName = gmm2_2_sofas$modelName,
                          parameters = gmm2_2_sofas$parameters,
                          n = nrow(SOFAS_NA_removed))
  
  # sim_data is a matrix with first column = group, next 3 columns = variables
  boot_data <- sim_data[, -1]  # remove first column (group labels)
  
  # Clean boot_data
  boot_data <- boot_data[complete.cases(boot_data), ]
  boot_data <- boot_data[apply(boot_data, 1, function(x) all(is.finite(x))), ]
  
  if (nrow(boot_data) < 5) {
    LRT_boot[i] <- NA
    next
  }
  
  boot_gmm2 <- Mclust(boot_data, G = 2, verbose = FALSE)
  boot_gmm3 <- Mclust(boot_data, G = 3, verbose = FALSE)
  
  LRT_boot[i] <- 2 * (boot_gmm3$loglik - boot_gmm2$loglik)
}

# Remove NAs and compute empirical p-value
LRT_boot <- LRT_boot[!is.na(LRT_boot)]
p_value <- mean(LRT_boot >= LRT_obs)
cat("Bootstrap p-value:", p_value, "\n")


######### CDSS
# Step 1: Fit GMM with 2 classes (null) and 3 classes (alternative)
gmm2_2_cdss <- Mclust(CDSS_NA_removed, G = 2)
gmm3_2_cdss <- Mclust(CDSS_NA_removed, G = 3)

# Observed LRT statistic
LRT_obs <- 2 * (gmm3_2_cdss$loglik - gmm2_2_cdss$loglik)
cat("Observed LRT:", LRT_obs, "\n")

# Step 2: Parametric bootstrap
n_boot <- 500
LRT_boot <- numeric(n_boot)
set.seed(123)

for (i in 1:n_boot) {
  sim_data <- mclust::sim(modelName = gmm2_2_cdss$modelName,
                          parameters = gmm2_2_cdss$parameters,
                          n = nrow(CDSS_NA_removed))
  
  # sim_data is a matrix with first column = group, next 3 columns = variables
  boot_data <- sim_data[, -1]  # remove first column (group labels)
  
  # Clean boot_data
  boot_data <- boot_data[complete.cases(boot_data), ]
  boot_data <- boot_data[apply(boot_data, 1, function(x) all(is.finite(x))), ]
  
  if (nrow(boot_data) < 5) {
    LRT_boot[i] <- NA
    next
  }
  
  boot_gmm2 <- Mclust(boot_data, G = 2, verbose = FALSE)
  boot_gmm3 <- Mclust(boot_data, G = 3, verbose = FALSE)
  
  LRT_boot[i] <- 2 * (boot_gmm3$loglik - boot_gmm2$loglik)
}

# Remove NAs and compute empirical p-value
LRT_boot <- LRT_boot[!is.na(LRT_boot)]
p_value <- mean(LRT_boot >= LRT_obs)
cat("Bootstrap p-value:", p_value, "\n")


#########  SANS
# Step 1: Fit GMM with 2 classes (null) and 3 classes (alternative)
gmm2_2_sans <- Mclust(SANS_NA_removed, G = 2)
gmm3_2_sans <- Mclust(SANS_NA_removed, G = 3)

# Observed LRT statistic
LRT_obs <- 2 * (gmm3_2_sans$loglik - gmm2_2_sans$loglik)
cat("Observed LRT:", LRT_obs, "\n")

# Step 2: Parametric bootstrap
n_boot <- 500
LRT_boot <- numeric(n_boot)
set.seed(123)

for (i in 1:n_boot) {
  sim_data <- mclust::sim(modelName = gmm2_2_sans$modelName,
                          parameters = gmm2_2_sans$parameters,
                          n = nrow(SANS_NA_removed))
  
  # sim_data is a matrix with first column = group, next 3 columns = variables
  boot_data <- sim_data[, -1]  # remove first column (group labels)
  
  # Clean boot_data
  boot_data <- boot_data[complete.cases(boot_data), ]
  boot_data <- boot_data[apply(boot_data, 1, function(x) all(is.finite(x))), ]
  
  if (nrow(boot_data) < 5) {
    LRT_boot[i] <- NA
    next
  }
  
  boot_gmm2 <- Mclust(boot_data, G = 2, verbose = FALSE)
  boot_gmm3 <- Mclust(boot_data, G = 3, verbose = FALSE)
  
  LRT_boot[i] <- 2 * (boot_gmm3$loglik - boot_gmm2$loglik)
}

# Remove NAs and compute empirical p-value
LRT_boot <- LRT_boot[!is.na(LRT_boot)]
p_value <- mean(LRT_boot >= LRT_obs)
cat("Bootstrap p-value:", p_value, "\n")


#########  LV1
# Step 1: Fit GMM with 2 classes (null) and 3 classes (alternative)
gmm2_2_LV1 <- Mclust(LV1_NA_removed, G = 2)
gmm3_2_LV1 <- Mclust(LV1_NA_removed, G = 3)

# Observed LRT statistic
LRT_obs <- 2 * (gmm3_2_LV1$loglik - gmm2_2_LV1$loglik)
cat("Observed LRT:", LRT_obs, "\n")

# Step 2: Parametric bootstrap
n_boot <- 500
LRT_boot <- numeric(n_boot)
set.seed(123)

for (i in 1:n_boot) {
  sim_data <- mclust::sim(modelName = gmm2_2_LV1$modelName,
                          parameters = gmm2_2_LV1$parameters,
                          n = nrow(LV1_NA_removed))
  
  # sim_data is a matrix with first column = group, next 3 columns = variables
  boot_data <- sim_data[, -1]  # remove first column (group labels)
  
  # Clean boot_data
  boot_data <- boot_data[complete.cases(boot_data), ]
  boot_data <- boot_data[apply(boot_data, 1, function(x) all(is.finite(x))), ]
  
  if (nrow(boot_data) < 5) {
    LRT_boot[i] <- NA
    next
  }
  
  boot_gmm2 <- Mclust(boot_data, G = 2, verbose = FALSE)
  boot_gmm3 <- Mclust(boot_data, G = 3, verbose = FALSE)
  
  LRT_boot[i] <- 2 * (boot_gmm3$loglik - boot_gmm2$loglik)
}

# Remove NAs and compute empirical p-value
LRT_boot <- LRT_boot[!is.na(LRT_boot)]
p_value <- mean(LRT_boot >= LRT_obs)
cat("Bootstrap p-value:", p_value, "\n")
