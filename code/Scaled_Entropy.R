### Loading GMM Models - SOFAS
gmm2_2_sofas <- readRDS("gmm2_2_sofas.rds")
gmm3_2_sofas <- readRDS("gmm3_2_sofas.rds")

# Get posterior probabilities
pp_gmm2_sofas <- gmm2_2_sofas$pprob
pp_gmm3_sofas <- gmm3_2_sofas$pprob

SE_sofas <- function(pprob) {
  # Get only the columns with posterior probs
  prob_cols <- grep("prob", names(pprob), value = TRUE)
  probs <- as.matrix(pprob[, prob_cols])
  
  n <- nrow(probs)
  k <- ncol(probs)
  
  # Avoid log(0) by adding a small constant
  entropy_raw <- -sum(probs * log(probs + 1e-10)) / (n * log(k))
  scaled_entropy <- 1 - entropy_raw
  
  return(round(scaled_entropy, 4))
}

entropy2_sofas <- SE_sofas(pp_gmm2_sofas)
entropy3_sofas <- SE_sofas(pp_gmm3_sofas)

cat("SOFAS Scaled Entropy (GMM2):", entropy2_sofas, "\n")
cat("SOFAS Scaled Entropy (GMM3):", entropy3_sofas, "\n")





### Loading GMM Models - CDSS
gmm2_2_cdss <- readRDS("gmm2_2_CDSS_c.rds")
gmm3_2_cdss <- readRDS("gmm3_2_CDSS_c.rds")

# Get posterior probabilities
pp_gmm2_cdss<- gmm2_2_sofas$pprob
pp_gmm3_cdss<- gmm3_2_sofas$pprob

SE_cdss <- function(pprob) {
  # Get only the columns with posterior probs
  prob_cols <- grep("prob", names(pprob), value = TRUE)
  probs <- as.matrix(pprob[, prob_cols])
  
  n <- nrow(probs)
  k <- ncol(probs)
  
  # Avoid log(0) by adding a small constant
  entropy_raw <- -sum(probs * log(probs + 1e-10)) / (n * log(k))
  scaled_entropy <- 1 - entropy_raw
  
  return(round(scaled_entropy, 4))
}

entropy2_cdss <- SE_cdss(pp_gmm2_cdss)
entropy3_cdss <- SE_cdss(pp_gmm3_cdss)

cat("CDSS Scaled Entropy (GMM2):", entropy2_cdss, "\n")
cat("CDSS Scaled Entropy (GMM3):", entropy3_cdss, "\n")





### Loading GMM Models - SANS
gmm2_2_sans <- readRDS("gmm2_2_sans_c.rds")
gmm3_2_sans <- readRDS("gmm3_2_sans_c.rds")

# Get posterior probabilities
pp_gmm2_sans<- gmm2_2_sans$pprob
pp_gmm3_sans<- gmm3_2_sans$pprob

SE_sans <- function(pprob) {
  # Get only the columns with posterior probs
  prob_cols <- grep("prob", names(pprob), value = TRUE)
  probs <- as.matrix(pprob[, prob_cols])
  
  n <- nrow(probs)
  k <- ncol(probs)
  
  # Avoid log(0) by adding a small constant
  entropy_raw <- -sum(probs * log(probs + 1e-10)) / (n * log(k))
  scaled_entropy <- 1 - entropy_raw
  
  return(round(scaled_entropy, 4))
}

entropy2_sans <- SE_cdss(pp_gmm2_sans)
entropy3_sans <- SE_cdss(pp_gmm3_sans)

cat("SANS Scaled Entropy (GMM2):", entropy2_sans, "\n")
cat("SANS Scaled Entropy (GMM3):", entropy3_sans, "\n")





### Loading GMM Models – LV1
gmm2_2_LV1 <- readRDS("gmm2_2_LV1_c.rds")
gmm3_2_LV1 <- readRDS("gmm3_2_LV1_c.rds")

# Get posterior probabilities
pp_gmm2_LV1<- gmm2_2_LV1$pprob
pp_gmm3_LV1<- gmm3_2_LV1$pprob

SE_LV1 <- function(pprob) {
  # Get only the columns with posterior probs
  prob_cols <- grep("prob", names(pprob), value = TRUE)
  probs <- as.matrix(pprob[, prob_cols])
  
  n <- nrow(probs)
  k <- ncol(probs)
  
  # Avoid log(0) by adding a small constant
  entropy_raw <- -sum(probs * log(probs + 1e-10)) / (n * log(k))
  scaled_entropy <- 1 - entropy_raw
  
  return(round(scaled_entropy, 4))
}

entropy2_LV1 <- SE_LV1(pp_gmm2_LV1)
entropy3_LV1 <- SE_LV1(pp_gmm3_LV1)

cat("LV1 Scaled Entropy (GMM2):", entropy2_LV1, "\n")
cat("LV1 Scaled Entropy (GMM3):", entropy3_LV1, "\n")