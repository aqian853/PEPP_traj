### Loading GMM Models
gmm2_2_cdss <- readRDS("gmm2_2_CDSS_c.rds")
gmm3_2_cdss <- readRDS("gmm3_2_CDSS_c.rds")
gmm2_2_sans <- readRDS("gmm2_2_sans_c.rds")
gmm3_2_sans <- readRDS("gmm3_2_sans_c.rds")
gmm2_2_sofas <- readRDS("gmm2_2_sofas.rds")
gmm3_2_sofas <- readRDS("gmm3_2_sofas.rds")
gmm2_2_LV1 <- readRDS("gmm2_2_LV1_c.rds")
gmm3_2_LV1 <- readRDS("gmm3_2_LV1_c.rds")


# Get posterior probabilities
pp_gmm2_sofas <- gmm2_2_sofas$pprob
pp_gmm3_sofas <- gmm3_2_sofas$pprob

pp_gmm2_sans<- gmm2_2_sans$pprob
pp_gmm3_sans<- gmm3_2_sans$pprob

pp_gmm2_cdss <- gmm2_2_cdss$pprob
pp_gmm3_cdss <- gmm3_2_cdss$pprob

pp_gmm2_LV1 <- gmm2_2_LV1$pprob
pp_gmm3_LV1 <- gmm3_2_LV1$pprob

# For 2-class model
app2_sofas <- sapply(1:2, function(k) {
  mean(pp_gmm2_sofas[pp_gmm2_sofas$class == k, paste0("prob", k)])
})

app2_sans <- sapply(1:2, function(k) {
  mean(pp_gmm2_sans[pp_gmm2_sans$class == k, paste0("prob", k)])
})

app2_cdss <- sapply(1:2, function(k) {
  mean(pp_gmm2_cds[pp_gmm2_cdss$class == k, paste0("prob", k)])
})

app2_LV1 <- sapply(1:2, function(k) {
  mean(pp_gmm2_LV1[pp_gmm2_LV1$class == k, paste0("prob", k)])
})

# For 3-class model
app3_sofas <- sapply(1:3, function(k) {
  mean(pp_gmm3_sofas[pp_gmm3_sofas$class == k, paste0("prob", k)])
})

app3_sans <- sapply(1:3, function(k) {
  mean(pp_gmm3_sans[pp_gmm3_sans$class == k, paste0("prob", k)])
})

app3_cdss <- sapply(1:3, function(k) {
  mean(pp_gmm3_cds[pp_gmm3_cdss$class == k, paste0("prob", k)])
})

app3_LV1 <- sapply(1:3, function(k) {
  mean(pp_gmm3_LV1[pp_gmm3_LV1$class == k, paste0("prob", k)])
})

# View APPs
cat("SOFAS Average Posterior Probabilities (2-class):\n")
print(app2_sofas)

cat("SANS Average Posterior Probabilities (2-class):\n")
print(app2_sans)

cat("CDSS Average Posterior Probabilities (2-class):\n")
print(app2_cdss)

cat("LV1 Average Posterior Probabilities (2-class):\n")
print(app2_LV1)

cat("SOFAS Average Posterior Probabilities (3-class):\n")
print(app3_sofas)

cat("SANS Average Posterior Probabilities (3-class):\n")
print(app3_sans)

cat("CDSS Average Posterior Probabilities (3-class):\n")
print(app3_cdss)

cat("LV1 Average Posterior Probabilities (3-class):\n")
print(app3_LV1)