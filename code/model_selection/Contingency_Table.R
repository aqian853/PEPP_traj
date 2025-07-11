library(dplyr)

### Loading GMM Models (18mo)
gmm2_2_cdss <- readRDS("gmm2_2_CDSS_c.rds")
gmm3_2_cdss <- readRDS("gmm3_2_CDSS_c.rds")
gmm2_2_sans <- readRDS("gmm2_2_sans_c.rds")
gmm3_2_sans <- readRDS("gmm3_2_sans_c.rds")
gmm2_2_sofas <- readRDS("gmm2_2_sofas.rds")
gmm3_2_sofas <- readRDS("gmm3_2_sofas.rds")
gmm2_2_LV1 <- readRDS("gmm2_2_LV1_c.rds")
gmm3_2_LV1 <- readRDS("gmm3_2_LV1_c.rds")

## Extracting pprob
div_cdss_2 <- gmm2_2_cdss[["pprob"]]
div_cdss_2 <- div_cdss_2 %>%
  rename(class_cdss_2 = class,
         prob1_cdss_2 = prob1,
         prob2_cdss_2 = prob2
  )
div_cdss_3 <- gmm3_2_cdss[["pprob"]]
div_cdss_3 <- div_cdss_3 %>%
  rename(class_cdss_3 = class,
         prob1_cdss_3 = prob1,
         prob2_cdss_3 = prob2,
         prob3_cdss_3 = prob3
  )

div_sans_2 <- gmm2_2_sans[["pprob"]]
div_sans_2 <- div_sans_2 %>%
  rename(class_sans_2 = class,
         prob1_sans_2 = prob1,
         prob2_sans_2 = prob2
  )
div_sans_3 <- gmm3_2_sans[["pprob"]]
div_sans_3 <- div_sans_3 %>%
  rename(class_sans_3 = class,
         prob1_sans_3 = prob1,
         prob2_sans_3 = prob2,
         prob3_sans_3 = prob3
  )

div_sofas_2 <- gmm2_2_sofas[["pprob"]]
div_sofas_2 <- div_sofas_2 %>%
  rename(class_sofas_2 = class,
         prob1_sofas_2 = prob1,
         prob2_sofas_2 = prob2,
  )
div_sofas_3 <- gmm3_2_sofas[["pprob"]]
div_sofas_3 <- div_sofas_3 %>%
  rename(class_sofas_3 = class,
         prob1_sofas_3 = prob1,
         prob2_sofas_3 = prob2,
         prob3_sofas_3 = prob3
  )

div_LV1_2 <- gmm2_2_LV1[["pprob"]]
div_LV1_2 <- div_LV1_2 %>%
  rename(class_LV1_2 = class,
         prob1_LV1_2 = prob1,
         prob2_LV1_2 = prob2,
  )
div_LV1_3 <- gmm3_2_LV1[["pprob"]]
div_LV1_3 <- div_LV1_3 %>%
  rename(class_LV1_3 = class,
         prob1_LV1_3 = prob1,
         prob2_LV1_3 = prob2,
         prob3_LV1_3 = prob3
  )

## Grouping by variables (2 and 3 class solutions)
div_cdss_joint <- full_join(div_cdss_2, div_cdss_3, by = "pin") 
div_sans_joint <- full_join(div_sans_2, div_sans_3, by = "pin") 
div_sofas_joint <- full_join(div_sofas_2, div_sofas_3, by = "pin") 
div_LV1_joint <- full_join(div_LV1_2, div_LV1_3, by = "pin") 

gmm_class_div_func <- full_join(div_cdss_joint, div_sans_joint, by = "pin") %>%
  full_join(div_sofas_joint, by = "pin") %>%
  full_join(div_LV1_joint, by = "pin")

gmm_class_div_func <- gmm_class_div_func %>% # Remove NAs
  filter(!(is.na(class_cdss_2) & is.na(class_sans_2) & is.na(class_sofas_2)))

## Creating a contingency table
## Adjust class and variable here
clust1 <- gmm_class_div_func$class_sans_2 ## Vertical
clust2 <- gmm_class_div_func$class_cdss_2 ## Horizontal

# Remove rows with NA in either clustering variable
valid_rows <- !is.na(clust1) & !is.na(clust2)
clust1 <- clust1[valid_rows]
clust2 <- clust2[valid_rows]

# Print table with absolute count
mat <- table(paste0("SANS_", clust1), paste0("CDSS_", clust2))
mat_tot <- addmargins(mat, margin = c(1, 2), FUN = sum)
print(mat_tot)
total_count <- sum(mat)

# Print table with percentage
percent_mat <- prop.table(mat) * 100
percent_mat_with_totals <- addmargins(percent_mat, margin = c(1, 2), FUN = sum)
percent_mat_with_totals_rounded <- round(percent_mat_with_totals, 2)
print(percent_mat_with_totals_rounded)
