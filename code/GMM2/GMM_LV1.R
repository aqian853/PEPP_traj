library(tidyverse)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(nFactors)
library(lcmm)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
install.packages("ggalluvial")
library(ggalluvial)
library(reshape2)

PEPP_2024_12_12_IDmatched <- read_sav(
  "PEPP_2024-12-12-IDmatched.sav")

#Total CDSS Scores
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_total_baseline = CDS_0, #total cds score at baseline
    cdss_total_1mo = CDS_1, #total cds score at 1-mo follow up
    cdss_total_2mo = CDS_2, #total cds score at 2-mo follow up
    cdss_total_3mo = CDS_3, #total cds score at 3-mo follow up
    cdss_total_6mo = CDS_6, #total cds score at 6-mo follow up
    cdss_total_9mo = CDS_9, #total cds score at 9-mo follow up
    cdss_total_12mo = CDS_12, #total cds score at 12-mo follow up
    cdss_total_18mo = CDS_18, #total cds score at 18-mo follow up
    cdss_total_24mo = CDS_24 #total cds score at 24-mo follow up
  )

#Individual item scores for baseline
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_baseline = cd1_0,
    cdss_2_baseline = cd2_0,
    cdss_3_baseline = cd3_0,
    cdss_4_baseline = cd4_0,
    cdss_5_baseline = cd5_0,
    cdss_6_baseline = cd6_0,
    cdss_7_baseline = cd7_0,
    cdss_8_baseline = cd8_0,
    cdss_9_baseline = cd9_0
  )

#Individual item scores for month 1
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_1mo = cd1_1,
    cdss_2_1mo = cd2_1,
    cdss_3_1mo = cd3_1,
    cdss_4_1mo = cd4_1,
    cdss_5_1mo = cd5_1,
    cdss_6_1mo = cd6_1,
    cdss_7_1mo = cd7_1,
    cdss_8_1mo = cd8_1,
    cdss_9_1mo = cd9_1
  )

#Individual item scores for month 2
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_2mo = cd1_2,
    cdss_2_2mo = cd2_2,
    cdss_3_2mo = cd3_2,
    cdss_4_2mo = cd4_2,
    cdss_5_2mo = cd5_2,
    cdss_6_2mo = cd6_2,
    cdss_7_2mo = cd7_2,
    cdss_8_2mo = cd8_2,
    cdss_9_2mo = cd9_2
  )

#Individual item scores for month 3
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_3mo = cd1_3,
    cdss_2_3mo = cd2_3,
    cdss_3_3mo = cd3_3,
    cdss_4_3mo = cd4_3,
    cdss_5_3mo = cd5_3,
    cdss_6_3mo = cd6_3,
    cdss_7_3mo = cd7_3,
    cdss_8_3mo = cd8_3,
    cdss_9_3mo = cd9_3
  )

#Individual item scores for month 6
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_6mo = cd1_6,
    cdss_2_6mo = cd2_6,
    cdss_3_6mo = cd3_6,
    cdss_4_6mo = cd4_6,
    cdss_5_6mo = cd5_6,
    cdss_6_6mo = cd6_6,
    cdss_7_6mo = cd7_6,
    cdss_8_6mo = cd8_6,
    cdss_9_6mo = cd9_6
  )

#Individual item scores for month 9
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_9mo = cd1_9,
    cdss_2_9mo = cd2_9,
    cdss_3_9mo = cd3_9,
    cdss_4_9mo = cd4_9,
    cdss_5_9mo = cd5_9,
    cdss_6_9mo = cd6_9,
    cdss_7_9mo = cd7_9,
    cdss_8_9mo = cd8_9,
    cdss_9_9mo = cd9_9
  )

#Individual item scores for month 12
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_12mo = cd1_12,
    cdss_2_12mo = cd2_12,
    cdss_3_12mo = cd3_12,
    cdss_4_12mo = cd4_12,
    cdss_5_12mo = cd5_12,
    cdss_6_12mo = cd6_12,
    cdss_7_12mo = cd7_12,
    cdss_8_12mo = cd8_12,
    cdss_9_12mo = cd9_12
  )

#Individual item scores for month 18
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_18mo = cd1_18,
    cdss_2_18mo = cd2_18,
    cdss_3_18mo = cd3_18,
    cdss_4_18mo = cd4_18,
    cdss_5_18mo = cd5_18,
    cdss_6_18mo = cd6_18,
    cdss_7_18mo = cd7_18,
    cdss_8_18mo = cd8_18,
    cdss_9_18mo = cd9_18
  )

#Individual item scores for month 24
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    cdss_1_24mo = cd1_24,
    cdss_2_24mo = cd2_24,
    cdss_3_24mo = cd3_24,
    cdss_4_24mo = cd4_24,
    cdss_5_24mo = cd5_24,
    cdss_6_24mo = cd6_24,
    cdss_7_24mo = cd7_24,
    cdss_8_24mo = cd8_24,
    cdss_9_24mo = cd9_24
  )

## Get non-na counts
# recode 77 as NA
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  mutate(across(starts_with("cdss"), ~ replace(., . == 77, NA)))

## Subset data for relevant clinical scores and non-NAs at baseline
PEPP_cdss_subset <- PEPP_2024_12_12_IDmatched %>%
  select( contains("pin"), contains("FEP_ID"),contains("Scan_ID"), contains("cdss_"))

colnames(PEPP_cdss_subset)

PEPP_cdss_subset <- PEPP_cdss_subset %>%
  filter(!is.na(cdss_total_baseline))

## Subsetting CDSS scores into LV1
PEPP_cdss_subset <- PEPP_cdss_subset %>%
  mutate(
    gen_dep_comp_baseline = cdss_1_baseline + cdss_2_baseline + cdss_3_baseline + cdss_6_baseline + cdss_8_baseline,
    gen_dep_comp_1mo = cdss_1_1mo + cdss_2_1mo + cdss_3_1mo + cdss_6_1mo + cdss_8_1mo,
    gen_dep_comp_2mo = cdss_1_2mo + cdss_2_2mo + cdss_3_2mo + cdss_6_2mo + cdss_8_2mo,
    gen_dep_comp_3mo = cdss_1_3mo + cdss_2_3mo + cdss_3_3mo + cdss_6_3mo + cdss_8_3mo,
    gen_dep_comp_6mo = cdss_1_6mo + cdss_2_6mo + cdss_3_6mo + cdss_6_6mo + cdss_8_6mo,
    gen_dep_comp_9mo = cdss_1_9mo + cdss_2_9mo + cdss_3_9mo + cdss_6_9mo + cdss_8_9mo,
    gen_dep_comp_12mo = cdss_1_12mo + cdss_2_12mo + cdss_3_12mo + cdss_6_12mo + cdss_8_12mo,
    gen_dep_comp_18mo = cdss_1_18mo + cdss_2_18mo + cdss_3_18mo + cdss_6_18mo + cdss_8_18mo,
    gen_dep_comp_24mo = cdss_1_24mo + cdss_2_24mo + cdss_3_24mo + cdss_6_24mo + cdss_8_24mo
  )

## Data visualization for LV1
PEPP_cdss_subset %>%
  select(pin, gen_dep_comp_baseline, gen_dep_comp_1mo, gen_dep_comp_2mo, 
         gen_dep_comp_3mo, gen_dep_comp_6mo, gen_dep_comp_9mo, 
         gen_dep_comp_12mo, gen_dep_comp_18mo, gen_dep_comp_24mo) %>%
  pivot_longer(cols = starts_with("gen_dep_comp_"), names_to = "time_point", values_to = "LV1_score") %>%
  mutate(pin = as.factor(pin),
         time_point = recode(time_point,
                             "gen_dep_comp_baseline" = "0",
                             "gen_dep_comp_1mo" = "1",
                             "gen_dep_comp_2mo" = "2",
                             "gen_dep_comp_3mo" = "3",
                             "gen_dep_comp_6mo" = "6",
                             "gen_dep_comp_9mo" = "9",
                             "gen_dep_comp_12mo" = "12",
                             "gen_dep_comp_18mo" = "18",
                             "gen_dep_comp_24mo" = "24"),
         time_point = as.numeric(time_point)) %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = pin)) +
  geom_line(alpha = 0.3) + 
  geom_point(size = 2, alpha = 0.5) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 6, 9, 12, 18, 24)) +
  labs(title = "CDSS General Depressive Composite Score Over Time", 
       x = "Time Point (Months)", 
       y = "CDSS General Depressive Composite Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(color = "none") +
  theme_classic()

############ Converting to Long data 
## Clean out those with less than 2 time points [clean]
## Filter out 1, 2, 24mo follow ups [filter]

## LV1
PEPP_cdss_LV1_long <- 
  PEPP_cdss_subset %>%
  select(pin, gen_dep_comp_baseline, gen_dep_comp_1mo, gen_dep_comp_2mo, gen_dep_comp_3mo,
         gen_dep_comp_6mo, gen_dep_comp_9mo, gen_dep_comp_12mo, gen_dep_comp_18mo, gen_dep_comp_24mo) %>%
  pivot_longer(cols = starts_with("gen_dep_comp_"), names_to = "time_point", values_to = "LV1_score")

PEPP_cdss_LV1_long <- PEPP_cdss_LV1_long %>%
  mutate(pin = as.numeric(pin),
         time_point = recode(time_point,
                             "gen_dep_comp_baseline" = "0",
                             "gen_dep_comp_1mo" = "1",
                             "gen_dep_comp_2mo" = "2",
                             "gen_dep_comp_3mo" = "3",
                             "gen_dep_comp_6mo" = "6",
                             "gen_dep_comp_9mo" = "9",
                             "gen_dep_comp_12mo" = "12",
                             "gen_dep_comp_18mo" = "18",
                             "gen_dep_comp_24mo" = "24"),
         time_point = as.numeric(time_point))

PEPP_cdss_LV1_long_clean <- PEPP_cdss_LV1_long %>%
  group_by(pin) %>%
  filter(sum(!is.na(LV1_score)) >= 2) %>%
  ungroup()

PEPP_cdss_LV1_long_filtered <- PEPP_cdss_LV1_long %>%
  filter(!time_point %in% c(1, 2, 24)) %>%
  group_by(pin) %>%
  filter(sum(!is.na(LV1_score)) >= 2) %>%
  ungroup()

PEPP_cdss_LV1_long_clean %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = pin)) +
  geom_line(alpha = 0.3) + 
  geom_point(size = 2, alpha = 0.5) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 6, 9, 12, 18, 24)) +
  labs(title = "CDSS General Depressive Composite Score Over Time", 
       x = "Time Point (Months)", 
       y = "CDSS General Depressive Composite Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(color = "none") +
  theme_classic()

PEPP_cdss_LV1_long_filtered %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = pin)) +
  geom_line(alpha = 0.3) + 
  geom_point(size = 2, alpha = 0.5) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 6, 9, 12, 18, 24)) +
  labs(title = "CDSS General Depressive Composite Score Over Time", 
       x = "Time Point (Months)", 
       y = "CDSS General Depressive Composite Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(color = "none") +
  theme_classic()


### TRAJECTORY MODELING ------ start here (This file is only for LV1 clean and filtered)
#set seed for reproducibility
set.seed(123)

#run models with 1-5 classes, each with 200 random starts using the 1-class model to set initial start values
### LV1_Clean
gmm1_LV1_c <- lcmm::hlme(LV1_score ~ time_point, subject = "pin", random =~1, ng = 1, 
                   data = PEPP_cdss_LV1_long_clean)
gmm2_LV1_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_LV1_c, 
                   m = hlme(LV1_score ~ time_point, subject = "pin", random=~1,
                            ng = 2, data = PEPP_cdss_LV1_long_clean, mixture =~time_point, nwg = T))
gmm3_LV1_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_LV1_c, 
                   m = hlme(LV1_score ~ time_point, subject = "pin", random=~1,
                            ng = 3, data = PEPP_cdss_LV1_long_clean, mixture =~time_point, nwg = T))
gmm4_LV1_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_LV1_c, 
                   m = hlme(LV1_score ~ time_point, subject = "pin", random=~1,
                            ng = 4, data = PEPP_cdss_LV1_long_clean, mixture =~time_point, nwg = T))
gmm5_LV1_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_LV1_c, 
                   m = hlme(LV1_score ~ time_point, subject = "pin", random=~1,
                            ng = 5, data = PEPP_cdss_LV1_long_clean, mixture =~time_point, nwg = T))

#make table with results for the 5 models
mod_comp_gmm_LV1_c <- summarytable(gmm1_LV1_c, gmm2_LV1_c, gmm3_LV1_c, gmm4_LV1_c, gmm5_LV1_c)
mod_comp_gmm_LV1_c  <- as.data.frame(mod_comp_gmm_LV1_c)
# Extract the model with the lowest BIC
lowest_bic_mod_gmm_LV1_c <- which.min(mod_comp_gmm_LV1_c $BIC)
cat("The model with the lowest BIC is: GMM", lowest_bic_mod_gmm_LV1_c, "\n")

# Extract cluster assignments from each GMM model
df1_LV1_c <- gmm1_LV1_c$pprob[,1:2] %>% rename(Class1 = class)
df2_LV1_c <- gmm2_LV1_c$pprob[,1:2] %>% rename(Class2 = class)
df3_LV1_c <- gmm3_LV1_c$pprob[,1:2] %>% rename(Class3 = class)
df4_LV1_c <- gmm4_LV1_c$pprob[,1:2] %>% rename(Class4 = class)
df5_LV1_c <- gmm5_LV1_c$pprob[,1:2] %>% rename(Class5 = class)

# Merge datasets by pin
df_LV1_c <- reduce(list(df1_LV1_c, df2_LV1_c, df3_LV1_c, df4_LV1_c, 
                        df5_LV1_c), left_join, by = "pin")

# Convert to long format
df_long_LV1_c <- df_LV1_c %>%
  pivot_longer(cols = starts_with("Class"), 
               names_to = "Model", 
               values_to = "Cluster") %>%
  mutate(Model = as.factor(gsub("Class", "", Model)),  # Convert Model to factor
         Cluster = as.factor(Cluster))  # Ensure Cluster is categorical

# Check structure
str(df_long_LV1_c)
# Set color palette based on max number of clusters
num_clusters <- length(unique(df_long_LV1_c$Cluster))
palette_colors <- RColorBrewer::brewer.pal(n = min(num_clusters, 12), name = "Set1")  
# Use up to 12 colors

# Alluvial plot
ggplot(df_long_LV1_c, aes(x = Model, stratum = Cluster, alluvium = pin, fill = Cluster)) +
  geom_flow(stat = "alluvium", alpha = 0.7) +  # Flow connections
  geom_stratum(alpha = 0.8) +  # Cluster strata
  scale_x_discrete(limits = as.character(1:6)) +  # Ensure all models appear
  scale_fill_manual(values = palette_colors) +  # Apply custom color palette
  labs(title = "Changes in Class Membership Across GMM Models",
       x = "Number of Classes in Model",
       y = "Count of Individuals") +
  theme_minimal()


## Change LV1 clean vs filtered here (one for the "modeled best" one for the "numerical best")
# Extract probabilities from the gmm model - THIS IS WHERE CLEAN VS FILTERED CHANGES ARE MADE
# Change the other number as the last probability according to n of gmm +2 (e.g. gmm2 = 3:4)
probabilities_LV1_c = gmm4_LV1_c$pprob[, 3:6]

prob_LV1_c = gmm3_LV1_c$pprob[, 3:5]

# Convert to matrix for headman
heatmap_data_Lv1_c <- as.matrix(probabilities_LV1_c)
rownames(heatmap_data_Lv1_c) <- gmm4_LV1_c$pprob$pin

hm_data_Lv1_c <- as.matrix(prob_LV1_c)
rownames(hm_data_Lv1_c) <- gmm3_LV1_c$pprob$pin

# Melt the matrix to long format
heatmap_long_Lv1_c <- melt(heatmap_data_Lv1_c)
colnames(heatmap_long_Lv1_c) <- c("Pin", "Class", "Probability")

hm_long_Lv1_c <- melt(hm_data_Lv1_c)
colnames(hm_long_Lv1_c) <- c("Pin", "Class", "Probability")

# Determine the class with the highest probability for each individual (Pin)
heatmap_long_Lv1_c$Max_Prob_Class <- apply(heatmap_data_Lv1_c, 1, function(x) which.max(x))

hm_long_Lv1_c$Max_Prob_Class <- apply(hm_data_Lv1_c, 1, function(x) which.max(x))

# Reorder the Pins based on the class with the highest probability
heatmap_long_Lv1_c$Pin <- factor(heatmap_long_Lv1_c$Pin, levels = unique(heatmap_long_Lv1_c$Pin[order(heatmap_long_Lv1_c$Max_Prob_Class)]))

hm_long_Lv1_c$Pin <- factor(hm_long_Lv1_c$Pin, levels = unique(hm_long_Lv1_c$Pin[order(hm_long_Lv1_c$Max_Prob_Class)]))

# Plot heatmap
ggplot(heatmap_long_Lv1_c, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()

ggplot(hm_long_Lv1_c, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()



trajectory_LV1_c <- as.data.frame(gmm4_LV1_c$pprob[, 1:2]) #best fit model

traj_LV1_c <- as.data.frame(gmm3_LV1_c$pprob[, 1:2])

# trajectory has class labels for each pin, merge them into PEPP_cdss data
PEPP_cdss_LV1_c_subset_plotting <- PEPP_cdss_LV1_long_clean %>%
  left_join(trajectory_LV1_c, by = "pin") %>%
  mutate(class = factor(class)) 

PEPP_cdss_LV1_c_subset_plotting_2 <- PEPP_cdss_LV1_long_clean %>%
  left_join(traj_LV1_c, by = "pin") %>%
  mutate(class = factor(class)) 

# Plotting
PEPP_cdss_LV1_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_cdss_LV1_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "CDSS General Depressive Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "CDSS General Depressive Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_cdss_LV1_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()

PEPP_cdss_LV1_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_cdss_LV1_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "CDSS General Depressive Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "CDSS General Depressive Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_cdss_LV1_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()

## facet wrap by latent class
PEPP_cdss_LV1_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_cdss_LV1_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "CDSS General Depressive Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "CDSS General Depressive Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_cdss_LV1_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class

PEPP_cdss_LV1_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_cdss_LV1_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "CDSS General Depressive Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "CDSS General Depressive Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_cdss_LV1_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class


## Now look at random intercept, random slope ------------------------------------------
## LV1_clean
gmm1_2_LV1_c <- lcmm::hlme(LV1_score ~ time_point, subject = "pin", random =~1 + 
                       time_point, ng = 1, data = PEPP_cdss_LV1_long_clean)
gmm2_2_LV1_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_LV1_c, 
                     m = hlme(LV1_score ~ time_point, subject = "pin",
                              random=~1 + time_point ,ng = 2, data = 
                                PEPP_cdss_LV1_long_clean, mixture 
                              =~time_point, nwg = T))
gmm3_2_LV1_c <- gridsearch(rep = 100, maxiter =200, minit= gmm1_2_LV1_c, 
                     m = hlme(LV1_score ~ time_point, subject = "pin", 
                              random=~1 + time_point,
                              ng = 3, data = PEPP_cdss_LV1_long_clean, mixture
                              =~time_point, nwg = T))
gmm4_2_LV1_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_LV1_c, 
                     m = hlme(LV1_score ~ time_point, subject = "pin", 
                              random=~1 + time_point,
                              ng = 4, data = PEPP_cdss_LV1_long_clean, mixture
                              =~time_point, nwg = T))
gmm5_2_LV1_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_LV1_c, 
                     m = hlme(LV1_score ~ time_point, subject = "pin",
                              random=~1 + time_point,
                              ng = 5, data = PEPP_cdss_LV1_long_clean, mixture
                              =~time_point, nwg = T))

#make table with results for the 5 models
mod_com_gmm_2_LV1_c <- summarytable(gmm1_2_LV1_c, gmm2_2_LV1_c, gmm3_2_LV1_c,
                                       gmm4_2_LV1_c, gmm5_2_LV1_c)
mod_com_gmm_2_LV1_c <- as.data.frame(mod_com_gmm_2_LV1_c)
# Extract the model with the lowest BIC
lowest_bic_mod_gmm_2_LV1_c <- which.min(mod_com_gmm_2_LV1_c$BIC)
cat("The model with the lowest BIC is: gmm_2", lowest_bic_mod_gmm_2_LV1_c, "\n")


# Extract cluster assignments from each GMM model
df1_2_LV1_c <- gmm1_2_LV1_c$pprob[,1:2] %>% rename(Class1 = class)
df2_2_LV1_c <- gmm2_2_LV1_c$pprob[,1:2] %>% rename(Class2 = class)
df3_2_LV1_c <- gmm3_2_LV1_c$pprob[,1:2] %>% rename(Class3 = class)
df4_2_LV1_c <- gmm4_2_LV1_c$pprob[,1:2] %>% rename(Class4 = class)
df5_2_LV1_c <- gmm5_2_LV1_c$pprob[,1:2] %>% rename(Class5 = class)

# Merge datasets by pin
df_2_LV1_c <- reduce(list(df1_2_LV1_c, df2_2_LV1_c, df3_2_LV1_c, df4_2_LV1_c, 
                          df5_2_LV1_c), left_join, by = "pin")

# Convert to long format
df_long_2_LV1_c <- df_2_LV1_c %>%
  pivot_longer(cols = starts_with("Class"), 
               names_to = "Model", 
               values_to = "Cluster") %>%
  mutate(Model = as.factor(gsub("Class", "", Model)),  # Convert Model to factor
         Cluster = as.factor(Cluster))  # Ensure Cluster is categorical

# Check structure
str(df_long_2_LV1_c)
# Set color palette based on max number of clusters
num_clusters <- length(unique(df_long_2_LV1_c$Cluster))
palette_colors <- RColorBrewer::brewer.pal(n = min(num_clusters, 12), name = "Set1")  # Use up to 12 colors

# Alluvial plot
ggplot(df_long_2_LV1_c, aes(x = Model, stratum = Cluster, alluvium = pin, fill = Cluster)) +
  geom_flow(stat = "alluvium", alpha = 0.7) +  # Flow connections
  geom_stratum(alpha = 0.8) +  # Cluster strata
  scale_x_discrete(limits = as.character(1:6)) +  # Ensure all models appear
  scale_fill_manual(values = palette_colors) +  # Apply custom color palette
  labs(title = "Changes in Class Membership Across GMM Models",
       x = "Number of Classes in Model",
       y = "Count of Individuals") +
  theme_minimal()

## Change LV1 clean vs filtered here (one for the "modeled best" one for the "numerical best")
# Extract probabilities from the gmm model - THIS IS WHERE CLEAN VS FILTERED CHANGES ARE MADE
# Change the other number as the last probability according to n of gmm +2 (e.g. gmm2 = 3:4)
probabilities_2_LV1_c = gmm5_2_LV1_c$pprob[, 3:7]

prob_2_LV1_c = gmm5_2_LV1_c$pprob[, 3:7]

# Convert to matrix for headman
heatmap_data_2_LV1_c <- as.matrix(probabilities_2_LV1_c)
rownames(heatmap_data_2_LV1_c) <- gmm5_2_LV1_c$pprob$pin

hm_data_2_LV1_c <- as.matrix(prob_2_LV1_c)
rownames(hm_data_2_LV1_c) <- gmm5_2_LV1_c$pprob$pin

# Melt the matrix to long format
heatmap_long_2_LV1_c <- melt(heatmap_data_2_LV1_c)
colnames(heatmap_long_2_LV1_c) <- c("Pin", "Class", "Probability")

hm_long_2_LV1_c <- melt(heatmap_data_2_LV1_c)
colnames(hm_long_2_LV1_c) <- c("Pin", "Class", "Probability")

# Determine the class with the highest probability for each individual (Pin)
heatmap_long_2_LV1_c$Max_Prob_Class <- apply(heatmap_data_2_LV1_c, 1, function(x) which.max(x))

hm_long_2_LV1_c$Max_Prob_Class <- apply(hm_data_2_LV1_c, 1, function(x) which.max(x))

# Reorder the Pins based on the class with the highest probability
heatmap_long_2_LV1_c$Pin <- factor(heatmap_long_2_LV1_c$Pin, 
                                   levels = unique(heatmap_long_2_LV1_c$Pin[order(heatmap_long_2_LV1_c$Max_Prob_Class)]))


hm_long_2_LV1_c$Pin <- factor(hm_long_2_LV1_c$Pin, 
                                   levels = unique(hm_long_2_LV1_c$Pin[order(hm_long_2_LV1_c$Max_Prob_Class)]))

# Plot heatmap
ggplot(heatmap_long_2_LV1_c, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()

ggplot(hm_long_2_LV1_c, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()


trajectory_2_LV1_c <- as.data.frame(gmm3_2_LV1_c$pprob[, 1:2]) #best fit model

traj_2_LV1_c <- as.data.frame(gmm3_2_LV1_c$pprob[, 1:2]) #best fit model

# trajectory has class labels for each pin, merge them into PEPP_cdss data
PEPP_cdss_2_Lv1_c_subset_plotting <- PEPP_cdss_LV1_long_clean %>%
  left_join(trajectory_2_LV1_c, by = "pin") %>%
  mutate(class = factor(class)) 

PEPP_cdss_2_Lv1_c_subset_plotting_2 <- PEPP_cdss_LV1_long_clean %>%
  left_join(traj_2_LV1_c, by = "pin") %>%
  mutate(class = factor(class)) 

# Plotting
PEPP_cdss_2_LV1_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_cdss_2_LV1_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "CDSS General Depressive Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "CDSS General Depressive Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_cdss_2_LV1_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()

PEPP_cdss_2_Lv1_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_cdss_2_Lv1_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "CDSS General Depressive Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "CDSS General Depressive Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_cdss_2_Lv1_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()


## facet wrap by latent class
PEPP_cdss_2_LV1_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_cdss_2_LV1_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "CDSS General Depressive Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "CDSS General Depressive Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_cdss_2_LV1_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class

PEPP_cdss_2_LV1_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = LV1_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_cdss_2_LV1_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "CDSS General Depressive Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "CDSS General Depressive Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_cdss_2_LV1_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class

## LV1_filtered
gmm1_2_LV1_f <- lcmm::hlme(LV1_score ~ time_point, subject = "pin", random =~1 + 
                             time_point, ng = 1, data = PEPP_cdss_LV1_long_filtered)
gmm2_2_LV1_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_LV1_f, 
                           m = hlme(LV1_score ~ time_point, subject = "pin",
                                    random=~1 + time_point ,ng = 2, data = 
                                      PEPP_cdss_LV1_long_filtered, mixture 
                                    =~time_point, nwg = T))
gmm3_2_LV1_f <- gridsearch(rep = 100, maxiter =200, minit= gmm1_2_LV1_f, 
                           m = hlme(LV1_score ~ time_point, subject = "pin", 
                                    random=~1 + time_point,
                                    ng = 3, data = PEPP_cdss_LV1_long_filtered, mixture
                                    =~time_point, nwg = T))
gmm4_2_LV1_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_LV1_f, 
                           m = hlme(LV1_score ~ time_point, subject = "pin", 
                                    random=~1 + time_point,
                                    ng = 4, data = PEPP_cdss_LV1_long_filtered, mixture
                                    =~time_point, nwg = T))
gmm5_2_LV1_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_LV1_f, 
                           m = hlme(LV1_score ~ time_point, subject = "pin",
                                    random=~1 + time_point,
                                    ng = 5, data = PEPP_cdss_LV1_long_filtered, mixture
                                    =~time_point, nwg = T))
