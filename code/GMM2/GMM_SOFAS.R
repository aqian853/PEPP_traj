## This script is GMM SOFAS

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
library(ggalluvial)
library(reshape2)

PEPP_2024_12_12_IDmatched <- read_sav(
  "PEPP_2024-12-12-IDmatched.sav")

# Recoding NA
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  mutate(across(starts_with("SOFAS_"), ~ replace(., . == 77, NA)))

# Subsetting
PEPP_sofas_subset <- PEPP_2024_12_12_IDmatched %>%
  select( contains("pin"), contains("FEP_ID"),contains("Scan_ID"), contains("SOFAS_"))

colnames(PEPP_sofas_subset)

PEPP_sofas_subset <- PEPP_sofas_subset %>%
  filter(!is.na(SOFAS_0))

############ Converting to Long data 
## Clean out those with less than 2 time points [clean]
## Filter out 1, 2, 24mo follow ups [filter]

## Total SOFAS
PEPP_sofas_long <- 
  PEPP_sofas_subset %>%
  select(pin, SOFAS_0, SOFAS_12, SOFAS_24) %>%
  pivot_longer(cols = starts_with("SOFAS_"), names_to = "time_point", values_to = "sofas_score")

PEPP_sofas_long <- PEPP_sofas_long %>%
  mutate(pin = as.numeric(pin),
         time_point = recode(time_point,
                             "SOFAS_0" = "0",
                             "SOFAS_12" = "12",
                             "SOFAS_24" = "24"),
         time_point = as.numeric(time_point))

PEPP_sofas_long_clean <- PEPP_sofas_long %>%
  group_by(pin) %>%
  filter(sum(!is.na(sofas_score)) >= 2) %>%
  ungroup()

PEPP_sofas_long_clean %>%
  ggplot(aes(x = time_point, y = sofas_score, group = pin, color = pin)) +
  geom_line(alpha = 0.3) + 
  geom_point(size = 2, alpha = 0.5) + 
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(title = "SOFAS Score Over Time", 
       x = "Time Point (Months)", 
       y = "SOFAS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(color = "none") +
  theme_classic()

## Trajectory - Go straight to GMM2, skip GMM1
#set seed for reproducibility
set.seed(123)

gmm1_2_sofas <- lcmm::hlme(sofas_score ~ time_point, subject = "pin", random =~1 + 
                              time_point, ng = 1, data = PEPP_sofas_long_clean)
gmm2_2_sofas <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sofas, 
                            m = hlme(sofas_score ~ time_point, subject = "pin",
                                     random=~1 + time_point ,ng = 2, data = 
                                       PEPP_sofas_long_clean, mixture 
                                     =~time_point, nwg = T))
gmm3_2_sofas <- gridsearch(rep = 100, maxiter =200, minit= gmm1_2_sofas, 
                            m = hlme(sofas_score ~ time_point, subject = "pin", 
                                     random=~1 + time_point,
                                     ng = 3, data = PEPP_sofas_long_clean, mixture
                                     =~time_point, nwg = T))
gmm4_2_sofas <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sofas, 
                            m = hlme(sofas_score ~ time_point, subject = "pin", 
                                     random=~1 + time_point,
                                     ng = 4, data = PEPP_sofas_long_clean, mixture
                                     =~time_point, nwg = T))
gmm5_2_sofas <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sofas, 
                            m = hlme(sofas_score ~ time_point, subject = "pin",
                                     random=~1 + time_point,
                                     ng = 5, data = PEPP_sofas_long_clean, mixture
                                     =~time_point, nwg = T))

#make table with results for the 5 models
mod_com_gmm_2_sofas <- summarytable(gmm1_2_sofas, gmm2_2_sofas, gmm3_2_sofas,
                                     gmm4_2_sofas, gmm5_2_sofas)
mod_com_gmm_2_sofas <- as.data.frame(mod_com_gmm_2_sofas)
# Extract the model with the lowest BIC
lowest_bic_mod_gmm_2_sofas <- which.min(mod_com_gmm_2_sofas$BIC)
cat("The model with the lowest BIC is: gmm_2", lowest_bic_mod_gmm_2_sofas, "\n")


# Extract cluster assignments from each GMM model
df1_2_sofas <- gmm1_2_sofas$pprob[,1:2] %>% rename(Class1 = class)
df2_2_sofas <- gmm2_2_sofas$pprob[,1:2] %>% rename(Class2 = class)
df3_2_sofas <- gmm3_2_sofas$pprob[,1:2] %>% rename(Class3 = class)
df4_2_sofas <- gmm4_2_sofas$pprob[,1:2] %>% rename(Class4 = class)
df5_2_sofas <- gmm5_2_sofas$pprob[,1:2] %>% rename(Class5 = class)

# Merge datasets by pin
df_2_sofas <- reduce(list(df1_2_sofas, df2_2_sofas, df3_2_sofas, df4_2_sofas, 
                           df5_2_sofas), left_join, by = "pin")

# Convert to long format
df_long_2_sofas <- df_2_sofas %>%
  pivot_longer(cols = starts_with("Class"), 
               names_to = "Model", 
               values_to = "Cluster") %>%
  mutate(Model = as.factor(gsub("Class", "", Model)),  # Convert Model to factor
         Cluster = as.factor(Cluster))  # Ensure Cluster is categorical

# Check structure
str(df_long_2_sofas)
# Set color palette based on max number of clusters
num_clusters <- length(unique(df_long_2_sofas$Cluster))
palette_colors <- RColorBrewer::brewer.pal(n = min(num_clusters, 12), name = "Set1")  # Use up to 12 colors

# Alluvial plot
ggplot(df_long_2_sofas, aes(x = Model, stratum = Cluster, alluvium = pin, fill = Cluster)) +
  geom_flow(stat = "alluvium", alpha = 0.7) +  # Flow connections
  geom_stratum(alpha = 0.8) +  # Cluster strata
  scale_x_discrete(limits = as.character(1:6)) +  # Ensure all models appear
  scale_fill_manual(values = palette_colors) +  # Apply custom color palette
  labs(title = "Changes in Class Membership Across GMM Models",
       x = "Number of Classes in Model",
       y = "Count of Individuals") +
  theme_minimal()

## Change sans clean vs filtered here (one for the "modeled best" one for the "numerical best")
# Extract probabilities from the gmm model - THIS IS WHERE CLEAN VS FILTERED CHANGES ARE MADE
# Change the other number as the last probability according to n of gmm +2 (e.g. gmm2 = 3:4)
probabilities_2_sofas = gmm2_2_sofas$pprob[, 3:4]

prob_2_sofas = gmm3_2_sofas$pprob[, 3:5]

# Convert to matrix for headman
heatmap_data_2_sofas <- as.matrix(probabilities_2_sofas)
rownames(heatmap_data_2_sofas) <- gmm2_2_sofas$pprob$pin

hm_data_2_sofas <- as.matrix(prob_2_sofas)
rownames(hm_data_2_sofas) <- gmm3_2_sofas$pprob$pin

# Melt the matrix to long format
heatmap_long_2_sofas <- melt(heatmap_data_2_sofas)
colnames(heatmap_long_2_sofas) <- c("Pin", "Class", "Probability")

hm_long_2_sofas <- melt(heatmap_data_2_sofas)
colnames(hm_long_2_sofas) <- c("Pin", "Class", "Probability")

# Determine the class with the highest probability for each individual (Pin)
heatmap_long_2_sofas$Max_Prob_Class <- apply(heatmap_data_2_sofas, 1, function(x) which.max(x))

hm_long_2_sofas$Max_Prob_Class <- apply(hm_data_2_sofas, 1, function(x) which.max(x))

# Reorder the Pins based on the class with the highest probability
heatmap_long_2_sofas$Pin <- factor(heatmap_long_2_sofas$Pin, 
                                    levels = unique(heatmap_long_2_sofas$Pin[order(heatmap_long_2_sofas$Max_Prob_Class)]))


hm_long_2_sofas$Pin <- factor(hm_long_2_sofas$Pin, 
                               levels = unique(hm_long_2_sofas$Pin[order(hm_long_2_sofas$Max_Prob_Class)]))

# Plot heatmap
ggplot(heatmap_long_2_sofas, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()

ggplot(hm_long_2_sofas, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()


trajectory_2_sofas <- as.data.frame(gmm2_2_sofas$pprob[, 1:2]) #best fit model

traj_2_sofas <- as.data.frame(gmm3_2_sofas$pprob[, 1:2]) #best fit model

# trajectory has class labels for each pin, merge them into PEPP_cdss data
PEPP_sofas_2_subset_plotting <- PEPP_sofas_long_clean %>%
  left_join(trajectory_2_sofas, by = "pin") %>%
  mutate(class = factor(class)) 

PEPP_sofas_2_subset_plotting_2 <- PEPP_sofas_long_clean %>%
  left_join(traj_2_sofas, by = "pin") %>%
  mutate(class = factor(class)) 

# Plotting
PEPP_sofas_2_subset_plotting %>%
  ggplot(aes(x = time_point, y = sofas_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sofas_2_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sofas_2_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()

PEPP_sofas_2_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = sofas_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sofas_2_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sofas_2_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()


## facet wrap by latent class
PEPP_sofas_2_subset_plotting %>%
  ggplot(aes(x = time_point, y = sofas_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sofas_2_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sofas_2_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class

PEPP_sofas_2_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = sofas_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sofas_2_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sofas_2_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class
