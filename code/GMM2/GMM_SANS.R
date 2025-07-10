## This script is GMM SANS Clean and Filtered

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

#Total global score w/o attention items
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  rename(
    sans_total_baseline = SANS_0, #total SANS score at baseline
    sans_total_1mo = SANS_1, #total SANS score at 1-mo follow up
    sans_total_2mo = SANS_2, #total SANS score at 2-mo follow up
    sans_total_3mo = SANS_3, #total SANS score at 3-mo follow up
    sans_total_6mo = SANS_6, #total SANS score at 6-mo follow up
    sans_total_9mo = SANS_9, #total SANS score at 9-mo follow up
    sans_total_12mo = SANS_12, #total SANS score at 12-mo follow up
    sans_total_18mo = SANS_18, #total SANS score at 18-mo follow up
    sans_total_24mo = SANS_24 #total SANS score at 24-mo follow up
  )

# Recoding NA
PEPP_2024_12_12_IDmatched <- PEPP_2024_12_12_IDmatched %>%
  mutate(across(starts_with("sans"), ~ replace(., . == 77, NA)))

# Subsetting
PEPP_sans_subset <- PEPP_2024_12_12_IDmatched %>%
  select( contains("pin"), contains("FEP_ID"),contains("Scan_ID"), contains("sans_"))

colnames(PEPP_sans_subset)

PEPP_sans_subset <- PEPP_sans_subset %>%
  filter(!is.na(sans_total_baseline))

############ Converting to Long data 
## Clean out those with less than 2 time points [clean]
## Filter out 1, 2, 24mo follow ups [filter]

## Total SANS
PEPP_sans_long <- 
  PEPP_sans_subset %>%
  select(pin, sans_total_baseline, sans_total_1mo, sans_total_2mo, sans_total_3mo,
         sans_total_6mo, sans_total_9mo, sans_total_12mo, sans_total_18mo, 
         sans_total_24mo) %>%
  pivot_longer(cols = starts_with("sans_total_"), names_to = "time_point", values_to = "sans_total_score")

PEPP_sans_long <- PEPP_sans_long %>%
  mutate(pin = as.numeric(pin),
         time_point = recode(time_point,
                             "sans_total_baseline" = "0",
                             "sans_total_1mo" = "1",
                             "sans_total_2mo" = "2",
                             "sans_total_3mo" = "3",
                             "sans_total_6mo" = "6",
                             "sans_total_9mo" = "9",
                             "sans_total_12mo" = "12",
                             "sans_total_18mo" = "18",
                             "sans_total_24mo" = "24"),
         time_point = as.numeric(time_point))

PEPP_sans_long_clean <- PEPP_sans_long %>%
  group_by(pin) %>%
  filter(sum(!is.na(sans_total_score)) >= 2) %>%
  ungroup()

PEPP_sans_long_clean <- PEPP_sans_long %>%
  filter(!time_point %in% c(1, 2, 24)) %>%
  group_by(pin) %>%
  filter(sum(!is.na(sans_total_score)) >= 2) %>%
  ungroup()

PEPP_sans_long_filtered %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = pin)) +
  geom_line(alpha = 0.3) + 
  geom_point(size = 2, alpha = 0.5) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 6, 9, 12, 18, 24)) +
  labs(title = "Total SANS Score Over Time", 
       x = "Time Point (Months)", 
       y = "Total SANS Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(color = "none") +
  theme_classic()

PEPP_sans_long_filtered %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = pin)) +
  geom_line(alpha = 0.3) + 
  geom_point(size = 2, alpha = 0.5) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 6, 9, 12, 18, 24)) +
  labs(title = "Total SANS Score Over Time", 
       x = "Time Point (Months)", 
       y = "Total SANS Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(color = "none") +
  theme_classic()



### TRAJECTORY MODELING ------ start here (This file is only for sans clean and filtered)
#set seed for reproducibility
set.seed(123)

#run models with 1-5 classes, each with 200 random starts using the 1-class model to set initial start values
### sans_clean
gmm1_sans_c <- lcmm::hlme(sans_total_score ~ time_point, subject = "pin", random =~1, ng = 1, 
                         data = PEPP_sans_long_clean)
gmm2_sans_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_sans_c, 
                         m = hlme(sans_total_score ~ time_point, subject = "pin", random=~1,
                                  ng = 2, data = PEPP_sans_long_clean, mixture =~time_point, nwg = T))
gmm3_sans_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_sans_c, 
                         m = hlme(sans_total_score ~ time_point, subject = "pin", random=~1,
                                  ng = 3, data = PEPP_sans_long_clean, mixture =~time_point, nwg = T))
gmm4_sans_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_sans_c, 
                         m = hlme(sans_total_score ~ time_point, subject = "pin", random=~1,
                                  ng = 4, data = PEPP_sans_long_clean, mixture =~time_point, nwg = T))
gmm5_sans_c <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_sans_c, 
                         m = hlme(sans_total_score ~ time_point, subject = "pin", random=~1,
                                  ng = 5, data = PEPP_sans_long_clean, mixture =~time_point, nwg = T))

#make table with results for the 5 models
mod_comp_gmm_sans_c <- summarytable(gmm1_sans_c, gmm2_sans_c, gmm3_sans_c, gmm4_sans_c, gmm5_sans_c)
mod_comp_gmm_sans_c  <- as.data.frame(mod_comp_gmm_sans_c)
# Extract the model with the lowest BIC
lowest_bic_mod_gmm_sans_c <- which.min(mod_comp_gmm_sans_c$BIC)
cat("The model with the lowest BIC is: GMM", lowest_bic_mod_gmm_sans_c, "\n")

# Extract cluster assignments from each GMM model
df1_sans_c <- gmm1_sans_c$pprob[,1:2] %>% rename(Class1 = class)
df2_sans_c <- gmm2_sans_c$pprob[,1:2] %>% rename(Class2 = class)
df3_sans_c <- gmm3_sans_c$pprob[,1:2] %>% rename(Class3 = class)
df4_sans_c <- gmm4_sans_c$pprob[,1:2] %>% rename(Class4 = class)
df5_sans_c <- gmm5_sans_c$pprob[,1:2] %>% rename(Class5 = class)

# Merge datasets by pin
df_sans_c <- reduce(list(df1_sans_c, df2_sans_c, df3_sans_c, df4_sans_c, 
                        df5_sans_c), left_join, by = "pin")

# Convert to long format
df_long_sans_c <- df_sans_c %>%
  pivot_longer(cols = starts_with("Class"), 
               names_to = "Model", 
               values_to = "Cluster") %>%
  mutate(Model = as.factor(gsub("Class", "", Model)),  # Convert Model to factor
         Cluster = as.factor(Cluster))  # Ensure Cluster is categorical

# Check structure
str(df_long_sans_c)
# Set color palette based on max number of clusters
num_clusters <- length(unique(df_long_sans_c$Cluster))
palette_colors <- RColorBrewer::brewer.pal(n = min(num_clusters, 12), name = "Set1")  
# Use up to 12 colors

# Alluvial plot
ggplot(df_long_sans_c, aes(x = Model, stratum = Cluster, alluvium = pin, fill = Cluster)) +
  geom_flow(stat = "alluvium", alpha = 0.7) +  # Flow connections
  geom_stratum(alpha = 0.8) +  # Cluster strata
  scale_x_discrete(limits = as.character(1:6)) +  # Ensure all models appear
  scale_fill_manual(values = palette_colors) +  # Apply custom color palette
  labs(title = "Changes in Class Membership Across GMM Models",
       x = "Number of Classes in Model",
       y = "Count of Individuals") +
  theme_minimal()

nrow(heatmap_data_sans_c)
nrow(heatmap_long_sans_c)


## Change sans clean vs filtered here (one for the "modeled best" one for the "numerical best")
# Extract probabilities from the gmm model - THIS IS WHERE CLEAN VS FILTERED CHANGES ARE MADE
# Change the other number as the last probability according to n of gmm +2 (e.g. gmm2 = 3:4)
probabilities_sans_c = gmm3_sans_c$pprob[, 3:5]
prob_sans_c = gmm5_sans_c$pprob[, 3:7]

# Convert to matrix for headman
heatmap_data_sans_c <- as.matrix(probabilities_sans_c)
rownames(heatmap_data_sans_c) <- gmm3_sans_c$pprob$pin

hm_data_sans_c <- as.matrix(prob_sans_c)
rownames(hm_data_sans_c) <- gmm5_sans_c$pprob$pin

# Melt the matrix to long format
heatmap_long_sans_c <- melt(heatmap_data_sans_c)
colnames(heatmap_long_sans_c) <- c("Pin", "Class", "Probability")

hm_long_sans_c <- melt(hm_data_sans_c)
colnames(hm_long_sans_c) <- c("Pin", "Class", "Probability")

# Determine the class with the highest probability for each individual (Pin)
heatmap_long_sans_c$Max_Prob_Class <- apply(heatmap_data_sans_c, 1, function(x) which.max(x))

hm_long_sans_c$Max_Prob_Class <- apply(hm_data_sans_c, 1, function(x) which.max(x))

# Reorder the Pins based on the class with the highest probability
heatmap_long_sans_c$Pin <- factor(heatmap_long_sans_c$Pin, levels = unique(heatmap_long_sans_c$Pin[order(heatmap_long_sans_c$Max_Prob_Class)]))

hm_long_sans_c$Pin <- factor(hm_long_sans_c$Pin, levels = unique(hm_long_sans_c$Pin[order(hm_long_sans_c$Max_Prob_Class)]))

# Plot heatmap
ggplot(heatmap_long_sans_c, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()

ggplot(hm_long_sans_c, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()



trajectory_sans_c <- as.data.frame(gmm3_sans_c$pprob[, 1:2]) #best fit model

traj_sans_c <- as.data.frame(gmm5_sans_c$pprob[, 1:2])

# trajectory has class labels for each pin, merge them into PEPP_cdss data
PEPP_sans_c_subset_plotting <- PEPP_sans_long_clean %>%
  left_join(trajectory_sans_c, by = "pin") %>%
  mutate(class = factor(class)) 

PEPP_sans_c_subset_plotting_2 <- PEPP_sans_long_clean %>%
  left_join(traj_sans_c, by = "pin") %>%
  mutate(class = factor(class)) 

# Plotting
PEPP_sans_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()

PEPP_sans_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()

## facet wrap by latent class
PEPP_sans_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class

PEPP_sans_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class

















## Now look at random intercept, random slope ------------------------------------------
## sans_filtered
gmm1_2_sans_f <- lcmm::hlme(sans_total_score ~ time_point, subject = "pin", random =~1 + 
                             time_point, ng = 1, data = PEPP_sans_long_filtered)
gmm2_2_sans_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sans_f, 
                           m = hlme(sans_total_score ~ time_point, subject = "pin",
                                    random=~1 + time_point ,ng = 2, data = 
                                      PEPP_sans_long_filtered, mixture 
                                    =~time_point, nwg = T))
gmm3_2_sans_f <- gridsearch(rep = 100, maxiter =200, minit= gmm1_2_sans_f, 
                           m = hlme(sans_total_score ~ time_point, subject = "pin", 
                                    random=~1 + time_point,
                                    ng = 3, data = PEPP_sans_long_filtered, mixture
                                    =~time_point, nwg = T))
gmm4_2_sans_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sans_f, 
                           m = hlme(sans_total_score ~ time_point, subject = "pin", 
                                    random=~1 + time_point,
                                    ng = 4, data = PEPP_sans_long_filtered, mixture
                                    =~time_point, nwg = T))
gmm5_2_sans_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sans_f, 
                           m = hlme(sans_total_score ~ time_point, subject = "pin",
                                    random=~1 + time_point,
                                    ng = 5, data = PEPP_sans_long_filtered, mixture
                                    =~time_point, nwg = T))

#make table with results for the 5 models
mod_com_gmm_2_sans_f <- summarytable(gmm1_2_sans_f, gmm2_2_sans_f, gmm3_2_sans_f,
                                    gmm4_2_sans_f, gmm5_2_sans_f)
mod_com_gmm_2_sans_f <- as.data.frame(mod_com_gmm_2_sans_f)
# Extract the model with the lowest BIC
lowest_bic_mod_gmm_2_sans_f <- which.min(mod_com_gmm_2_sans_f$BIC)
cat("The model with the lowest BIC is: gmm_2", lowest_bic_mod_gmm_2_sans_f, "\n")


# Extract cluster assignments from each GMM model
df1_2_sans_f <- gmm1_2_sans_f$pprob[,1:2] %>% rename(Class1 = class)
df2_2_sans_f <- gmm2_2_sans_f$pprob[,1:2] %>% rename(Class2 = class)
df3_2_sans_f <- gmm3_2_sans_f$pprob[,1:2] %>% rename(Class3 = class)
df4_2_sans_f <- gmm4_2_sans_f$pprob[,1:2] %>% rename(Class4 = class)
df5_2_sans_f <- gmm5_2_sans_f$pprob[,1:2] %>% rename(Class5 = class)

# Merge datasets by pin
df_2_sans_f <- reduce(list(df1_2_sans_f, df2_2_sans_f, df3_2_sans_f, df4_2_sans_f, 
                          df5_2_sans_f), left_join, by = "pin")

# Convert to long format
df_long_2_sans_f <- df_2_sans_f %>%
  pivot_longer(cols = starts_with("Class"), 
               names_to = "Model", 
               values_to = "Cluster") %>%
  mutate(Model = as.factor(gsub("Class", "", Model)),  # Convert Model to factor
         Cluster = as.factor(Cluster))  # Ensure Cluster is categorical

# Check structure
str(df_long_2_sans_f)
# Set color palette based on max number of clusters
num_clusters <- length(unique(df_long_2_sans_f$Cluster))
palette_colors <- RColorBrewer::brewer.pal(n = min(num_clusters, 12), name = "Set1")  # Use up to 12 colors

# Alluvial plot
ggplot(df_long_2_sans_f, aes(x = Model, stratum = Cluster, alluvium = pin, fill = Cluster)) +
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
probabilities_2_sans_f = gmm2_2_sans_f$pprob[, 3:4]

prob_2_sans_f = gmm5_2_sans_f$pprob[, 3:7]

# Convert to matrix for headman
heatmap_data_2_sans_f <- as.matrix(probabilities_2_sans_f)
rownames(heatmap_data_2_sans_f) <- gmm2_2_sans_f$pprob$pin

hm_data_2_sans_f <- as.matrix(prob_2_sans_f)
rownames(hm_data_2_sans_f) <- gmm5_2_sans_f$pprob$pin

# Melt the matrix to long format
heatmap_long_2_sans_f <- melt(heatmap_data_2_sans_f)
colnames(heatmap_long_2_sans_f) <- c("Pin", "Class", "Probability")

hm_long_2_sans_f <- melt(heatmap_data_2_sans_f)
colnames(hm_long_2_sans_f) <- c("Pin", "Class", "Probability")

# Determine the class with the highest probability for each individual (Pin)
heatmap_long_2_sans_f$Max_Prob_Class <- apply(heatmap_data_2_sans_f, 1, function(x) which.max(x))

hm_long_2_sans_f$Max_Prob_Class <- apply(hm_data_2_sans_f, 1, function(x) which.max(x))

# Reorder the Pins based on the class with the highest probability
heatmap_long_2_sans_f$Pin <- factor(heatmap_long_2_sans_f$Pin, 
                                   levels = unique(heatmap_long_2_sans_f$Pin[order(heatmap_long_2_sans_f$Max_Prob_Class)]))


hm_long_2_sans_f$Pin <- factor(hm_long_2_sans_f$Pin, 
                              levels = unique(hm_long_2_sans_f$Pin[order(hm_long_2_sans_f$Max_Prob_Class)]))

# Plot heatmap
ggplot(heatmap_long_2_sans_f, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()

ggplot(hm_long_2_sans_f, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()


trajectory_2_sans_f <- as.data.frame(gmm2_2_sans_f$pprob[, 1:2]) #best fit model

traj_2_sans_f <- as.data.frame(gmm5_2_sans_f$pprob[, 1:2]) #best fit model

# trajectory has class labels for each pin, merge them into PEPP_cdss data
PEPP_sans_2_c_subset_plotting <- PEPP_sans_long_filtered %>%
  left_join(trajectory_2_sans_f, by = "pin") %>%
  mutate(class = factor(class)) 

PEPP_sans_2_c_subset_plotting_2 <- PEPP_sans_long_filtered %>%
  left_join(traj_2_sans_f, by = "pin") %>%
  mutate(class = factor(class)) 

# Plotting
PEPP_sans_2_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_2_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_2_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()

PEPP_sans_2_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_2_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_2_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()


## facet wrap by latent class
PEPP_sans_2_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_2_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_2_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class

PEPP_sans_2_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_2_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_2_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class












## sans_filtered
gmm1_2_sans_f <- lcmm::hlme(sans_total_score ~ time_point, subject = "pin", random =~1 + 
                              time_point, ng = 1, data = PEPP_sans_long_filtered)
gmm2_2_sans_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sans_f, 
                            m = hlme(sans_total_score ~ time_point, subject = "pin",
                                     random=~1 + time_point ,ng = 2, data = 
                                       PEPP_sans_long_filtered, mixture 
                                     =~time_point, nwg = T))
gmm3_2_sans_f <- gridsearch(rep = 100, maxiter =200, minit= gmm1_2_sans_f, 
                            m = hlme(sans_total_score ~ time_point, subject = "pin", 
                                     random=~1 + time_point,
                                     ng = 3, data = PEPP_sans_long_filtered, mixture
                                     =~time_point, nwg = T))
gmm4_2_sans_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sans_f, 
                            m = hlme(sans_total_score ~ time_point, subject = "pin", 
                                     random=~1 + time_point,
                                     ng = 4, data = PEPP_sans_long_filtered, mixture
                                     =~time_point, nwg = T))
gmm5_2_sans_f <- gridsearch(rep = 100, maxiter = 200, minit= gmm1_2_sans_f, 
                            m = hlme(sans_total_score ~ time_point, subject = "pin",
                                     random=~1 + time_point,
                                     ng = 5, data = PEPP_sans_long_filtered, mixture
                                     =~time_point, nwg = T))

#make table with results for the 5 models
mod_com_gmm_2_sans_f <- summarytable(gmm1_2_sans_f, gmm2_2_sans_f, gmm3_2_sans_f,
                                     gmm4_2_sans_f, gmm5_2_sans_f)
mod_com_gmm_2_sans_f <- as.data.frame(mod_com_gmm_2_sans_f)
# Extract the model with the lowest BIC
lowest_bic_mod_gmm_2_sans_f <- which.min(mod_com_gmm_2_sans_f$BIC)
cat("The model with the lowest BIC is: gmm_2", lowest_bic_mod_gmm_2_sans_f, "\n")


# Extract cluster assignments from each GMM model
df1_2_sans_f <- gmm1_2_sans_f$pprob[,1:2] %>% rename(Class1 = class)
df2_2_sans_f <- gmm2_2_sans_f$pprob[,1:2] %>% rename(Class2 = class)
df3_2_sans_f <- gmm3_2_sans_f$pprob[,1:2] %>% rename(Class3 = class)
df4_2_sans_f <- gmm4_2_sans_f$pprob[,1:2] %>% rename(Class4 = class)
df5_2_sans_f <- gmm5_2_sans_f$pprob[,1:2] %>% rename(Class5 = class)

# Merge datasets by pin
df_2_sans_f <- reduce(list(df1_2_sans_f, df2_2_sans_f, df3_2_sans_f, df4_2_sans_f, 
                           df5_2_sans_f), left_join, by = "pin")

# Convert to long format
df_long_2_sans_f <- df_2_sans_f %>%
  pivot_longer(cols = starts_with("Class"), 
               names_to = "Model", 
               values_to = "Cluster") %>%
  mutate(Model = as.factor(gsub("Class", "", Model)),  # Convert Model to factor
         Cluster = as.factor(Cluster))  # Ensure Cluster is categorical

# Check structure
str(df_long_2_sans_f)
# Set color palette based on max number of clusters
num_clusters <- length(unique(df_long_2_sans_f$Cluster))
palette_colors <- RColorBrewer::brewer.pal(n = min(num_clusters, 12), name = "Set1")  # Use up to 12 colors

# Alluvial plot
ggplot(df_long_2_sans_f, aes(x = Model, stratum = Cluster, alluvium = pin, fill = Cluster)) +
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
probabilities_2_sans_f = gmm2_2_sans_f$pprob[, 3:4]

prob_2_sans_f = gmm5_2_sans_f$pprob[, 3:7]

# Convert to matrix for headman
heatmap_data_2_sans_f <- as.matrix(probabilities_2_sans_f)
rownames(heatmap_data_2_sans_f) <- gmm2_2_sans_f$pprob$pin

hm_data_2_sans_f <- as.matrix(prob_2_sans_f)
rownames(hm_data_2_sans_f) <- gmm5_2_sans_f$pprob$pin

# Melt the matrix to long format
heatmap_long_2_sans_f <- melt(heatmap_data_2_sans_f)
colnames(heatmap_long_2_sans_f) <- c("Pin", "Class", "Probability")

hm_long_2_sans_f <- melt(heatmap_data_2_sans_f)
colnames(hm_long_2_sans_f) <- c("Pin", "Class", "Probability")

# Determine the class with the highest probability for each individual (Pin)
heatmap_long_2_sans_f$Max_Prob_Class <- apply(heatmap_data_2_sans_f, 1, function(x) which.max(x))

hm_long_2_sans_f$Max_Prob_Class <- apply(hm_data_2_sans_f, 1, function(x) which.max(x))

# Reorder the Pins based on the class with the highest probability
heatmap_long_2_sans_f$Pin <- factor(heatmap_long_2_sans_f$Pin, 
                                    levels = unique(heatmap_long_2_sans_f$Pin[order(heatmap_long_2_sans_f$Max_Prob_Class)]))


hm_long_2_sans_f$Pin <- factor(hm_long_2_sans_f$Pin, 
                               levels = unique(hm_long_2_sans_f$Pin[order(hm_long_2_sans_f$Max_Prob_Class)]))

# Plot heatmap
ggplot(heatmap_long_2_sans_f, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()

ggplot(hm_long_2_sans_f, aes(x = Class, y = Pin, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Soft Assignment: Membership Probabilities Heatmap", x = "Latent Class", y = "Pin") +
  theme_minimal()


trajectory_2_sans_f <- as.data.frame(gmm2_2_sans_f$pprob[, 1:2]) #best fit model

traj_2_sans_f <- as.data.frame(gmm5_2_sans_f$pprob[, 1:2]) #best fit model

# trajectory has class labels for each pin, merge them into PEPP_cdss data
PEPP_sans_2_c_subset_plotting <- PEPP_sans_long_filtered %>%
  left_join(trajectory_2_sans_f, by = "pin") %>%
  mutate(class = factor(class)) 

PEPP_sans_2_c_subset_plotting_2 <- PEPP_sans_long_filtered %>%
  left_join(traj_2_sans_f, by = "pin") %>%
  mutate(class = factor(class)) 

# Plotting
PEPP_sans_2_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_2_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_2_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()

PEPP_sans_2_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_2_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_2_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()


## facet wrap by latent class
PEPP_sans_2_c_subset_plotting %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_2_c_subset_plotting$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_2_c_subset_plotting$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class

PEPP_sans_2_c_subset_plotting_2 %>%
  ggplot(aes(x = time_point, y = sans_total_score, group = pin, color = class)) +
  geom_line(alpha = 0.1, size = 0.5) +  # Make lines more transparent and thinner
  geom_smooth(aes(group = class), method = "loess", size = 2, se = TRUE, alpha = 0.4) +  # Match SE band color with lines
  scale_x_continuous(breaks = unique(PEPP_sans_2_c_subset_plotting_2$time_point)) +
  labs(x = "Time Point (Months)", y = "SANS Composite Score", colour = "Latent Class") +
  geom_point(size = 1, alpha = 0.1) +  # Smaller and more transparent points
  labs(title = "SANS Composite Score Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  #guides(color = "none", fill = "none") +  # Remove the legend for fill (SE band)
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(PEPP_sans_2_c_subset_plotting_2$class)), name = "Set1")) +  # Distinct colors
  theme_classic()+
  facet_wrap(~ class,ncol = 2)  # Facet by latent class
