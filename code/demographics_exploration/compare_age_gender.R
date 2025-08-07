library(haven)
library(dplyr)
library(ggplot2)

PEPP_data <- read_sav("PEPP_2024-12-12-IDmatched.sav")
gmm_class_div_18 <- read.csv("gmm_class_div_18mo.csv")

joint_PEPP_data <- left_join(PEPP_data, gmm_class_div_18,by = "pin")

## t test for sans groups' age
merged_sans_age <- gmm_class_div_18 %>%
  select("pin", "class_sans_2") %>%
  left_join(PEPP_data %>%
              select ("pin","ageentry"), by = "pin")

merged_sans_age <- merged_sans_age %>%
  mutate(group = case_when(
    class_sans_2 == 1 ~ "Persistent High",
    class_sans_2 == 2 ~ "Remitting"
  )) %>%
  filter(!is.na(group))

age_high_sans <- merged_sans_age %>%
  filter(class_sans_2 == 1) %>%
  pull(ageentry)
age_remit_sans <- merged_sans_age %>%
  filter(class_sans_2 == 2) %>%
  pull(ageentry)

t.test(age_high_sans, age_remit_sans, var.equal = FALSE)

ggplot(merged_sans_age, aes(x = group, y = ageentry, fill = group)) + 
  geom_boxplot() + 
  labs(title = "Age Comparison by SANS Group",
       x = "Class",
       y = "Age") + 
  theme_minimal() + 
  theme(legend.position = "none")


## ANOVA for lv1 groups' age
merged_lv1_age <- gmm_class_div_18 %>%
  select("pin", "class_LV1_3") %>%
  left_join(PEPP_data %>%
              select ("pin","ageentry"), by = "pin") %>%
  filter(!is.na(ageentry))

merged_lv1_age <- merged_lv1_age %>%
  mutate(group = case_when(
    class_LV1_3 == 1 ~ "Persistent High",
    class_LV1_3 == 2 ~ "Persistent Low",
    class_LV1_3 == 3 ~ "Remitting"
  )) %>%
  filter(!is.na(group))

anova_lv1_age <- aov(ageentry ~ group, data = merged_lv1_age)
summary(anova_lv1_age)

ggplot(merged_lv1_age, aes(x = group, y = ageentry, fill = group)) + 
  geom_boxplot() + 
  labs(title = "Age Comparison by LV1 Group",
       x = "Class",
       y = "Age") + 
  theme_minimal() + 
  theme(legend.position = "none")

## ANOVA for combined
merged_combined_age <- gmm_class_div_18 %>%
  select("pin", "class_sans_2", "class_LV1_3") %>%
  left_join(PEPP_data %>%
              select ("pin","ageentry"), by = "pin") %>%
  filter(!is.na(ageentry))

merged_combined_age <- merged_combined_age %>%
  mutate(group = case_when(
    class_LV1_3 == 1 & class_sans_2 == 1 ~ "High LV1 High SANS",
    class_LV1_3 == 2 & class_sans_2 == 1 ~ "Low LV1 High SANS",
    class_LV1_3 == 3 & class_sans_2 == 1 ~ "Remit LV1 High SANS",
    class_LV1_3 == 1 & class_sans_2 == 2 ~ "High LV1 Low SANS",
    class_LV1_3 == 2 & class_sans_2 == 2 ~ "Low LV1 Low SANS",
    class_LV1_3 == 3 & class_sans_2 == 2 ~ "Remit LV1 Low SANS"
  )) %>%
  filter(!is.na(group))

anova_comb_age <- aov(ageentry ~ group, data = merged_combined_age)
summary(anova_comb_age)

ggplot(merged_combined_age, aes(x = group, y = ageentry, fill = group)) + 
  geom_boxplot() + 
  labs(title = "Age Comparison by Combined LV1 and SANS Groups",
       x = "Class",
       y = "Age") + 
  theme_minimal() + 
  theme(legend.position = "none")

TukeyHSD(anova_comb_age)


## Chi-square for gender (sans and lv1)
merged_sans_gender <- gmm_class_div_18 %>%
  select("pin", "class_sans_2") %>%
  left_join(PEPP_data %>%
              select ("pin","gender"), by = "pin")

merged_sans_gender <- merged_sans_gender %>%
  mutate(group = case_when(
    class_sans_2 == 1 ~ "Persistent High",
    class_sans_2 == 2 ~ "Remitting"
  )) %>%
  filter(!is.na(group))
