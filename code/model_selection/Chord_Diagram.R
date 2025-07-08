library(circlize)
library(mclust)
library(dplyr)

### Loading GMM Models
gmm2_2_cdss <- readRDS("gmm2_2_CDSS_c.rds")
gmm3_2_cdss <- readRDS("gmm3_2_CDSS_c.rds")
gmm2_2_sans <- readRDS("gmm2_2_sans_c.rds")
gmm3_2_sans <- readRDS("gmm3_2_sans_c.rds")
gmm2_2_sofas <- readRDS("gmm2_2_sofas.rds")
gmm3_2_sofas <- readRDS("gmm3_2_sofas.rds")
gmm2_2_LV1 <- readRDS("gmm2_2_LV1_c.rds")
gmm3_2_LV1 <- readRDS("gmm3_2_LV1_c.rds")

## Extracting pprob from gmms & renaming columns
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

gmm_class_div <- full_join(div_cdss_joint, div_sans_joint, by = "pin") %>% # does not include SOFAS
  full_join(div_LV1_joint, by = "pin")

gmm_class_div_clean <- gmm_class_div %>% # Remove NAs
  filter(!(is.na(class_cdss_2) & is.na(class_sans_2)))

gmm_class_div_func <- full_join(div_cdss_joint, div_sans_joint, by = "pin") %>% # include SOFAS
  full_join(div_sofas_joint, by = "pin") %>%
  full_join(div_LV1_joint, by = "pin")

gmm_class_div_clean_func <- gmm_class_div_func %>% # Remove NAs
  filter(!(is.na(class_cdss_2) & is.na(class_sans_2) & is.na(class_sofas_2)))









## Creating a contingency table (excludes NA by default)
clust1 <- gmm_class_div_clean_func$class_sans_2
clust2 <- gmm_class_div_clean_func$class_cdss_2

# Remove rows with NA in either clustering variable
valid_rows <- !is.na(clust1) & !is.na(clust2)
clust1 <- clust1[valid_rows]
clust2 <- clust2[valid_rows]

mat <- table(paste0("SANS_", clust1), paste0("CDSS_", clust2))
print(mat)

grid.col <- c(
  "SANS_1" = "grey70",
  "SANS_2" = "grey70",
  "CDSS_1" = "red",
  "CDSS_2" = "blue"
)

# Generate link colors based on target group
link.col <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat),
                   dimnames = dimnames(mat))

# Assign colors to each column (targets)
link.col[, "CDSS_1"] <- "red"
link.col[, "CDSS_2"] <- "lightblue"

# Draw the chord diagram with custom colors
chordDiagram(
  mat,
  transparency = 0.3,
  grid.col = grid.col,
  col = link.col,
  annotationTrack = "grid"
)

circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    sector.name <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    ylim <- get.cell.meta.data("ylim")
    circos.text(
      x = mean(xlim),
      y = ylim[1] + 2,
      labels = sector.name,
      facing = "bending",
      niceFacing = TRUE,
      cex = 0.8,
      adj = c(0.5, 0.5)
    )
  },
  bg.border = NA
)




##### Percentage Generator
# Extract the raw count
count <- mat["SANS_2", "CDSS_2"]

# Total individuals in CDSS class
total_cdss2 <- sum(mat[, "CDSS_2"])

# Total individuals in SANS class
total_sans2 <- sum(mat["SANS_2", ])

# Calculate percentages
percent_from_cdss2 <- round(100 * count / total_cdss2, 1)
percent_of_sans2 <- round(100 * count / total_sans2, 1)

# Print the sentence
cat(paste0(
  percent_from_cdss2, 
  "% of individuals from class 2 trajectory of CDSS also belong to the class 2 
  trajectory of SANS, representing ",
  percent_of_sans2, "% of it."
))