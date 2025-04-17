# --------------------------------------------------------
# PCA Analysis on Survey Bullying Data
# --------------------------------------------------------
# Description: This script performs Principal Component Analysis
#              on bullying-related survey data, visualizes results,
#              and saves scree plots and biplots for interpretation.
# --------------------------------------------------------

# Load the required libraries
library(ggplot2)
library(FactoMineR)     # For PCA computation
library(factoextra)     # For visualization of PCA
library(ggrepel)
library(psych)          # Optional, but useful for data diagnostics

# Step 1: Load the dataset
# (Make sure the CSV file path is correct)
raw_data <- read.csv("Survey Data.csv", stringsAsFactors = TRUE)

# Step 2: Select relevant columns
# Based on the codebook, we focus on direct and online bullying/victimization items
bullying_data <- raw_data[, c(
  paste0("db", 1:18),    # Direct bullying items
  paste0("dv", 1:18),    # Direct victimization items
  paste0("ov", 1:8),     # Online victimization items
  "tb", "vb", "sb", "pb", # Composite bullying scores
  "tv", "vv", "sv", "pv", "ov" # Composite victimization scores
)]

# Step 3: Handle missing data
# As per codebook, 0 indicates missing — so we replace 0 with NA
bullying_data[bullying_data == 0] <- NA

# Now, impute missing values with the mean of each column
bullying_imputed <- as.data.frame(lapply(bullying_data, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}))

# Step 4: Standardize the data
# PCA is sensitive to scale, so we scale all variables to have mean 0 and SD 1
scaled_data <- scale(bullying_imputed)

# Step 5: Run PCA (no graph output from FactoMineR)
pca_result <- PCA(scaled_data, graph = FALSE)

# Step 6: Visualize the scree plot
# This shows how much variance each principal component explains
scree_plot <- fviz_eig(pca_result,
                       addlabels = TRUE,
                       barfill = "#2c7fb8",
                       barcolor = "#2c7fb8",
                       linecolor = "#e41a1c",
                       ggtheme = theme_minimal()) +
  labs(title = "PCA Scree Plot",
       x = "Principal Components",
       y = "Percentage of Explained Variance") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Save the scree plot
ggsave("scree_plot.png", scree_plot, width = 10, height = 6, dpi = 300)

# Step 7: Create a biplot
# This plot shows both variables and individuals on the PCA plane
biplot <- fviz_pca_biplot(pca_result,
                          col.ind = "#525252",   # Color for individuals
                          col.var = "#e41a1c",   # Color for variables
                          repel = TRUE,
                          labelsize = 4,
                          pointsize = 2,
                          arrowsize = 1,
                          ggtheme = theme_minimal()) +
  labs(title = "PCA Biplot (PC1 vs PC2)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Save the biplot
ggsave("biplot.png", biplot, width = 12, height = 8, dpi = 300)

# Step 8 (Optional): Color individuals by gender (if column exists)
# This helps us see if patterns vary by gender groups
if ("gender" %in% colnames(raw_data)) {
  gender_biplot <- fviz_pca_biplot(pca_result,
                                   col.ind = raw_data$gender,
                                   addEllipses = TRUE,
                                   ggtheme = theme_minimal()) +
    labs(title = "PCA Biplot Colored by Gender") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("biplot_by_gender.png", gender_biplot, width = 12, height = 8, dpi = 300)
}

# Step 9: Print the plots to the viewer
print(scree_plot)
print(biplot)
if (exists("gender_biplot")) print(gender_biplot)

# Done! The PCA has been performed and visualized.
