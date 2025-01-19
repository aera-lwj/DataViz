# Load necessary libraries
library(caret)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(GGally)
library(scales)
library(viridis)
library(patchwork)

# Load and clean the dataset
spotify_data <- read.csv("dataset.csv")
spotify_data <- na.omit(spotify_data)  

# Filter the top 500 most popular songs for each genre
pop_data <- spotify_data %>%
  filter(track_genre == "pop") %>%
  arrange(desc(popularity)) %>%
  slice(1:500)

classical_data <- spotify_data %>%
  filter(track_genre == "classical") %>%
  arrange(desc(popularity)) %>%
  slice(1:500)

# Combine the data for analysis
combined_data <- bind_rows(pop_data, classical_data)

# p1 Density plot: Popularity distribution across genres
p1 <- ggplot(combined_data, aes(x = popularity, fill = track_genre)) +
  geom_density(alpha = 0.6) +
  labs(title = "Popularity Distribution Across Genres",
       x = "Popularity", y = "Density",
       fill = "Genre") +
  theme_minimal() +
  scale_fill_manual(values = c("pop" = "orange", "classical" = "blue"))

print(p1)

# p2 Parallel coordinates plot: Dimensional Comparison of Audio Features
p2 <- ggparcoord(combined_data, 
                 columns = c("danceability", "energy", "valence", "tempo"), 
                 groupColumn = "track_genre", 
                 order = "allClass") +  
  labs(title = "Distribution of Audio Features Across Genres",
       x = "Audio Features", 
       y = "Feature Value", 
       color = "Genre") +
  theme_minimal() +
  scale_colour_manual(values = c("pop" = "orange", "classical" = "blue")) +
  geom_line(alpha = 0.1)

print(p2)

# p3 Heatmap: Correlation of audio features
correlation_matrix <- cor(combined_data %>% 
                            select(danceability, energy, valence, tempo))
correlation_matrix[upper.tri(correlation_matrix)] <- NA
correlation_melt <- melt(correlation_matrix)

p3 <- ggplot(correlation_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "cividis", limits = c(-1, 1), name = "Correlation", na.value = "white") +
  labs(title = "Correlation of Audio Features",
       x = NULL, y = NULL) +
  theme_minimal() +
  geom_text(aes(label = ifelse(!is.na(value), round(value, 2), "")), color = "black", size = 4)

print(p3)

# p4 Scatter plot: PCA of Audio Features
pca_result <- prcomp(combined_data[, c("danceability", "energy", "valence", "tempo")], 
                     center = TRUE, scale. = TRUE)

# Create a dataframe for PCA results
spotify_pca <- data.frame(
  genre = combined_data$track_genre,
  popularity = combined_data$popularity,
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2]
)

coefficient_string <- function(coeffs) {
  paste(paste(round(coeffs, 2), " * ", names(coeffs), collapse = " + "), collapse = "")
}

# PCA Scatter Plot
p4 <- ggplot(spotify_pca, aes(x = PC1, y = PC2, colour = genre, size = popularity)) +
  geom_point(alpha = 0.5) + 
  scale_colour_manual(values = c("pop" = "orange", "classical" = "blue"), 
                      guide = guide_legend(override.aes = list(size = 5))) +
  labs(title = "PCA of Audio Features",
       x = paste("PC1 = ", coefficient_string(pca_result$rotation[, 1])),
       y = paste("PC2 = ", coefficient_string(pca_result$rotation[, 2])),
       colour = "Genre",
       size = "Popularity") +
  theme_minimal() +
  theme(axis.title = element_text(size = 8))
        

print(p4)

# Patchwork
(p1 | p2) / (p3 | p4)


