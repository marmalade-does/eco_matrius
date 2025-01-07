# Install required packages if not already installed --------------------
if (!requireNamespace("Rcompadre", quietly = TRUE)) install.packages("Rcompadre")
if (!requireNamespace("popdemo", quietly = TRUE)) install.packages("popdemo")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")


# Load required libraries ------------------------------------------------
library(Rcompadre)
library(popdemo)
library(dplyr)
library(ggplot2) # for graph plotting
library(tidyr) # for makng the data look nice
library(patchwork) # to be able to show the three graphs at once

# define population vector
# these value are taken from the paper, can be ajusted
m_inicial <- 200
v_inicial <- matrix(c(
  0,
  0,
  m_inicial,
  0
))
print(v_inicial)

inter_mat <- matrix(
  c(0, 0.45, 0, 0,
    0, 0.469, 0.231, 0,
    0.983, 0, 0.045, 0.855,
    0, 0, 0.9, 0),
  nrow = 4, byrow = TRUE
)
inter_vec <- v_inicial
inter_mat
inter_vec

eigs(inter_mat)$lambda

# Define the function to do 50 step projection
project_population <- function(projeccio_poblacio, mat_ursus) {
  for (t_step in 1:50) {
    projeccio_poblacio <- cbind(
      projeccio_poblacio,
      round(mat_ursus %*% projeccio_poblacio[, t_step], 0)
    )
  }
  return(projeccio_poblacio)
}

# Define the function for processing the data
process_population_data <- function(projeccio_poblacio) {
  data <- t(projeccio_poblacio) %>%
    as.data.frame()
  names(data) <- c('Cries', 'Immadurs', 'Adults', 'Mares')
  data <- data %>%
    mutate(
      Total = Cries + Immadurs + Adults + Mares,
      Any = 2000:2050
    ) %>%
    pivot_longer(cols = Cries:Total, names_to = 'Estadi', values_to = 'N') # Modify as needed
  return(data)
}

# Project the population for each matrix
inter_vec <- project_population(inter_vec, inter_mat)

# Process the population projection data
dades_finals_inter <- process_population_data(inter_vec)

# Define a function to plot data
create_population_plot <- function(data, id, title_suffix) {
  plot <- data %>%
    ggplot(aes(x = Any, y = N, color = Estadi)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "loess") + # Ensure smoother method is appropriate
    ggtitle(
      paste('Projecció de mida poblacional de ID:', id, '-', title_suffix),
      subtitle = 'Anys 2000 a 2050'
    ) +
    labs(x = "Any", y = "N", color = "Estadi") +
    theme_minimal()
  return(plot)
}

# Create plots using the function
plot_inter <-  create_population_plot(dades_finals_inter, "inter", "intiervention")
plot_inter
