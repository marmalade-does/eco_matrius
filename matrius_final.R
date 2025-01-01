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


# Fetch the latest version of the COMPADRE and COMADRE databases ---------
# Note: Use 'compadre' for plants and 'comadre' for animals. This step requires an active internet connection.
# compadre is a plant database, not needed for _ursus americanus_
# compadre_db <- cdb_fetch("compadre")
comadre_db <- cdb_fetch("comadre")

# Check if a species exists in the database ------------------------------
cdb_check_species(comadre_db, "Ursus americanus")  # Black bear species

# Fetch data for a specific species --------------------------------------
# Here we download data for Ursus americanus (American black bear) and store it as a database.
ursus_db <- cdb_check_species(comadre_db, "Ursus americanus", return_db = TRUE)

# Explore the database content -------------------------------------------
# View the entire database object 
View(ursus_db)

# View the data slot for a clearer representation
View(ursus_db@data)

# Extract a specific matrix ----------------------------------------------
# we extract 3 matrixes from the databases.

# sin actividad humana -- full back to nature 
mat_ursus_249883 <- matA(ursus_db[ursus_db$MatrixID == 249883, ])[[1]]
#  poplacion aspen donde sin caza, com comida atropo-
mat_ursus_249885 <- matA(ursus_db[ursus_db$MatrixID == 249885, ])[[1]]
# poplacion aspen donde se permite caza
mat_ursus_249887 <- matA(ursus_db[ursus_db$MatrixID == 249887, ])[[1]] 

# test that extraction worked
print(mat_ursus_249883)
print(mat_ursus_249885)
print(mat_ursus_249887)

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

# these comads are in the popdemo package, they extact important parameters
# there are other packeges that can find these values
# These values aren't used lated in the code but are useful for undersatanding the population's dynamics
# lambda is the dominnate eigenvector - used to project population growth
lambda_249883 <- eigs(mat_ursus_249883)$lambda
# ss is the "The stable stage distribution is the proportional distribution of individuals across different stages (e.g., age classes, size classes) in a population when it reaches equilibrium."
ss_249883 <- eigs(mat_ursus_249883)$ss
# For the other two populations
lambda_249885 <- eigs(mat_ursus_249885)$lambda
lambda_249887 <- eigs(mat_ursus_249887)$lambda
ss_249885 <- eigs(mat_ursus_249885)$ss
ss_249887 <- eigs(mat_ursus_249887)$ss

elas_249883 <- elas(mat_ursus_249883)
elas_249885 <- elas(mat_ursus_249885)
elas_249887 <- elas(mat_ursus_249887)


# elas(mat_ursus_249881) #gives another matrix that says which values in the 
# matrix are the most *weight bearing*
# eg whch one has the largest change on population dynamics on marginal change.

projeccio_poblacio_249883 <- v_inicial
projeccio_poblacio_249885 <- v_inicial
projeccio_poblacio_249887 <- v_inicial

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
projeccio_poblacio_249883 <- project_population(projeccio_poblacio_249883, mat_ursus_249883)
projeccio_poblacio_249885 <- project_population(projeccio_poblacio_249885, mat_ursus_249885)
projeccio_poblacio_249887 <- project_population(projeccio_poblacio_249887, mat_ursus_249887)

# Process the population projection data
dades_finals_249883 <- process_population_data(projeccio_poblacio_249883)
dades_finals_249885 <- process_population_data(projeccio_poblacio_249885)
dades_finals_249887 <- process_population_data(projeccio_poblacio_249887)

# Both can be seen on console to ensure success
# print(projeccio_poblacio_249883)
# print(projeccio_poblacio_249885)
# print(projeccio_poblacio_249887)
# print(dades_finals_249883)
# print(dades_finals_249885)
# print(dades_finals_249887)


# Define a function to plot data
create_population_plot <- function(data, id, title_suffix) {
  plot <- data %>%
    filter(Any <= 2020) %>%
    ggplot(aes(x = Any, y = N, color = Estadi)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    ggtitle(
      paste('Projecció de mida poblacional de ID:', id, '-', title_suffix),
      subtitle = 'Anys 2005 a 2055'
    )
  return(plot)
}

# Create plots using the function
plot_249883 <- create_population_plot(dades_finals_249883, "249883", "full nature :p")
plot_249885 <- create_population_plot(dades_finals_249885, "249885", "interacción humana + caza :(")
plot_249887 <- create_population_plot(dades_finals_249887, "249887", "interacción humana sin caza :)")

# display the graph on terminal - note this uses the "patchwork" librairy
plot_249883 + plot_249885 + plot_249887