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
# For example:
cdb_check_species(comadre_db, "Ursus americanus")  # Black bear species

# Fetch data for a specific species --------------------------------------
# Here we download data for Ursus americanus (American black bear) and store it as a database.
ursus_db <- cdb_check_species(comadre_db, "Ursus americanus", return_db = TRUE)

# sin actividad humana -- full back to nature 
mat_ursus_249883 <- matA(ursus_db[ursus_db$MatrixID == 249883, ])[[1]]
#  poplacion aspen donde sin caza, com comida atropo-
mat_ursus_249885 <- matA(ursus_db[ursus_db$MatrixID == 249885, ])[[1]]
# poplacion aspen donde se permite caza
mat_ursus_249887 <- matA(ursus_db[ursus_db$MatrixID == 249887, ])[[1]] 

# initial population vector
m_inicial <- 200
v_inicial <- matrix(c(
  0,
  0,
  m_inicial,
  0
))
print(v_inicial)

# elas(mat_ursus_249881) #gives another matrix that says which values in the 
# matrix are the most *weight bearing*
# eg whch one has the largest change on population dynamics on marginal change.

projeccio_poblacio_249883 <- v_inicial
projeccio_poblacio_249885 <- v_inicial
projeccio_poblacio_249887 <- v_inicial


################## modificaciones para comprobar recomendaciones ############
# ver los matrices originales
mat_ursus_249883
mat_ursus_249885
mat_ursus_249887

# Redefine the matrixes with more maternal survival and more 
# Updated transition matrix 1 with slightly increased survival and fecundity
# Updated transition matrix 1 with more pronounced increases 
# Below are three updated transition matrices with slightly lowered fecundities 
# (generally in the top row) while retaining higher survival/transition rates. 

# Transition Matrix 1
# Below are three updated transition matrices where fecundities (in the top row) 
# can exceed 1, reflecting the possibility of bears having more than two cubs. 
# Survival/transition rates remain at or below 1.

# Transition Matrix 1
mat_ursus_249883 <- matrix(c(
  0.0,  0.0,  2.0,  0.0,    # Fecundities from stage 3 and 4
  0.7,  0.7,  0.0,  0.0,   # Survival transitions
  0.0,  0.45, 0.85, 0.98,
  0.0,  0.0,  0.55, 0.0
), nrow = 4, byrow = TRUE)
print(mat_ursus_249883)

# Transition Matrix 2
# Recreate the transition matrix based on the image:
mat_ursus_249883 <- matrix(c(
  0,     0,     0.983, 0,
  0.45,  0.469, 0,     0,
  0,     0.231, 0.045, 0.9,
  0,     0,     0.855, 0
), nrow = 4, byrow = TRUE)

print(mat_ursus_249883)




# Define the function to perform projection
project_population <- function(projeccio_poblacio, mat_ursus) {
  for (t_step in 1:50) {
    projeccio_poblacio <- cbind(
      projeccio_poblacio,
      round(mat_ursus %*% projeccio_poblacio[, t_step], 0)
    )
    print(projeccio_poblacio)
  }
  return(projeccio_poblacio)
}


# Define the function for processing data
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

# Use the function for each object
projeccio_poblacio_249883 <- project_population(projeccio_poblacio_249883, mat_ursus_249883)
projeccio_poblacio_249885 <- project_population(projeccio_poblacio_249885, mat_ursus_249885)
projeccio_poblacio_249887 <- project_population(projeccio_poblacio_249887, mat_ursus_249887)


# Use the function for each dataset
dades_finals_249883 <- process_population_data(projeccio_poblacio_249883)
dades_finals_249885 <- process_population_data(projeccio_poblacio_249885)
dades_finals_249887 <- process_population_data(projeccio_poblacio_249887)

# Veure en consola
# print(projeccio_poblacio_249883)
# print(projeccio_poblacio_249885)
# print(projeccio_poblacio_249887)

# print(dades_finals_249883)
# print(dades_finals_249885)
# print(dades_finals_249887)


# Una funcion para plotear los datos
create_population_plot <- function(data, id, title_suffix) {
  plot <- data %>%
    filter(Any <= 2020) %>%
    ggplot(aes(x = Any, y = N, color = Estadi)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    ggtitle(
      paste('Projecció de ID:', id, '-', title_suffix),
      subtitle = 'Anys 2005 a 2055'
    )
  return(plot)
}

# Create plots using the function
plot_249883 <- create_population_plot(dades_finals_249883, "249883", "undisturbed")
plot_249885 <- create_population_plot(dades_finals_249885, "249885", "Low removal")
plot_249887 <- create_population_plot(dades_finals_249887, "249887", "High removal")

plot_249883 + plot_249885 + plot_249887

