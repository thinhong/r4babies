library(simulist)
library(epiparameter)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)

# ==== Source code ======
# Note: code deprecated for development version of epiparameter. Make sure to install epiparameter v0.2.0
# pak::pkg_install("epiverse-trace/epiparameter@v0.2.0")
# https://drthinhong.com/idm4b/simll.html


# ===== Define epi parameters
contact_distribution <- epiparameter::epidist(
  disease = "COVID-19", 
  epi_dist = "contact distribution", 
  prob_distribution = "pois", 
  prob_distribution_params = c(mean = 2)
)

infectious_period <- epiparameter::epidist(
  disease = "COVID-19",
  epi_dist = "infectious period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 3, scale = 2)
)

onset_to_hosp <- epiparameter::epidist_db(
  disease = "COVID-19",
  epi_dist = "onset to hospitalisation",
  single_epidist = TRUE
)

onset_to_death <- epiparameter::epidist_db(
  disease = "COVID-19",
  epi_dist = "onset to death",
  single_epidist = TRUE
)

# Create data with 2 epidemics
# First peak
set.seed(1)
p1 <- simulist::sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  outbreak_size = c(1000, 1500),
  hosp_risk = 0.2,
  hosp_death_risk = 0.2,
  anonymise = T,
  population_age = c(0, 25),
  outbreak_start_date = as.Date("2023-01-01")
)

# Second peak
set.seed(17)
p2 <- simulist::sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  outbreak_size = c(1000, 1200),
  hosp_risk = 0.2,
  hosp_death_risk = 0.2,
  anonymise = T,
  population_age = c(3, 35),
  outbreak_start_date = as.Date("2023-05-01")
)

# 22 districts in HCMC
districts <- c("Cu Chi", "Hoc Mon", "Quan 12", "Go Vap", "Binh Chanh", "Binh Tan", "Tan Phu", "Tan Binh", "Phu Nhuan", "Binh Thanh", "Thu Duc", "Quan 6", "Quan 11", "Quan 10", "Quan 3", "Quan 1", "Quan 5", "Can Gio", "Nha Be", "Quan 4", "Quan 8", "Quan 7")

# Let have the first peak in the west, the second peak in the south
sampling_space <- round(rnorm(10000, mean = 6, sd = 6))
p1$district <- sample(sampling_space[sampling_space >= 1 & sampling_space <= 22], nrow(p1))
p1$district <- districts[p1$district]

sampling_space <- round(rnorm(10000, mean = 15, sd = 5))
p2$district <- sample(sampling_space[sampling_space >= 1 & sampling_space <= 22], nrow(p2))
p2$district <- districts[p2$district]

p1$outbreak <- "1st outbreak"
p2$outbreak <- "2nd outbreak"

df <- rbind(p1, p2)

getwd()
saveRDS(df %>% select(-ct_value), "data/simulated_covid.rds")
