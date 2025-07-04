# Install required packages for the Shiny app
packages <- c(
  "shiny",
  "shinydashboard", 
  "DT",
  "plotly",
  "dplyr",
  "readr",
  "lubridate",
  "ggplot2"
)

# Install packages that are not already installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, dependencies = TRUE)
}

# Load all packages to verify installation
lapply(packages, library, character.only = TRUE)

cat("All packages installed and loaded successfully!\n")
