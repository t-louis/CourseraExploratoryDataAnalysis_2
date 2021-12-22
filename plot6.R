#####################################################################
## Function comparison plotting the total amount of PM2.5 emissions
## per year for motor vehicle related pollution for the city of 
## Baltimore (fips = 24510) and Los Angeles (fips = 06037)
#####################################################################

# Load pre-requisites libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# Define plot function
plot6 <- function () {
  
  # Read the database in case not done yet and
  # cache it for further use in other functions
  if(!exists("NEI")) {
    NEI <<- readRDS("summarySCC_PM25.rds")
  }
  if(!exists("SCC")) {
    SCC <<- readRDS("Source_Classification_Code.rds")
  }
  
  # We assume that NEI and SCS has already been read in
  # memory.
  # We need to extract the list of SCC code related to
  # motor vehicle related
  scc <- SCC[, c("SCC", "Short.Name")]
  scc <- scc[grepl("Motor", scc$Short.Name, ignore.case = TRUE), ]
  scc <- scc[grepl("Veh", scc$Short.Name, ignore.case = TRUE), ]
  scc <- scc$SCC
  
  # We subset NEI by selecting only Emissions and year for Baltimore
  emiBA <- NEI[NEI$fips == "24510" & NEI$SCC %in% scc, c("Emissions", "year")]
  
  # Use pipeline command to process total emissions per year
  # and we scale the total emissions in tons 
  emiBA <- emiBA %>% group_by(year) %>% summarise(totalEmissions=sum(Emissions, na.rm=TRUE))
  
  # We subset NEI by selecting only Emissions and year for Los Angeles
  emiLA <- NEI[NEI$fips == "06037" & NEI$SCC %in% scc, c("Emissions", "year")]
  
  # Use pipeline command to process total emissions per year
  # and we scale the total emissions in tons 
  emiLA <- emiLA %>% group_by(year) %>% summarise(totalEmissions=sum(Emissions, na.rm=TRUE))
  
  # Merge both summaries
  emi <- merge(emiBA, emiLA, by="year")
  names(emi) <- c("Year", "Baltimore", "Los_Angeles")

  # Perform the desired plot
  plot <- ggplot(data=emi) +
    coord_cartesian(xlim=c(1999, 2008)) +
    geom_line(aes(Year, Baltimore, color="Baltimore City")) +
    geom_line(aes(Year, Los_Angeles, color="Los Angeles")) +
    ggtitle("Total PM2.5 Emissions motor vehicles (Baltimore, Los Angeles)") +
    labs(y="Total Emissions (tons)") + 
    theme_bw()
  print(plot)
  
  # Plot to PNG
  dev.copy(png, file="plot6.png")
  dev.off()
  
}

# Call the plot function
plot6()

