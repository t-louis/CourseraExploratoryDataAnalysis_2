#####################################################################
## Function plotting the total amount of PM2.5 emissions
## per year for the city of Baltimore (fips = 24510)
#####################################################################

# Load pre-requisites libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# Define plot function
plot3 <- function () {
  
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
  # We subset NEI by selecting only Emissions and year for Baltimore
  emi <- NEI[NEI$fips == "24510", c("Emissions", "year", "type")]
  
  # Use pipeline command to process total emissions per year
  # and we scale the total emissions on thousands tons so that
  # we can pretty plot it in Y-axis
  emi <- emi %>% group_by(year, type) %>% summarise(totalEmissions=sum(Emissions, na.rm=TRUE)/1e03)
  
  # Perform the desired plot
  plot <- ggplot(data=emi, aes(year, totalEmissions)) +
    coord_cartesian(xlim=c(1999, 2008)) +
    geom_line(aes(color=type)) +
    facet_wrap(.~type, nrow=2, ncol=2) +
    ggtitle("Total PM2.5 Emissions per year (Baltimore City)") +
    labs(x="Year") +
    labs(y="Total Emissions (thousand tons)") + 
    theme_bw()
  print(plot)
  
  # Plot to PNG
  dev.copy(png, file="plot3.png")
  dev.off()
  
}

# Call the plot function
plot3()

