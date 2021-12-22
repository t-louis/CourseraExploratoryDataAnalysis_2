#####################################################################
## Function plotting the total amount of PM2.5 emissions in USA
## per year
#####################################################################

# Load pre-requisites libraries
library(tidyverse)
library(dplyr)

# Define plot function
plot1 <- function () {
  
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
  # We subset NEI by selecting only Emissions and year
  emi <- NEI[, c("Emissions", "year")]
  
  # Use pipeline command to process total emissions per year
  # and we scale the total emissions on million tons so that
  # we can pretty plot it in Y-axis
  emi <- emi %>% group_by(year) %>% summarise(totalEmissions=sum(Emissions, na.rm=TRUE)/1e06)
  
  # Add text label in data for plotting purpose
  emi$label <- sprintf("%d/%.2f", emi$year, emi$totalEmissions)
  
  # Perform the desired plot
  with(emi, plot(year, totalEmissions, col="red", type="l", xlim=c(1998, 2010), main="Total PM2.5 Emissions per year", xlab="Year", ylab="Total Emission (million tons)"))
  axis(side=1, at=seq(1998, 2010, by=1))
  
  # Annotate plot to add label on points
  text(emi$year, y=emi$totalEmissions, emi$label, pos=4)
  
  # Plot to PNG
  dev.copy(png, file="plot1.png")
  dev.off()
  
}

# Call the plot function
plot1()

