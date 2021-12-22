#####################################################################
## Function plotting the total amount of PM2.5 emissions
## per year for the city of Baltimore (fips = 24510)
#####################################################################

# Load pre-requisites libraries
library(tidyverse)
library(dplyr)

# Define plot function
plot2 <- function () {
  
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
  emi <- NEI[NEI$fips == "24510", c("Emissions", "year")]
  
  # Use pipeline command to process total emissions per year
  # and we scale the total emissions on thousands tons so that
  # we can pretty plot it in Y-axis
  emi <- emi %>% group_by(year) %>% summarise(totalEmissions=sum(Emissions, na.rm=TRUE)/1e03)
  
  # Add text label in data for plotting purpose
  emi$label <- sprintf("%d/%.2f", emi$year, emi$totalEmissions)
  
  # Perform the desired plot
  with(emi, plot(year, totalEmissions, col="red", type="l", xlim=c(1998, 2010), main="Total PM2.5 Emissions per year (Baltimore City)", xlab="Year", ylab="Total Emission (thousand tons)"))
  axis(side=1, at=seq(1998, 2010, by=1))
  
  # Annotate plot to add label on points
  text(emi$year, y=emi$totalEmissions, emi$label, pos=4)
  
  # Plot to PNG
  dev.copy(png, file="plot2.png")
  dev.off()
  
}

# Call the plot function
plot2()

