plot1 <- function(){
        library(dplyr)
        PM25Data <- readRDS("summarySCC_PM25.rds")
        SourceClassificationCode <- readRDS("Source_Classification_Code.rds")
        TotalEmissionsbyYear <- PM25Data %>%  # Group by year and summarize total emissions across the board
                group_by(year) %>%
                summarize(Total.Emissions = sum(Emissions, na.rm = TRUE))
        
        with(TotalEmissionsbyYear, # plot data 
             plot(x = year, 
                  y = Total.Emissions, 
                  ylab = "Total Annual Emissions [Tons]", 
                  xlab = "Year",
                  main = "Total Annual Emissions in the US by Year",
                  cex = 2,
                  pch = 2,
                  col = "black",
                  lwd = 3))
        # Find delta between 2008 and 1999
        TotalEmissions2008 <- TotalEmissionsbyYear[TotalEmissionsbyYear$year == 2008, 2]
        TotalEmissions1999 <- TotalEmissionsbyYear[TotalEmissionsbyYear$year == 1999, 2]
        
        DifferenceTotalEmissions <- TotalEmissions2008 - TotalEmissions1999
}