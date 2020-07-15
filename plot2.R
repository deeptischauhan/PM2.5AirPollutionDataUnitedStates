plot2 <- function(){
        library(dplyr)
        PM25Data <- readRDS("summarySCC_PM25.rds")
        SourceClassificationCode <- readRDS("Source_Classification_Code.rds")
        TotalEmissionsBaltimore <- PM25Data %>%
                subset(fips == "24510") %>%
                group_by(year) %>%
                summarize(Total.Emissions.Baltimore = sum(Emissions, 
                                                          na.rm = TRUE))
        
        with(TotalEmissionsBaltimore, 
             plot(x = year, 
                  y = Total.Emissions.Baltimore, 
                  ylab = "Total Annual Emissions [Tons]", 
                  xlab = "Year",
                  main = "Total Annual Emissions in Baltimore by Year",
                  cex = 2,
                  pch = 2,
                  col = "black",
                  lwd = 3))
        
        TotalEmissionsBaltimore2008 <- TotalEmissionsBaltimore[TotalEmissionsBaltimore$year == 2008, 2]
        TotalEmissionsBaltimore1999 <- TotalEmissionsBaltimore[TotalEmissionsBaltimore$year == 1999, 2]
        
        DifferenceEmissions <- TotalEmissionsBaltimore2008 - TotalEmissionsBaltimore1999
}