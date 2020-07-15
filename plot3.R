plot3 <- function(){
        library(dplyr)
        library(ggplot2)
        PM25Data <- readRDS("summarySCC_PM25.rds")
        SourceClassificationCode <- readRDS("Source_Classification_Code.rds")
        tot.emissions.type <- PM25Data %>% 
                subset(fips == "24510") %>%
                group_by(year, type) %>%
                summarize(Total.Emissions.Type = sum(Emissions, na.rm = TRUE))
        
        EmissionsType <- ggplot(data = tot.emissions.type, aes(year, Total.Emissions.Type))
        
        EmissionsType <- EmissionsType + 
                geom_point(color = "black", 
                           size = 4, 
                           alpha = 1/3) + 
                facet_grid(. ~ type) +
                xlab("Year") +
                ylab("Total Emissions [Tons]") +
                ggtitle("Total Annual Emissions in Baltimore by Year")
        
        EmissionsType
}