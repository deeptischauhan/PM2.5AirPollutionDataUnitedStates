plot5 <- function(){
        library(dplyr)
        library(ggplot2)
        PM25Data <- readRDS("summarySCC_PM25.rds")
        SourceClassificationCode <- readRDS("Source_Classification_Code.rds")
        SCCVehicle <- SourceClassificationCode[grep("[Vv]eh", SourceClassificationCode$Short.Name), ]
        
        EmissionsMoterBaltimore <- PM25Data %>% 
                subset(fips == "24510" & PM25Data$SCC %in% SCCVehicle$SCC) %>%
                merge(y = SCCVehicle, by.x = "SCC", by.y = "SCC") %>%
                group_by(year) %>%
                summarize(VehicleEmissionsType = sum(Emissions, na.rm = TRUE))
        
        
        EmissionsMoterBaltimorePlot <- ggplot(EmissionsMoterBaltimore, aes(year, VehicleEmissionsType)) +
                geom_point(color = "black", 
                           size = 4, 
                           alpha = 1/3) + 
                xlab("Year") +
                ylab("Total Emissions [Tons]") +
                ggtitle("Total Annual Vehicle Emissions in Baltimore City")
        
        print(EmissionsMoterBaltimorePlot)
        
        EmissionsMoterBaltimore2008 <- EmissionsMoterBaltimore[EmissionsMoterBaltimore$year  == 2008, 2]
        EmissionsMoterBaltimore1999 <- EmissionsMoterBaltimore[EmissionsMoterBaltimore$year  == 1999, 2]
        
        DifferenceEmissions <- EmissionsMoterBaltimore2008 - EmissionsMoterBaltimore1999
}