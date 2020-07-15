plot6 <- function(){
        library(dplyr)
        library(ggplot2)
        PM25Data <- readRDS("summarySCC_PM25.rds")
        SourceClassificationCode <- readRDS("Source_Classification_Code.rds")
        SCCVehicle <- SourceClassificationCode[grep("[Vv]eh", SourceClassificationCode$Short.Name), ]
        
        EmissionsMotorLA <- PM25Data %>% 
                subset(fips == "06037" & PM25Data$SCC %in% SCCVehicle$SCC) %>%
                merge(y = SCCVehicle, by.x = "SCC", by.y = "SCC") %>%
                group_by(year) %>%
                summarize(Vehicle.Emissions.Type = sum(Emissions, na.rm = TRUE))
        EmissionsMotorBaltimore <- PM25Data %>% 
                subset(fips == "24510" & PM25Data$SCC %in% SCCVehicle$SCC) %>%
                merge(y = SCCVehicle, by.x = "SCC", by.y = "SCC") %>%
                group_by(year) %>%
                summarize(Vehicle.Emissions.Type = sum(Emissions, na.rm = TRUE))
        
        EmissionsMotorBaltimore2 <- cbind(EmissionsMotorBaltimore, "City" = rep("Baltimore", 4))
        EmissionsMotorLA2 <- cbind(EmissionsMotorLA, "City" = rep("LA", 4))
        
        EmissionsMotor <- rbind(EmissionsMotorBaltimore2, EmissionsMotorLA2)
        
        EmissionsMotorPlot1 <- ggplot(EmissionsMotor, aes(year, Vehicle.Emissions.Type, col = City)) +
                geom_point(size = 4, 
                           alpha = 1/3) +
                xlab("Year") +
                ylab("Total Emissions [Tons]") +
                ggtitle("Comparison of Total Annual Vehicle Emissions in Baltimore and Los Angeles")
        
        print(EmissionsMotorPlot1)
        
        EmissionsMotorLA2008 <- EmissionsMotorLA[EmissionsMotorLA$year  == 2008, 2]
        EmissionsMotorLA1999 <- EmissionsMotorLA[EmissionsMotorLA$year  == 1999, 2]
        
        
        DifferenceLA<- EmissionsMotorLA2008 - EmissionsMotorLA1999
        
        EmissionsMotorBaltimore2008 <- EmissionsMotorBaltimore[EmissionsMotorBaltimore$year  == 2008, 2]
        EmissionsMotorBaltimore1999 <- EmissionsMotorBaltimore[EmissionsMotorBaltimore$year  == 1999, 2]
        
        DifferenceBaltimore <- EmissionsMotorBaltimore2008 - EmissionsMotorBaltimore1999
        
        abs(DifferenceLA) > abs(DifferenceBaltimore)
}