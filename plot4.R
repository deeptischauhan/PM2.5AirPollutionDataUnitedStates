plot4 <- function(){
        library(dplyr)
        library(ggplot2)
        PM25Data <- readRDS("summarySCC_PM25.rds")
        SourceClassificationCode <- readRDS("Source_Classification_Code.rds")
        SCCCoalData <- SourceClassificationCode[grep("[Cc]oal",SourceClassificationCode$EI.Sector),]
        CoalData <- subset(PM25Data, PM25Data$SCC %in% SCCCoalData$SCC)
        
        
        PM25DataCoalData <- merge(x = CoalData, 
                               y = SourceClassificationCode, 
                               by.x = "SCC", 
                               by.y = "SCC")
        
        PM25DataCoalDatabyYear <- PM25DataCoalData %>% 
                group_by(year) %>%
                summarize(TotalCoalCombustion = sum(Emissions, na.rm = TRUE))
        
        PM25DataCoalDataPlot <- ggplot(PM25DataCoalDatabyYear, aes(year, TotalCoalCombustion))
        
        PM25DataCoalDataPlot <- PM25DataCoalDataPlot + 
                geom_point(color = "black", 
                           size = 4, 
                           alpha = 1/3) + 
                xlab("Year") +
                ylab("Total Emissions [Tons]") +
                ggtitle("Total Annual Coal Combustion Emissions in the US")
        
        print(PM25DataCoalDataPlot)
        
        PM25DataCoalDatabyYear2008 <- PM25DataCoalDatabyYear[PM25DataCoalDatabyYear$year == 2008, 2]
        PM25DataCoalDatabyYear1999 <- PM25DataCoalDatabyYear[PM25DataCoalDatabyYear$year == 1999, 2]
        
        DifferencePM25DataCoalData <- PM25DataCoalDatabyYear2008 - PM25DataCoalDatabyYear1999
        DifferencePM25DataCoalData
}