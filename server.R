library(shiny)
library(rCharts)

#setwd('C:/Users/patlaf/Dropbox/github/DataProduct/')

## Create a tidier dataset
house <- read.csv('http://patlaf.com/house.csv')
house <- house[,-1]
house <- house[complete.cases(house),]
house <- house[house$YearConstruction > 1940,]


## Aggregate the price of the house based on the "Group House" input
averagePrice <- function(input,house) {
    if(input$groupAggregate == 1) {
        t <- aggregate(house$Price, by=list(house$NbrBedroom), mean)
        t$NbrHouse <- as.numeric(table(house$NbrBedroom))
        n <- 'Number of Bedrooms'
    } else if (input$groupAggregate == 2) {
        t <- aggregate(house$Price, by=list(house$YearConstruction), mean)
        t$NbrHouse <- as.numeric(table(house$YearConstruction))
        n <- 'Year of Construction'
    } else if (input$groupAggregate == 3) {
        t <- aggregate(house$Price, by=list(house$NbrBathroom), mean)
        t$NbrHouse <- as.numeric(table(house$NbrBathroom))
        n <- 'Number of Bathroom'
    } else {
        t <- aggregate(house$Price, by=list(house$NbrLevel), mean)
        t$NbrHouse <- as.numeric(table(house$NbrLevel))
        n <- 'Number of Level'
    }
    names(t) <- c(n,'Mean Price','Number of Houses')
    t
}

## Return the number of row, mean number of bedrooms and mean construction year
datasetInfo <- function(house) {
    t <- data.frame('NbrRows'=0,'MeanBedroom'=0,'MeanConstructionYear'=0)
    t$NbrRows <- nrow(house)
    t$MeanBedroom <- mean(house$NbrBedroom)
    t$MeanConstructionYear <- mean(house$YearConstruction)
    t
}

## Subset the dataset based on the input form
subsetHouse <- function(input,house) {
    house <- subset(house,NbrBedroom <= input$bedroom)
    house <- subset(house,(YearConstruction >= as.numeric(input$year[1]) & YearConstruction <= as.numeric(input$year[2])))
    house                
}


## Output the value
shinyServer(function(input, output) {
    
    output$datasetInfo <- renderPrint(datasetInfo(house))
    output$filteredDatasetInfo <- renderPrint(datasetInfo(subsetHouse(input,house)))
        
    output$Price <- renderTable(averagePrice(input, subsetHouse(input,house)))
    
    output$graph <- renderChart2({
        t2 <- averagePrice(input, subsetHouse(input,house))
        names(t2) <- c('Group','Price')
        x1 <- nPlot(Price~Group, type="lineChart", data=t2)
        x1$yAxis(axisLabel = 'Price', tickFormat = "#! function(d) {return '$' + d} !#")
        return(x1)
    })

    output$dataset <- renderDataTable(subsetHouse(input,house))
    
})
    