library(shiny)
library(rCharts)
library(plyr)
#setwd('C:/Users/patlaf/Dropbox/github/DataProduct/')

## Create a tidier dataset
house <- read.csv('http://patlaf.com/house.csv')
house <- house[,-1]
house <- house[complete.cases(house),]
house <- house[,-c(5,6)]
house <- house[house$YearConstruction > 1940,]
house <- cbind(house,cluster = kmeans(house[,c('longitude', 'latitude')],2,nstart=25)$cluster, row.names = NULL)


## Aggregate the price of the house based on the "Group House" input
averagePrice <- function(input,house) {
    if(input$groupAggregate == 1) {
        t <- aggregate(house$Price, by=list(house$NbrBedroom, house$cluster), mean)
        t$NbrHouse <- count(house,c('NbrBedroom','cluster'))$freq
        n <- 'Number of Bedrooms'
    } else if (input$groupAggregate == 2) {
        t <- aggregate(house$Price, by=list(house$YearConstruction, house$cluster), mean)
        t$NbrHouse <- count(house,c('YearConstruction','cluster'))$freq
        n <- 'Year of Construction'
    } else if (input$groupAggregate == 3) {
        t <- aggregate(house$Price, by=list(house$NbrBathroom, house$cluster), mean)
        t$NbrHouse <- count(house,c('NbrBathroom','cluster'))$freq
        n <- 'Number of Bathroom'
    } else {
        t <- aggregate(house$Price, by=list(house$NbrLevel, house$cluster), mean)
        t$NbrHouse <- count(house,c('NbrLevel','cluster'))$freq
        n <- 'Number of Level'
    }
    names(t) <- c(n,'Cluster' ,'Mean Price','Number of Houses')
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
    splitCluster(input$NbrCluster,house)                
}


## create cluster
splitCluster <- function(k, house) {
    house$cluster = kmeans(house[,c('longitude', 'latitude')],k,nstart=25)$cluster
    house
}

## Output the value
shinyServer(function(input, output) {
    
    output$datasetInfo <- renderPrint(datasetInfo(house))
    output$filteredDatasetInfo <- renderPrint(datasetInfo(subsetHouse(input,house)))
        
    output$Price <- renderTable(averagePrice(input, subsetHouse(input,house)))
    
    output$graph <- renderChart2({
        t2 <- averagePrice(input, subsetHouse(input,house))
        names(t2) <- c('Group','Cluster','Price')
        x1 <- nPlot(Price~Group, type="lineChart", group= 'Cluster', data=t2)
        x1$yAxis(axisLabel = 'Price', tickFormat = "#! function(d) {return '$' + d} !#")
        return(x1)
    })

    output$dataset <- renderDataTable(subsetHouse(input,house))
    
})
    