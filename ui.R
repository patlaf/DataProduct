library(shiny)
library(rCharts)
#setwd('C:/Users/patlaf/Dropbox/github/DataProduct/')

## Create a tidier dataset
house <- read.csv('http://patlaf.com/house.csv')
house <- house[,-1]
house <- house[complete.cases(house),]
house <- house[house$YearConstruction > 1940,]

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Mean House Price"),
    
    sidebarPanel(
        p('Find the code at http://github.com/patlaf/DataProduct'),
        h4('Initial house dataset summary:'),
        verbatimTextOutput('datasetInfo'),
        
        hr(),
        
        h3('Filter Dataset'),
        p('Use the slider to filter the number of houses'),
        sliderInput("year","Construction Years ", min=min(house$YearConstruction), max=max(house$YearConstruction), value=c(1990,2014)),
        sliderInput("bedroom","Maximum Number of Bedrooms:",min = min(house$NbrBedroom),max = max(house$NbrBedroom),value = 3),
        
        h3('Group Houses'),
        p('Select how the price will by aggregated'),
        selectInput("groupAggregate", label = 'Group Houses by ', 
                    choices = list("Number of Bedroom" = 1, "Number of Bathroom" = 3, "Number of Level" = 4,"Year of Construction" = 2),selected = 1),
        hr(),
        
        h4('Base on selection, the house dataset is now:'),
        verbatimTextOutput('filteredDatasetInfo')
    ),
        
    mainPanel(
        p('An application to show the mean price of houses depending on the number of bedrooms, bathrooms, levels or year of construction'),
        
        h3('Mean Price'),
        h5('Base on the filter on the left (in the menu): '),
        htmlOutput('Price'),
        showOutput('graph','nvd3'),
        
        
        h3('Filtered Dataset'),
        dataTableOutput('dataset')
    )
    
))