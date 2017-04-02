#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
coalwind <- read.csv("coalwind.csv")
coalwindIowa <- coalwind[coalwind$State=='Iowa', ]
coalwindKansas <- coalwind[coalwind$State=='Kansas', ]
coalwindMinnesota <- coalwind[coalwind$State=='Minnesota', ]
coalwindNorthDakota <- coalwind[coalwind$State=='NorthDakota', ]
coalwindOklahoma <- coalwind[coalwind$State=='Oklahoma', ]
coalwindTexas <- coalwind[coalwind$State=='Texas', ]

# Define server logic required to draw the plot
shinyServer(function(input, output) {
    
    # subset the data based on state selection
    
    datasetInput <- reactive({
      switch(input$dataset,
             "Iowa" = coalwindIowa,
             "Kansas" = coalwindKansas,
             "Minnesota" = coalwindMinnesota,
             "North Dakota" = coalwindNorthDakota,
             "Oklahoma" = coalwindOklahoma,
             "Texas" = coalwindTexas)
    })
    
    model <- reactive({
      brushed_data <- brushedPoints(datasetInput(), input$brush1,
                                    xvar = "Year", yvar = "PercentGen")
      if(nrow(brushed_data) <2){
        return(NULL)
      }
      lm(Year ~ PercentGen, data = brushed_data)
    })
    output$slopeOut <- renderText({
      if(is.null(model())){
        "No Model Found"
      } else {
        model()[[1]][2]
      }
    })
    
    output$intOut <- renderText({
      if(is.null(model())){
        "No Model Found"
      } else {
        model()[[1]][1]
      }
    })
    
    # draw the plot
    
    output$coalWindPlot <- renderPlot({
    
    wind <- datasetInput()[datasetInput()$Fuel=='Wind', ]
    coal <- datasetInput()[datasetInput()$Fuel=='Coal', ]
      
    plot(wind$Year, wind$PercentGen,
         ylim=c(0,1),
         xlab="Year",
         ylab="Percentage of Electricity Generation",
         col="blue",
         cex=2)
    points(coal$Year, coal$PercentGen, col="black", pch=4, cex=2)
    legend(2014, 0.95, c("Coal", "Wind"), col = c("black","blue"), pch=c(4,1))
  })
  
})