library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(tidyverse)
library(copula)
library(VineCopula)

source("./functions.R")

shapefile_path <- "./src/randomized/Rheda_Wiedebrueck_EFHZFH_Faelle.shp"
purchases <- st_read(shapefile_path)

indices <- get_index_rows()

shape_cop_data <- data_frame(purchases$prei,
                             purchases$flac, 
                             purchases$bauj,
                             purchases$wofl,
                             purchases$stst,
                             purchases$kpwofl,
                             purchases$year,
                             purchases$brwb)

custom_colors <- c("#b31e3b","#06416b","#C8AD55")


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "./style.css")
  ),
  fluidRow(
    column(8,
           leafletOutput("map", height = 500)
    ),
    column(4,
           tabsetPanel(
             tabPanel("Detail Information", uiOutput("infoList"), icon = icon("lock"))
           )
    )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel("Mean Prices", uiOutput(outputId = "plot") ),
      tabPanel("Median Prices", uiOutput(outputId = "plot2")),
      tabPanel("Prediction", uiOutput(outputId = "plot3")),
      tabPanel("Copulas", sidebarLayout(
                            sidebarPanel(
                              selectInput("cop_col1", "Column 1", choices = c("Price"=1, "Property area"=2, "Year of Construction"=3, "Living space area"=4, "Standard level"=5, "Price per m² living space"=6, "Year of purchase"=7, "Land guideline value"=8)),
                              selectInput("cop_col2", "Column 2", choices = c("Price"=1, "Property area"=2, "Year of Construction"=3, "Living space area"=4, "Standard level"=5, "Price per m² living space"=6, "Year of purchase"=7, "Land guideline value"=8)),
                              actionButton("generate_copula", "Generate Copula")
                            ),
                            mainPanel(
                              plotOutput(outputId = "plot4")
                            )
                          )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 8.3, lat = 51.85, zoom = 13) %>%
      addCircleMarkers(data = purchases,
                       lng = purchases$lon,
                       lat = purchases$lat,
                       radius = 5,
                       color = ifelse(purchases$gema =="052540",'#b31e3b',"#06416b"),
                       fillOpacity = 0.7,
                       layerId = ~kfkz) # Falls das Shapefile eine Spalte 'NAME' hat
  })
  
  observeEvent(input$zoom_in, {
    leafletProxy("map") %>% setZoom(zoom = 12)
  })
  
  observeEvent(input$zoom_out, {
    leafletProxy("map") %>% setZoom(zoom = 8)
  })
  
  output$infoList <- renderUI({
    p <- c("Please select a data point on the map to display detailed information")
  })
  
  output$plot3 <- renderUI({
    p <- c("Please select a data point on the map to display detailed information")
  })
  
  output$plot <- renderUI({ 
    p <- 
        calculate_year_mean() |> 
        ggplot() + 
        geom_line(aes(year,mean, colour="City")) +
        geom_line(aes(year,rheda_mean, colour="District Rheda")) + 
        geom_line(aes(year,wieden_mean, colour="District Wiedenbrück")) +
        scale_color_manual(values = custom_colors) +
        labs(colour = "Considered Area")
        
    ggplotly(p) %>%
      plotly::layout(
        xaxis = list(
          title = list(text = "Year", font = list(size = 12))
        ),
        yaxis = list(
          title = list(text ="Mean Price in €", font = list(size = 12))
        ),
        autosize = TRUE,
        margin = list(t = 10, r = 10, b = 10, l = 50)
      )
  }) 
  
  output$plot2 <- renderUI({ 
    p <- 
      calculate_year_median() |> 
      ggplot() + 
      geom_line(aes(year,median, colour="City")) +
      geom_line(aes(year,rheda_median, colour="District Rheda")) + 
      geom_line(aes(year,wieden_median, colour="District Wiedenbrück")) +
      scale_color_manual(values = custom_colors) + 
      labs(colour = "Considered Area")
    
    ggplotly(p)%>%
      plotly::layout(
        xaxis = list(
          title = list(text = "Year", font = list(size = 12))
        ),
        yaxis = list(
          title = list(text ="Median Price in €", font = list(size = 12))
        ),
        autosize = TRUE,
        margin = list(t = 10, r = 10, b = 10, l = 50)
      )
  }) 
  
  observeEvent(input$generate_copula, {
    col1 <- as.numeric(input$cop_col1)
    col2 <- as.numeric(input$cop_col2)
    
    if(col1 == col2){
      rtData <- pobs(shape_cop_data[,c(col1)])
      
      sel <- BiCopSelect(rtData[,1], rtData[,1])
    }else{
      rtData <- pobs(shape_cop_data[,c(col1, col2)])
      
      sel <- BiCopSelect(rtData[,1], rtData[,2])
    }
    
    cop_sel<-VC2copula::BiCop2copula(sel$family, sel$par, sel$par2) 
    output$plot4 <- renderPlot({ 
      persp(cop_sel, dCopula, col="#06416b",border="lightgray", zlab="", xlab="Column 1", ylab="Column 2")
    }) 
  })
  
  observeEvent(input$map_marker_click, { # update the location selectInput on map clicks
    i <- input$map_marker_click
    tmpdat <- purchases[purchases$kfkz==i$id,]
    
    output$plot3 <- renderUI({ 
      
      env <- reactiveFileReader(1000, session, "./src/price_development_model.rda", LoadToEnvironment)
      
      regression_data <- predict_prices(env()[[names(env())[1]]], tmpdat)
      median <- calculate_year_median()
      
      p <- ggplot() + 
        geom_line(data=past_prices_acc_index(tmpdat), aes(year,index_price, colour="Price according to index")) +
        geom_line(data=regression_data[regression_data$year<=2024,], aes(year,pred_price, colour="Linear regression")) +
        geom_line(data=regression_data[regression_data$year>=2024,], aes(year,pred_price, colour="Linear regression"), linetype="dashed") +
        geom_line(data=median[median$year>=tmpdat$year,], aes(year,median, colour="City median")) +
        geom_vline(xintercept=2024, colour="lightgray") +
        scale_color_manual(values = custom_colors) + 
        labs(colour = "Prediction Method")
      
      ggplotly(p)%>%
        plotly::layout(
          xaxis = list(
            title = list(text = "Year", font = list(size = 12))
          ),
          yaxis = list(
            title = list(text ="Predicted Price in €", font = list(size = 12))
          ),
          autosize = TRUE,
          margin = list(t = 10, r = 10, b = 10, l = 50)
        )
    }) 
    
    output$infoList <- renderUI({
      tags$table(
        class="table table-hover",
        tags$tr(
          tags$td("Price"),
          tags$td(paste(tmpdat$prei, "€"))
        ),
        tags$tr(
          tags$td("Adjusted Price"),
          tags$td(paste(tmpdat$adjstd_,"€"))
        ),
        tags$tr(
          tags$td("Year of construction"),
          tags$td(tmpdat$bauj)
        ),
        tags$tr(
          tags$td("Year of purchase"),
          tags$td(tmpdat$year)
        ),
        tags$tr(
          tags$td("Living space"),
          tags$td(paste(tmpdat$wofl, "m²"))
        ),
        tags$tr(
          tags$td("Standard level"),
          tags$td(tmpdat$stst)
        ),
        tags$tr(
          tags$td("Property area"),
          tags$td(paste(tmpdat$flac, "m²"))
        ),
        tags$tr(
          tags$td("Land guideline value"),
          tags$td(paste(tmpdat$brwb))
        )
      )
    })
  })
}

shinyApp(ui, server)
