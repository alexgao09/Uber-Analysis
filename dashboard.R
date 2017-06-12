library(shiny)
library(leaflet)
library(shinydashboard)

source("uber_analysis.R")

tolerance <- 3000 # max number to plot and perform k means clustering on
min_cl_num <- 5 # minimum number of clusters
max_cl_num <- 20 # maximum number of clusters


ui = dashboardPage(
  dashboardHeader(title = "Uber rides analysis"),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
  
    sidebarPanel(h4("K-means clustering analysis varying over the date"),
                 
    sliderInput("slider", "Date Index:",
                value = 1, min = 1, max = 184),
    
    sliderInput("kmc", "Number of Clusters:",
                value = min_cl_num, min = min_cl_num, max = max_cl_num),
    
    plotOutput("histhr")),
  
  mainPanel(
    leafletOutput("map")
  ),
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")

))


server <- shinyServer(function(input, output) {
  p=apr_to_sep_dat
  
  pal <- colorFactor( # define palette for points
    palette = "Dark2",
    domain = factor(p$hr_map))
  
  output$map = renderLeaflet({
    leaflet() %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>% # layout
      
      addCircleMarkers(data = p[first_indices[input$slider]:(first_indices[input$slider]+
                                                               min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),], # adds start locations
                       lat = ~ Lat, lng = ~ Lon,
                       fillOpacity=1, 
                       radius=0.5,
                       color= ~pal(hr_map)) %>%
      
       addMarkers(data=data.frame(kmeans(p[first_indices[input$slider]:(first_indices[input$slider]+
                                                               min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),c("Lat","Lon")],
                              input$kmc)$centers),
                  lat = ~Lat, lng = ~ Lon,
                  label= ~ paste("Latitude:", round(Lat,3), "Longitude", round(Lon,3))) %>%
      
      addLegend("bottomright", pal = pal, values = p[first_indices[input$slider]:(first_indices[input$slider]+
                                                                                    min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),"hr_map"],
                title = paste("Date:", p[first_indices[input$slider],"Date"]),
                opacity = 1) %>%
      
      setView(lng = -73.98928, lat = 40.75042, zoom = 12) # sets zoom to be in NYC
  })
  
  output$histhr <- renderPlot({
    
    ggplot(data=data.frame(count(p[first_indices[input$slider]:(first_indices[input$slider]+
                                                                  min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),"hr_map"])), aes(x=x, y=freq,fill=I("red"),col=I("blue"))) +  
      geom_bar(stat="identity", position="dodge", width=0.9, alpha=0.3)+
      theme(text = element_text(size = 11), 
            axis.line = element_line(colour = "black"), 
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.background = element_rect(fill = "grey92"))+ # Very important line - makes it look nice
      ggtitle(paste("Histogram of ride frequency for date:", p[first_indices[input$slider],"Date"]))

  })
})



runApp(shinyApp(ui, server),launch.browser = TRUE) # Create app



