
library(shiny)
library(shinythemes)
library(leaflet)
library(raster)
library(tidyverse)
library(windscape)
library(gdistance)
library(shinydashboard)

# initalize location
s <- data.frame(lat=37.871444, lon=-122.262277)
s <- data.frame(lat=runif(1, -40, 40), lon=runif(1, -140, 140))
coordinates(s) <- c("lon", "lat")
ll <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(s) <- ll

bg <- "black"

downwind <- readRDS("data/downwind_annual.rds")
upwind <- readRDS("data/upwind_annual.rds")
datasets <- tibble(path = list.files("data", full.names=T, pattern="rds"),
                   info = str_replace_all(path, "data/|\\.rds", "")) %>%
   separate(info, c("direction", "season"))


ui <- navbarPage("windscape [beta]",
                 theme = shinytheme("slate"),
                 tabPanel("tool",
                          
                          tags$head(includeScript("google-analytics.js")),
                          
                          tags$style(type = "text/css", 
                                     "#map {height: calc(100vh - 80px) !important;}
                                     
                                      #image img {max-width: 100%; width: 100%; height: auto}
                                     
                                     .leaflet-container { background: #333333; }
                                     .leaflet-control { background: #333333; color: #ffffff; }
                                     .legend svg text { fill: #ffffff; }
                                     .legend svg line { stroke: #ffffff; }
                                     
                                     .shiny-notification {position:fixed;
                                     height: 50px;width: 250px;
                                     top: calc(50% - 50px);; left: calc(50% - 50px);;}
                                     "),
                          
                          column(2,
                                 selectInput("direction", "windshed direction", unique(datasets$direction)),
                                 selectInput("season", "season", unique(datasets$season)),
                                 selectInput("colortrans", "color ramp transformation",
                                             c("square root", "linear", "log10")),
                                 selectInput("palette", "color palette",
                                             c("<none>", "rainbow", "viridis", "proton", "verdant", "bluetooth", "vivelafrance"),
                                             "rainbow"),
                                 sliderInput("opacity", "color opacity", 0, 1, .5),
                                 sliderInput("contours", "number of contours", 0, 100, 50),
                                 downloadButton("downloadData", "Download raster")
                          ),
                          
                          column(10,
                                 leafletOutput("map")
                          )
                 ),
                 tabPanel( "about",
                           
                           imageOutput("image"),
                           br(),
                           "This tool geneates maps of 'windsheds'.",
                           "Just as any location on a landscape has an upstream watershed and a downstream delta,",
                           "it also has an upwind and downwind dispersal catchment representing areas of likely inbound and outbound wind dispersal.",
                           "Click the map to select a location and view its wind dispersal landscape.",
                           "These values are measured in 'wind hours' -- the time to reach a given location traveling by suface winds.",
                           "The model integrates over decades of hourly wind data to estimate long-term average wind travel times between locations.",
                           br(), br(),
                           "This model uses landscape connectivity algorithms based on graph therory as implemented in the",
                           a("windscape", href="https://github.com/matthewkling/windscape"), 
                           "R package, in combination with decades of hourly wind data from the",
                           a("Climate Forecast System Renanalysis.", href="https://cfs.ncep.noaa.gov/cfsr/"),
                           br(), br(),
                           "Created by", a("Matthew Kling.", href="http://matthewkling.net")
                           
                 )
                 
)

server <- function(input, output) {
   
   # image on about page
   output$image <- renderImage({
      list(src = "www/img.jpg",
           alt = "windscape")
   }, deleteFile = FALSE)
   
   site <- reactiveValues(point = s, ll=coordinates(s))
   
   observeEvent(input$map_click, {
      s <- data.frame(lat=input$map_click$lat, lon=input$map_click$lng)
      coordinates(s) <- c("lon", "lat")
      crs(s) <- ll
      site$point <- s
      site$ll <- coordinates(s)
   })
   
   windshed <- reactive({
      #trans <- datasets %>%
      #   filter(direction == input$direction,
      #          season == input$season) %>%
      #   pull(path) %>%
      #   readRDS()
      trans <- switch(input$direction,
                      "downwind" = downwind,
                      "upwind" = upwind)
      w <- accCost(trans, site$ll) %>% "/"(3600)
      d1 <- crop(w, extent(-360, 0, -90, 90)) %>% shift(360)
      d2 <- crop(w, extent(0, 360, -90, 90))
      w <- min(stack(d1, d2)) %>% rotate()
      w
   })
   
   
   w <- reactive({
      withProgress(message = "PROCESSING:", detail="generating windshed",
                   value = 1, {
                      
                      w <- windshed()
                      trans <- switch(input$colortrans,
                                      "square root" = sqrt,
                                      "linear" = identity,
                                      "log10" = function(x) log10(x+1))
                      trans(w)
                   })
   })
   
   # leaflet basemap
   output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 2, maxZoom = 7)) %>%
         setView(lng=coordinates(s)[1], lat=coordinates(s)[2], zoom=4) %>%
         addProviderTiles(providers$CartoDB.DarkMatter)
   })
   
   # marker for focal site
   observe({
      lat <- input$map_click$lat
      leafletProxy("map") %>%
         clearMarkers() %>%
         addAwesomeMarkers(lng=site$ll[1], lat=site$ll[2],
                           icon=makeAwesomeIcon("send", markerColor="darkred"),
                           layerId=site$active)
   })
   
   # raster layer
   observe({
      
      if(input$palette == "<none>"){
         
         leafletProxy("map") %>%
            clearImages() %>%
            clearControls()
         
      }else{
         invtrans <- switch(input$colortrans,
                            "square root" = function(x) x^2,
                            "linear" = identity,
                            "log10" = function(x) (10^x)-1  )
         
         colors <- switch(input$palette,
                          "rainbow" = c("cyan", "turquoise", "limegreen", "yellow", "red", "darkorchid", "black"),
                          "viridis" = rev(c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")),
                          "proton" = c("cyan", "red", "black"),
                          "verdant" = c("yellowgreen", "green", "forestgreen", "darkgreen", "black"),
                          "bluetooth" = c("darkblue", "dodgerblue", "lightblue", "white"),
                          "clownfish" = c("orange", "white"),
                          "vivelafrance" = c("darkblue", "white", "darkred"))
         
         pal <- colorNumeric(colors,
                             domain = c(0, max(values(w()), na.rm=T)),
                             na.color = "transparent")
         
         leafletProxy("map") %>%
            clearImages() %>%
            clearControls() %>%
            addRasterImage(w(), color=pal, opacity=input$opacity) %>%
            addLegend(title="wind hours", pal=pal, bins=10, opacity=input$opacity,
                      values=seq(0, max(values(w()), na.rm=T), length.out=100),
                      labFormat=labelFormat(transform=invtrans))
         
      }
      
      
      
   })
   
   # contours
   observe({
      withProgress(message = "PROCESSING:", detail="building contours",
                   value = 1, {
                      
                      if(input$contours == 0){
                         leafletProxy("map") %>% 
                            clearShapes()
                      } else{
                         contours <- rasterToContour(w(), maxpixels=1000000, nlevels=input$contours)
                         contours <- map(seq(-720, 720, 360), function(x) shift(contours, x)) %>%
                            do.call("rbind", .)
                         
                         leafletProxy("map") %>%
                            clearShapes() %>%
                            addPolylines(data=contours, color="white", weight=1, opacity=.5)
                      }
                      
                   })
      
   })
   
   # data download
   output$downloadData <- downloadHandler(
      filename = function() {
         paste0("windshed_", 
                round(site$ll[1], 2), "_", round(site$ll[2], 2), 
                "_", input$direction, ".tif")
      },
      content = function(file) {
         writeRaster(windshed(), file)
      }
   )
   
}

shinyApp(ui = ui, server = server)