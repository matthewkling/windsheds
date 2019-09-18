
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
coordinates(s) <- c("lon", "lat")
ll <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(s) <- ll

bg <- "black"

downwind <- readRDS("data/downwind.rds")
upwind <- readRDS("data/upwind.rds")

   ui <- navbarPage("windscapes [beta]",
                 theme = shinytheme("slate"),
                 tabPanel("tool",
                          
                          tags$style(type = "text/css", 
                                     "#map {height: calc(100vh - 80px) !important;}
                                     
                                     .leaflet-container { background: #000000; }
                                     
                                     .shiny-notification {position:fixed;
                                     height: 50px;width: 250px;
                                     top: calc(50% - 50px);; left: calc(50% - 50px);;}
                                     "),
                          
                          column(2,
                                 selectInput("direction", "windshed type",
                                             c("downwind (outbound)", "upwind (inbound)")),
                                 selectInput("colortrans", "color ramp transformation",
                                             c("square root", "linear", "log10")),
                                 selectInput("palette", "color palette",
                                             c("rainbow", "proton", "verdant", "bluetooth", "vivelafrance")),
                                 sliderInput("opacity", "color opacity", 0, 1, .5),
                                 sliderInput("contours", "number of contours", 1, 100, 50)
                          ),
                          
                          column(10,
                                 leafletOutput("map")
                          )
                 ),
                 tabPanel("about",
                          "This tool geneates maps of 'windsheds'.",
                          "Just as any location on a landscape has an upstream watershed and a downstream delta,",
                          "it also has an upwind and downwind dispersal catchment representing areas of likely inbound and outbound wind dispersal.",
                          br(), br(),
                          "Click the map to select a location and view its wind dispersal landscape.",
                          "These values are measured in 'wind hours' -- the time to reach a given location traveling by suface winds.",
                          "The model integrates over decades of hourly wind data to estimate long-term average wind travel times between locations.",
                          br(), br(),
                          "Created by Matthew Kling.")
                 
)

server <- function(input, output) {
   
   site <- reactiveValues(point = s, ll=coordinates(s))
   
   observeEvent(input$map_click, {
      s <- data.frame(lat=input$map_click$lat, lon=input$map_click$lng)
      coordinates(s) <- c("lon", "lat")
      crs(s) <- ll
      site$point <- s
      site$ll <- coordinates(s)
   })
   
   windshed <- reactive({
      trans <- switch(input$direction,
                      "downwind (outbound)" = downwind,
                      "upwind (inbound)" = upwind)
      w <- accCost(trans, site$ll) %>% "/"(3600)
      d1 <- crop(w, extent(-360, 0, -90, 90)) %>% shift(360)
      d2 <- crop(w, extent(0, 360, -90, 90))
      w <- min(stack(d1, d2)) %>% rotate()
      w
   })
   
   output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 2, maxZoom = 7)) %>%
         setView(lng=coordinates(s)[1], lat=coordinates(s)[2], zoom=4) %>%
         addProviderTiles(providers$CartoDB.DarkMatter#,
                          #options = providerTileOptions(minZoom = 4, maxZoom = 7)
         )
   })
   
   observe({
      withProgress(message = "PROCESSING -- STAND BY...", 
                   value = 1, {
                      w <- windshed()
                      
                      title <- switch(input$colortrans,
                                      "square root" = "sqrt(hours)",
                                      "linear" = "wind hours",
                                      "log10" = "log10(hours+1)")
                      trans <- switch(input$colortrans,
                                      "square root" = sqrt,
                                      "linear" = identity,
                                      "log10" = function(x) log10(x+1))
                      w <- trans(w)
                      
                      colors <- switch(input$palette,
                                       "rainbow" = c("cyan", "turquoise", "limegreen", "yellow", "red", "darkorchid", "black"),
                                       "proton" = c("cyan", "magenta", "darkred", "black"),
                                       "verdant" = c("yellowgreen", "green", "forestgreen", "darkgreen", "black"),
                                       "bluetooth" = c("darkblue", "dodgerblue", "lightblue", "white"),
                                       "clownfish" = c("orange", "white"),
                                       "vivelafrance" = c("darkblue", "white", "darkred"))
                      
                      pal <- colorNumeric(colors,
                                          domain = c(0, max(values(w), na.rm=T)),
                                          na.color = "transparent")
                      
                      contours <- rasterToContour(w, maxpixels=1000000, nlevels=input$contours)
                      contours <- map(seq(-720, 720, 360), function(x) shift(contours, x)) %>%
                         do.call("rbind", .)
                      
                      leafletProxy("map") %>%
                         clearMarkers() %>%
                         clearImages() %>%
                         clearShapes() %>%
                         clearControls() %>%
                         addRasterImage(w, color=pal, opacity=input$opacity) %>%
                         addPolylines(data=contours, color="white", weight=1, opacity=.5) %>%
                         addAwesomeMarkers(lng=site$ll[1], lat=site$ll[2],
                                           icon=makeAwesomeIcon("send", markerColor="darkred"),
                                           layerId=site$active) %>%
                         addLegend(title=title, pal=pal, bins=10, 
                                   values=seq(0, max(values(w), na.rm=T), length.out=100))
                   })
      
   })
   
}

shinyApp(ui = ui, server = server)