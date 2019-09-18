
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




ui <- dashboardPage(
      #skin = "red",
      dashboardHeader(title = "windscapes"),
      dashboardSidebar(
            selectInput("direction", "windshed",
                        c("downwind (outbound)", "upwind (inbound)")),
            selectInput("colortrans", "color ramp transformation",
                        c("square root", "linear", "log10")),
            selectInput("palette", "color palette",
                        c("rainbow", "proton", "verdant", "bluetooth")),
            sliderInput("opacity", "color opacity", 0, 1, .5),
            sliderInput("contours", "number of contours", 0, 100, 50)
      ),
      dashboardBody(
            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}
                              .skin-blue .main-header .logo {
                                background-color: #525252;
                              }

                                .skin-blue .main-header .navbar {
                                background-color: #525252;
                                }

                                .skin-blue .main-sidebar {
                                background-color: #000000;
                                }

                                .content-wrapper, .right-side {
                                background-color: #000000;
                                }"),
            leafletOutput("map")
      )
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
            leaflet() %>%
                  setView(lng=coordinates(s)[1], lat=coordinates(s)[2], zoom=4) %>%
                  addProviderTiles(providers$CartoDB.DarkMatter)
            #Stamen.TonerLite
            #Esri.WorldGrayCanvas
      })

      observe({

            w <- windshed()

            trans <- switch(input$colortrans,
                            "square root" = sqrt,
                            "linear" = identity,
                            "log10" = function(x) log10(x+1))
            w <- trans(w)

            colors <- switch(input$palette,
                             "rainbow" = c("cyan", "turquoise", "limegreen", "yellow", "red", "darkorchid", "black"),
                             "proton" = c("cyan", "magenta", "darkred", "black"),
                             "verdant" = c("green", "forestgreen", "darkgreen", "black"),
                             "bluetooth" = c("darkblue", "dodgerblue", "lightblue", "white"))

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
                  addRasterImage(w, color=pal, opacity=input$opacity) %>%
                  addPolylines(data=contours, color="white", weight=1, opacity=.5) %>%
                  addAwesomeMarkers(lng=site$ll[1], lat=site$ll[2],
                                    icon=makeAwesomeIcon("send", markerColor="darkred"),
                                    layerId=site$active)
      })



}

# Run application
shinyApp(ui = ui, server = server)

