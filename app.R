#' Shiny UI for the Application
#'
#' This function creates the user interface (UI) for the Shiny application, allowing users to search for a location and displaying its latitude and longitude on a map.
#'
#' @title Shiny UI for the Application
#' @description This UI includes a text input box for entering the location, a search button, and a map display area.


library(shiny)
library(leaflet)
library(jsonlite)
library(rvest)

library(lab5Package)



ui<-fluidPage(
  titlePanel("Looking for a place?"),
  sidebarLayout(
    sidebarPanel(
      h3(textInput(inputId = "begin",label = "Enter here")),
      actionButton(inputId = "Find",label = "Search")
    ),
    
    # The map along with the geographical coordinates.
    mainPanel(
      tags$style("#Location {font-size:16px;
               color:darkblack;
                 display:block; }"),     
      div(style="text-align:center;
        box-shadow: 4px 4px 5px #888888;
          width:250px;
          height:200px;
          padding-top:100px;
          position:relative;",
          textOutput(outputId = "Location")),
      div(style="text-align:center;
          width:800px;
          height:800px;
          padding-top:100px;
          position:relative;",
          leafletOutput(outputId = "mymap"))
    )
  )
)

# Specifying the server logic to be displayed on the map.
server <- function(input, output) {
  lat_long<-function(loca_tion)
  {
    loca_tion <- gsub(" ", "+", loca_tion) # replacing 'space' with '+' to get the location
    X<-list(address=loca_tion) 
    url_lnk <- "https://nominatim.openstreetmap.org/search?q="
    get_coords = paste0(url_lnk, loca_tion, "&format=geojson")
    
    response <- read_html(get_coords) %>%
      html_node("p") %>%
      html_text() %>%
      fromJSON()
    
    lat <- response$features$geometry$coordinates[[1]][2]
    lon <- response$features$geometry$coordinates[[1]][1]
    
    
    list("latitude"=lat, "longitude" =lon)
  }
  reactivedata = reactive({
    return(lat_long(input$begin))
  })
  
  
  observeEvent(input$Find,{
    
    
    
    x1=reactivedata()
    
    lat <- as.numeric(unname(x1[[1]]))
    lng <- as.numeric(unname(x1[[2]]))
    
    o1<-paste("Latitude:",lat,sep="")
    o2<-paste("Longitude:",lng,sep="")
    
    output$Location <- renderText(paste(c(o1,o2)))
    
    icon.fa <- makeAwesomeIcon(
      icon = "flag", markerColor = "red",
      library = "fa",
      iconColor = "black"
    )
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(
          "OpenStreetMap",
          # give the layer a name
          group = "OpenStreetMap"
        ) %>%
        addAwesomeMarkers(
          lat = lat,
          lng = lng,
          label = "Here",
          icon = icon.fa
        )
      
      
    })
  })
}

# Run the shinyApp
shinyApp(ui = ui, server = server)

