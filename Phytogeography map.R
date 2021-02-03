
library(shiny)
library(leaflet)
library(rgdal)
install.packages("shinydashboard")
library(shinydashboard)

regiones_fito <- readOGR(dsn = "C:/.../DIR_ESTAD_CBA_RegFitogeograf_LatLong.shp")
bins <- c(1:9)
pal <- colorBin("RdYlBu", domain = as.integer(regiones_fito$TEXTUSERID), bins)
lista <- regiones_fito$NOMBRE

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Especies"),
  dashboardSidebar(selectInput("provincia_fito", 
                               label = "Província fitogeográfica:",
                               choices = lista, selected = lista   
                               )),
  dashboardBody(
    fluidRow(width = 12, box(leafletOutput(outputId = "el_mapa")))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   data_input <-reactive(input$provincia_fito) 
   output$el_mapa <- renderLeaflet(
     leaflet() %>%
       addProviderTiles(providers$Stamen.Terrain) %>%
       setView(lat = -31.417, lng = -64.183, zoom = 7) %>%
       addPolygons(data = regiones_fito,
                   color = "#6600000",
                   weight = 1, 
                   label = regiones_fito@data[["NOMBRE"]],
                   fillColor = pal(as.integer(regiones_fito$TEXTUSERID)),
                   highlight = highlightOptions(weight = 5,
                                                color = "#666666",
                                                fillOpacity = 0.4,
                                                bringToFront = F )))
}

# Run the application 
shinyApp(ui = ui, server = server)

