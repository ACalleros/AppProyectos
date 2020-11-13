library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(fuzzyjoin)

distritos <- st_read('distritos.gpkg') %>% 
  #mutate_if(is.character, iconv) %>% 
  st_transform(., 4326) %>% 
  st_zm(drop = T)
centros <- st_centroid(distritos)
proyectos <- openxlsx::read.xlsx('~/PMDU/PROYECTOS_FINAL.xlsx') %>% 
  fill(everything()) %>% 
  rename(id_dto = 4)

proyectos <- fuzzy_full_join(distritos, proyectos, match_fun = str_detect) %>% 
  st_as_sf() %>% 
  st_drop_geometry() 
  


# UI ----
ui <- fluidPage(
    titlePanel("Proyectos Estratégicos"),
      sidebarPanel(
        selectInput("estrategia",
                    "Proyecto:",
                    unique(distritos$estrategia)),
        selectInput("subsistema",
                    "subsistema:",
                    unique(c('Todos', proyectos$SUBSISTEMA)), 
                    selected = 'Todos'),
        textInput("txt", "Enter the text to display below:"),
        wellPanel(textOutput("cnty"))),
    mainPanel(leafletOutput('map'),
              dataTableOutput('tabla'))
    )

# server ----
server <- function(input, output) { 
  # Mapa ----
  output$map <- renderLeaflet({ if (input$estrategia == 'Gestión ambiental') {
    leaflet() %>% 
      addProviderTiles("Stamen.Toner") %>% 
      addPolygons(data = distritos, 
                  fillColor = "aliceblue", 
                  color = "grey",
                  layerId = ~nombre,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) 
      
  }
    else {
      leaflet() %>% 
        addProviderTiles("Esri.WorldImagery") %>% 
        addPolygons(data = distritos, 
                    fillColor = "aliceblue", 
                    color = "grey",
                    layerId = ~nombre,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE)) %>% 
        fitBounds(-101.33959, 19.61621, -101.08393, 19.77006)
    }
  })
  observe({ 
    event <- input$map_shape_click
    output$cnty <- renderText(distritos$nombre[distritos$nombre == event$id])
    
    
  #TABLA ----
  output$tabla <-  if(input$subsistema !='Todos') {
      renderDataTable(proyectos %>% 
                        #st_drop_geometry() %>% 
                        as.data.frame() %>% 
                        filter(SUBSISTEMA == input$subsistema) %>% 
                        select(nombre, NOMBRE.PROYECTO, DESCRIPCIÓN.GENERAL),
                      options = list(
                        deferRender = TRUE,
                        scrollY = 200,
                        scroller = TRUE)) 
      
  }
    else{
      renderDataTable(proyectos %>% 
                        as.data.frame() %>% 
                        select(nombre, NOMBRE.PROYECTO, DESCRIPCIÓN.GENERAL),
                      options = list(
                        deferRender = TRUE,
                        scrollY = 200,
                        scroller = TRUE))
    }
  })
  
  }
                                  
# shinyApp()
shinyApp(ui = ui, server = server)
