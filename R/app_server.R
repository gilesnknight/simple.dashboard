#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
#' @import dplyr 
#' @noRd
#' 


# IMPORT DATA --------------------------------------------------------------

PER_SSC_GEO <- sf::st_read("inst/extdata/PER_SSC_GEO.gpkg") # Map data only
PER_SSC_DATA <- base::readRDS("inst/extdata/PER_SSC_DATA.rds") # Full data

MEL_SSC_GEO <- sf::st_read("inst/extdata/MEL_SSC_GEO.gpkg")
MEL_SSC_DATA <- base::readRDS("inst/extdata/MEL_SSC_DATA.rds")

SYD_SSC_GEO <- sf::st_read("inst/extdata/SYD_SSC_GEO.gpkg")
SYD_SSC_DATA <- base::readRDS("inst/extdata/SYD_SSC_DATA.rds")

# Server ------------------------------------------------------------------

app_server <- function( input, output, session ) {
  


# PER SSC -----------------------------------------------------------------
  
# PER SSC map
  
  # Output basemap only
  output$PER_SSC_map <- renderLeaflet({
    base_map()
  })
  
  # Add suburb polygons
  map_add_polys(
    df = PER_SSC_GEO,
    mapID = "PER_SSC_map",
    structureID = PER_SSC_GEO$SSC_CODE16,
    structureName = PER_SSC_GEO$SSC_NAME16,
    structureTree = PER_SSC_GEO$PerAnyTree,
    structureShrub = PER_SSC_GEO$PerGrass,
    structureGrass = PER_SSC_GEO$PerShrub,
    minZoom = 8,
    maxZoom = 14,
    lng1 = 115.191,
    lat1 = -31.243,
    lng2 = 116.743,
    lat2 = -33.116,
    viewLng = 115.850,
    viewLat = -32.100,
    viewZoom = 10
  )
  
 # Piechart
   
  # Initial suburb data plotted for pie chart - suburb ID 51218
  PER_SSC_pie_data <- filter_piechart(
    filter_SSC(
      df = PER_SSC_DATA,
      uniqueID = PER_SSC_DATA$SSC_CODE16,
      clickID = 51218
    ),
    columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
  )
  
  # Plot the initial pie chart
  output$PER_vegtype_pie <- plotly::renderPlotly({
    vegtype_pie(df = PER_SSC_pie_data,
                pieVals = PER_SSC_pie_data$percent)
  })
  
  observe({
    
    if (!is.null(PER_map_SSC$click)) {
      
      # Filters full data to row of clicked suburb
      PER_SSC_pie_data <- filter_SSC(
        df = PER_SSC_DATA,
        uniqueID = PER_SSC_DATA$SSC_CODE16,
        clickID = PER_map_SSC$click
      )
      
      # Takes filtered row and extracts data just for the veg-type piechart
      PER_vegtype_pie_data <- filter_piechart(
        PER_SSC_pie_data,
        columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
      )
      
      # Updates plotly plot with new data
      vegtype_pie_proxy(plotlyID = "PER_vegtype_pie",
                        df = PER_vegtype_pie_data,
                        pieVals = PER_vegtype_pie_data[["percent"]])
    }
    
  })
  
# Scatter plot  

  # PER SSC filter scatter data
  PER_SSC_scatter_data <- filter_scatter(
    PER_SSC_DATA,
    columnsToPlot = c('SSC_CODE16','SSC_NAME16','PerAnyTree', 'GrDwDens', 'GrDenQuint', 'UrbDwDens', 'UrbDenQuin', 'ResDwDens', 'ResDenQuin'),
    uniqueID = PER_SSC_DATA$SSC_CODE16
  )


  # Renders PER SSC SCATTER GROSS
  output$PER_SSC_gross_scatter <- plotly::renderPlotly({
    scatter_plot(df = PER_SSC_scatter_data,
                 xVals = PER_SSC_scatter_data$PerAnyTree,
                 yVals = PER_SSC_scatter_data$yaxis_ran,
                 source = 'PER_SSC_plot1',
                 structureName = PER_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Gross Density Quintiles</b>",
                 tree = PER_SSC_scatter_data$PerAnyTree,
                 densityQuintile = PER_SSC_scatter_data$GrDenQuint,
                 density = PER_SSC_scatter_data$GrDwDens)
  })
  
  # Updates plot with new data - needs function
  observe({
    if (!is.null(PER_map_SSC$click)){
      selected_quintile <- base::as.numeric(PER_SSC_scatter_data[PER_SSC_scatter_data[['SSC_CODE16']] == PER_map_SSC$click, ]['GrDenQuint'])
      
      PER_SSC_selected_quintile <- filter(PER_SSC_scatter_data, GrDenQuint == selected_quintile)
      
      plotly::plotlyProxy("PER_SSC_gross_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = PER_SSC_selected_quintile$PerAnyTree,
            y = PER_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = PER_SSC_selected_quintile$SSC_NAME16,
              tree = PER_SSC_selected_quintile$PerAnyTree,
              densityQuintile = PER_SSC_selected_quintile$GrDenQuint,
              density = PER_SSC_selected_quintile$GrDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = PER_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = PER_map_SSC$click,
                                      groupID = "GrDenQuint",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = PER_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    groupID = "GrDenQuint",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = PER_SSC_scatter_data, xVals = "PerAnyTree"))))
    } 
    else{
      return(NULL)
    }
  })
  
  # Renders PER SSC SCATTER URBAN
  output$PER_SSC_urban_scatter <- plotly::renderPlotly({
    scatter_plot(df = PER_SSC_scatter_data,
                 xVals = PER_SSC_scatter_data$PerAnyTree,
                 yVals = PER_SSC_scatter_data$yaxis_ran,
                 source = 'PER_SSC_plot1',
                 structureName = PER_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Urban Density Quintiles</b>",
                 tree = PER_SSC_scatter_data$PerAnyTree,
                 densityQuintile = PER_SSC_scatter_data$UrbDenQuin,
                 density = PER_SSC_scatter_data$UrbDwDens)
  })
  
  # Updates plot with new data
  observe({
      selected_quintile <- base::as.numeric(PER_SSC_scatter_data[PER_SSC_scatter_data[['SSC_CODE16']] == PER_map_SSC$click, ]['UrbDenQuin'])
      
      PER_SSC_selected_quintile <- filter(PER_SSC_scatter_data, UrbDenQuin == selected_quintile)
      
      plotly::plotlyProxy("PER_SSC_urban_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = PER_SSC_selected_quintile$PerAnyTree,
            y = PER_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = PER_SSC_selected_quintile$SSC_NAME16,
              tree = PER_SSC_selected_quintile$PerAnyTree,
              densityQuintile = PER_SSC_selected_quintile$UrbDenQuin,
              density = PER_SSC_selected_quintile$UrbDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = PER_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = PER_map_SSC$click,
                                      groupID = "UrbDenQuin",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = PER_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    groupID = "UrbDenQuin",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = PER_SSC_scatter_data, xVals = "PerAnyTree"))))
  })
  
  # Renders PER SSC SCATTER RES
  output$PER_SSC_res_scatter <- plotly::renderPlotly({
    scatter_plot(df = PER_SSC_scatter_data,
                 xVals = PER_SSC_scatter_data$PerAnyTree,
                 yVals = PER_SSC_scatter_data$yaxis_ran,
                 source = 'PER_SSC_plot1',
                 structureName = PER_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Residential Density Quintiles</b>",
                 tree = PER_SSC_scatter_data$PerAnyTree,
                 densityQuintile = PER_SSC_scatter_data$ResDenQuin,
                 density = PER_SSC_scatter_data$ResDwDens)
  })
  
  # Updates plot with new data
  observe({
    if (!is.null(PER_map_SSC$click)){
      selected_quintile <- base::as.numeric(PER_SSC_scatter_data[PER_SSC_scatter_data[['SSC_CODE16']] == PER_map_SSC$click, ]['ResDenQuin'])
      
      PER_SSC_selected_quintile <- filter(PER_SSC_scatter_data, ResDenQuin == selected_quintile)
      
      plotly::plotlyProxy("PER_SSC_res_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = PER_SSC_selected_quintile$PerAnyTree,
            y = PER_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = PER_SSC_selected_quintile$SSC_NAME16,
              tree = PER_SSC_selected_quintile$PerAnyTree,
              densityQuintile = PER_SSC_selected_quintile$ResDenQuin,
              density = PER_SSC_selected_quintile$ResDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = PER_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = PER_map_SSC$click,
                                      groupID = "ResDenQuin",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = PER_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    groupID = "ResDenQuin",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = PER_SSC_scatter_data, xVals = "PerAnyTree"))))
    } 
    else{
      return(NULL)
    }
  })  
  
# Map Click ---------------------------------------------------------------
  
  # PER
  
  PER_map_SSC <- reactiveValues(click = vector(mode = 'numeric'))
  PER_map_SSC$click <- 51218
  
  observeEvent(input$PER_SSC_map_shape_click, {
    click <- isolate(input$PER_SSC_map_shape_click)
    isolate({
      PER_map_SSC$click = click$id
    })
    print(PER_map_SSC$click)
  })
  
  
}



