library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)
library(plotly)  # Load plotly

# Base data
base_data <- data.frame(
  Subwatershed = c("1", "1", "1", "1", "1", "1", 
                   "2", "2", "2", "2", "2", "2"),
  Surface_type = c("Local Roads", "Collector & Arterial Roads", "Residential Parking",
                   "Commercial Parking", "Industrial Parking",
                   "Institutional Parking"),
  stringsAsFactors = FALSE
)

# Shiny app
ui <- fluidPage(
  titlePanel("Salt Loading - WHR Redside Dace CH"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("D3", "Parking application rate (g/m2):", min = 27, max = 90, value = 27, step = 10),
      sliderInput("G3", "Road application rate (kg/lane-length km):", min = 88, max = 130, value = 88, step = 10)
    ),
    mainPanel(
      plotlyOutput("modelPlot")  # Use plotlyOutput for interactivity
    )
  )
)

# Define surface order for the legend
surface_levels <- c("Commercial Parking", "Industrial Parking", 
                    "Institutional Parking", "Residential Parking",
                    "Collector & Arterial Roads", "Local Roads")

server <- function(input, output) {
  # Reactive dataset based on slider inputs and base_data
  reactive_data <- reactive({
    calculated_data <- base_data %>%
      mutate(
        Total_Loading_kg = case_when(
          Subwatershed == "1" & Surface_type == "Local Roads" ~ input$G3 * 144.9552135,
          Subwatershed == "1" & Surface_type == "Collector & Arterial Roads" ~ input$G3 * 168.2377,
          Subwatershed == "1" & Surface_type == "Residential Parking" ~ ((input$D3/1000) * 472721.1),
          Subwatershed == "1" & Surface_type == "Commercial Parking" ~ ((input$D3/1000) * 155790.7795),
          Subwatershed == "1" & Surface_type == "Industrial Parking" ~ ((input$D3/1000) * 1180558.159),
          Subwatershed == "1" & Surface_type == "Institutional Parking" ~ ((input$D3/1000) * 142997.1846),
          
          Subwatershed == "2" & Surface_type == "Local Roads" ~ input$G3 * 308.276,
          Subwatershed == "2" & Surface_type == "Collector & Arterial Roads" ~ input$G3 * 622.337,
          Subwatershed == "2" & Surface_type == "Residential Parking" ~ ((input$D3/1000) * 1981128.27),
          Subwatershed == "2" & Surface_type == "Commercial Parking" ~ ((input$D3/1000) * 273194.2226),
          Subwatershed == "2" & Surface_type == "Industrial Parking" ~ ((input$D3/1000) * 542477.697),
          Subwatershed == "2" & Surface_type == "Institutional Parking" ~ ((input$D3/1000) * 282493.9978)
        )
      )
    
    total_loading_subwatershed <- calculated_data %>% 
      group_by(Subwatershed) %>% 
      summarize(Total_Loading = sum(Total_Loading_kg), .groups = "drop")
    
    calculated_data %>%
      left_join(total_loading_subwatershed, by = "Subwatershed") %>%
      mutate(
        Proportion = Total_Loading_kg / Total_Loading,
        Proportion_Percentage = scales::percent(Proportion, accuracy = 0.1),
        Surface_type = factor(Surface_type, levels = surface_levels)
      )
  })
  
  # Render interactive plot with plotly
  output$modelPlot <- renderPlotly({
    plot_data <- reactive_data()
    
    p <- ggplot(plot_data, aes(x = Subwatershed, y = Total_Loading_kg, fill = Surface_type, 
                               text = paste("Subwatershed:", Subwatershed,
                                            "<br>Surface Type:", Surface_type,
                                            "<br>Proportion:", Proportion_Percentage))) +
      geom_bar(stat = "identity", width = 0.75) +
      scale_fill_manual(values = c("Local Roads" = "firebrick", 
                                   "Collector & Arterial Roads" = "lightpink2",
                                   "Residential Parking" = "green4", 
                                   "Industrial Parking" = "dodgerblue3",
                                   "Institutional Parking" = "palegreen2",
                                   "Commercial Parking" = "darkslategray2")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      labs(
        x = "Subwatershed",
        y = "Total Loading (kg)",
        fill = "Surface Type"
      ) +
      theme_minimal() +

      # Display total loading for the whole watershed at the top of the stacked bar
      geom_text(aes(label = ifelse(Surface_type == "Commercial Parking", 
                                   scales::comma(Total_Loading), "")), 
                position = position_stack(vjust = 1), 
                size = 5, color = "black") +  # Adjust label position slightly above bars
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
      ) 
    
    # Convert ggplot to plotly and use `text` for tooltips
    ggplotly(p, tooltip = "text")  
  })
}

# Run the app
shinyApp(ui = ui, server = server)




