# Define UI
ui <- fluidPage(
  titlePanel("Energy Consumption Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Select plot type
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Building ID vs. Total Energy Consumption",
                              "Building ID vs. Total Predicted Energy Consumption",
                              "Square feet vs. Total Energy Consumption",
                              "Square feet vs. Total Predicted Energy Consumption",
                              "Bedrooms vs. Total Energy Consumption",
                              "Bedrooms vs. Total Predicted Energy Consumption",
                              "Quarters vs. Total Energy Consumption",
                              "Quarters vs. Total Predicted Energy Consumption"),
                  selected = "Building ID vs. Total Energy Consumption")
    ),
    
    mainPanel(
      plotOutput("energy_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Function to create plots based on selected plot type
  output$energy_plot <- renderPlot({
    # Retrieve the selected plot type
    plot_type <- input$plot_type
    
    # Create the selected plot
    if (plot_type == "Building ID vs. Total Energy Consumption") {
      # Plot 1: Building ID vs. Total Energy Consumption (Observed)
      ggplot(complete_merged_data, aes(x = factor(bldg_id), y = total_energy)) +
        geom_line() +
        labs(x = "Building ID", y = "Total Energy Consumption") +
        ggtitle("Building ID vs. Total Energy Consumption") +
        ylim(y_limits) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else if (plot_type == "Building ID vs. Total Predicted Energy Consumption") {
      # Plot 2: Building ID vs. Total Predicted Energy Consumption
      ggplot(temp_incr, aes(x = factor(bldg_id), y = predicted_total_energy)) +
        geom_line() +
        labs(x = "Building ID", y = "Total Predicted Energy Consumption") +
        ggtitle("Building ID vs. Total Predicted Energy Consumption") +
        ylim(y_limits) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else if (plot_type == "Square feet vs. Total Energy Consumption") {
      # Plot 3: Square feet vs. Total Energy Consumption
      ggplot(complete_merged_data, aes(x = factor(in.sqft), y = total_energy)) +
        geom_point() +
        labs(x = "Square feet", y = "Total Energy Consumption") +
        ggtitle("Square feet vs. Total Energy Consumption") +
        ylim(y_limits)
    } else if (plot_type == "Square feet vs. Total Predicted Energy Consumption") {
      # Plot 4: Square feet vs. Total Predicted Energy Consumption
      ggplot(temp_incr, aes(x = factor(in.sqft), y = predicted_total_energy)) +
        geom_point() +
        labs(x = "Square feet", y = "Total Predicted Energy Consumption") +
        ggtitle("Square feet vs. Total Predicted Energy Consumption") +
        ylim(y_limits)
    } else if (plot_type == "Bedrooms vs. Total Energy Consumption") {
      # Plot 5: Bedrooms vs. Total Energy Consumption
      ggplot(complete_merged_data, aes(x = factor(in.bedrooms), y = total_energy)) +
        geom_point() +
        labs(x = "Bedrooms", y = "Total Energy Consumption") +
        ggtitle("Bedrooms vs. Total Energy Consumption") +
        ylim(y_limits)
    } else if (plot_type == "Bedrooms vs. Total Predicted Energy Consumption") {
      # Plot 6: Bedrooms vs. Total Predicted Energy Consumption
      ggplot(temp_incr, aes(x = factor(in.bedrooms), y = predicted_total_energy)) +
        geom_point() +
        labs(x = "Bedrooms", y = "Total Predicted Energy Consumption") +
        ggtitle("Bedrooms vs. Total Predicted Energy Consumption") +
        ylim(y_limits)
    } else if (plot_type == "Quarters vs. Total Energy Consumption") {
      # Plot 7: Quarters vs. Total Energy Consumption
      ggplot(complete_merged_data, aes(x = quarter, y = total_energy)) +
        geom_bar(stat = "identity", fill = "green", na.rm = TRUE) +
        labs(x = "Quarters", y = "Total Energy Consumption") +
        ggtitle("Quarters vs. Total Energy Consumption") +
        ylim(y_limits)
    } else if (plot_type == "Quarters vs. Total Predicted Energy Consumption") {
      # Plot 8: Quarters vs. Total Predicted Energy Consumption
      ggplot(temp_incr, aes(x = quarter, y = predicted_total_energy)) +
        geom_bar(stat = "identity", fill = "skyblue", na.rm = TRUE) +
        labs(x = "Quarters", y = "Total Predicted Energy Consumption") +
        ggtitle("Quarters vs. Total Predicted Energy Consumption") +
        ylim(y_limits)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
