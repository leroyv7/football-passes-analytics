library(shiny)
library(tidyverse)
library(scales)
library(ggsoccer)
library(plotly)

theme_set(
  theme_minimal(base_size = 13)
)
# =========================
# Load datasets
# =========================

progressive_data <- readRDS(
  "combined_progressive_lane_counts.rds"
)

lane_data <- readRDS(
  "combined_lane_counts.rds"
)

pass_origins <- readRDS(
  "combined_pass_origins.rds"
)

pass_destinations <- readRDS(
  "combined_pass_destinations.rds"
)

progressive_origins <- readRDS(
  "combined_progressive_origins.rds"
)

# =========================
# Team colours
# =========================

team_colours <- c(
  "Bayern Munich" = "#4A0015",
  "Borussia Mönchengladbach" = "#2E5E3B",
  "Arsenal" = "#EF0107",
  "Manchester City" = "#6CABDD",
  "Juventus" = "#000000",
  "AS Roma" = "#6B0F1A",
  "Paris Saint-Germain" = "#001F3F",
  "Lyon" = "#007FFF",
  "Barcelona" = "#004D98",
  "Real Madrid" = "#C9A227"
)

# =========================
# UI
# =========================

ui <- fluidPage(
  
  titlePanel(
    "Elite Possession Teams Dashboard"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 2,
      
      selectInput(
        "team",
        "Choose a team",
        choices = unique(progressive_data$team)
      ),
      
      hr(),
      
      p(
        "This dashboard profiles how elite possession teams used vertical lanes during the 2015/16 domestic season."
      ),
      
      p(
        "Use the team selector tab to compare passing distribution, progressive passing, and spatial passing patterns."
      )
      
    ),
    
    mainPanel(
      width = 10,
      
      fluidRow(
        
        column(
          width = 4,
          wellPanel(
            style = "padding:12px;",
            h4("Most Used Lane"),
            textOutput("top_lane")
          )
        ),
        
        column(
          width = 4,
          wellPanel(
            style = "padding:12px;",
            h4("Highest Progressive Lane %"),
            textOutput("top_progressive_lane")
          )
        ),
        
        column(
          width = 4,
          wellPanel(
            style = "padding:12px;",
            h4("Total Progressive Passes"),
            textOutput("total_progressive")
          )
        )
        
      ),
      
      
      h4("Distribution Analysis"),
      
      fluidRow(
        column(
          width = 6,
          plotlyOutput("pass_plot", height = "320px")
        ),
        
        column(
          width = 6,
          plotlyOutput("progressive_plot", height = "320px")
        )
      ),
      
      tags$h4(
        "Spatial Passing Patterns",
        style = "margin-top:10px;"
      ),
      
      fluidRow(
        column(
          width = 6,
          plotOutput("origins_plot", height = "340px")
        ),
        
        column(
          width = 6,
          plotOutput("destinations_plot", height = "340px")
        )
      ),
      
      tags$h4(
        "Progressive Passing Origins",
        style = "margin-top:10px;"
      ),
      
      fluidRow(
        column(
          width = 12,
          plotOutput("progressive_origins_plot", height = "340px")
        )
      )
    )
    
  )
  
)

# =========================
# Server
# =========================

server <- function(input, output) {
  
  # -------------------------
  # KPI cards
  # -------------------------
  
  output$top_lane <- renderText({
    
    lane_data %>%
      filter(team == input$team) %>%
      arrange(desc(pct)) %>%
      slice(1) %>%
      mutate(label = paste0(vertical_lane, " (", percent(pct, accuracy = 0.1), ")")) %>%
      pull(label)
    
  })
  
  output$top_progressive_lane <- renderText({
    
    progressive_data %>%
      filter(team == input$team) %>%
      arrange(desc(pct)) %>%
      slice(1) %>%
      mutate(label = paste0(vertical_lane, " (", percent(pct, accuracy = 0.1), ")")) %>%
      pull(label)
    
  })
  
  output$total_progressive <- renderText({
    
    progressive_origins %>%
      filter(team == input$team) %>%
      nrow() %>%
      comma()
    
  })
  
  
  # -------------------------
  # Pass distribution
  # -------------------------
  
  output$pass_plot <- renderPlotly({
    
    filtered_data <- lane_data %>%
      filter(team == input$team)
    
    p <- ggplot(
      filtered_data,
      aes(
        x = factor(
          vertical_lane,
          levels = c(
            "Left Wing",
            "Left Half-Space",
            "Centre",
            "Right Half-Space",
            "Right Wing"
          )
        ),
        y = pct,
        text = paste0(
          vertical_lane,
          "<br>",
          percent(pct, accuracy = 0.1)
        )
      )
    ) +
      
      geom_col(
        width = 0.7,
        fill = team_colours[input$team]
      ) +
      
      scale_y_continuous(
        labels = percent_format()
      ) +
      
      labs(
        title = paste(
          input$team,
          "vertical pass distribution"
        ),
        subtitle = "2015/16 domestic league",
        x = NULL,
        y = "Share of passes"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # -------------------------
  # Progressive pass distribution
  # -------------------------
  
  output$progressive_plot <- renderPlotly({
    
    filtered_data <- progressive_data %>%
      filter(team == input$team)
    
    p <- ggplot(
      filtered_data,
      aes(
        x = factor(
          vertical_lane,
          levels = c(
            "Left Wing",
            "Left Half-Space",
            "Centre",
            "Right Half-Space",
            "Right Wing"
          )
        ),
        y = pct,
        text = paste0(
          vertical_lane,
          "<br>",
          percent(pct, accuracy = 0.1)
        )
      )
    ) +
      
      geom_col(
        width = 0.7,
        fill = team_colours[input$team]
      ) +
      
      scale_y_continuous(
        labels = percent_format()
      ) +
      
      labs(
        title = paste(
          input$team,
          "progressive passing structure"
        ),
        subtitle = "2015/16 domestic league",
        x = NULL,
        y = "Share of progressive passes"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # -------------------------
  # Pass origins heatmap
  # -------------------------
  
  output$origins_plot <- renderPlot({
    
    filtered_data <- pass_origins %>%
      filter(team == input$team)
    
    p <- ggplot(
      filtered_data,
      aes(
        x = location.x,
        y = location.y
      )
    ) +
      
      annotate_pitch(
        dimensions = pitch_statsbomb,
        colour = "grey70",
        fill = "#1B1B1B"
      ) +
      
      geom_bin2d(
        bins = 25,
        alpha = 0.85
      ) +
      
      coord_fixed(
        xlim = c(0, 120),
        ylim = c(0, 80)
      ) +
      
      scale_fill_viridis_c(
        name = "Passes"
      ) +
      
      labs(
        title = paste(
          input$team,
          "pass origin heatmap"
        ),
        subtitle = "2015/16 domestic league",
        x = NULL,
        y = NULL
      ) +
      
      theme_pitch() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          colour = "white",
          size = 15
        ),
        plot.subtitle = element_text(
          colour = "white"
        ),
        plot.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        panel.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        legend.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        legend.text = element_text(
          colour = "white"
        ),
        legend.title = element_text(
          colour = "white"
        ),
        legend.position = "right"
      )
    
    print(p)
    
  })
  
  # -------------------------
  # Pass destinations heatmap
  # -------------------------
  
  output$destinations_plot <- renderPlot({
    
    filtered_data <- pass_destinations %>%
      filter(team == input$team)
    
    p <- ggplot(
      filtered_data,
      aes(
        x = pass.end_location.x,
        y = pass.end_location.y
      )
    ) +
      
      annotate_pitch(
        dimensions = pitch_statsbomb,
        colour = "grey70",
        fill = "#1B1B1B"
      ) +
      
      geom_bin2d(
        bins = 25,
        alpha = 0.85
      ) +
      
      coord_fixed(
        xlim = c(0, 120),
        ylim = c(0, 80)
      ) +
      
      scale_fill_viridis_c(
        name = "Passes"
      ) +
      
      labs(
        title = paste(
          input$team,
          "pass destination heatmap"
        ),
        subtitle = "2015/16 domestic league",
        x = NULL,
        y = NULL
      ) +
      
      theme_pitch() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          colour = "white",
          size = 15
        ),
        plot.subtitle = element_text(
          colour = "white"
        ),
        plot.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        panel.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        legend.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        legend.text = element_text(
          colour = "white"
        ),
        legend.title = element_text(
          colour = "white"
        ),
        legend.position = "right"
      )
    
    print(p)
    
  })
  
  # -------------------------
  # Progressive pass origins heatmap
  # -------------------------
  
  output$progressive_origins_plot <- renderPlot({
    
    filtered_data <- progressive_origins %>%
      filter(team == input$team)
    
    p <- ggplot(
      filtered_data,
      aes(
        x = location.x,
        y = location.y
      )
    ) +
      
      annotate_pitch(
        dimensions = pitch_statsbomb,
        colour = "grey70",
        fill = "#1B1B1B"
      ) +
      
      geom_bin2d(
        bins = 25,
        alpha = 0.85
      ) +
      
      coord_fixed(
        xlim = c(0, 120),
        ylim = c(0, 80)
      ) +
      
      scale_fill_viridis_c(
        name = "Progressive passes"
      ) +
      
      labs(
        title = paste(
          input$team,
          "progressive pass origins"
        ),
        subtitle = "2015/16 domestic league",
        x = NULL,
        y = NULL
      ) +
      
      theme_pitch() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          colour = "white",
          size = 15
        ),
        plot.subtitle = element_text(
          colour = "white"
        ),
        plot.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        panel.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        legend.background = element_rect(
          fill = "#1B1B1B",
          colour = NA
        ),
        legend.text = element_text(
          colour = "white"
        ),
        legend.title = element_text(
          colour = "white"
        ),
        legend.position = "right"
      )
    
    print(p)
    
  })
  
}

# =========================
# Run app
# =========================

shinyApp(ui = ui, server = server)