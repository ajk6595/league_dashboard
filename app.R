# Load packages
library(shiny)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(ggthemes)

# Load the data
player <- read_csv("data/player_stats.csv")

# Clean the data
player <- player %>% 
  janitor::clean_names() %>%
  mutate(
    w_percent = as.numeric(sub("%", "", w_percent)),
    kp = as.numeric(sub("%", "", kp)),
    pos = factor(pos, levels = c("Top", "Jungle", "Middle", "ADC", "Support"))
  ) %>%
  select(player, team, pos, w_percent, kda, kp, gd10, xpd10, cspm, dpm) %>%
  arrange(player)

# Returns quartile rank
quartile_rank <- function(x = 0:99) {
  # Set quartile
  quart_breaks <- c(
    -Inf,
    quantile(
      x,
      probs = c(0.25, 0.5, 0.75),
      na.rm = TRUE
    ),
    Inf
  )
  cut(x = x, breaks = quart_breaks, labels = FALSE)
}

# Obtain quartile information
player_quart <- player %>%
  mutate(
    kda_quant = quartile_rank(kda), 
    kp_quant = quartile_rank(kp), 
    gd10_quant = quartile_rank(gd10), 
    xpd10_quant = quartile_rank(xpd10), 
    cspm_quant = quartile_rank(cspm), 
    dpm_quant = quartile_rank(dpm)
  ) %>%
  select(player, contains("_quant")) %>%
  pivot_longer(
    cols = -player,
    names_to = "variable",
    values_to = "value"
  ) %>%
  arrange(player)

# Define UI
ui <- fluidPage(
  # Create app title
  titlePanel("League of Legends: Worlds 2020 Main Event"),
  
  # Sidebar layout with slider input for number of bins
  sidebarLayout(
    position = "right",
    sidebarPanel(
      
      # Create select input widget to select player
      selectInput(
        inputId = "player",
        label = "Select Player:",
        choices = player$player,
        selected = "Ghost"
      ),
      
      # Create radio button widget to select x-axis
      radioButtons(
        inputId = "x_axis",
        label = "Select X-Axis Variable:",
        choices = list(
          "Kill, Death, Assist (KDA)" = "kda",
          "Kill Participation" = "kp",
          "Gold Difference" = "gd10",
          "Exp Difference" = "xpd10",
          "Creep Score Per Minute" = "cspm",
          "Damage Per Minute" = "dpm"
        ),
        selected = "kda"
      ),
      
      # Create radio button widget to select fill
      radioButtons(
        inputId = "fill",
        label = "Select Fill/Legend Variable:",
        choices = list("Team" = "team", "Position" = "pos"),
        selected = "team"
      ),
      
      # Add helper description about the dataset
      br(),
      p(em(strong("Note: ")), em("The dataset originates from League of Legends to examine player statistics from the League of Legends 2020 World Championship. The data can be obtained from Oracle's Elixir, which features .csv files for the statistics from the League of Legends World Championship.")),
      p(em(strong("Source: ")), a("Oracle's Elixir", href = "https://oracleselixir.com/"))
    ),
    
    # Show a plot of the generated distribution 
    mainPanel(
      plotOutput(outputId = "distPlot", width = "100%", height = "800px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Visualize the player's overall performance
  output$distPlot <- renderPlot({
    set.seed(123)
    
    plot1 <- player_quart %>%
      filter(player == input$player) %>%
      ggplot(aes(x = variable, y = value)) +
      geom_col(width = 1, fill = "#F28291") +
      geom_hline(yintercept = 1:4, linetype = "dotted", color = "gray20") +
      geom_segment(x = 0.5:5.5, y = 0, xend = 0.5:5.5, yend = 4) +
      scale_x_discrete(
        NULL,
        expand = c(0, 0),
        limits = c("kp_quant", "dpm_quant", "gd10_quant", "xpd10_quant", "cspm_quant", "kda_quant")
      ) +
      scale_y_continuous(
        NULL,
        expand = c(0, 0),
        limits = c(0, 5)
      ) +
      coord_polar() +
      annotate(
        geom = "text",
        x = 3.5,
        y = c(0.8, 1.8, 2.8, 3.8),
        label = c("1st-25th", "25th-50th", "50th- 75th", "75th-99th"),
        size = 3,
        vjust = "bottom"
      ) +
      annotate(
        geom = "text",
        x = c("kda_quant", "kp_quant", "gd10_quant", "xpd10_quant", "cspm_quant", "dpm_quant"),
        y = c(5, 5, 5, 5, 5, 5),
        label = c("KILL, DEATH\nASSIST (KDA)", "KILL\nPARTICIPATION", "GOLD\nDIFFERENCE", "EXP\nDIFFERENCE", "CS PER\nMINUTE", "DAMAGE\nPER MINUTE"),
        size = 3
      ) +
      labs(
        title = input$player,
        subtitle = paste(
          "Team: ", player %>% filter(player == input$player) %>% pull(team), "\n",
          "Position: ", player %>% filter(player == input$player) %>% pull(pos)
        )
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "grey50", size = 2),
        plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 12, hjust = 0.5)
      )
    
    # Prepare player information
    player_point <- player %>%
      filter(player == input$player)
    
    # Prepare team color information
    player <- player %>%
      mutate(
        team = ifelse(team == player_point %>% pull(team), team, "Other"),
        team = factor(team, levels = c(player_point %>% pull(team), "Other"))
      )
    
    # Create labels
    labels <- c(
      "kda" = "KILL, DEATH, ASSIST (KDA)",
      "kp" = "KILL PARTICIPATION",
      "gd10" = "GOLD DIFFERENCE",
      "xpd10" = "EXP DIFFERENCE",
      "cspm" = "CS PER MINUTE",
      "dpm" = "DAMAGE PER MINUTE",
      "team" = "TEAM",
      "pos" = "POSITION"
    )
    
    # Color palettes
    colors <- list(
      "team" = c("#F28291", "gray50"),
      "pos" = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
    )
    
    # Visualize a scatterplot of win rate against other variables
    plot2 <- ggplot(data = player, aes_string(y = "w_percent", x = input$x_axis)) +
      geom_jitter(aes_string(fill = input$fill), color = "black", height = 3, alpha = 0.75, shape = 21, size = 2) +
      geom_point(data = player_point, aes_string(fill = input$fill), color = "black", shape = 21, size = 7, show.legend = FALSE) +
      geom_text_repel(data = player_point, aes(label = player), show.legend = FALSE, box.padding = 0.5, seed = 123, min.segment.length = 0) +
      scale_y_continuous(
        "WIN PERCENTAGE", 
        breaks = seq(0, 100, 25),
        labels = scales::label_percent(accuracy = 1, scale = 1),
        expand = c(0, 0),
        limits = c(-5, 105)
      ) +
      scale_x_continuous(
        labels[[input$x_axis]],
        expand = c(0.05, 0)
      ) +
      scale_fill_manual(
        labels[[input$fill]],
        values = colors[[input$fill]],
        guide = guide_legend(title.position = "top", title.hjust = 0.5)
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.line = element_line(color = "grey50", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", color = "gray50")
      )
    
    plot1 / plot2
  })
}

# Run the app
shinyApp(ui = ui, server = server)