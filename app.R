library(tidyverse)
library(explore)
library(tidydice)
library(shiny)
library(colourpicker)
library(shinybusy)
library(ggplot2)

# Helper variables ----

colors <- c(c("White" = "white",
              "Black" = "black",
              "Grey" = "grey"))

plot_coin <- function(data, fill = "grey", fill_success = "gold") {

  
  data <- data %>% mutate(color_fill = ifelse(success, fill_success, fill))
  circle <- tidydice:::circle_points(center = c(1,0), diameter = 0.8)
  
  plot <- ggplot(circle, aes(x,y,)) +
          geom_polygon(color = "darkgrey", fill = data$color_fill[1], size = 1)
  
  
  for(i in 2:10)  {
    
     circle <- tidydice:::circle_points(center = c(i,0), diameter = 0.8)

     plot <- plot + 
       geom_polygon(
         data = circle,
         aes(x,y),
         color = "darkgrey", fill = data$color_fill[i], size = 0.8)

     plot
  }
  
  plot <- plot +
    xlim(c(0,11)) + 
    ylim(c(-1,1)) +
    coord_fixed() +
    theme_void()
  
  plot
  
}

  
# Define UI ----
ui <- fluidPage(
  
  titlePanel("Coin Simulation"),
  use_busy_spinner(spin = "fading-circle"),
  
#  mainPanel(
    tabsetPanel(
      
      tabPanel("Design",
              
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            
            colourInput("color_fill", "Fill:", value = "#C7C7C7"),
            colourInput("color_success", "Fill Success:", value = "#FCCB05"),
            #colourInput("color_point", "Points:", value = "white"),
            #selectInput("color_line", "Line:", colors, selected = "white"),
            
            sliderInput("cheat", "Cheat?", min = 0, max = 10, value = 0)
            
            #sliderInput("line_size", "Line Size:", min = 0, max = 5, value = 1)
         ),
                 
         mainPanel(
           plotOutput("designPlot")
         )
      ) # sidebarLayout
    ), # tabsetPanel Design
      
    tabPanel("Flip", 
            plotOutput("flipPlot")
    ),
    
    tabPanel("Repeat",
             
      sidebarLayout(
        sidebarPanel(
          width = 3, 
                 
          radioButtons("rounds", "Repeat", 
                       c("100" = "100", "1 000" = "1000", "10 000" = "10000", " 100 000" = 100000), 
                       inline = FALSE,
                       width = NULL)
        ),
        
        mainPanel(
          plotOutput("repeatPlot")
        )
      ) #sidebarPanel
    ),

    tabPanel("Check",
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          sliderInput("head_check", "Check success:", min = 0, max = 10, value = 5),
        ),
        mainPanel(
          plotOutput("checkPlot")
        )
      )
    )
    
    #,tabPanel("Help", tableOutput("help"))
      
  ) # tabsetPanel

) # fluidPage

# Define server logic ----
server <- function(input, output, session) {
  
  output$designPlot <- renderPlot({

    data <- force_coin(1) %>% 
      force_coin(1) %>% 
      force_coin(1) %>% 
      force_coin(1) %>% 
      force_coin(1) %>% 
      force_coin(2) %>%
      force_coin(2) %>% 
      force_coin(2) %>% 
      force_coin(2) %>% 
      force_coin(2)

   data %>% 
      plot_coin(
        fill = input$color_fill,
        fill_success = input$color_success
        #line_color = input$color_line,
        #point_color = input$color_point
      )
    
  })
  
  output$flipPlot <- renderPlot({

    #show_modal_spinner(text = "Rolling 60 dice...")
    show_spinner()
        
    cheat <- input$cheat

    if (input$cheat > 0) {
      cheat <- input$cheat
      prob_head <- 1/2 + 1/2*cheat/10
      prob <- c(1 - prob_head, prob_head)
      data <- flip_coin(times = 10, rounds = 1, prob = prob)
    } else {
      data <- flip_coin(times = 10, rounds = 1)
    }
    
    flip_cnt <- nrow(data)
    head_cnt <- sum(data$success)
    
    p <- data %>%  
      plot_coin(
        fill = input$color_fill,
        fill_success = input$color_success
        #line_color = input$color_line,
        #point_color = input$color_point
        #line_size = input$line_size
      ) +
      ggtitle(paste("Success:", head_cnt, "of", flip_cnt))
    
    #remove_modal_spinner()
    hide_spinner()
    
    p
    
  })

  observeEvent(input$button_roll, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Flip It!')
  })  
  
  output$repeatPlot <- renderPlot({
    
    data <- flip_coin(10, rounds = as.numeric(input$rounds), agg = TRUE)

    cheat <- input$cheat
    
    if (input$cheat > 0) {
      cheat <- input$cheat
      prob_head <- 1/2 + 1/2*cheat/10
      prob <- c((1 - prob_head), prob_head)
      data <- flip_coin(10, rounds = as.numeric(input$rounds), agg = TRUE, prob = prob)
    } else {
      data <- flip_coin(10, rounds = as.numeric(input$rounds), agg = TRUE)
    }
    
        
    data %>% 
      #filter(success <= 25) %>%
      explore::explore_bar(
        success, 
        #numeric = TRUE,
        numeric = FALSE,
        title = paste("Success 10 coins, repeated", 
                      format(as.numeric(input$rounds), big.mark = " ", scientific = FALSE), 
                      "times")                   
      )
      
  })
  
  output$checkPlot <- renderPlot({
    
    color_success <- "red"
    if (input$color_success != "grey") {
      color_success <- input$color_success
    }
    
    binom_coin(times = 10) %>% 
    plot_binom(
      highlight = seq(input$head_check, 60),
      title = "Binomial Distribution of 10 fair coins",
      color = "lightgrey",
      color_highlight = color_success) +
      scale_x_continuous(minor_breaks = seq(1 , 10, 1), breaks = seq(1, 10, 1))
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)