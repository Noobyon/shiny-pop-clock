#
# This is a Shiny web application, demonstrating how to build a population clock. You can run the clock by clicking
# the 'Run App' button above.
#


library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)

# Define UI for prints pop clock
ui <- fluidPage(
  
  HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
  HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
  
  tags$script('
  $(function() {
    var time_now = new Date()
    $("input#client_time").val(time_now.getTime())
    $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
  });'
              ),
  
  h1(id = "client_time", textOutput("client_time")),
  
  h1(id = "big-head", textOutput("currentPop"), windowTitle = "New Zealand Population Clock"),
    tags$style((HTML("#big-head {color: navy;}"))),
  
  h2(textOutput("currentTime")),
  
  h3("On average, NZ's population will increase when bar is full"),
  plotOutput("progBar", height = "30px"),
  
  h3("On average, a new person will be born when bar is full"),
  plotOutput("progBarBirths", height = "30px"),
  
  h3("On average, a death will occur when bar is full"),
  plotOutput("progBarDeaths", height = "30px"),
  
  h3("On average, a new person will migrate into New Zealand when bar is full"),
  plotOutput("progBarMig", height = "30px")
  
  
  
)

# Define server logic to show current population, update every second ----
server <- function(input, output, session) {
  
  current_pop_estimate <- function(init_population, init_population_date, pop_increase_time){
    current_date_time <- Sys.time() - time_zone_offset()
    fracs_increase <- difftime(current_date_time, init_population_date, units = "secs")/pop_increase_time
    current_pop <- data.frame(population = floor(init_population + fracs_increase), 
                              prog_to_next = round(fracs_increase - floor(fracs_increase), digits = 3))
  }
  
  components_estimate <- function(birth_increase_time, death_increase_time, mig_gain_time){
    current_date_time <- Sys.time()
    fracs_births <- difftime(current_date_time, init_population_date, units = "secs")/birth_increase_time
    fracs_deaths <- difftime(current_date_time, init_population_date, units = "secs")/death_increase_time
    fracs_migs <- difftime(current_date_time, init_population_date, units = "secs")/mig_gain_time
    prog_bars <- data.frame(prog_to_birth = round(fracs_births - floor(fracs_births), digits = 3),
                            prog_to_death = round(fracs_deaths - floor(fracs_deaths), digits = 3),
                            prog_to_mig = round(fracs_migs - floor(fracs_migs), digits = 3))

  }
  
  init_population <- 5084300
  init_population_date <- as_datetime("2020-06-30 13:00:00")
  pop_increase_time <- duration(minutes = 17, seconds = 44)
  birth_increase_time <- duration(minutes = 8, seconds = 45)
  death_increase_time <- duration(minutes = 14, seconds = 26)
  mig_gain_time <- duration(minutes = 88, seconds = 19)
  
  client_time <- reactive(as.numeric(input$client_time) / 1000) # in s
  time_zone_offset <- reactive(as.numeric(input$client_time_zone_offset) * 60 ) # in s 
  
  output$currentPop <- renderText({
    invalidateLater(1000, session)
    paste("The New Zealand population is", current_pop_estimate(init_population, init_population_date, pop_increase_time)$population)
    })
  
  output$progBar <- renderPlot({
    invalidateLater(1000, session)
    ggplot(NULL) +
      geom_col(data = current_pop_estimate(init_population, init_population_date, pop_increase_time), 
               aes(x = 0, y = prog_to_next), width = 0.01, fill = "#ff8c00") +
      coord_flip() +
      scale_y_continuous(limits = c(-0.0,1.0), expand = c(0, 0)) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=2))
  }, height = 20)
  
  output$progBarBirths <- renderPlot({
    invalidateLater(1000, session)
    ggplot(data = components_estimate(birth_increase_time, death_increase_time, mig_gain_time)) +
      geom_col(aes(x = 0, y = prog_to_birth), width = 0.01, fill = "#00bfff") +
      coord_flip() +
      scale_y_continuous(limits = c(-0.0,1.0), expand = c(0, 0)) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=2))
  }, height = 20)
  
  output$progBarDeaths <- renderPlot({
    invalidateLater(1000, session)
    ggplot(data = components_estimate(birth_increase_time, death_increase_time, mig_gain_time)) +
      geom_col(aes(x = 0, y = prog_to_death), width = 0.01, fill = "#ffcccb") +
      coord_flip() +
      scale_y_continuous(limits = c(-0.0,1.0), expand = c(0, 0)) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=2))
  }, height = 20)
  
  output$progBarMig <- renderPlot({
    invalidateLater(1000, session)
    ggplot(data = components_estimate(birth_increase_time, death_increase_time, mig_gain_time)) +
      geom_col(aes(x = 0, y = prog_to_mig), width = 0.01, fill = "#8fbc8f") +
      coord_flip() +
      scale_y_continuous(limits = c(-0.0,1.0), expand = c(0, 0)) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=2))
  }, height = 20)
  
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("As at",Sys.time() - time_zone_offset())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

