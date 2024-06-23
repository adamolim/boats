# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Rowing ShinyAPP
# https://rowingclubbern.shinyapps.io/boats/
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Author: Michele Adamoli 03.02.2024


# install.packages("shiny")
# install.packages('rsconnect')
library(shiny) # Web Application Framework for R - Version 1.6.0
library(shinythemes)
library(tidyverse) # Easily Install and Load the'Tidyverse' - Version 1.3.0
library(plotly) # Create Interactive Web Graphics via "plotly.js" - Version 4.9.3
library(scales)
library(viridis)
library(shinymanager)

# Reproductible values

set.seed(3)

# define some basic credentials (on data.frame)

# you can hash the password using scrypt
# and adding a column is_hashed_password
# data.frame with credentials info
credentials <- data.frame(
  user = c("user", "victor"),
  password = c(
    scrypt::hashPassword("1234"),
    scrypt::hashPassword("12345")
  ),
  is_hashed_password = TRUE,
  comment = c("alsace", "auvergne"),
  stringsAsFactors = FALSE
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Source analysis and functions ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

source("analysis.R")
source("utils.R")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# User interface ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

ui <-
  
  # Navigation bar
  
  navbarPage(
    # authentication module
    auth_ui(
      id = "auth",
      # add image on top ?
      tags_top =
        tags$div(
          tags$h2("", style = "align:center"),
          tags$img(src = "logo.jpeg", width = 100)
        ),
      # add information on bottom ?
      tags_bottom = tags$div(tags$p(
        "For any question, please  contact ",
        tags$a(href = "mailto:sport@rowing.ch.com?Subject=Shiny%20aManager",
               target = "_top", "administrator")
      )),
      # # change auth ui background ?
      # # https://developer.mozilla.org/fr/docs/Web/CSS/background
      # background  = "linear-gradient(rgba(0, 0, 255, 0.5),
      #                  rgba(255, 255, 0, 0.5)),
      #                  url('https://www.r-project.org/logo/Rlogo.png');",
      # set language ?
      lan = use_language("en")
    ),
    
    # 5.1 Overview  ####
    
    tabPanel(
      "Overview",
      
      # Page
      
      fluidPage(
        theme = shinytheme("paper"),
        # Tab title
        
        titlePanel(title = span(
          img(
            src = "logo.jpeg",
            height = 45,
            align = "right"
          ),
          "Rowing club bern"
        )),
        
        # Add the description of the tabPanel with a new div tag
        
        tags$div(
          tags$div(tags$h3(
            "Boats' statistics"
          )),
          tags$p(
            "The rowing club Bern constantly renew his boats in order to allow rowers
            to enjoy this sport and be competitive in regatta. Some boats
            are nevertheless pretty old and people love them after all. 
            This plot shows which boats are the oldest and the most used"
          ),
          tags$br()
        ),
        
        # Sidebar layout with a input and output definitions
        
        sidebarLayout(# Sidebar panel for inputs
          
          sidebarPanel(
            # Input for a choose year
            
            sliderInput(
              inputId = "year_overview",
              label = "Reference year",
              min = 2020,
              max = y_max,
              value = y_max,
              sep = "",
              ticks = FALSE,
              dragRange = FALSE
            )
          ),
          
          # Main panel for displaying outputs
          mainPanel(# Output: Tabset w/ plot, summary, and table
            
            plotlyOutput("overview")))
      )
    ),
    
    # 3.1 Main categories ####
    
    navbarMenu(
      "Main categories",
      
      # 3.1.1 Sculling vs. sweep rowing ####
      
      tabPanel(
        "Sculling vs. sweep rowing",
        
        # Page
        
        fluidPage(
          # Tab title
          
          titlePanel(title = span(
            img(
              src = "logo.jpeg",
              height = 45,
              align = "right"
            ),
            "Sculling or sweep rowing"
          )),
          
          # Add the description of the tabPanel with a new div tag
          
          tags$div(
            tags$div(tags$h3("The eternal debate...")),
            tags$p(
              "In RCB, almost all rowers start learning rowing with the
                     sculling technique.
                     Many rowers love also sweeping."
            ),
            tags$br()
          ),
          
          # Sidebar layout with a input and output definitions
          
          sidebarLayout(
            # Sidebar panel for inputs
            
            sidebarPanel(# Input: Selector for the dimension
              
              fn_selectInput_discipline("Dim_Sweep_scull")),
            
            # Main panel for displaying outputs
            mainPanel(# Output: Tabset w/ plot, summary, and table
              
              tabsetPanel(
                type = "tabs",
                tabPanel("Absolute", plotlyOutput("Sweep_scull")),
                tabPanel("Percentage", plotlyOutput("Sweep_scull_pct"))
              ))
          )
          
        )
      ),
      
      # 3.2.1 Boats category ####
      
      tabPanel(
        "Boats category",
        
        # Page
        
        fluidPage(
          # Tab title
          
          titlePanel(title = span(
            img(
              src = "logo.jpeg",
              height = 45,
              align = "right"
            ),
            "Boats category"
          )),
          
          # Add the description of the tabPanel with a new div tag
          
          tags$div(
            tags$div(tags$h3(
              "Are you a 'soloist' or a 'cog in the machine'?"
            )),
            tags$p(
              "Some rowers prefer to enjoy the calm of the lake all alone,
                     some others need a team in order to go further and faster.
                     Distances and trips are showed for the persons and for the boats."
            ),
            tags$br()
          ),
          
          # Sidebar layout with a input and output definitions
          
          sidebarLayout(
            # Sidebar panel for inputs
            
            sidebarPanel(# Input: Selector for the dimension
              
              # Input: Selector for the dimension
              
              fn_selectInput_discipline("Dim_boats_category")),
            
            # Main panel for displaying outputs
            mainPanel(# Output: Tabset w/ plot, summary, and table
              
              tabsetPanel(
                type = "tabs",
                tabPanel("Absolute", plotlyOutput("boats_category")),
                tabPanel("Percentage", plotlyOutput("boats_category_pct"))
              ))
          )
          
        )
      ),
      
      # 3.3.3  Gigs ####
      
      tabPanel("Gigs",
               
               # Page
               
               fluidPage(
                 # Tab title
                 
                 titlePanel(title = span(
                   img(
                     src = "logo.jpeg",
                     height = 45,
                     align = "right"
                   ),
                   "Gigs category"
                 )),
                 
                 # Add the description of the tabPanel with a new div tag
                 
                 tags$div(
                   tags$div(tags$h3("Racing boats or gigs?'")),
                   tags$p(
                     "Everyone starts out in the heavy and stable gigs. Gigs remain faithful companions in bad weather
                     conditions, for friendly events or on adventurous trips at home and abroad."
                   ),
                   tags$br()
                 ),
                 
                 # Sidebar layout with a input and output definitions
                 
                 sidebarLayout(
                   # Sidebar panel for inputs
                   
                   sidebarPanel(# Input: Selector for the dimension
                     
                     # Input: Selector for the dimension
                     
                     fn_selectInput_discipline("C_gigs")),
                   
                   # Main panel for displaying outputs
                   mainPanel(# Output: Tabset w/ plot, summary, and table
                     
                     tabsetPanel(
                       type = "tabs",
                       tabPanel("Absolute", plotlyOutput("c_gigs")),
                       tabPanel("Percentage", plotlyOutput("c_gigs_pct"))
                     ))
                 )
                 
               ))
      
    ),
    
    # 4.1 Boats  ####
    
    navbarMenu(
      "Boats",
      
      # 4.1.1 Club's singles  ####
      
      tabPanel(
        "Club's singles",
        
        # Page
        
        fluidPage(
          # Tab title
          
          titlePanel(title = span(
            img(
              src = "logo.jpeg",
              height = 45,
              align = "right"
            ),
            "Club's singles"
          )),
          
          # Add the description of the tabPanel with a new div tag
          
          tags$div(
            tags$div(tags$h3("What are the favorite singles in the club?")),
            tags$p(
              "Every rower has his own favorite boat.
                       Let's have a look on the accomplished kilometers by boat."
            ),
            tags$br()
          ),
          
          # Sidebar layout with a input and output definitions
          
          sidebarLayout(
            # Sidebar panel for inputs
            
            sidebarPanel(
              # Input for the boat: einer
              
              fn_selectInput_boat(id = "Boat1_club", vec_boat = "BOAT_NAME_1x_CLUB"),
              
              fn_sliderInput_boat_first_year("Boat1_club"),
              
              fn_sliderInput_boat_last_year("Boat1_club")
              
            ),
            
            # Main panel for displaying outputs
            mainPanel(# Output: Tabset w/ plot, summary, and table
              
              tabsetPanel(
                type = "tabs",
                tabPanel("Yearly", plotlyOutput("Boat1_km_club")),
                tabPanel("Cumulative", plotlyOutput("Boat1_cumsum_club"))
              ))
          )
        )
      ),
      
      # 4.1.1 Private singles  ####
      
      tabPanel(
        "Private singles",
        
        # Page
        
        fluidPage(
          # Tab title
          
          titlePanel(title = span(
            img(
              src = "logo.jpeg",
              height = 45,
              align = "right"
            ),
            "Private singles"
          )),
          
          # Add the description of the tabPanel with a new div tag
          
          tags$div(
            tags$div(tags$h3("What are the private's favorite singles?")),
            tags$p(
              "Every rower has his own favorite boat.
              Let's have a look on the accomplished kilometers by boat.
              Please note that, this boats' name have been anonymized for the sake of data protection."
            ),
            tags$br()
          ),
          
          # Sidebar layout with a input and output definitions
          
          sidebarLayout(
            # Sidebar panel for inputs
            
            sidebarPanel(
              # Input for the boat: einer
              
              fn_selectInput_boat(id = "Boat1_private", vec_boat = "BOAT_NAME_1x_PRIVATE"),
              
              fn_sliderInput_boat_first_year("Boat1_private"),
              
              fn_sliderInput_boat_last_year("Boat1_private")
            ),
            
            # Main panel for displaying outputs
            mainPanel(# Output: Tabset w/ plot, summary, and table
              
              tabsetPanel(
                type = "tabs",
                tabPanel("Yearly", plotlyOutput("Boat1_km_private")),
                tabPanel("Cumulative", plotlyOutput("Boat1_cumsum_private"))
              ))
          )
        )
      ),
      
      # 4.1.2 Doubles  ####
      
      tabPanel(
        "Doubles and pairs",
        
        # Page
        
        fluidPage(
          # Tab title
          
          titlePanel(title = span(
            img(
              src = "logo.jpeg",
              height = 45,
              align = "right"
            ),
            "Doubles and pairs"
          )),
          
          # Add the description of the tabPanel with a new div tag
          
          tags$div(
            tags$div(tags$h3(
              "What are the favorite doubles our pairs in the club?"
            )),
            tags$p(
              "Every rower has his own favorite boat.
              Let's have a look on the accomplished kilometers by boat."
            ),
            tags$br()
          ),
          
          # Sidebar layout with a input and output definitions
          
          sidebarLayout(
            # Sidebar panel for inputs
            
            sidebarPanel(
              # Input for the boat: einer
              
              fn_selectInput_boat(id = "Boat2", vec_boat = "BOAT_NAME_2x"),
              
              fn_sliderInput_boat_first_year("Boat2"),
              
              fn_sliderInput_boat_last_year("Boat2")
            ),
            
            # Main panel for displaying outputs
            mainPanel(# Output: Tabset w/ plot, summary, and table
              
              tabsetPanel(
                type = "tabs",
                tabPanel("Yearly", plotlyOutput("Boat2_km")),
                tabPanel("Cumulative", plotlyOutput("Boat2_cumsum"))
              ))
          )
        )
      ),
      
      # 4.1.3 Quads  ####
      
      tabPanel(
        "Quads and fours",
        
        # Page
        
        fluidPage(
          # Tab title
          
          titlePanel(title = span(
            img(
              src = "logo.jpeg",
              height = 45,
              align = "right"
            ),
            "Quads and fours"
          )),
          
          # Add the description of the tabPanel with a new div tag
          
          tags$div(
            tags$div(tags$h3(
              "What are the favorite quads our fours in the club?"
            )),
            tags$p(
              "Every rower has his own favorite boat.
              Let's have a look on the accomplished kilometers by boat."
            ),
            tags$br()
          ),
          
          # Sidebar layout with a input and output definitions
          
          sidebarLayout(
            # Sidebar panel for inputs
            
            sidebarPanel(
              # Input for the boat: einer
              
              fn_selectInput_boat(id = "Boat4", vec_boat = "BOAT_NAME_4x"),
              
              fn_sliderInput_boat_first_year("Boat4"),
              
              fn_sliderInput_boat_last_year("Boat4")
            ),
            
            # Main panel for displaying outputs
            mainPanel(# Output: Tabset w/ plot, summary, and table
              
              tabsetPanel(
                type = "tabs",
                tabPanel("Yearly", plotlyOutput("Boat4_km")),
                tabPanel("Cumulative", plotlyOutput("Boat4_cumsum"))
              ))
          )
        )
      ),
      
      # 4.1.4 Eights  ####
      
      tabPanel("Eights",
               
               # Page
               
               fluidPage(
                 # Tab title
                 
                 titlePanel(title = span(
                   img(
                     src = "logo.jpeg",
                     height = 45,
                     align = "right"
                   ), "Eights"
                 )),
                 
                 # Add the description of the tabPanel with a new div tag
                 
                 tags$div(
                   tags$div(tags$h3("What are the favorite eights in the club?")),
                   tags$p(
                     "Every rower has his own favorite boat.
                      Let's have a look on the accomplished kilometers by boat."
                   ),
                   tags$br()
                 ),
                 
                 # Sidebar layout with a input and output definitions
                 
                 sidebarLayout(
                   # Sidebar panel for inputs
                   
                   sidebarPanel(
                     # Input for the boat: einer
                     
                     fn_selectInput_boat(id = "Boat8", vec_boat = "BOAT_NAME_8x"),
                     
                     fn_sliderInput_boat_first_year("Boat8"),
                     
                     fn_sliderInput_boat_last_year("Boat8")
                   ),
                   
                   # Main panel for displaying outputs
                   mainPanel(# Output: Tabset w/ plot, summary, and table
                     
                     tabsetPanel(
                       type = "tabs",
                       tabPanel("Yearly", plotlyOutput("Boat8_km")),
                       tabPanel("Cumulative", plotlyOutput("Boat8_cumsum"))
                     ))
                 )
               ))
    ),
    
    # 6.1 Disclaimer  ####
    
    tabPanel(
      "Disclaimer",
      
      # Page
      
      fluidPage(
        theme = shinytheme("paper"),
        # Tab title
        
        titlePanel(title = span(
          img(
            src = "logo.jpeg",
            height = 45,
            align = "right"
          ),
          "Disclaimer"
        )),
        
        # Add the description of the tabPanel with a new div tag
        
        tags$div(
          tags$p(
            "Underlying data have been fully anonymized. The information provided within 
            this application is intended for internal use only.
            It is provided without warranty and under GNU General Public License v3. 
            More information available by sport@rowing.ch or adamolim@gmail.com."
          )
        )
      )
    )
  )


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Server ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

server <- function(input, output) {
  # authentication module ####
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(auth)
  })
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(auth)
  })
  # 3. Main categories ####
  
  # 3.1 Sweep or scull
  # 3.1.1 Km ####
  
  output$Sweep_scull <- renderPlotly({
    # Reactive filter
    
    d_ANA_08_9 <- d_ANA_08_8 %>%
      filter(DIMENSION == input$Dim_Sweep_scull)
    
    # Plot
    
    fn_ggplotly_discipline(reactive_data = d_ANA_08_9,
                           category = "TypeRigging",
                           absolute = TRUE)
    
  })
  
  # 3.1.2 Percentage ####
  
  output$Sweep_scull_pct <- renderPlotly({
    # Reactive filter
    
    d_ANA_08_9_pct <- d_ANA_08_8_pct %>%
      filter(DIMENSION == input$Dim_Sweep_scull)
    
    # Plot
    
    fn_ggplotly_discipline(reactive_data = d_ANA_08_9_pct,
                           category = "TypeRigging",
                           absolute = FALSE)
    
  })
  
  
  # 3.2 Boats category ####
  
  # 3.2.1 Km ####
  
  output$boats_category <- renderPlotly({
    # Reactive filter
    
    d_ANA_09_9 <- d_ANA_09_8 %>%
      mutate(Seats = as.character(Seats)) %>%
      filter(DIMENSION == input$Dim_boats_category)
    
    # Plot
    
    fn_ggplotly_discipline(reactive_data = d_ANA_09_9,
                           category = "Seats",
                           absolute = TRUE)
    
  })
  
  # 3.2.2 Percentage ####
  
  output$boats_category_pct <- renderPlotly({
    # Reactive filter
    
    d_ANA_09_9_pct <- d_ANA_09_8_pct %>%
      mutate(Seats = as.character(Seats)) %>%
      filter(DIMENSION == input$Dim_boats_category)
    
    # Plot
    
    fn_ggplotly_discipline(reactive_data = d_ANA_09_9_pct,
                           category = "Seats",
                           absolute = FALSE)
    
  })
  
  # 3.3 C-gigs ####
  
  # 3.2.1 Km ####
  
  output$c_gigs <- renderPlotly({
    # Reactive filter
    
    d_ANA_10_9 <- d_ANA_10_8 %>%
      mutate(CGig = as.character(CGig)) %>%
      filter(DIMENSION == input$C_gigs)
    
    # Plot
    
    fn_ggplotly_discipline(reactive_data = d_ANA_10_9,
                           category = "CGig",
                           absolute = TRUE)
    
  })
  
  # 3.2.2 Percentage ####
  
  output$c_gigs_pct <- renderPlotly({
    # Reactive filter
    
    d_ANA_10_9_pct <- d_ANA_10_8_pct %>%
      mutate(CGig = as.character(CGig)) %>%
      filter(DIMENSION == input$C_gigs)
    
    # Plot
    
    fn_ggplotly_discipline(reactive_data = d_ANA_10_9_pct,
                           category = "CGig",
                           absolute = FALSE)
    
  })
  
  # 4. Boats   ####
  
  # 4.1 Club's singles   ####
  
  # Select All button ----
  
  # 4.1.1 Boats, km   ####
  
  output$Boat1_km_club <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat1_club_first_year
    year_end <- input$Boat1_club_last_year
    vec_boot <- input$Boat1_club
    
    # Process data
    
    reactive_data <-
      fn_process_km_boats(
        x = d_ANA_SKIFF_CLUB,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
    
  })
  
  # 4.1.2 Boats,  cumulative  ####
  
  output$Boat1_cumsum_club <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat1_club_first_year
    year_end <- input$Boat1_club_last_year
    vec_boot <- input$Boat1_club
    
    # Process data
    
    reactive_data <-
      fn_process_cml_boats(
        x = d_ANA_SKIFF_CLUB,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 4.2 Private singles   ####
  
  # 4.2.1 Boats, km   ####
  
  output$Boat1_km_private <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat1_private_first_year
    year_end <- input$Boat1_private_last_year
    vec_boot <- input$Boat1_private
    
    # Process data
    
    reactive_data <-
      fn_process_km_boats(
        x = d_ANA_SKIFF_PRIVATE,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 4.2.2 Boats,  cumulative  ####
  
  output$Boat1_cumsum_private <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat1_private_first_year
    year_end <- input$Boat1_private_last_year
    vec_boot <- input$Boat1_private
    
    # Process data
    
    reactive_data <-
      fn_process_cml_boats(
        x = d_ANA_SKIFF_PRIVATE,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 4.3 Doubles ####
  
  # 4.3.1 Boats, km   ####
  
  output$Boat2_km <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat2_first_year
    year_end <- input$Boat2_last_year
    vec_boot <- input$Boat2
    
    # Process data
    
    reactive_data <-
      fn_process_km_boats(
        x = d_ANA_DOUBLE,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 4.3.2 Boats,  cumulative  ####
  
  output$Boat2_cumsum <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat2_first_year
    year_end <- input$Boat2_last_year
    vec_boot <- input$Boat2
    
    # Process data
    
    reactive_data <-
      fn_process_cml_boats(
        x = d_ANA_DOUBLE,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 4.3 Quads ####
  
  # 4.3.1 Boats, km   ####
  
  output$Boat4_km <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat4_first_year
    year_end <- input$Boat4_last_year
    vec_boot <- input$Boat4
    
    # Process data
    
    reactive_data <-
      fn_process_km_boats(
        x = d_ANA_QUADS,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 4.3.2 Boats,  cumulative  ####
  
  output$Boat4_cumsum <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat4_first_year
    year_end <- input$Boat4_last_year
    vec_boot <- input$Boat4
    
    # Process data
    
    reactive_data <-
      fn_process_cml_boats(
        x = d_ANA_QUADS,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 4.3 Eights ####
  
  # 4.3.1 Boats, km   ####
  
  output$Boat8_km <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat8_first_year
    year_end <- input$Boat8_last_year
    vec_boot <- input$Boat8
    
    # Process data
    
    reactive_data <-
      fn_process_km_boats(
        x = d_ANA_EIGHTS,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 4.3.2 Boats,  cumulative  ####
  
  output$Boat8_cumsum <- renderPlotly({
    # Define years and boats
    
    year_start <- input$Boat8_first_year
    year_end <- input$Boat8_last_year
    vec_boot <- input$Boat8
    
    # Process data
    
    reactive_data <-
      fn_process_cml_boats(
        x = d_ANA_EIGHTS,
        vec_boot = vec_boot,
        year_start = year_start,
        year_end = year_end
      )
    
    # Plot
    
    fn_ggplotly_boat(
      reactive_data = reactive_data,
      year_start = year_start,
      year_end = year_end
    )
    
  })
  
  # 5. Oveview #####
  
  output$overview <- renderPlotly({
    # Define years and boats
    
    year_overview <- input$year_overview
    
    # Process data
    
    d_ANA_OVERVIEW_00 <- d_RCB_DATA_01 %>%
      filter(SaisonYear <= year_overview) %>%
      filter(Boot %in%  vec_boot_ymax) %>%
      select(Boot, SaisonYear) %>%
      unique() %>%
      count(Boot, name = "Age")
    
    d_ANA_OVERVIEW_READY <- d_RCB_DATA_01 %>%
      filter(SaisonYear <= year_overview) %>%
      filter(Boot %in%  vec_boot_ymax) %>%
      group_by(Boot, Seats) %>%
      summarise(Laenge = sum(Laenge, na.rm = FALSE)) %>%
      ungroup() %>%
      right_join(d_ANA_OVERVIEW_00, by = join_by(Boot)) %>%
      rename("Cumulated distance (km)" = "Laenge")
    
    # Plot Overview
    
    fn_ggplotly_overview(reactive_data = d_ANA_OVERVIEW_READY,
                         year_overview = year_overview)
    
  })
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Shiny App ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

shinyApp(ui = ui, server = server)
