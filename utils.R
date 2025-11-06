# UI ####

# UI-function for input discipline

fn_selectInput_discipline <- function(id) {
  selectInput(
    inputId = id,
    label = "Select...",
    choices = c(
      "Persons' kilometers" = "Laenge_PERS",
      "Persons' trips" = "NUMBER_TRIP_PERS",
      "Boats' kilometers" = "Laenge_BOAT",
      "Boats' trips" = "NUMBER_TRIP_BOAT"
    )
  )
}

# UI-function for input boats

fn_selectInput_boat <- function(id, vec_boat_choice, vec_boat_selected) {
  selectizeInput(
    inputId = id,
    label = "Boats",
    choices = get(vec_boat_choice),
    selected = get(vec_boat_selected),
    multiple = TRUE
  )
}

# Input: Selector for the first year

fn_sliderInput_boat_first_year <- function(id) {
  sliderInput(
    inputId = paste0(id, "_first_year"),
    label = "First year",
    min = y_min,
    max = y_max,
    value = y_min,
    sep = "",
    ticks = FALSE,
    dragRange = FALSE
  )
}

# Input: Selector for the last year

fn_sliderInput_boat_last_year <- function(id) {
  sliderInput(
    inputId = paste0(id, "_last_year"),
    label = "Last year",
    min = y_min,
    max = y_max,
    value = y_max,
    sep = "",
    ticks = FALSE,
    dragRange = FALSE
  )
}


# SERVER ####

# Data process ----

# Km - Filter and reorder

fn_process_km_boats <- function(x, vec_boot, year_start, year_end){

x %>%
  filter(Boot %in% vec_boot) %>%
  filter(SaisonYear >= year_start) %>%
  filter(SaisonYear <= year_end) %>%
  mutate(Boot = as.factor(Boot)) %>%
  arrange(desc(SaisonYear), desc(Laenge)) %>%
  mutate(Boot = fct_inorder(Boot))
  
}

# Cumulative - Filter and reorder

fn_process_cml_boats <- function(x, vec_boot, year_start, year_end){
  
  x %>%
    filter(Boot %in% vec_boot) %>%
    filter(SaisonYear >= year_start) %>%
    filter(SaisonYear <= year_end)  %>%
    mutate(Boot = as.factor(Boot)) %>%
    arrange(desc(SaisonYear), desc(Laenge)) %>%
    mutate(Boot = fct_inorder(Boot)) %>% 
    group_by(Boot) %>%
    arrange(SaisonYear) %>%
    mutate(Laenge = cumsum(Laenge)) %>%
    ungroup()
  
}


# Plots ----

# Plot discplines

fn_ggplotly_discipline <-
  function(reactive_data, category, absolute = FALSE) {
    ggplotly(
      ggplot(reactive_data,
             mapping = aes(
               x = SaisonYear,
               y = Value,
               color = get(category)
             )) +
        
        geom_point(size = 2.5) +
        
        geom_point(aes(text = if (absolute) {
          paste(round(Value, digits = -2), UNIT)
        } else{
          paste(round(Value * 100, digits = 1), "% ", UNIT)
        }), size = 2.5) +
        
        geom_line(size = 0.3) +
        
        theme_classic() +
        
        theme(
          text = element_text(size = 13),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          )
        ) +
        
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        
        scale_colour_viridis_d() +
        
        scale_x_continuous(breaks = seq(y_min, y_max, 1)) +
        
        scale_y_continuous(labels = if (absolute) {
          comma_format(big.mark = ' ')
        } else{
          label_percent()
        }, scale_y_continuous(limits = c(0, NA))),
      
      tooltip = "text") %>%
      layout(legend = list(title = ""))
  }

# Plot boat

fn_ggplotly_boat <-
  function(reactive_data, year_start, year_end) {
    ggplotly(
      ggplot(reactive_data, mapping = aes(
        x = SaisonYear, y = Laenge,
        color = Boot
      )) +
        
        geom_point(size = 2.5) +
        
        geom_point(aes(text = paste(Boot, ": ", Laenge, " km")), size = 2.5) +
        
        geom_line(size = 0.3) +
        
        theme_classic() +
        
        theme(
          text = element_text(size = 13),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          legend.title = element_blank()
        ) +
        
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        
        scale_colour_viridis_d() +
        
        scale_y_continuous(limits = c(0, NA)) +

        scale_x_continuous(
          breaks = seq(year_start,
                       year_end,
                       1)
        ),
      tooltip = "text"
    ) %>%
      layout(legend = list(title = ""))
  }

# Plot Overview

fn_ggplotly_overview <-
  function(reactive_data, year_overview) {
    
    # Number of boats for the title
    
    number_boot_dynamic <- length(reactive_data$Boot %>% unique())
    
    # Plot
    
    ggplotly(
      ggplot(
        reactive_data,
        mapping = aes(x = Age, y = `Cumulated distance (km)`,
                      color = Seats)
      ) +
        
        geom_vline(xintercept = 5, color = "grey") +
        
        # geom_point(size = 2.5) +
        
        geom_jitter(aes(
          text = paste(
            Boot,
            ": \nDistance:",
            `Cumulated distance (km)`,
            " km \nAge: ",
            Age,
            " years old"
          )
        ), size = 2.5, width = 0.03) +
        
        theme_classic() +
        
        theme(
          text = element_text(size = 13),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          legend.title = element_blank(),
          plot.margin = margin(t = 20, r = 10, unit = "pt")
        ) +
        
        scale_colour_viridis_d() +
        
        scale_x_continuous(breaks = pretty_breaks(), limits = c(0, y_max - 2011 + 2)) +
        scale_y_continuous(labels = comma_format(big.mark = ' ')),
      
      tooltip = "text"
    ) %>%
      layout(legend = list(title = ""),
             title = paste0('Situation in ', year_overview, ' (n = ', number_boot_dynamic, ')'))
  }


