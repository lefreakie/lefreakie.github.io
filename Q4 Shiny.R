library(shiny)
library(ggplot2)
library(tidyverse)
#library(lubridate)
#library(gapminder)
library(ggthemes)

metadata <- readr::read_rds(file = "/Users/am/Desktop/1. Semester DAV/Introduktion til Datavidenskab/Data/metadata_snpeff_tidy_300K_downsampled - Copy.rds")
# metadata <- readr::read_rds(file = "metadata_snpeff_tidy_300K_downsampled.rds")

interesting_lineages <- c("B.1.1.7",   # British  = ALFA
                          "B.1.351",   # South Africa = BETA
                          "B.1.617.2", # Indian variant = DELTA
                          "P.1",       # Brazil = GAMMA
                          "BA.2",      # omicron
                          "BA.2.12.1") # yet another omicron sub-variant"

metadata_subset <- metadata %>% 
  select(date, region, country, pangolin_lineage, count_S, count_N) %>% 
  filter(region == "Europe") %>% 
  filter(!is.na(date)) %>% 
  mutate(pango2 = ifelse(pangolin_lineage %in% interesting_lineages,
                         pangolin_lineage, "Others"))

countries <- c("Denmark", "France", "Germany", "Norway", "Turkey")

wider_sars_c <- metadata_subset %>% 
  group_by(date, pango2, country) %>% 
  filter(country %in% countries) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = pango2, values_fill=0) %>%
  # values_fill: transforms NAs to zero !
  rename(n_alpha = B.1.1.7,
         n_beta = B.1.351, 
         n_delta = B.1.617.2,
         n_gamma = P.1,
         n_omi1 = BA.2,
         n_omi2 = BA.2.12.1,
         n_others = Others)

wider_sars_frequencies_c <- wider_sars_c %>% 
  mutate(summation = sum(c(n_alpha, n_beta, n_delta, n_gamma, n_omi1, n_omi2, n_others)),
         p_alpha = n_alpha/summation, 
         p_beta = n_beta/summation, 
         p_delta = n_delta /summation,
         p_gamma = n_gamma /summation,
         p_omi = (n_omi1+ n_omi2) /summation,
         p_others = n_others /summation)

sars_long_c <- wider_sars_frequencies_c %>%
  pivot_longer(
    cols = starts_with("p_"),
    names_to = "lineage",
    values_to = "proportion",
    values_drop_na = TRUE
  )

## Ny Shiny

start_date <- min(sars_long_c$date)
end_date <- max(sars_long_c$date)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("Country_Choice", 
                  "Choice of Country", 
                  choices = sars_long_c$country, 
                  multiple=TRUE,
                  selected = sars_long_c$country),
      selectize = FALSE,
      dateRangeInput(inputId = "date",
                     strong("Date Range"),
                     start = start_date, end = end_date,
                     min = start_date, max =end_date)),
    mainPanel(
      plotOutput("Graph")
    )
  )
)


server <- function(input, output, session) {
  filter_sars_long_c <- reactive({
    filter(sars_long_c, between(date ,input$date[1], input$date[2]))
  })
  
  filter1_cols <- reactive({
    filter(filter_sars_long_c(), country %in% input$Country_Choice)
  })

  output$Graph <- renderPlot({
    ggplot(filter1_cols(), aes(x = date, y = proportion, color=lineage)) +
      geom_line() +
      facet_wrap(~ country, nrow = 5) 
  }, height = 500)
  
}

shinyApp(ui, server)


  