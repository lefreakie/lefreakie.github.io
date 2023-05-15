source("setup.R")

results_list <- read_rds("data/full_regression_list.Rds")
all_list_names <- read_from_excel("data/all_list_names.xlsx", T)
all_list_names_no_baseline <- read_rds("data/all_list_no_names.Rds")
prs_inventoried <- read_from_excel("data/prs_inventoried.xlsx", T)

hash_table <- read_rds("data/hash_table.Rds")
groupings_table <- read_rds("data/groupings_table.Rds")

ui <- fluidPage(
  
  titlePanel(h1("Prediction of readmissions with Anorexia Nervosa", style = {'background-color: #092052; color: white; padding: 10px'})),
  
  sidebarLayout(
    sidebarPanel(width = 2,
      tags$h3("Choice of variable"),
      tags$hr(style = {'margin: 0 0 20px 0;'}),
      checkboxGroupInput(inputId = "checkbox",
                         label = NULL,
                         choiceValues = names(all_list_names_no_baseline),
                         choiceNames = names(all_list_names_no_baseline),
                         selected = "behavior"
      ),
      tags$hr(style = {'margin: 20px 0 20px 0;'}),
      radioButtons(inputId = "radiobuttons",
                   label = NULL,
                   choiceValues = list("all", "significant"),
                   choiceNames = list("All p-values", "Significant p-values only"),
                   selected = "all")
    ),
    
    mainPanel(
      verbatimTextOutput("vars"),
      tabsetPanel(
        id = "myTabs",
        type = "tabs",
        tabPanel("Plot", plotOutput("plot", height=800)),
        tabPanel("Table", tableOutput("table")),
        tabPanel("Plot and Table", uiOutput("plot_table")
        )
      )
    )
  )
  
)


server <- function(input, output, session){
  
  selected_dataset <- reactive({
    hash_table[[input$checkbox]] %>%
      rename(pvalue = 6) %>%
      {if(input$radiobuttons == "significant") {
        filter(., pvalue <= 0.05)
      } else {
        .
      }}
  })
  
  # observe ({
  #   print(hash_table[[input$checkbox]][6] )
  # })
  
  map_to_variable <- function(variables){
    if(!is_null(groupings_table[[variables]])) groupings_table[[variables]]
    else groupings_table[[str_replace(variables, "(.*[a-z|A-Z]+)[0-9]$", "\\1")]]
  }
  
  output$table <- renderTable({
    selected_dataset() %>%
      rowwise(variables) %>%
      mutate(group_names = map_to_variable(variables)) %>%
      ungroup()
  })
  
  outputted_dataset <- function(){
    selected_dataset() %>%
      rowwise(variables) %>%
      mutate(group_names = as.factor(map_to_variable(variables))) %>%
      ungroup()
  }
  
  make_factor_vars <- function(vars, groups){
    factor(interaction(vars, groups, drop = T), labels = vars)
  }
  
  output$plot <- renderPlot({
    outputted_dataset() %>%
      ggplot(aes(y = factor(interaction(variables, group_names, drop = T), labels = variables), group = group_names, color = group_names)) + 
      theme_classic() +
      theme(legend.position = c(.9, .9)) +
      ylab("") +
      geom_point(aes(x = `exp(coef)`), shape = 15, size = 1) +
      geom_errorbarh(aes(xmin = `lower .95`, xmax = `upper .95`), height = .2, size = 1) +
      geom_linerange(aes(xmin = `lower .95`, xmax = `upper .95`)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      xlim(0, 3)
  })
  
  output$plot2 <- renderPlot({
    outputted_dataset() %>%
      ggplot(aes(y = factor(interaction(variables, group_names, drop = T), labels = variables), group = group_names, color = group_names)) + 
      theme_classic() +
      theme(legend.position = c(.9, .9)) +
      ylab("") +
      geom_point(aes(x = `exp(coef)`), shape = 15, size = 1) +
      geom_errorbarh(aes(xmin = `lower .95`, xmax = `upper .95`), height = .2, size = 1) +
      geom_linerange(aes(xmin = `lower .95`, xmax = `upper .95`)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      xlim(0, 3)
  })
  
  output$table2 <- renderTable({
    selected_dataset() %>%
      rowwise(variables) %>%
      mutate(group_names = map_to_variable(variables)) %>%
      ungroup()
  })
  
  output$plot_table <- renderUI({
    conditionalPanel(condition = "input.myTabs == 'Plot and Table'",
                     plotOutput("plot2"),
                     tableOutput("table2"))
  })
  
}

shinyApp(ui, server)
