source("setup.R")

results_list <- read_rds("data/full_regression_list.Rds")
all_list_names <- read_from_excel("data/all_list_names.xlsx", T)
all_list_names_no_baseline <- read_rds("data/all_list_no_names.Rds")
prs_inventoried <- read_from_excel("data/prs_inventoried.xlsx", T)

hash_table <- read_rds("data/hash_table.Rds")
groupings_table <- read_rds("data/groupings_table.Rds")

ui <- fluidPage(
    sidebarPanel("Choice of variable"),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "checkbox",
                               label = "TEST",
                               choiceValues = names(all_list_names_no_baseline),
                               choiceNames = names(all_list_names_no_baseline),
                               selected = "behavior",
                               inline = T
            )
        ),
        mainPanel(
            verbatimTextOutput("vars"),
            tabsetPanel(
              type = "tabs",
              tabPanel("Plot", plotOutput("plot", height = "700px")),
              tabPanel("Table", tableOutput("table")),
              tabPanel("Plot and Table",
                       uiOutput("plot_table"))
            )
        )
    )
)

server <- function(input, output){
    
    selected_dataset <- reactive({
        hash_table[[input$checkbox]]
    })
    
    map_to_variable2 <- function(variables){case_when(
        variables %in% prs_inventoried$behavior ~"Behavior",
        variables %in% prs_inventoried$lifestyle ~"Lifestyle",
        variables %in% prs_inventoried$health ~"Health",
        variables %in% prs_inventoried$mental ~"Mental",
        variables %in% prs_inventoried$congenital ~"Congenital",
        variables %in% all_list_names$mother ~"Mother",
        variables %in% all_list_names$father ~"Father",
        variables %in% all_list_names$educ ~"Education",
        variables %in% all_list_names$income ~"Income",
        variables %in% all_list_names$birth ~"Birth",
        variables %in% all_list_names$civ_stat ~"Civil_Status",
        variables %in% all_list_names$baseline ~"Baseline",
        variables %in% all_list_names$PC ~"Principal Components",
        variables %in% prs_inventoried$ld_pred ~"ldPred",
        variables %in% all_list_names$patient ~"Patient mental illness"
    )}
    
    map_to_variable <- function(variables){
        if(!is_null(groupings_table[[variables]])) groupings_table[[variables]]
        else groupings_table[[str_replace(variables, "(.*[a-z|A-Z]+)[0-9]$", "\\1")]]
    }
    
    outputted_dataset <- function(){
        selected_data <- selected_dataset()
        selected_data %>%
            #filter(.[,6] <= 0.05) %>%
            rowwise(variables) %>%
            mutate(group_names = as.factor(map_to_variable(variables))) %>%
            ungroup()
    }
    
    output$table <- renderTable({
        outputted_dataset() #%>%
            #filter(.[,6] <= 0.05) %>%
    })
    
    
    
    #    max_coef <- max(selected_dataset %>% ds))
    
    make_factor_vars <- function(vars, groups){
        factor(interaction(vars, groups, drop = T), labels = vars)
    }

    output$plot <- renderPlot({
        outputted_dataset() %>%
            ggplot(aes(y = factor(interaction(variables, group_names, drop = T), labels = variables), 
                       group = group_names, 
                       color = group_names)) + 
            theme_classic() +
            ylab("Variables") +
            theme(legend.position = c(.9, .9)) +
            geom_point(aes(x = `exp(coef)`), shape = 15, size = 1) +
            geom_errorbarh(aes(xmin = `lower .95`, xmax = `upper .95`), height = .2) +
            geom_linerange(aes(xmin = `lower .95`, xmax = `upper .95`)) +
            geom_vline(xintercept = 1, linetype = "dashed") +
            xlim(0, 3)
    })
    
    output$plot_table <- renderUI({
        plotOutput("plot", height = "700px")
        tableOutput("table")
    })
    
}

### Hvad

shinyApp(ui, server)


