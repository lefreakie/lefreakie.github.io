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
                               choiceNames = names(all_list_names_no_baseline)
                               )
        ),
        mainPanel(
            verbatimTextOutput("vars"),
            plotOutput("plot", height = "700px"),
            tableOutput("table")
        )
    )
)

server <- function(input, output){

    selected_dataset <- reactive({
#    return (eval(parse(text = input$dataset)))
      hash_table[[input$checkbox]]
        }
    )
    
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
    
#    output$vars <- renderPrint(hash_table[[input$checkbox]])
    
    
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
    
#    max_coef <- max(selected_dataset %>% ds))
    
    make_factor_vars <- function(vars, groups){
      factor(interaction(vars, groups, drop = T), labels = vars)
    }
    
    output$plot <- renderPlot({
            outputted_dataset() %>%
#            arrange(-group_names, -variables) %>%
            ggplot(aes(y = factor(interaction(variables, group_names, drop = T), labels = variables), group = group_names, color = group_names)) + 
            theme_classic() +
            theme(legend.position = c(.9, .9)) +
            geom_point(aes(x = `exp(coef)`), shape = 15, size = 1) +
            geom_errorbarh(aes(xmin = `lower .95`, xmax = `upper .95`), height = .2) +
            geom_linerange(aes(xmin = `lower .95`, xmax = `upper .95`)) +
            geom_vline(xintercept = 1, linetype = "dashed") +
            xlim(0, 3)
    })
    
}

shinyApp(ui, server)

a <- hash_table[[c("behavior", "father")]]

a <- a %>%
  rowwise(variables) %>%
  mutate(group_names = if(!is_null(groupings_table[[variables]])) groupings_table[[variables]] else groupings_table[[str_replace(variables, "(.*[a-z|A-Z]+)[0-9]$", "\\1")]]) %>%
  ungroup()

factor(interaction(a$group_names, a$variables, drop = T), labels = a$group_names)

