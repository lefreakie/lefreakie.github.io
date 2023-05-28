source("setup.R")

results_list <- read_rds("data/full_regression_list.Rds")
all_list_names <- read_from_excel("data/all_list_names.xlsx", T)
all_list_names_no_baseline <- read_rds("data/all_list_no_names.Rds")
prs_inventoried <- read_from_excel("data/prs_inventoried.xlsx", T)
prs_exp <- data.frame(read_from_excel("data/prs_explanatory.xlsx", T))

hash_table <- read_rds("data/hash_table.Rds")
groupings_table <- read_rds("data/groupings_table.Rds")

ui <- fluidPage(
  titlePanel(h1("Prediction of readmissions with Anorexia Nervosa", style = {'background-color: #092052; color: white; padding: 10px'})),
  #sidebarPanel("Choice of variable"),
  sidebarLayout(
    sidebarPanel(
      h2("Choice of Variable"),
      checkboxGroupInput(
        inputId = "checkbox",
        label = "Variables",
        choiceValues = names(all_list_names_no_baseline),
        choiceNames = list("Behavior", "Lifestyle", "Health", "Miscellaneous", "Mental", 
                           "Father", "Mother", "Education", "Income", "Birth", "Civil Status"),
        selected = "baseline",
        inline = T
      ),
      h3("Choice of p-values"),
      radioButtons(
        inputId = "radiobuttons",
        label = NULL,
        choiceValues = list("all", "significant"),
        choiceNames = list("All p-values", "Significant p-values only"),
        selected = "all"
      ),
      
      radioButtons(inputId = "radiobutton1",
                   label = NULL,
                   choiceValues = list("trait_group", "trait"),
                   choiceNames = list("Reported trait group", "Reported trait name"),
                   selected = "trait_group")
    ),
    
    mainPanel(
      verbatimTextOutput("vars"),
      tabsetPanel(
        id = "myTabs",
        type = "tabs",
        tabPanel("Plot", plotOutput("plot", height = "700px", width = "800px")),
        tabPanel("Table", tableOutput("table")),
        tabPanel("Plot and Table",
                 uiOutput("plot_table"))
      )
    )
  )
)



server <- function(input, output) {
  exp_groups <- reactive({
    prs_exp
  })
  
  selected_dataset <- reactive({
    hash_table[[input$checkbox]] %>%
      rename(pvalue = 6) %>%
      {if(input$radiobuttons == "significant") {
        filter(., pvalue <= 0.05)
      } else {
        .
      }}
  })
  
  
  map_to_variable2 <- function(variables) {
    case_when(
      variables %in% prs_inventoried$behavior ~ "Behavior",
      variables %in% prs_inventoried$lifestyle ~ "Lifestyle",
      variables %in% prs_inventoried$health ~ "Health",
      variables %in% prs_inventoried$mental ~ "Mental",
      variables %in% prs_inventoried$congenital ~ "Congenital",
      variables %in% all_list_names$mother ~ "Mother",
      variables %in% all_list_names$father ~ "Father",
      variables %in% all_list_names$educ ~ "Education",
      variables %in% all_list_names$income ~ "Income",
      variables %in% all_list_names$birth ~ "Birth",
      variables %in% all_list_names$civ_stat ~ "Civil_Status",
      variables %in% all_list_names$baseline ~ "Baseline",
      variables %in% all_list_names$PC ~ "Principal Components",
      variables %in% prs_inventoried$ld_pred ~ "ldPred",
      variables %in% all_list_names$patient ~ "Patient mental illness"
    ) 
  }
  
  map_to_variable <- function(variables) {
    if (!is_null(groupings_table[[variables]]))
      groupings_table[[variables]] %>% 
      gsub("^(\\w)", "\\U\\1", ., perl = TRUE) 
    else
      groupings_table[[str_replace(variables, "(.*[a-z|A-Z]+)[0-9]$", "\\1")]] %>% 
      gsub("^(\\w)", "\\U\\1", ., perl = TRUE) 
  }
  
  
  
  outputted_dataset1 <- function() {
    selected_dataset() %>%
      rowwise(variables) %>%
      mutate(group_names = map_to_variable(variables)) %>%
      ungroup()
  }
  
  
  outputted_dataset <- reactive({
    left_join(outputted_dataset1(),
              exp_groups(),
              by = c("variables" = "id")) %>%
      mutate_at("Reported_trait_group",
                ~ ifelse(is.na(Reported_trait_group), group_names, .)) %>%
      mutate_at("Reported_trait",
                ~ ifelse(is.na(Reported_trait), group_names, .)) %>% 
      mutate(variables = recode(variables, "gender" = "Male", "P_Skizofreni" = "Schizophrenia", 
                                "P_MDD" = "Major Depressive Disorder", "P_Bipolar" = "Bipolar disorder",
                                "first_alder" = "Age at admission", "Anbragt" = "Removed from home",
                                "Somatisk_sygdom" = "Somatic disease", 
                                "civ_stat" = "Mother in a committed relationsship",
                                "apgarscore_efter5minutter" = "Having an apgarscore at 10",
                                "gestationsalder_dage2" = "Born before term",
                                "gestationsalder_dage3" = "Born after term")) %>% 
      mutate(group_names = recode(group_names, "Civ_stat" = "Civil Status",
                                  "PC" = "Principal Components"))
    
  })
  
  
  output$table <- renderTable({
    outputted_dataset()
  })
  
  
  make_factor_vars <- function(vars, groups) {
    factor(interaction(vars, groups, drop = T), labels = vars)
  }
  
  output$plot <- renderPlot({
    label_column <- if (input$radiobutton1 == "trait_group") {
      outputted_dataset()$Reported_trait_group
    } else {
      outputted_dataset()$Reported_trait
    } 
    
    outputted_dataset() %>%
      ggplot(aes(y = variables, color = group_names)) +
      theme_classic() +
      ylab("Variables") +
      xlab("Hazard Ratio") +
      #theme(legend.position = c(.9, .9)) +
      geom_point(aes(x = `exp(coef)`),
                 shape = 15,
                 size = 2) +
      labs(color = "Groups") +
      geom_errorbarh(aes(xmin = `lower .95`, xmax = `upper .95`), height = .2) +
      geom_linerange(aes(xmin = `lower .95`, xmax = `upper .95`)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      xlim(0, 3) +
      ggrepel::geom_text_repel(aes(x = `exp(coef)`, label = label_column),
                               direction = "both")
  })
  
  output$plot2 <- renderPlot({
    label_column <- if (input$radiobutton1 == "trait_group") {
      outputted_dataset()$Reported_trait_group
    } else {
      outputted_dataset()$Reported_trait
    } 
    
    outputted_dataset() %>%
      ggplot(#aes(y = factor(interaction(variables, group_names, drop = T), labels = variables), 
        #       group = group_names,color = group_names
        aes(y = variables, color = group_names
        )) +
      theme_classic() +
      ylab("Variables") +
      #theme(legend.position = c(.9, .9)) +
      geom_point(aes(x = `exp(coef)`),
                 shape = 15,
                 size = 2) +
      geom_errorbarh(aes(xmin = `lower .95`, xmax = `upper .95`), height = .2) +
      geom_linerange(aes(xmin = `lower .95`, xmax = `upper .95`)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      xlim(0, 3) +
      ggrepel::geom_text_repel(aes(x = `exp(coef)`, label = label_column))
  })
  
  
  output$table2 <- renderTable({
    outputted_dataset() 
  })
  
  output$plot_table <- renderUI({
    conditionalPanel(condition = "input.myTabs == 'Plot and Table'",
                     plotOutput("plot2", height = "700px", width = "1000px"),
                     tableOutput("table2"))
  })
}

shinyApp(ui, server)


