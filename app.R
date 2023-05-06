source("setup.R")

results_list <- read_rds("data/full_regression_list.Rds")
all_list_names <- read_from_excel("data/all_list_names.xlsx", T)
prs_inventoried <- read_from_excel("data/prs_inventoried.xlsx", T)

results_behavior <- results_list[[1]]$results
results_lifestyle <- results_list[[2]]$results
results_health <- results_list[[3]]$results
results_congential <- results_list[[4]]$results
results_mental <- results_list[[5]]$results
results_father <- results_list[[6]]$results
results_mother <- results_list[[7]]$results
results_education <- results_list[[8]]$results
results_income <- results_list[[9]]$results
results_birth <- results_list[[10]]$results
results_civstat <- results_list[[11]]$results
results_prs <- results_list[[562]]$results
results_socio <- results_list[[1485]]$results
results_all <- results_list[[2047]]$results

ui <- fluidPage(
    sidebarPanel("Choice of variable"),
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset", "Select a dataset:",
                         choices = list("Behavior" = "dataset1",
                                        "Lifestyle" = "dataset2",
                                        "Health" = "dataset3",
                                        "Congential" = "dataset4",
                                        "Mental" = "dataset5",
                                        "Fater" = "dataset6",
                                        "Mother" = "dataset7",
                                        "Education" = "dataset8",
                                        "Income" = "dataset9",
                                        "Birth" = "datset10",
                                        "Civil Status" = "dataset11",
                                        "PRS'er" = "dataset12",
                                        "Socioeconomic" = "dataset13",
                                        "All Variables" = "dataset"),
                         selected = "dataset1")
        ),
        mainPanel(
            plotOutput("plot", height = "700px"),
            tableOutput("table")
        )
    )
)

server <- function(input, output){
    dataset1 <- results_behavior
    dataset2 <- results_lifestyle
    dataset3 <- results_health
    dataset4 <- results_congential
    dataset5 <- results_mental
    dataset6 <- results_father
    dataset7 <- results_mother
    dataset8 <- results_education
    dataset9 <- results_income
    dataset10 <- results_birth
    dataset11 <- results_civstat
    dataset12 <- results_prs
    dataset13 <- results_socio
    dataset <- results_all
    
    selected_dataset <- reactive({
        if (input$dataset == "dataset1"){
            return(dataset1)
        } else if(input$dataset == "dataset2") {
            return(dataset2)
        } else if(input$dataset == "dataset3") {
            return(dataset3) 
        } else if(input$dataset == "dataset4") {
            return(dataset4) 
        } else if (input$dataset == "dataset5") {
            return(dataset5)
        } else if(input$dataset == "dataset6") {
            return(dataset6)
        } else if(input$dataset == "dataset7") {
            return(dataset7)
        } else if(input$dataset == "dataset8") {
            return(dataset8)
        } else if(input$dataset == "dataset9") {
            return(dataset9)
        } else if(input$dataset == "dataset10") {
            return(dataset10)
        } else if(input$dataset == "dataset11") {
            return(dataset11) 
        } else if(input$dataset == "dataset12") {
            return(dataset12)
        } else if(input$dataset == "dataset13") 
            return(dataset13)
        else {
            return(dataset)
        }
    })
    
    map_to_variable <- function(variables){case_when(
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
    
    output$table <- renderTable({
        selected_dataset() %>%
            mutate(group_names = map_to_variable(variables))
    })
    
#    max_coef <- max(selected_dataset %>% ds))
    
    output$plot <- renderPlot({
        selected_dataset() %>%
            mutate(group_names = map_to_variable(variables)) %>%
            ggplot(aes(y = variables, color = group_names)) + 
            theme_classic() +
            geom_point(aes(x = `exp(coef)`), shape = 15, size = 1) +
            geom_errorbarh(aes(xmin = `lower .95`, xmax = `upper .95`), height = .2) +
            geom_linerange(aes(xmin = `lower .95`, xmax = `upper .95`)) +
            geom_vline(xintercept = 1, linetype = "dashed") +
            xlim(0, 3)
    })
}

shinyApp(ui, server)

