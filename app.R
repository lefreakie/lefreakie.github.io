#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readxl)

results_list <- read_rds("/Users/am/Desktop/full_regression_list.Rds")

mother <- unlist(data.frame(read_excel("/Users/am/Desktop/all_list_names.xlsx", sheet = "mother")))
father <- unlist(data.frame(read_excel("/Users/am/Desktop/all_list_names.xlsx", sheet = "father")))
educ <- unlist(data.frame(read_excel("/Users/am/Desktop/all_list_names.xlsx", sheet = "educ")))
income <- unlist(data.frame(read_excel("/Users/am/Desktop/all_list_names.xlsx", sheet = "income")))
baseline <- unlist(data.frame(read_excel("/Users/am/Desktop/all_list_names.xlsx", sheet = "baseline")))
birth <- unlist(data.frame(read_excel("/Users/am/Desktop/all_list_names.xlsx", sheet = "birth")))
civ_stat <- unlist(data.frame(read_excel("/Users/am/Desktop/all_list_names.xlsx", sheet = "civ_stat")))

behavior <- unlist(data.frame(read_excel("/Users/am/Desktop/prs_inventoried.xlsx", sheet = "behavior")))
lifestyle <- unlist(data.frame(read_excel("/Users/am/Desktop/prs_inventoried.xlsx", sheet = "lifestyle")))
health <- unlist(data.frame(read_excel("/Users/am/Desktop/prs_inventoried.xlsx", sheet = "health")))
mental <- unlist(data.frame(read_excel("/Users/am/Desktop/prs_inventoried.xlsx", sheet = "mental")))
congenital <- unlist(data.frame(read_excel("/Users/am/Desktop/prs_inventoried.xlsx", sheet = "congenital")))

results1 <- results_list[[1]]$results %>%
    mutate(group_names = case_when(
        variables %in% behavior ~"Behavior",
        variables %in% lifestyle ~"Lifestyle",
        variables %in% health ~"Health",
        variables %in% mental ~"Mental",
        variables %in% congenital ~"Congenital",
        variables %in% mother ~"Mother",
        variables %in% father ~"Father",
        variables %in% educ ~"Education",
        variables %in% income ~"Income",
        variables %in% birth ~"Birth",
        variables %in% civ_stat ~"Civil_Status",
        variables %!in% health ~"Baseline"
    ))

results2 <- results_list[[2]]$results %>%
    mutate(group_names = case_when(
        variables %in% behavior ~"Behavior",
        variables %in% lifestyle ~"Lifestyle",
        variables %in% health ~"Health",
        variables %in% mental ~"Mental",
        variables %in% congenital ~"Congenital",
        variables %in% mother ~"Mother",
        variables %in% father ~"Father",
        variables %in% educ ~"Education",
        variables %in% income ~"Income",
        variables %in% birth ~"Birth",
        variables %in% civ_stat ~"Civil_Status",
        variables %!in% health ~"Baseline"
    ))

results_all <- results_list[[2047]]$results %>%
    mutate(group_names = case_when(
        variables %in% behavior ~"Behavior",
        variables %in% lifestyle ~"Lifestyle",
        variables %in% health ~"Health",
        variables %in% mental ~"Mental",
        variables %in% congenital ~"Congenital",
        variables %in% mother ~"Mother",
        variables %in% father ~"Father",
        variables %in% educ ~"Education",
        variables %in% income ~"Income",
        variables %in% birth ~"Birth",
        variables %in% civ_stat ~"Civil_Status",
        variables %!in% health ~"Baseline"
    ))

ui <- fluidPage(
    sidebarPanel("Choice of variable"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("dataset", "Select a dataset:",
                         choices = list("Behavior" = "dataset1",
                                        "Lifestyle" = "dataset2",
                                        "All Variables" = "dataset3"),
                         selected = "dataset1")
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output){
    dataset1 <- results1
    dataset2 <- results2
    dataset3 <- results_all
    
    selected_dataset <- reactive({
        if (input$dataset == "dataset1") {
            return(dataset1)
        } else if(input$dataset == "dataset2" ) {
            return(dataset2)
        }
        else {
            return(dataset3)
        }
    })
    
    output$plot <- renderPlot({
        selected_dataset() %>%
        ggplot(aes(y = variables, color = group_names)) + 
            theme_classic() +
            geom_point(aes(x = `exp(coef)`), shape = 15, size = 1) +
            geom_linerange(aes(xmin = `lower .95`, xmax = `upper .95`)) +
            geom_vline(xintercept = 1, linetype = "dashed") +
            xlim(c(0, max(`upper_95`)))
    })
}

shinyApp(ui, server)
