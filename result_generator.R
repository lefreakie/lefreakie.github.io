source("setup.R")

all_list_names <- unlist(read_from_excel("data/all_list_names.xlsx"), recursive = F)
prs_categories <- unlist(read_from_excel("data/prs_categories.xlsx"), recursive = F)
prs_explanatory <- read_from_excel("data/prs_explanatory.xlsx")[[1]]
prs_inventoried <- unlist(read_from_excel("data/prs_inventoried.xlsx"), recursive = F)
prs_list <- read_from_excel("data/prs_list.xlsx")
somatic_variable_names <- unlist(read_from_excel("data/somatic_variable_names.xlsx"), recursive = F)
somatic_variable_names_with_names <- unlist(read_from_excel("data/somatic_variable_names_with_baseline.xlsx"), recursive = F)

total <- list()

for (i in seq(1, 2047, 89)){
#
#  regression_names <- read_from_excel(paste("data/regression_name_list", i, ".xlsx", sep=""))
#  regression_results <- read_from_excel(paste("data/regression_results", i, ".xlsx", sep=""))
#  regression_tests <- read_from_excel(paste("data/regression_test_scores", i, ".xlsx", sep=""))

#  for (j in 1:89){
#  regression <- list(results = as.data.frame(regression_results[[j]]), names = as.data.frame(regression_names[[j]]), test_scores = as.data.frame(regression_tests[[j]]))

#  total <- append(total, list(regression))
#  }
}

total[[1]]
# for tests:
# 1 => test score
# 2 => degrees of freedom
# 3 => p value

write_rds(total, "data/full_regression_list.Rds")
read_rds("data/full_regression_list.Rds")
      