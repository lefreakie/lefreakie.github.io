source("setup.R")

all_list_names <- read_from_excel("data/all_list_names.xlsx", T)
all_list_no_baseline <- read_rds("data/all_list_no_names.Rds")
prs_categories <- read_from_excel("data/prs_categories.xlsx", T)
prs_explanatory <- readxl::read_excel("data/prs_explanatory.xlsx")
prs_inventoried <- read_from_excel("data/prs_inventoried.xlsx", T)
prs_list <- read_from_excel("data/prs_list.xlsx")
somatic_variable_names <- read_from_excel("data/somatic_variable_names.xlsx", T)
somatic_variable_names_with_baseline <- read_from_excel("data/somatic_variable_names_with_baseline.xlsx", T)
all_list_no_baseline <- all_list_names[c(names(prs_categories), names(somatic_variable_names))]

intersect(prs_inventoried$mental, prs_inventoried$health)

total <- list()

for (i in seq(1, 2047, 89)){

  regression_names <- read_from_excel(paste("data/regression_name_list", i, ".xlsx", sep=""))
  regression_results <- read_from_excel(paste("data/regression_results", i, ".xlsx", sep=""))
  regression_tests <- read_from_excel(paste("data/regression_test_scores", i, ".xlsx", sep=""))

  for (j in 1:89){
  regression <- list(results = as.data.frame(regression_results[[j]]), names = as.data.frame(regression_names[[j]]), test_scores = as.data.frame(regression_tests[[j]]))

  total <- append(total, list(regression))
  }
}

baseline_summary <- read_from_excel("data/baseline_summary.xlsx")

write_rds(baseline_summary, "data/baseline_summary.Rds")
read_rds("data/baseline_summary.Rds")
# for tests:
# 1 => test score
# 2 => degrees of freedom
# 3 => p value

#write_rds(total, "data/full_regression_list.Rds")
read_rds("data/full_regression_list.Rds")
      