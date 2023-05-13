source("setup.R")

results_list <- read_rds("data/full_regression_list.Rds")
baseline_summary <- read_rds("data/baseline_summary.Rds")[[1]]

hash_table <- read_rds("data/hash_table.Rds")
groupings_table <- read_rds("data/groupings_table.Rds")
prs_inventoried <- read_from_excel("data/prs_inventoried.xlsx", T)
somatic_variable_names_with_baseline <- read_from_excel("data/somatic_variable_names_with_baseline.xlsx", T)
all_list_names_with_variables <- c(prs_inventoried, somatic_variable_names_with_baseline)

groupings <- hashmap(compare_fn = all.equal, key_preproc_fn = sort)

#for (a in names(prs_inventoried)){
for (a in names(all_list_names_with_variables)){
  groupings[all_list_names_with_variables[[a]]] = rep(a, length(all_list_names_with_variables[[a]]))
  #  groupings[prs_inventoried[[a]]] = rep(a, length(prs_inventoried[[a]]))
###  for (b in prs_inventoried[[a]]){
###    groupings[[b]] = a
###  }
}
#for (a in names(somatic_variable_names_with_baseline)){
#  groupings[somatic_variable_names_with_baseline[[a]]] = rep(a, length(somatic_variable_names_with_baseline[[a]]))
#}


hash_table <- hashmap(compare_fn = all.equal, key_preproc_fn = sort)

for (i in 1:2047){
  result_names <- results_list[[i]]$names
  names(result_names) <- NULL
  names(results_list[[i]]$names) <- NULL
  hash_table[[hash_index(result_names)]] <- results_list[[i]]$results
}
hash_table[[NULL]] <- as.data.frame(baseline_summary)

hash_table[[NULL]]

#write_rds(hash_table, "data/hash_table.Rds")
#write_rds(groupings, "data/groupings_table.Rds")

#keys(hash_table)

#map(x, ~var %in% .x ~ paste(.x))