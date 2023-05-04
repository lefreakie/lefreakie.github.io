source("setup.R")

results_list <- read_rds("data/full_regression_list.Rds")

hash_table <- read_rds("data/hash_table.Rds")
groupings_table <- read_rds("data/groupings_table.Rds")

groupings <- hashmap(compare_fn = all.equal, key_preproc_fn = sort)

for (a in names(prs_inventoried)){
  groupings[prs_inventoried[[a]]] = rep(a, length(prs_inventoried[[a]]))
###  for (b in prs_inventoried[[a]]){
###    groupings[[b]] = a
###  }
}
for (a in names(somatic_variable_names_with_baseline)){
  groupings[somatic_variable_names_with_baseline[[a]]] = rep(a, length(somatic_variable_names_with_baseline[[a]]))
}


hash_table <- hashmap(compare_fn = all.equal, key_preproc_fn = sort)

for (i in 1:2047){
  result_names <- results_list[[i]]$names
  names(result_names) <- NULL
  names(results_list[[i]]$names) <- NULL
  hash_table[[sort(unlist(result_names))]] <- results_list[[i]]$results
}

#write_rds(hash_table, "data/hash_table.Rds")
#write_rds(groupings_table, "data/grouping_table.Rds")

#keys(hash_table)

#map(x, ~var %in% .x ~ paste(.x))