source("setup.R")

results_list <- read_rds("data/full_regression_list.Rds")

hash_table <- hashmap(compare_fn = all.equal, key_preproc_fn = sort)

for (i in 1:2047){
  hash_table[[sort(unlist(results_list[[i]]$names))]] <- results_list[[i]]$results
}

keys(hash_table)
