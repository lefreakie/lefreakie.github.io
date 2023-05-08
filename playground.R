test <- results_list[[200]]

names <- test$names

var <- test$results$variables[-1]

catnames <- (all_list_names)

prs_inventoried[a]

for (a in names(prs_inventoried)){
  catnames[a] <- prs_inventoried[a]
}


results <- test$results
names(catnames)

vec <- vector(mode = "character", length = length(var))

for (i in seq_along(var)){
  for (c in names(catnames)) {
    if(var[i] %in% catnames[[c]]){
      vec[i] = c
    }
#    print(any(catnames[[12]] %in% var[i]))
  }
}

map(var, ~.x %in% unlist(catnames, use.names = F))


#method <- function(x){if (var %in% x) paste(x))}

results %>%
  rowwise(variables) %>%
  mutate(group_names = if(!is_null(groupings[[variables]])) groupings[[variables]] else groupings[[str_replace(variables, "(.*[a-z|A-Z]+)[0-9]$", "\\1")]]) %>%
  ungroup()

results[[1]] %in% unlist(somatic_variable_names_with_baseline, use.names = F)

str_replace(results[[1]], "(.*[a-z|A-Z]+)[0-9]$", "\\1")

str_detect(results[[1]], "[A-Z|a-z]*[0-9]$")

# største upper .95 værdi
#max(unlist(map(map(results_list, ~.x[[1]][9]), max)))
