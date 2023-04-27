package_list = c("tidyverse", "usethis", "r2r")

`%!in%` <- negate(`%in%`)

# tjek for manglende pakker og installer om nødvendigt
if (any(package_list %!in% installed.packages())) {
  invisible(sapply(package_list, function(x){if ((x %!in% installed.packages())){
    install.packages(x)
    }
  }))
}

# indlæs pakker
invisible(sapply(package_list, function(x){as.name(x)}))

read_from_excel <- function(input){
  sheets <- readxl::excel_sheets(input)
  output <- map(sheets, function(x){readxl::read_xlsx(input, sheet = x)})
  output
}
