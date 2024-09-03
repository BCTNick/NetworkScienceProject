library(rvest)
library(httr)
library(dplyr)

get_author_id <- function(name) {
  base_url <- "https://scholar.google.com/scholar"
  search_url <- paste0(base_url, "?q=", URLencode(name))
  
  page <- read_html(search_url)
  
  # Extracting author profile links
  author_nodes <- html_nodes(page, "h3.gs_ai_name a")
  if (length(author_nodes) == 0) return(NA)
  
  author_urls <- html_attr(author_nodes, "href")
  
  # Return the first author's ID
  return(str_extract(author_urls[1], "user=[^&]+"))
}

library(scholar)

get_affiliation <- function(author_id) {
  if (is.na(author_id)) return(NA)
  profile <- tryCatch({
    get_profile(sub("user=", "", author_id))
  }, error = function(e) {
    return(NA)
  })
  return(profile$affiliation)
}

author_names <- c("John Doe", "Jane Smith", "Albert Einstein")  # Your list of 1,800 names

results <- data.frame(name = author_names, author_id = NA, affiliation = NA, stringsAsFactors = FALSE)

for (i in 1:nrow(results)) {
  author_id <- get_author_id(results$name[i])
  results$author_id[i] <- author_id
  results$affiliation[i] <- get_affiliation(author_id)
  Sys.sleep(runif(1, 1, 3))  # Sleep between requests to avoid being blocked
}

# Print the results
print(results)
