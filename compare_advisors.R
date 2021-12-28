library(scholar)
library(tidyverse)

#scholar package from https://github.com/jkeirstead/scholar

#get scholar data
id1 <- 'HaszNbkAAAAJ'

# Get his profile and print his name
profile <- get_profile(id1)

# Get his citation history, i.e. citations to his work in a given year 
cites <- get_citation_history(id1)

# Get his publications (a large data frame)
pubs <- get_publications(id1)

id2 <- "0lTzdfwAAAAJ"

compare_scholar_careers(ids = c(id1, id2))
