library(scholar)
library(tidyverse)
library(ggthemes)
library(packcircles)
library(ggrepel)

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

compare <- compare_scholar_careers(ids = c(id1, id2))

ggplot(compare, aes(year, cites, color = name))+
  geom_path()+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  scale_color_colorblind()

coauthor_names <- get_complete_authors(id = id1, pubid = pubs %>% head(1) %>% select(pubid))

#find coauthors from 2019 till now
coauthor_names <- pubs %>% 
  filter(year > 2018) %>% 
  pmap_df(function(...){
    current <-  tibble(...)
    get_complete_authors(id = id1, pubid = current$pubid)
  })


#pulls full names from coauthor_names
coauthor_names <- names(coauthor_names)

#split names into columns, keep unique
coauthor_names <- coauthor_names %>% 
  tibble() %>% 
  str_split(pattern = ',') %>% 
  flatten() %>% 
  str_trim() %>% 
  str_remove_all(pattern = '[()\"]') %>% 
  str_split_fixed(pattern = " ", n = 3) %>% 
  data.frame() %>% 
  rename(first = X1, middle = X2, last = X3) %>% 
  mutate(last = case_when(nchar(last) < 1 ~ middle,
                          TRUE ~ last),
         middle= case_when(middle == last ~ '',
                           TRUE ~ middle)) %>% 
  unique()

#so searchs don't find ids ... https://stackoverflow.com/questions/66461284/retrieving-google-scholar-id-within-a-for-function
p_get_scholar_id <- possibly(get_scholar_id, otherwise = NA_character_)

#pull scholar ids for each coauthor
coauthor_ids <- coauthor_names %>% 
  pmap_df(function(...){
    current <- tibble(...)
    current %>% 
      mutate(scholar_id = p_get_scholar_id(last_name = current$last, first_name = current$first))
  })

#drop NAs and reduce to uniques
coauthor_ids <- coauthor_ids %>% 
  select(scholar_id) %>% 
  unique() %>% 
  drop_na()

#profile for list iteration
get_whole_profile <- function(x){
  temp <- get_profile(id = x$scholar_id) 
  
  return(temp)
}

temp <- coauthor_ids %>% split(., coauthor_ids)
temp <- lapply(temp, get_whole_profile)

#pull data from profile lst
pull_profile_data <- function(profile_list) {
  df <- tibble(id = profile_list$id,
               name = profile_list$name,
               affiliation = profile_list$affiliation,
               total_cites = profile_list$total_cites,
               h_index = profile_list$h_index)
  return(df)
}

temp2 <- lapply(temp, pull_profile_data) %>% bind_rows()

#circle packing plot https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level.html


# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(temp2$h_index, sizetype='area')

# We can add these packing information to the initial data frame
temp3 <- cbind(temp2, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
#plot(temp3$radius, temp3$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot() + 
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id), fill = 'green', colour = "black", alpha = 0.6) +
  # Add text in the center of each bubble + control its size
  geom_text_repel(data = temp3, aes(x, y, size=1, label = name)) +
  scale_size_continuous(range = c(1,4)) +
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()
