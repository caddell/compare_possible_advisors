library(scholar)
library(tidyverse)
library(ggthemes)
library(packcircles)
library(ggrepel)
library(patchwork)

#scholar package from https://github.com/jkeirstead/scholar

#get scholar data
id1 <- 'HaszNbkAAAAJ'
id2 <- "0lTzdfwAAAAJ"

# Get his profile and print his name
profile <- get_profile(id1)

# Get his citation history, i.e. citations to his work in a given year 
cites <- get_citation_history(id1)

# Get his publications (a large data frame)
pubs <- get_publications(id1)

compare <- compare_scholar_careers(ids = c(id1, id2))

ggplot(compare, aes(year, cites, color = name))+
  geom_path()+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  scale_color_colorblind()


pull_recent_coauthors <- function(id, year_start) {
  
  pubs <- get_publications(id) %>% 
    filter(year >= year_start)
  
  coauthor_names <- pubs %>% 
    pmap_df(function(...){
        current <- tibble(...)
        get_complete_authors(id = id, pubid = current$pubid, delay = .5)
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
  
  #some searches don't find ids ... https://stackoverflow.com/questions/66461284/retrieving-google-scholar-id-within-a-for-function
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
  
  return(coauthor_ids)
}

#profile for list iteration
get_whole_profile <- function(x){
  temp <- get_profile(id = x$scholar_id) 
  return(temp)
}

#pull data from profile lst
pull_profile_data <- function(profile_list) {
  df <- tibble(id = profile_list$id,
               name = profile_list$name,
               affiliation = profile_list$affiliation,
               total_cites = profile_list$total_cites,
               h_index = profile_list$h_index)
  return(df)
}

#get scholar data
id1 <- 'HaszNbkAAAAJ'
id2 <- "0lTzdfwAAAAJ"

coauthor_ids_id1<- pull_recent_coauthors(id = id1, year_start = 2018)
coauthor_ids_id2<- pull_recent_coauthors(id = id2, year_start = 2018)

#add tags and combine both sets
coauthor_ids_id1 <- coauthor_ids_id1 %>% mutate(author_id = id1)
coauthor_ids_id2 <- coauthor_ids_id2 %>% mutate(author_id = id2)

coauthor_ids <- bind_rows(coauthor_ids_id1, coauthor_ids_id2)

#get profiles for each coauthor
coauthor_details <- coauthor_ids %>% 
  select(scholar_id) %>% 
  split(., coauthor_ids %>% select(scholar_id)) %>% 
  lapply(., get_whole_profile) 

#get simple data and put it in dataframe for each coauthor
coauthor_details_df <- coauthor_details %>% 
  lapply(., pull_profile_data) %>% 
  bind_rows() %>% 
  left_join(coauthor_ids, by = c("id" = "scholar_id")) %>% 
  mutate(label = case_when(id == author_id ~ name,
                           TRUE ~ "")) %>% 
  arrange(author_id)


#circle packing plot https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level.html
# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value

draw_author_circle_graph <- function(coauthor_df, filter_author_id) {
  # coauthor_df <- coauthor_details_df
  # filter_author_id <- id2
  max_h_index <- coauthor_df %>% select(h_index) %>% max()
  
  coauthor_df <- coauthor_df %>% 
    filter(author_id == filter_author_id | h_index == max_h_index) %>% 
    mutate(packing_id = row_number())
  
  packing <- circleProgressiveLayout(coauthor_df$h_index, sizetype='area') %>% 
    mutate(packing_id = row_number())
  
  # We can add these packing information to the initial data frame / filter to authors we care about
  coauthor_df <- cbind(coauthor_df, packing %>% select(-packing_id)) %>% 
    filter(author_id == filter_author_id)
  
  packing <- semi_join(packing, coauthor_df)
  
  # The next step is to go from one center + a radius to the coordinates of a circle that
  # is drawn by a multitude of straight lines.
  dat.gg <- circleLayoutVertices(packing, npoints=50) %>% 
    left_join(coauthor_df %>% select(label, packing_id), by = c("id" = "packing_id"))
  
  # Make the plot
  p <- ggplot() + 
    # Make the bubbles
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill = factor(label)), colour = "black", alpha = 0.6) +
    # Add text in the center of each bubble + control its size
    #geom_text_repel(data = coauthor_df, aes(x, y, size=1, label = label)) +
    scale_size_continuous(range = c(1,4)) +
    scale_x_continuous(limits = c(-30,30))+
    # General theme:
    theme_void() + 
    theme(legend.position="none") +
    coord_equal()+
    scale_fill_colorblind()
  plot(p)
  return(p)
}

p1 <- draw_author_circle_graph(coauthor_details_df,id1)
p2 <- draw_author_circle_graph(coauthor_details_df,id2)

#combine both plots
p_combined <-  p1 + p2

#find x and y ranges
p_ranges_x <- c(ggplot_build(p_combined[[1]])$layout$panel_scales_x[[1]]$range$range,
                ggplot_build(p_combined[[2]])$layout$panel_scales_x[[1]]$range$range)

p_ranges_y <- c(ggplot_build(p_combined[[1]])$layout$panel_scales_y[[1]]$range$range,
                ggplot_build(p_combined[[2]])$layout$panel_scales_y[[1]]$range$range)

#plot with same scale
p_combined +
  plot_annotation(title = 'Author Networks Since 2018 (h-index as radius)',
                  subtitle = 'Potential PIs in gold',
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5))) & 
  xlim(min(p_ranges_x), max(p_ranges_x)) & 
  ylim(min(p_ranges_y), max(p_ranges_y)) 

