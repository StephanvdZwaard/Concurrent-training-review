generate_template_extraction <- function(data) {
  
  data_extraction <-  data %>% filter(include_fulltext_R2 == 1) %>%
                      mutate(n_authors = str_count(authors, ',')+1) %>%
                      mutate(first_author = ifelse(n_authors>2,
                                                   gsub('[[:punct:]]','',word(authors,1)),
                                                   authors)) %>%
                      mutate(first_author = ifelse(first_author %in% c('de','da','e'),
                                                   paste0(first_author,' ',gsub('[[:punct:]]','',word(authors,2))),
                                                   first_author)) %>% 
                      mutate(first_author = ifelse(n_authors == 2,
                                                   gsub(',',' &',first_author),
                                                   first_author)) %>% 
                      mutate(first_author = ifelse(n_authors > 2,
                                                   paste0(first_author,' et al.'),
                                                   first_author)) %>% 
                      mutate(first_author = ifelse(first_author == 'hajizadeh et al.','maleki et al.',first_author)) %>%
                      select(first_author, pub_date, title) %>% rename(year = pub_date) %>% 
                      cbind(data.frame(N = NA,
                                       age = NA,
                                       sex = NA,
                                       VO2max_rel = NA,
                                       VO2max_abs = NA,
                                       POpeak_rel = NA,
                                       POpeak_abs = NA,
                                       Training_sessions_wk = NA,
                                       Training_h_wk = NA,
                                       Experience_y = NA,
                                       Strength_bench_press = NA,
                                       Strength_pull_up = NA,
                                       Strength_back_squat = NA,
                                       Strength_deadlift = NA,
                                       Training_uninterrupted = NA,
                                       Training_detraining = NA,
                                       Training_previous_exp = NA,
                                       Technique = NA,
                                       Duration = NA,
                                       Frequency = NA,
                                       ST_description = NA,
                                       ET_description = NA,
                                       ET_mode        = NA,
                                       Hypertrophy    = NA,
                                       Strength_lower_body = NA,
                                       Strength_upper_body = NA,
                                       Power = NA,
                                       VO2max = NA))
  
  return(data_extraction)
}