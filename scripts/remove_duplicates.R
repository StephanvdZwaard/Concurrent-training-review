remove_duplicates <- function(data) {
  
  # Remove duplicates by identical title names
  data <- data %>% distinct(title, .keep_all=T) 
  
  # Remove duplicates by approximate string matching
  data_match <- c()
  pb = txtProgressBar(min = 0, max = nrow(data), initial = 0, style=3) 
  for (i in 1:nrow(data)) {
    
    # search for similar titles with same publication year  
    pubs <- data %>% filter(pub_date == data$pub_date[i]) %>% pull(rowid)
    
    # Perform approximate string matching based on titles (within same year)
    ind  <- agrep(data$title[i], 
                  data %>% filter(pub_date == data$pub_date[i]) %>% pull(title), 
                  ignore.case = TRUE, value = FALSE, max.distance = 0.025)
    
    # Store row-index of duplicate title
    if (length(ind) > 1 & !c(data$title[i] %in% c("response","strength training","blood flow restriction training"))) {
       data_match <- rbind(data_match,
                           data.frame(rowid = data$rowid[i],
                                      duplicate_id = pubs[ind[!c(pubs[ind] %in% data$rowid[i])]],
                                      duplicate_seq = paste0(sort(pubs[ind]), collapse=' - '))) 
    }

    setTxtProgressBar(pb,i)
  }
  close(pb)
  
  # Combine duplicate search with original data
  data_test <- data      %>% left_join(data_match, by=c('rowid'))
  data_test <- data_test %>% left_join(data %>% select(rowid,title,authors,search) %>% rename(title_duplicate=title, authors_duplicate=authors, duplicate_search=search),
                                       by=c('duplicate_id'='rowid')) %>%
                             arrange(duplicate_seq,rowid) %>%
                             mutate(remove_duplicate = ifelse(!is.na(duplicate_seq) & duplicate_search=='pubmed',1,0)) #keep pubmed record if present.
  
  return(data_test)
  
}