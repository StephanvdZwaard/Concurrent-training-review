load_search_data <- function() {
  
  # ------------------- Pubmed records -------------------
  
    pubmed <- list.files('data/search-data/','PubMed')
    
    data_pubmed <- c() 
    for (i in 1:length(pubmed)) {
      data <- read.csv(paste0('search-data/',pubmed[i]))
      data_pubmed <- rbind(data_pubmed,data)
    }
    
    data_pubmed <- data_pubmed %>% unique() %>% 
                   select(Title, Authors, `Journal.Book`, `Publication.Year`)%>%
                   mutate(search='pubmed')
    
    colnames(data_pubmed) <- c('title','authors','journal','pub_date','search')
  
  # -------------- Web of Science records -----------------
  
    wos <- list.files('data/search-data/','*xls')
    
    data_wos <- c() 
    for (i in 1:length(wos)) {
      data     <- read_excel(paste0('search-data/',wos[i]))
      data_wos <- rbind(data_wos,data)
    }
    
    data_wos <- data_wos %>% unique() %>% 
                select(`Article Title`, Authors, `Source Title`, `Publication Year`) %>%
                mutate(search='wos')
    
    colnames(data_wos) <- c('title','authors','journal','pub_date','search')
  
  # ---------------- SPORTDiscus records ------------------
  
    sportd <- list.files('data/search-data/','SportDiscus')
    
    data_sportd <- c() 
    for (i in 1:length(sportd)) {
      data <- read.csv(paste0('search-data/',sportd[i]))
      data_sportd <- rbind(data_sportd,data)
    }
    
    data_sportd <- data_sportd %>% unique() %>%
                   select(`Article.Title`, Author, `Journal.Title`, `Publication.Date`) %>%
                   mutate(search='sportdiscus',
                          Publication.Date = as.numeric(substr(Publication.Date,2,5)))
    
    colnames(data_sportd) <- c('title','authors','journal','pub_date','search')
    
  # Merge records
    data <- full_join(data_pubmed, data_wos, by=c('title','authors','journal','pub_date','search')) %>%
            full_join(data_sportd, by=c('title','authors','journal','pub_date','search'))
    
    return(data)
}