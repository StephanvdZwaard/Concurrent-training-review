show_data_fig1 <- function() {
  
  #Full search
  data <- read_excel('ct-data-full-search.xlsx')
  print(paste0('search: ',nrow(data)))
  print(data %>% group_by(search) %>% summarise(n=n()))
  
  #Screening title and abstract
  R1_data <- read_excel('screened-data/Screening Raven Huiberts laatste versie.xlsx')
  R2_data <- read_excel('screened-data/ct-data-no-duplicates_R2.xlsx')
  R2_data <- R2_data %>% select(rowid,screened_title_R2:Comment)
  R12_data <- left_join(R1_data,R2_data, by=c('rowid'))
  print(paste0('duplicates: ',nrow(data)-nrow(R12_data)))
  print(paste0('without duplicates: ',nrow(R12_data)))
  print(paste0('reviews: ',nrow(R12_data %>% filter(review==1))))
  print(paste0('screened titles (without reviews): ',nrow(R12_data %>% filter(review==0))))
  print(paste0('excluded titles: ',nrow(R12_data %>% filter(review==0))-nrow(R12_data %>% filter(screened_abstract_R2==1))))
  
  #Screening fulltext
  R1_data  <- read_excel('screened-data/screen-fulltext screening Raven.xlsx')
  R2_data  <- read_excel('screened-data/screen-fulltext_R2.xlsx')
  R2_data  <- R2_data %>% select(rowid,include_fulltext_R2,Comment_R2)
  R12_data <- left_join(R1_data,R2_data, by=c('rowid'))
  print(paste0('screened fulltexts: ',nrow(R12_data)))
  print(paste0('included fulltexts: ',nrow(R12_data %>% filter(include_fulltext_R2==1))))
  print(paste0('excluded fulltexts: ',nrow(R12_data)-nrow(R12_data %>% filter(include_fulltext_R2==1))))
  
  R12_data %>% filter(include_fulltext_R2==0) %>% 
    mutate(Comment_R2 = tolower(Comment_R2)) %>%
    mutate(Comment_R2 = case_when(str_detect(Comment_R2,'age')                    ~ 'no adult participants',
                                  str_detect(Comment_R2,'no relevant outcomes|outcomes')   ~ 'unsuitable outcomes',
                                  str_detect(Comment_R2,'no relevant control')    ~ 'no relevant control',
                                  str_detect(Comment_R2,'design|case report')  ~ 'unsuitable design',
                                  str_detect(Comment_R2,'et mode|aerobics|horse') ~ 'unsuitable endurance mode',
                                  str_detect(Comment_R2,'not retrieved') ~ 'not retrieved',
                                  str_detect(Comment_R2,'confounded by judo') ~ 'unsuitable strength training',
                                  str_detect(Comment_R2,'no et|limited et|et not standardized|limited volume of et') ~ 'unsuitable endurance training',
                                  TRUE ~ Comment_R2)) %>%
    group_by(Comment_R2) %>% summarise(n=n()) %>% arrange(desc(n)) %>% print()
  
  
}
