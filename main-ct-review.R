# Script to analyse search for CT review
#
# Author:   S. van der Zwaard
# Date:     02-11-2022
#

library(tidyverse)
library(lubridate)
library(stringr)
library(stringi)
library(readxl)
library(ggpubr)
library(cowplot)

#install.packages(c('lubridate','readxl','stringr'))

source('scripts/remove_duplicates.R')
source('scripts/check_search.R')
source('scripts/show_data_fig1.R')
source('scripts/create_fig2.R')
source('scripts/create_fig5.R')
source('scripts/create_fig6.R')

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

# Process data
data <- data %>%
        mutate(title   = tolower(title),
               authors = tolower(authors)) %>%
        mutate(authors = stri_trans_general(authors, "Latin-ASCII")) %>% 
        mutate(title   = gsub(" \\(abstract\\)", "", title)) %>%
        mutate(title   = gsub("&", "and", title)) %>%
        mutate(title   = sub("[.]$", "", title)) %>%
        rowid_to_column() %>%
        mutate(review = ifelse(str_detect(title,'review|meta-analysis'),1,0)) 

# -- Save data full search -- 
writexl::write_xlsx(data,path='ct-data-full-search.xlsx')
  
# -- Save data no duplicates & reviews -- 
data <- remove_duplicates(data)
data_filt <- rbind(data %>%  filter(is.na(duplicate_seq)),
                   data %>%  filter(remove_duplicate!=1 & !is.na(duplicate_seq)) %>% group_by(duplicate_seq) %>% arrange(search) %>% filter(row_number()==1) %>% ungroup()) 
data_filt <- data_filt %>% arrange(review, pub_date, rowid)

writexl::write_xlsx(data_filt %>% select(rowid:review), path='ct-data-no-duplicates.xlsx')

# ------------------------------------------------------------------------------

#Screening title and abstract
R1_data <- read_excel('data/screened-data/Screening Raven Huiberts laatste versie.xlsx')
R2_data <- read_excel('data/screened-data/ct-data-no-duplicates_R2.xlsx')
R2_data <- R2_data %>% select(rowid,screened_title_R2:Comment)

R12_data <- left_join(R1_data,R2_data, by=c('rowid'))
R12_data %>% filter(screened_abstract_R1 != screened_abstract_R2 | screened_abstract_R1 ==1 & screened_title_R2 ==0 | screened_title_R1==0 & screened_abstract_R2==1) %>% view()

writexl::write_xlsx(R12_data %>% filter(screened_abstract_R1 != screened_abstract_R2 | screened_abstract_R1 ==1 & screened_title_R2 ==0 | screened_title_R1==0 & screened_abstract_R2==1) %>% arrange(screened_abstract_R2),
                    path='compare_R12_v3.xlsx')


#Screening fulltexts
writexl::write_xlsx(R12_data %>% filter(screened_abstract_R1 ==1 & screened_abstract_R2 ==1) %>% select(-c(`Comment.x`,contains('screened_title'))) %>% arrange(pub_date),
                    path='screen-fulltext.xlsx')

R1_data  <- read_excel('data/screened-data/screen-fulltext screening Raven.xlsx')
R2_data  <- read_excel('data/screened-data/screen-fulltext_R2.xlsx')
R2_data  <- R2_data %>% select(rowid,include_fulltext_R2,Comment_R2)
R12_data <- left_join(R1_data,R2_data, by=c('rowid'))

R12_data %>% filter(include_fulltext_R1 != include_fulltext_R2) %>% view() #filter(!is.na(Comment_R1) | !is.na(Comment_R2)) %>% view()

writexl::write_xlsx(R12_data %>% filter(include_fulltext_R1 != include_fulltext_R2),
                    path='compare_fulltext_R12.xlsx')

writexl::write_xlsx(R12_data %>% filter(include_fulltext_R1 == include_fulltext_R2) %>% filter(!is.na(Comment_R1) | !is.na(Comment_R2)),
                    path='check_comments.xlsx')

#Final selection for data extraction 
writexl::write_xlsx(R12_data %>% filter(include_fulltext_R2 == 1),
                    path='final_inclusion.xlsx')

data_extraction <- R12_data %>% filter(include_fulltext_R2 == 1) %>%
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

writexl::write_xlsx(data_extraction,
                    path='data_extraction.xlsx')


data_test %>% mutate(duplicate = ifelse(gsub('[[:punct:]]','',word(authors.x, 1)) == gsub('[[:punct:]]','',word(authors.y,1)),1,0)) %>% view()


R2_data %>% filter(!is.na(`Interesting?`)) %>% 
            mutate(category = case_when(str_detect(title,'myonucl|satellite|stem cell')             ~ 'sat',
                                        str_detect(title,'football|soccer|repeated-sprint ability') ~ 'soccer',
                                        str_detect(title,'swim')                                    ~ 'swimming',
                                        str_detect(title,'gene|genotype|polymorphism')              ~ 'genetics',
                                        str_detect(title,'fasting')                                 ~ 'sportvasten',
                                        TRUE ~ '')) %>%  writexl::write_xlsx(path='interesting_articles.xlsx')



# --------------- Figures ---------------
show_data_fig1()

data  <- read_excel('data/fig2 goed.xlsx')
data  <- data %>% filter(!c(`status used`==0 & `gender used`==0)) %>% arrange(study,desc(select_CT)) %>% group_by(study) %>% filter(row_number()==1) %>% select(1:5)
create_fig2(data)

data  <- read_excel('data/data for figure 5.xlsx')
create_fig5(data)

data  <- read_excel('data/data for figure 6.xlsx')
create_fig6(data)


