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

source('scripts/load_search_data.R')
source('scripts/remove_duplicates.R')
source('scripts/generate_template_extraction.R')
source('scripts/check_search.R')
source('scripts/show_data_fig1.R')
source('scripts/create_fig2.R')
source('scripts/create_fig5.R')
source('scripts/create_fig6.R')

# Get search data
data <- load_search_data()

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

data_extraction <- create_template_extract(R12_data)

writexl::write_xlsx(data_extraction,
                    path='data_extraction.xlsx')


# --------------- Figures ---------------
show_data_fig1()

data  <- read_excel('data/fig2 goed.xlsx')
data  <- data %>% filter(!c(`status used`==0 & `gender used`==0)) %>% arrange(study,desc(select_CT)) %>% group_by(study) %>% filter(row_number()==1) %>% select(1:5)
create_fig2(data)

data  <- read_excel('data/data for figure 5.xlsx')
create_fig5(data)

data  <- read_excel('data/data for figure 6.xlsx')
create_fig6(data)


