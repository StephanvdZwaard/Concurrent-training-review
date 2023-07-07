# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for obtaining performing concurrent training literature review                          #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Perform literature search including automated tools and outlier detection for concurrent training review   #
# Authors:      Stephan van der Zwaard [s.vanderzwaard@amsterdamumc.nl]                                                    #
# Date:         02-11-2022                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    4.2.1 (2022-10-31) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #

    # ------------------------------------------------------
    # Import libraries
    # ------------------------------------------------------   
    
      library(tidyverse)
      library(lubridate)
      library(stringr)
      library(stringi)
      library(readxl)
      library(ggpubr)
      library(cowplot)
      library(meta)
      library(metafor)

    # ------------------------------------------------------
    # Set options
    # ------------------------------------------------------   
    
      # set options
      options(stringsAsFactors = FALSE)

    # ------------------------------------------------------
    # Load helper scripts
    # ------------------------------------------------------   
    
      source('scripts/load_search_data.R')
      source('scripts/remove_duplicates.R')
      source('scripts/generate_template_extraction.R')
      source('scripts/generate_funnel_plot.R')
      source('scripts/show_data_fig1.R')
      source('scripts/create_fig2.R')
      source('scripts/create_fig4.R')
      source('scripts/create_fig5.R')

    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Literature search                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    
    # ------------------------------------------------------
    # Full search
    # ------------------------------------------------------   
    
        # Get data from database searches
        data <- load_search_data()
        
        # Process data (title, authors and detect whether it concerns a review article)
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
      
        
    # ------------------------------------------------------
    # Search without duplicates
    # ------------------------------------------------------   
        
        # Detect and remove duplicate titles from multiple database searches
        data <- remove_duplicates(data)
        data_filt <- rbind(data %>%  filter(is.na(duplicate_seq)),
                           data %>%  filter(remove_duplicate!=1 & !is.na(duplicate_seq)) %>% 
                                     group_by(duplicate_seq) %>% 
                                     arrange(search) %>% 
                                     filter(row_number()==1) %>% 
                                     ungroup()) 
        data_filt <- data_filt %>% 
                     arrange(review, pub_date, rowid)
        
        # -- Save data search without duplicates -- 
        writexl::write_xlsx(data_filt %>% select(rowid:review), path='ct-data-no-duplicates.xlsx')

        
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                    Literature screening                                                  #
# ------------------------------------------------------------------------------------------------------------------------ #
    
        
        # ------------------------------------------------------
        # Screening titles & abstracts
        # ------------------------------------------------------  
  
          # Load screening from reviewers
          R1_data <- read_excel('data/screened-data/Screening Raven Huiberts laatste versie.xlsx')
          R2_data <- read_excel('data/screened-data/ct-data-no-duplicates_R2.xlsx')
          R2_data <- R2_data %>% select(rowid,screened_title_R2:Comment)
          
          # Combine screening of both reviewers and check if there are any discrepancies
          R12_data <- left_join(R1_data,R2_data, by=c('rowid'))
          R12_data %>% filter(screened_abstract_R1 != screened_abstract_R2 | screened_abstract_R1 ==1 & screened_title_R2 ==0 | screened_title_R1==0 & screened_abstract_R2==1) %>% view()
          
          # Save results
          writexl::write_xlsx(R12_data %>% 
                              filter(screened_abstract_R1 != screened_abstract_R2 | screened_abstract_R1 ==1 & screened_title_R2 ==0 | screened_title_R1==0 & screened_abstract_R2==1) %>% 
                              arrange(screened_abstract_R2),
                              path='compare_R12_v3.xlsx')

          # ------------------------------------------------------
          # Screening full-texts
          # ------------------------------------------------------  
          
            #Save fulltexts to be screened
            writexl::write_xlsx(R12_data %>% 
                                filter(screened_abstract_R1 ==1 & screened_abstract_R2 ==1) %>% select(-c(`Comment.x`,contains('screened_title'))) %>% 
                                arrange(pub_date),
                                path='screen-fulltext.xlsx')
          
            # Load screening from reviewers and join. 
            R1_data  <- read_excel('data/screened-data/screen-fulltext screening Raven.xlsx')
            R2_data  <- read_excel('data/screened-data/screen-fulltext_R2.xlsx')
            R2_data  <- R2_data %>% select(rowid,include_fulltext_R2,Comment_R2)
            
            # Combine screening of both reviewers and check if there are any discrepancies
            R12_data <- left_join(R1_data,R2_data, by=c('rowid'))
            R12_data %>% filter(include_fulltext_R1 != include_fulltext_R2) %>% view() #filter(!is.na(Comment_R1) | !is.na(Comment_R2)) %>% view()

            # Save results
            writexl::write_xlsx(R12_data %>% filter(include_fulltext_R1 != include_fulltext_R2),
                                path='compare_fulltext_R12.xlsx')
            
            # writexl::write_xlsx(R12_data %>% filter(include_fulltext_R1 == include_fulltext_R2) %>% filter(!is.na(Comment_R1) | !is.na(Comment_R2)),
            #                     path='check_comments.xlsx')

            #Final selection for data extraction 
            writexl::write_xlsx(R12_data %>% filter(include_fulltext_R2 == 1),
                                path='final_inclusion.xlsx')
 
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                    Data extraction.                                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
 
            
        # Create template for data extraction           
        data_extraction <- create_template_extract(R12_data)
        
        writexl::write_xlsx(data_extraction,
                            path='data_extraction.xlsx')
        
        
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                    Sensitivity analysis                                                  #
# ------------------------------------------------------------------------------------------------------------------------ #

        
        # Perform sensitivity analysis
        source('scripts/check-outliers.R')
        

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                    Results - Figures                                                     #
# ------------------------------------------------------------------------------------------------------------------------ #
        
      # Generate figure 1 data
        show_data_fig1()

      # Generate figure 2 
        data  <- read_excel('data/fig2 goed.xlsx')
        data  <- data %>% filter(!c(`status used`==0 & `gender used`==0)) %>% arrange(study,desc(select_CT)) %>% group_by(study) %>% filter(row_number()==1) %>% select(1:5)
        create_fig2(data)

      # Generate figure 3 via RevMan application

      # Generate figure 4
        data  <- read_excel('data/data for figure 4.xlsx')
        create_fig4(data)

      # Generate figure 5
        data  <- read_excel('data/data for figure 5.xlsx')
        create_fig5(data)

      # Generate figure 6 via RevMan application
        
