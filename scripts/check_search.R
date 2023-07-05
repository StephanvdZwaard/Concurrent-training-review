# Articles selected by Raven
author_names   <- tolower(c('Balabinis|Beattie|Bell|Bishop|Chtara|Damasceno|Dolezal|Fyfe|Glowacki|Gravelle|Hakkinen|Häkkinen|Hendrickson|Jackson|Johnston|Kelly|Kraemer|Leveritt|Levin|Libardi|Losnegard|Mikkola|Millet|Paavolainen|Psilander|Rønnestad|Shaw|Silva|Skovgaard|Souza|de Souza|Spiliopoulou|Støren|Sunde|Terzis|Trowell|Tsitkanou|Vikmoen'))
author_names   <- stri_trans_general(author_names, "Latin-ASCII")

# Additional articles selected by other CT reviews
schumann_names <- tolower(c('Cadore|Cantrell|Gettman|haykowsky|Hennessy|Hickson|Jones|Karavirta|Kazior|Laird|Lee|Lundberg|McCarthy|Osuka|Panissa|Robineau|Sale|Shamim|Sillanpää|Timmins|'))
pito_names     <- tolower(c('Ghahramanloo|Varela-sanz|BEZERRA|AZARBAYJANI|SCHAUN|NINDL|SELLAMI|MICHELL|ARAZI|Cho|Gergley|Izquierdo|Sousa|'))
petre_names    <- tolower(c('Craig|Volpe|Mirghani|'))
wilson_names   <- tolower(c('Ahtiainen|'))
leveritt_names <- tolower(c('Dudley|Hunter|Nelson|Sale|Abernethy|'))
murlasits_names <- tolower(c('Banitalebi|Davitt|Eklund|Makhlouf|Pinto|Schumann|Wilhelm|'))
sabag_names    <- tolower(c('Gentil|Kikuchi|Ross|'))
sousa_names    <- tolower(c('Petré|Wong|'))
eddens_names   <- tolower(c('MacNeil|Collins|Okamoto|McGawley|'))
vikmoen_names  <- tolower(c('Sedano|Izquierdo-Gabarren|Koninckx|Ferrauti|Bastiaans'))
review_names   <- paste0(schumann_names, pito_names, petre_names, wilson_names, leveritt_names, murlasits_names, sabag_names, sousa_names, eddens_names, vikmoen_names, collapse="")
review_names   <- stri_trans_general(review_names, "Latin-ASCII")

data %>% #filter(str_detect(word(authors,1),author_names) | str_detect(word(authors,1,2),author_names)) %>% 
         filter(str_detect(word(authors,1),review_names) | str_detect(word(authors,1,2),review_names)) %>% 
         mutate(first_author = gsub('[[:punct:]]','',word(authors,1))) %>%
         mutate(first_author = ifelse(first_author %in% c('de','da','e'),
                                      gsub('[[:punct:]]','',word(authors,2)),
                                      first_author)) %>%
         mutate(include  = ifelse(first_author == str_match(first_author,gsub('-','',review_names))[,1],1,0)) %>%
         filter(include == 1) %>%
         filter(review == 0) %>%
         arrange(first_author,pub_date,title) %>% 
         select(title,first_author,pub_date,everything()) %>% view()

# 16 extra artikelen die niet binnen de search zitten.
