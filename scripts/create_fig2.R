create_fig2 <- function(data) {
  
colnames(data) <- c('study','var1','end_trained','str_trained','sex')
data <- data %>% mutate(end_trained = as.numeric(end_trained),
                        str_trained = as.numeric(str_trained)) %>%
        mutate(end_trained = case_when(end_trained == 1 ~ 'Untrained',
                                       end_trained == 2 ~ 'Trained',
                                       end_trained == 3 ~ 'Highly trained',
                                       TRUE ~ '')) %>%
        mutate(str_trained = case_when(str_trained == 1 ~ 'Untrained',
                                       str_trained == 2 ~ 'Trained',
                                       str_trained == 3 ~ 'Highly trained',
                                       TRUE ~ '')) %>%
        mutate(end_trained = factor(end_trained, levels = c('Untrained','Trained','Highly trained'), ordered = T),
               str_trained = factor(str_trained, levels = c('Untrained','Trained','Highly trained'), ordered = T))

theme_specific <- theme_classic() + theme(legend.position='none',
                                          axis.title   = element_text(size=12, face='bold'),
                                          axis.text.y  = element_text(size=11, face='bold'),
                                          axis.text.x  = element_text(size=12, face='bold'),
                                          strip.text   = element_text(size=12, face='bold'))

A <- data %>% group_by(end_trained,.drop = F) %>% summarise(n = n()) %>% mutate(type = 'Endurance') %>% filter(!is.na(end_trained)) %>%
          ggplot(aes(x=end_trained, y=n, fill=n, color='black')) + geom_col() + 
          geom_text(aes(x=end_trained, y=1.6, label=n)) +
          facet_wrap(~type) +
          #scale_fill_manual(values="#228B22") +
          scale_fill_gradient(low="white", high="#228B22", limits=c(1, 30)) +
          scale_y_continuous(limits = c(0,30)) + labs(y='Number of studies\n') +
          scale_color_manual(values='black') + labs(x='') + 
          theme_classic() + theme_specific

B <- data %>% group_by(str_trained,.drop = F) %>% summarise(n = n()) %>% mutate(type = 'Strength') %>% filter(!is.na(str_trained)) %>%
          ggplot(aes(x=str_trained, y=n, fill=n, color='black')) + geom_col() + 
          geom_text(aes(x=str_trained, y=1.6, label=n)) +
          facet_wrap(~type) +
          #scale_fill_manual(values="#228B22") +
          scale_fill_gradient(low="white", high="#228B22", limits=c(1, 30)) +
          scale_y_continuous(limits = c(0,30)) + labs(y='Number of studies\n') +
          scale_color_manual(values='black') + labs(x='') + 
          theme_classic() + theme_specific

C <- data %>% group_by(end_trained,str_trained, .drop=F) %>% summarise(n = n()) %>% filter(!is.na(end_trained) & !is.na(str_trained)) %>%
          ggplot(aes(x=str_trained, y=end_trained, fill=n, color='black')) + geom_tile(aes(color='black'),size=.75) + 
          geom_text(aes(x=str_trained, y=end_trained, label=n)) +
          scale_fill_gradient(low="white", high="#228B22", limits=c(1, 15), na.value = '#d3d3d3') +
          scale_color_manual(values='black') + labs(x='\nStrength', y='Endurance') + 
          theme_classic() + theme_specific

# Return and save figure
# ------------------------------------------------------   

# Save image as png
tiff(paste0("/Users/stephanvanderzwaard/Projects/research/2022-CT-review/results/Figure2_",format(Sys.Date(),"%d%m%y"),".tiff"),
     bg = "transparent", width = 6, height = 12, unit = "in", pointsize = 10, res = 1200)

graphic <- ggarrange(B, A, C,
                     labels = c("A.","B.","C."),
                     ncol = 1,
                     align = "v")
print({graphic})

dev.off()


# Return plot object
return(graphic)

}
