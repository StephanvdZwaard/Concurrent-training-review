create_fig6 <- function(data) {
  
   data  <- data %>% 
            mutate(Outcome = factor(Outcome, levels=c('Strength (upper-body)','Strength (lower-body)','Power','Hypertrophy','VO2max'))) %>%
            mutate(P_subgroup = case_when(P_subgroup < .01 ~ '**',
                                          P_subgroup <= .05 ~ '*',
                                          TRUE ~ '')) 
   
   data_p_end <- data %>% filter(Type == 'Endurance') %>% 
             pivot_wider(names_from = 'Training status', values_from = 'Training status') %>%
             group_by(Outcome) %>% mutate(Untrained        = ifelse(is.na(Untrained),'Untrained',Untrained),
                                          Trained          = ifelse(is.na(Trained),'Trained',Trained),
                                          `Highly trained` = ifelse(is.na(`Highly trained`),'Highly trained',`Highly trained`)) %>%
             filter(row_number()==1) %>%
             mutate(P_comparison = case_when(P_comparison < .01 ~ '**',
                                             P_comparison <= .05 ~ '*',
                                             TRUE ~ '')) %>%
             mutate(P_comparison = ifelse(P_comparison == '',NA,P_comparison))
   
   data_p_str <-  data %>% filter(Type == 'Strength') %>% 
             pivot_wider(names_from = 'Training status', values_from = 'Training status') %>%
             group_by(Outcome) %>% mutate(Untrained        = ifelse(is.na(Untrained),'Untrained',Untrained),
                                          Trained          = ifelse(is.na(Trained),'Trained',Trained),
                                          `Highly trained` = ifelse(is.na(`Highly trained`),'Highly trained',`Highly trained`),) %>%
             filter(row_number()==1) %>%
             mutate(P_comparison = case_when(P_comparison < .01 ~ '**',
                                             P_comparison < .05 ~ '*',
                                             TRUE ~ '')) %>%
             mutate(P_comparison = ifelse(P_comparison == '',NA,P_comparison))
   
   ylims <- 2#max(abs(c(data$CI_lower,data$CI_upper)),na.rm=T)
  
  A <- ggplot(data %>% filter(Type=='Endurance'), aes(x=`Training status`, y=SMD, ymin = CI_lower, ymax = CI_upper)) + 
        geom_rect(aes(ymin=-.2, ymax=.2), xmin =0, xmax = 4, fill = '#e9e9e9') +
        geom_hline(yintercept=0, linetype=2, color='darkgrey',linewidth=.65) +
        geom_pointrange(aes(color=Type)) +
        geom_text(aes(x=`Training status`, y=ifelse(SMD>0,CI_upper+.1,CI_lower-.1), label=P_subgroup),size=4.5, vjust=0.75) + 
        facet_wrap(~Outcome, ncol=1) + 
        geom_signif(data = data_p_end,
                    aes(xmin=Untrained, xmax = `Highly trained`, annotations = P_comparison, y_position = 1.4),
                    textsize = 5, tip_length = 0.005, vjust = 0.2,
                    manual = TRUE) +
        ylim(c(-ylims,ylims))+ 
        labs(x='',color='') + 
        coord_flip() + 
        scale_color_manual(values=c("blue")) + 
        theme_bw() + theme(legend.position = 'none', plot.margin = margin(1,1,0,0, "cm")) 
  A <- add_sub(A, "favors ST/ET ←                       → favors CT", x = 0.5, y=0.1, hjust = 0.55, vjust = -1.45, vpadding=grid::unit(0, "lines"), size=9, color='darkgrey')
  
  B <- ggplot(data %>% filter(Type=='Strength'), aes(x=`Training status`, y=SMD, ymin = CI_lower, ymax = CI_upper)) + 
        geom_rect(aes(ymin=-.2, ymax=.2), xmin =0, xmax = 4, fill = '#e9e9e9') +
        geom_hline(yintercept=0, linetype=2, color='darkgrey',linewidth=.65) +
        geom_pointrange(aes(color=Type)) +
        geom_text(aes(x=`Training status`, y=ifelse(SMD>0,CI_upper+.1,CI_lower-.1), label=P_subgroup),
                  size=4.5, vjust=0.75) + 
        facet_wrap(~Outcome, ncol=1) + 
        geom_signif(data = data_p_str,
                    aes(xmin=Untrained, xmax = `Highly trained`, annotations = P_comparison, y_position = 1.4),
                    textsize = 5, tip_length = 0.005, vjust = 0.2,
                    manual = TRUE) +
        ylim(c(-ylims,ylims))+ 
        labs(x='',color='') + 
        coord_flip() + 
        scale_color_manual(values=c("red")) + 
        theme_bw() + theme(legend.position = 'none', plot.margin = margin(1,1,0,0, "cm"))
  B <- add_sub(B, "favors Strenght/Endurance Training ←                       → favors Concurrent Training", x = 0.5, y=0.1, hjust = 0.55, vjust = -2.05, vpadding=grid::unit(0, "lines"), size=9, color='darkgrey')
  
  # Return and save figure
  # ------------------------------------------------------   
  
  # Save image as png
  tiff(paste0("/Users/stephanvanderzwaard/Projects/research/2022-CT-review/results/Figure6_",format(Sys.Date(),"%d%m%y"),".tiff"),
       bg = "transparent", width = 8, height = 6, unit = "in", pointsize = 10, res = 1200)

  graphic <- ggarrange(A, B,
                       labels = c("A. Endurance training status","B. Strength training status"),
                       ncol = 2,
                       hjust = -0.1,
                       align = "v")
  print({graphic})
  
  dev.off()
  
  
  # Return plot object
  return(graphic)
  
}