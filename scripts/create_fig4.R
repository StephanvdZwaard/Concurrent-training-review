create_fig4 <- function(data) {
  
  data   <- data %>% 
            mutate(Outcome = factor(Outcome, levels=c('Strength (upper-body)','Strength (lower-body)','Power','Hypertrophy','VO2max'))) %>%
            mutate(P_subgroup = case_when(P_subgroup < .01 ~ '**',
                                          P_subgroup <= .05 ~ '*',
                                          TRUE ~ '')) 
  data_p <- data %>% pivot_wider(names_from = 'Sex', values_from = 'Sex') %>%
            group_by(Outcome) %>% mutate(Female = ifelse(is.na(Female),'Female',Female),
                                         Male   = ifelse(is.na(Male),'Male',Male)) %>%
            filter(row_number()==1) %>%
            mutate(P_comparison = case_when(P_comparison < .01 ~ '**',
                                            P_comparison <= .05 ~ '*',
                                            TRUE ~ '')) %>%
            mutate(P_comparison = ifelse(P_comparison == '',NA,P_comparison))
                                              
  ylims <- max(abs(c(data$CI_lower,data$CI_upper)),na.rm=T)
  
  A <- ggplot(data, aes(x=Sex, y=SMD, ymin = CI_lower, ymax = CI_upper)) + 
        geom_rect(aes(ymin=-.2, ymax=.2), xmin =0, xmax = 3, fill = '#e9e9e9') +
        geom_hline(yintercept=0, linetype=2, color='darkgrey',linewidth=.65) +
        geom_pointrange() +
        geom_text(aes(x=Sex, y=ifelse(SMD>0,CI_upper+.1,CI_lower-.1), label=P_subgroup),size=4.5, vjust=0.75) + 
        facet_wrap(~Outcome, ncol=1) + 
        geom_signif(data = data_p,
                    aes(xmin=Male, xmax = Female, annotations = P_comparison, y_position = 1.4),
                    textsize = 5, tip_length = 0.005, vjust = 0.2,
                    manual = TRUE) +
        ylim(c(-ylims,ylims))+ 
        labs(x='') + 
        coord_flip() + 
        theme_bw()
  
  A <- add_sub(A, "favors Strength/Endurance training ←                       → favors Concurrent training", x = 0.5, y=0.1, hjust = 0.55, vjust = -2.05, vpadding=grid::unit(0, "lines"), size=6.5, color='darkgrey')
  
  # Return and save figure
  # ------------------------------------------------------   
  
  # Save image as png
  tiff(paste0("Figure4_",format(Sys.Date(),"%d%m%y"),".tiff"),
       bg = "transparent", width = 4, height = 6, unit = "in", pointsize = 10, res = 1200)
  
  print({ggdraw(A)})
  
  dev.off()
  
  
  # Return plot object
  return(A)
  
}

  