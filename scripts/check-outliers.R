# Perform sensitivity analysis: generate funnel plots and check for outliers

# Load data
data <- read_excel('data/test_data.xlsx')
data <- read.csv('/Users/stephanvanderzwaard/Projects/research/2022-CT-review/data/revmandata.csv')

# Generate funnel plots for sensitivity analysis: save image as png
tiff(paste0("FigureS16_",format(Sys.Date(),"%d%m%y"),".tiff"),
     bg = "transparent", width = 8, height = 11, unit = "in", pointsize = 12, res = 1200)

outlier_datas <- c()
par(mfcol = c(5, 3), mar = c(4, 4, 1, 1))  # Set up a 2 x 2 plotting space

for (i in 1:3) {
  
  for (outcome in 1:5) {
    
    if (i<3) {outcome_no = outcome} else {outcome_no = outcome+5}
    if (i<3) {compare_no = i}       else {compare_no = 2}
    
    datas <- data %>% mutate(outcome = outcome) %>%
                      mutate(outcome = case_when(outcome==3 ~ 'Power',
                                                 outcome==1 ~ 'Strength (lower-body)',
                                                 outcome==2 ~ 'Strength (upper-body)',
                                                 outcome==4 ~ 'Hypertrophy',
                                                 outcome==5 ~ 'VO2max',
                                                 TRUE ~ ''))
    
     datas <- datas %>% filter(Outcome.Number==outcome_no & Comparison.Number==compare_no & Data.Type != 'CON') %>% 
                        select(Name,Mean.1:Total.2,Weight,outcome,Subgroup.Number) %>%
                        filter(Weight != 0) %>%
                        mutate_at(.vars = vars(2:9),
                                  .funs = list(~as.numeric(gsub(",",'',.)))) %>% select(!contains('Event'))
     colnames(datas) <- c('study','mean_1','sd_1','n_1','mean_2','sd_2','n_2','weights','outcome','subgroup')
     datas <- as.data.frame(datas)
     
     datas <- datas %>% mutate(sd_1 = ifelse(row_number() %in% c(1,33,34), sd_1*sqrt(n_1), sd_1),
                               sd_2 = ifelse(row_number() %in% c(1,33,34), sd_2*sqrt(n_2), sd_2),
                               subgroup = factor(subgroup))
     
     my_data <- escalc(n1i = n_1,    n2i  = n_2,
                       m1i  = mean_1, m2i  = mean_2, 
                       sd1i = sd_1,   sd2i = sd_2, 
                       data = datas, measure = "SMD", 
                       append = TRUE)
                      
     ma_model_1 <- rma(yi, vi, data = my_data, slab = my_data$study)
     summary(ma_model_1)
     
     ma_model_1 <- rma(yi, vi, data = my_data, slab = my_data$study, mods = ~ 0 + subgroup)
     summary(ma_model_1)
     
     if (outcome != 4) {
       funnel_asym <- regtest(ma_model_1)
       print(paste0("Publication bias: ",my_data$outcome[1]," - P=",round(funnel_asym$pval,3)))
     }
     
     # Generate funnel plots
     #generate_funnel_plot(ma_model_1, my_data, outcome+(i-1)*5)
    
     inf <- influence(ma_model_1)
     #plot(inf)                 
     
     outlier_data <- cbind(my_data, data.frame(comparison=i), inf$inf, inf$dfbs, inf$is.infl) %>% rename(dbfs = intrcpt, is.infl = `inf$is.infl`) %>% 
                     mutate(is.outlier1 = ifelse(cook.d > 4/nrow(datas),T,F),
                            is.outlier2 = ifelse(cook.d > .5,T,F)) %>% 
                     filter(is.infl == T | is.outlier1==T | is.outlier2 ==T)
     outlier_datas <-rbind(outlier_datas,outlier_data)
     }
}

dev.off()
