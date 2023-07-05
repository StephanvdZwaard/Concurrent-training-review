generate_funnel_plot <- function(model, my_data, panel) {
  
  # Generate funnel plot
  if (panel %in% seq(1,4)) {
    funnel(model, ylim=c(0,1), pch=subgroup, xlab="", ylab='Standard Error'); 
  } else if (panel %in% c(10,15)) {
    funnel(model, ylim=c(0,1), pch=subgroup, xlab='Standardized Mean Difference', ylab=""); 
  } else if (panel == 5) {
    funnel(model, ylim=c(0,1), pch=subgroup, xlab='Standardized Mean Difference', ylab='Standard Error'); 
  } else {
    funnel(model, ylim=c(0,1), pch=subgroup, xlab="", ylab=""); 
  }
  
  
  # Add subgroup info
  if(i==1) {
    subgroups <- c('Males','Females') 
    pch <- c(1,2)
  } else if (i==2) {
    subgroups <- c('Untrained ET','Trained ET','Highly-trained ET')
    pch <- c(1,2,3)
  } else {
    subgroups <- c('Untrained ST','Trained ST','Highly-trained ST') 
    pch <- c(1,2,3)
  }
  
  # Add title and legend
  legend('topright', bty='n', cex = .7, legend = subgroups, pch = pch)
  title(my_data$outcome[1])
  
  return(recordPlot())
}