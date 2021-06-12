#Function for saving ggplots
plot_save<- function(plot, 
                      width = 853, 
                      height = 480, 
                      text.factor = 1, 
                      filename
) {
  
  dpi <- text.factor * 100
  width.calc <- width / dpi
  height.calc <- height / dpi
  
  ggsave(filename = filename,
         dpi = dpi,
         width = width.calc,
         height = height.calc,
         units = 'in',
         plot = plot)
}