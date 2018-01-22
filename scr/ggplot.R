# modified ggplot themes
theme_facet <- theme_classic() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 16),
    axis.title.y = element_text(vjust = 1.5),
    
    legend.position = "bottom",
    
    panel.background = element_rect(fill = "white", color = NA), 
    panel.border = element_rect(fill = NA, color = "black"), 
    panel.grid.major = element_line(color = "grey85"), 
    panel.grid.minor = element_blank(), 
    strip.background = element_rect(fill = "grey95", color = "black"), 
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16))  

theme_standard <- theme_classic() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 16),
    axis.title.y = element_text(vjust = 1.5),
    
    legend.position = "bottom",
    
    panel.background = element_rect(fill = "white", color = NA), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_rect(fill = "grey95", color = "black"), 
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16)) 


# add to ggplot.R script?
# Multiple plot function (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

