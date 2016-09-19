# helper functions

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# returns just the legend from a plot
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# pretty_graph ------------------------------------------------------------
pretty_graph <- function(size) {
  #theme_bw(base_size=size) +
  require(grid)  # make sure you have unit
  theme(text = element_text(family="sans", face="plain",
                            color="black", size=size),
        rect = element_rect(fill="transparent", color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size=1, lineend=1, linetype=1),
        text = element_text(size=size, color="black"),
        panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size=size*.75, color="black"),
        axis.ticks = element_line(color="black"),
        axis.ticks.length = unit(size*.009, "cm"),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size=size, face="bold")
  )
}

nolines_graph <- function(size) {
  #theme_bw(base_size=size) +
  require(grid)  # make sure you have unit
  theme(text = element_text(family="sans", face="plain",
                            color="black", size=size),
        rect = element_rect(fill="transparent", color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size=1, lineend=1, linetype=1),
        text = element_text(size=size, color="black"),
        panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size=size*.75, color="black"),
        axis.ticks = element_line(color="black"),
        axis.ticks.length = unit(size*.009, "cm"),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size=size, face="bold"),
        axis.title.x = element_text(size=size, color="black", face="bold"),
        axis.title.y = element_text(size=size, color="black", face="bold"),
        legend.title = element_text(color="black", face="bold")
  )
}

pretty_tilt <- function(size) {
  #theme_bw(base_size=size) +
  require(grid)  # make sure you have unit
  theme(text = element_text(family="sans", face="plain",
                            color="black", size=size),
        rect = element_rect(fill="transparent", color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size=1, lineend=1, linetype=1),
        text = element_text(size=size, color="black"),
        panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size=size*.75, color="black"),
        axis.text.x = element_text(size=size*.7, color='black', angle=45, vjust=1, hjust=1),
        axis.ticks = element_line(color="black"),
        axis.ticks.length = unit(size*.009, "cm"),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size=size, face="bold")
  )
}

nolines_tilt <- function(size) {
  #theme_bw(base_size=size) +
  require(grid)  # make sure you have unit
  theme(text = element_text(family="sans", face="plain",
                            color="black", size=size),
        rect = element_rect(fill="transparent", color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size=1, lineend=1, linetype=1),
        text = element_text(size=size, color="black"),
        panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size=size*.75, color="black"),
        axis.text.x = element_text(size=size*.7, color='black', angle=45, vjust=1, hjust=1),
        axis.ticks = element_line(color="black"),
        axis.ticks.length = unit(size*.009, "cm"),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size=size, face="bold")
  )
}

pretty_black <- function(size) {
  #theme_bw(base_size=size) +
  require(grid)  # make sure you have unit
  theme(text = element_text(family="sans", face="plain",
                            color="white", size=size),
        rect = element_rect(fill="transparent", color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="white"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size=1, lineend=1, linetype=1, colour="white"),
        text = element_text(size=size, color="white"),
        panel.background = element_rect(fill = "black",colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "black",colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size=size*.75, color="white"),
        axis.text.x = element_text(size=size*.7, color='white'),
        axis.ticks = element_line(color="white"),
        axis.ticks.length = unit(size*.009, "cm"),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size=size, face="bold")
  )
}

# Multiple plot function
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

pretty_flip <- function(size) {
  #theme_bw(base_size=size) +
  require(grid)  # make sure you have unit
  theme(text = element_text(family="sans", face="plain",
                            color="black", size=size),
        rect = element_rect(fill="transparent", color=NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color="black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size=1, lineend=1, linetype=1),
        text = element_text(size=size, color="black"),
        panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size=size*.75, color="black"),
        axis.ticks = element_line(color="black"),
        axis.ticks.length = unit(size*.009, "cm"),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size=size, face="bold")
  )
}
# 
# p + pretty_graph(18)
# pp + pretty_graph(18)
# ppp + pretty_graph(18)
# q + pretty_graph(18) %replace% theme(plot.background=element_rect(size=1, color="black"))
# 
# ##### Fake Data
# #####
# dat <- read.table(header=TRUE, sep="", stringsAsFactors=FALSE, text='
# day  person	books	genre	sex
# mon	jake	1	mystery	male
# tues	jake	3	mystery	male
# wed	jake	6	mystery	male
# thurs	jake	2	horror	male
# fri	jake	3	horror	male
# sat	jake	4	reference	male
# sun	jake	1	adult	male
# mon	grace	5	romance	female
# tues	grace	7	romance	female
# wed	grace	8	reference	female
# thurs	grace	7	reference	female
# fri	grace	8	reference	female
# sat	grace	6	reference	female
# sun	grace	8	romance	female
# mon	hendry	0	mystery	male
# tues	hendry	6	mystery	male
# wed	hendry	0	adult	male
# thurs	hendry	1	adult	male
# fri	hendry	2	adult	male
# sat	hendry	1	adult	male
# sun	hendry	2	adult	male'
# )
# dat$day <- as.factor(dat$day)
# levels(dat$day) <- c("mon", "tue", "wed", "thurs", "fri", "sat", "sun")
# ######
# p <- ggplot(dat, aes(x=day, y=person, fill=as.factor(books))) + geom_tile()
# pp <- ggplot(dat, aes(x=day, y=books, color=as.factor(sex), shape=as.factor(genre))) + geom_point()
# ppp <- ggplot(dat, aes(x=day, y=books, shape=as.factor(genre))) + geom_point() + facet_wrap(~ sex)
# q <- ggplot(dat, aes(y=books, x=person, fill=genre)) + geom_bar() + facet_wrap(~ day, nrow=1) 
# 
# datSE <- summarySE(dat, measurevar="books", groupvars=c("person", "genre"))
# ggplot(datSE, aes(x=genre, y=books, fill=person)) + geom_bar(stat="identity", color="black", position=position_dodge(1)) + pretty_graph(18)
