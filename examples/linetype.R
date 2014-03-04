library(ggplotly)
library(ggplot2)

#' Generate data
set.seed(1)
n.groups <- 20
Groups <- data.frame(x=rep(1:10, times=n.groups),
                     group = rep(1:n.groups, each=10))
Groups$lt <- c("even", "odd")[(Groups$group%%2+1)] # linetype
Groups$group <- as.factor(Groups$group)
Groups$y <- rnorm(length(Groups$x), Groups$x, .5) +
  rep(rnorm(n.groups, 0, 2), each=10)


#' Simple line plot
AllBlack <- ggplot(Groups) +
  geom_line(aes(x=x, y=y, group=group)) + 
  ggtitle("geom_line")
ggplotly(AllBlack, p)


#' Simple line plot with colors...
AllColors <- ggplot(Groups) +
  geom_line(aes(x=x, y=y, colour=group, group=group)) +
  ggtitle("geom_line + scale_colour_discrete")
## TODO: set plot title.
ggplotly(AllColors, p)
## TODO: set trace text with name parameter.

#' Simple line plot with colors and linetype
ColorsTwoTypes <- ggplot(Groups) +
  geom_line(aes(x=x, y=y, colour=group, group=group, linetype=lt)) +
  ggtitle("geom_line + scale_linetype_manual")
ggplotly(ColorsTwoTypes, p)


#' Use automatic linetypes from ggplot with coerced factors
Types <- ggplot(Groups) +
  geom_line(aes(x=x, y=y, colour=group, group=group,linetype=group))+
  ggtitle("geom_line + scale_linetype automatic")
Types

#' Use automatic linetypes from ggplot with coerced factors
makedf <- function(plotly){
  data.frame(plotly, ggplot2=names(plotly))
}
dash.data <- makedf(named.lty)
NamedTypes <- ggplot(dash.data) +
  geom_segment(aes(-1, ggplot2, xend=1, yend=ggplot2, linetype=ggplot2))+
  ggtitle("geom_line + scale_linetype_identity")+
  scale_linetype_identity()+
  geom_text(aes(0, ggplot2, label=plotly))
## TODO: implement traces for geom_segment.
ggplotly(NamedTypes, p)

## TODO: S3 methods for converting geoms to traces.

## TODO: more test linetype plots.

#' Manually specify linetypes using <length, space, length, space...> notation
data$lt <- rep(c("2423", "2415", "331323", "F2F4", "solid"), each=10)
p5 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group, linetype=lt)) + 
  scale_linetype_identity("group", guide="legend", labels = c("1", "2", "3", "4", "5")) + 
  scale_colour_discrete("group") + 
  ggtitle("Manual Linetypes: dash-space length")
p5
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5))

#' All possible linetypes
lts <- scales::linetype_pal()(13)
lt1 <- data.frame(x=0, xend=.25, y=1:13, yend=1:13, lt=lts, lx = -.125)
p6 <- ggplot()+geom_segment(data=lt1, aes(x=x, xend=xend, y=y, yend=yend, linetype=lt)) + 
  scale_linetype_identity() + geom_text(data=lt1, aes(x=lx, y=y, label=lt), hjust=0) + 
  ggtitle("Scales package: all linetypes")
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6))

lts2 <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
lt2 <- data.frame(x=0, xend=.25, y=1:6, yend=1:6, lt=lts2, lx=-.125)
p7 <- ggplot() + geom_segment(data=lt2, aes(x=x, xend=xend, y=y, yend=yend, linetype=lt)) + 
  scale_linetype_identity() + geom_text(data=lt2, aes(x=lx, y=y, label=lt), hjust=0) +
  ggtitle("Named linetypes")
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7))

