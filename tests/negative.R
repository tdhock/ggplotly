library(ggplotly)
iplot <- ggplot(iris)+
  geom_point(aes(Petal.Width, Sepal.Width))
L <- gg2list(iplot)
m <- L[[1]]$marker
if(is.null(m)){
  stop("no marker")
}
if(!is.null(m$size)){
  stop("size found")
}
