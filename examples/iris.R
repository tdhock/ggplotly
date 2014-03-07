library(ggplotly)
iplot <- ggplot(iris)+
  geom_point(aes(Petal.Width, Sepal.Width))
ggplotly(iplot, p)
## These should all give the same result.
cplots <-
  list(global=iplot+aes(color=Species),
       point=ggplot(iris)+
       geom_point(aes(Petal.Width, Sepal.Width, color=Species)),
       qplot=qplot(Petal.Width, Sepal.Width, color=Species, data=iris),
       qplot=qplot(Petal.Width, Sepal.Width, colour=Species, data=iris),
       qplot=qplot(Petal.Width, Sepal.Width, col=Species, data=iris))
L <- gg2list(cplots[[1]])
str(L)
ggplotly(cplot, p)

