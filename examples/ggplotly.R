if(interactive()){
  if(!"p" %in% ls()){
    stop("first create a plotly interface object p:\np <- plotly::plotly(username, key)")
  }
  ggiris <- qplot(Petal.Width, Sepal.Length, data=iris, color=Species)
  ggplotly(ggiris, p)
  data(canada.cities, package="maps")
  viz <- ggplot(canada.cities, aes(long, lat))+
    borders(regions="canada", name="borders")+
    coord_equal()+
    geom_point(aes(text=name, size=pop), colour="red",
               alpha=1/2, name="cities")
  ggplotly(viz, p)
}
