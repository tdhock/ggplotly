library(maps)

data(canada.cities)

viz <- ggplot(canada.cities, aes(long, lat))+
  borders(regions="canada", name="borders")+
  coord_equal()+
  geom_point(aes(text=name, size=pop), alpha=1/2, name="cities")+
  ## TODO: legend for sizes?
  continuous_scale("size","area",palette=function(x){
    scales:::rescale(sqrt(abs(x)), c(2,20), c(0,1))
  })
## TODO: construct axes based on ggplot2 scales?
library(ggplotly)
ggplotly(viz, p)

