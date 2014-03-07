library(maps)

data(canada.cities)

viz <- ggplot(canada.cities, aes(long, lat))+
  borders(regions="canada", name="borders")+
  coord_equal()+
  geom_point(aes(text=name, size=pop), colour="red",
             alpha=1/2, name="cities")
library(ggplotly)
ggplotly(viz, p)

