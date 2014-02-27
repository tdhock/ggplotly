library(maps)

data(canada.cities)

viz <- ggplot(canada.cities, aes(long, lat))+
  borders(regions="canada")+
  coord_equal()+
  geom_point(aes(text=name, size=pop), alpha=1/2)+
  ## TODO: legend for sizes?
  continuous_scale("size","area",palette=function(x){
    scales:::rescale(sqrt(abs(x)), c(2,20), c(0,1))
  })
## TODO: construct axes based on ggplot2 scales?
ggplotly(viz, p)

