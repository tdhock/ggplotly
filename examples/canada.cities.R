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
L <- gg2list(viz)

layout <- list()
result <- p$plotly(L$geoms[[1]]$trace,
                   L$geoms[[2]]$trace,
                   kwargs=list(layout=layout))
## TODO: construct axes based on ggplot2 scales?
browseURL(result$url)

