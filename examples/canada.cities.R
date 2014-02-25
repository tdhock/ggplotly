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
print(viz)

built <- ggplot_build(viz)

canada <- built$data[[1]]
poly.list <- split(canada, canada$group)
poly.na.df <- data.frame()
for(i in seq_along(poly.list)){
  poly.na.df <- rbind(poly.na.df, poly.list[[i]], NA)
}
poly.na <- as.list(poly.na.df[c("x", "y")])
cities <- built$data[[2]]
dots <- c(as.list(cities[c("x", "y", "text")]),
          list(type="scatter",
               mode="markers",
               marker=list(size=cities$size, opacity=1/2)))
layout <- NULL
result <- p$plotly(poly.na, dots, kwargs=list(layout=layout))
## TODO: construct axes based on ggplot2 scales?
browseURL(result$url)

