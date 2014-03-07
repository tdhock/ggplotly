library(ggplotly)
check <- function(gg, expected){
  m <- match.call()
  list(ggplot=gg, expected=expected, name = as.character(m$gg))
}
check.named <- function(expected, generated, trace){
  for(L in list(expected, generated)){
    stopifnot(is.list(L))
    stopifnot(!is.null(names(L)))
    if(any(names(L) == "")){
      print(names(L))
      stop("un-named elements")
    }
  }
  for(name in names(expected)){
    this.trace <- c(trace, name)
    e <- expected[[name]]
    g <- generated[[name]]
    bad <- function(msg="did not generate what we expected"){
      if(missing(msg)){
        print(list(expected=e, generated=g))
      }
      print(this.trace)
      stop(msg)
    }
    if(is.list(e)){
      if(!is.list(g)){
        bad()
      }
      check.named(e, g, this.trace)
    }else if(is.atomic(e)){
      if(!is.atomic(g) || length(g) != length(e)) {
        bad()
      }
      if(is.numeric(e)){
        if(!is.numeric(g)){
          bad()
        }
        ## a tolerance of 0.001 means that the size calculation with
        ## the normalize function below will not agree with the
        ## ggplot2 size calculation.
        char.if.different <- all.equal(e, g, 0.01)
        if(is.character(char.if.different)){
          print(rbind(expected=e, generated=g))
          print(char.if.different)
          bad("not numerically equal")
        }
      }else if(is.character(e) || is.factor(e)){
        if(any(e != g)){
          bad()
        }
      }else{
        print(e)
        stop("do not know what to do with this expectation")
      }
    }else{
      print(e)
      stop("do not know what to do with this expectation")
    }
  }
}
## Generate data
set.seed(1)
n.groups <- 20
Groups <- data.frame(x=rep(1:10, times=n.groups),
                     group = rep(1:n.groups, each=10))
Groups$lt <- c("even", "odd")[(Groups$group%%2+1)] # linetype
Groups$group <- as.factor(Groups$group)
Groups$y <- rnorm(length(Groups$x), Groups$x, .5) +
  rep(rnorm(n.groups, 0, 2), each=10)


## Simple black lineplot.
AllBlack <- ggplot(Groups) +
  geom_line(aes(x=x, y=y, group=group)) + 
  ggtitle("geom_line")
group.list <- split(Groups, Groups$group)
AllBlack.expected <- list()
for(group.i in seq_along(group.list)){
  g <- group.list[[group.i]]
  AllBlack.expected[[group.i]] <-
    list(x=g$x, y=g$y, type="scatter", mode="lines",
         line=list(color="black"))
}

## Canada city population map.
library(maps)
data(canada.cities)
DefaultCities <- ggplot(canada.cities, aes(long, lat))+
  borders(regions="canada", name="borders")+
  coord_equal()+
  geom_point(aes(text=name, size=pop), colour="red",
             alpha=1/2, name="cities")
b <- borders(regions="canada")$data
group.list <- split(b, b$group)
line.df <- data.frame()
for(group.i in seq_along(group.list)){
  g <- group.list[[group.i]]
  line.df <- rbind(line.df, g, NA)
}
normalize <- function(x, m, M){
  x <- na.omit(x)
  zero.one <- (x-min(x))/(max(x)-min(x))
  stopifnot(range(zero.one) == c(0,1))
  m.M <- zero.one*(M-m) + m
  stopifnot(range(m.M) == c(m, M))
  m.M
}
DefaultCities.expected <-
  list(list(x=line.df$long, y=line.df$lat,
            type="scatter", mode="lines", name="borders",
            line=list(dash="solid", color="grey50")),
       with(canada.cities,{
         list(x=long, y=lat, text=name, type="scatter", mode="markers",
              name="cities",
              marker=list(opacity=1/2, color="red", 
                size=normalize(pop, 1, 6)))
       }))
## TODO: legend for sizes?
##viz <- viz0+
  ## continuous_scale("size","area",palette=function(x){
  ##   scales:::rescale(sqrt(abs(x)), c(2,20), c(0,1))
  ## })
## TODO: construct axes based on ggplot2 scales?
to.check <-
  list(check(AllBlack, AllBlack.expected),
       check(DefaultCities, DefaultCities.expected))
for(L in to.check){
  generated <- gg2list(L$gg)
  for(trace.i in seq_along(L$expected)){
    e <- L$exp[[trace.i]]
    g <- generated[[trace.i]]
    check.named(e, g, c(L$name, trace.i))
  }
}

