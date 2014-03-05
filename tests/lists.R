check <- function(gg, expected){
  list(ggplot=gg, expected=expected)
}
check.named <- function(expected, generated){
  for(L in list(expected, generated)){
    stopifnot(is.list(L))
    stopifnot(!is.null(names(L)))
    if(any(names(L) == "")){
      print(names(L))
      stop("un-named elements")
    }
  }
  for(name in names(expected)){
    e <- expected[[name]]
    g <- generated[[name]]
    bad <- function(){
      print(list(expected=e, generated=g))
      stop("did not generate what we expected")
    }
    if(is.list(e)){
      if(!is.list(g)){
        bad()
      }
      check.named(e, g)
    }else if(is.atomic(e)){
      if(!is.atomic(g) ||
         length(g) != length(e) ||
         any(e != g)){
        bad()
      }
    }else{
      print(e)
      stop("do not know what to do with this expectation")
    }
  }
}
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
group.list <- split(Groups, Groups$group)
expected <- list()
for(group.i in seq_along(group.list)){
  g <- group.list[[group.i]]
  expected[[group.i]] <- list(x=g$x, y=g$y, type="scatter", mode="lines",
                              marker=list(color="black"))
}
to.check <- list(check(AllBlack, expected))
for(L in to.check){
  generated <- gg2list(L$gg)
  for(trace.i in seq_along(expected)){
    e <- L$exp[[trace.i]]
    g <- generated[[trace.i]]
    check.named(e, g)
  }
}
