## ########################################################################## ##
## License : DF-Stat                                                          ##
## DF-Code : Scatter PLot using ggplot 2                                      ##
## Update  : 2024-11-17                                                       ##
## ########################################################################## ##

scatter.plot <- function(y, x, color = 'black', ylabs = NULL, 
                         xlabs = NULL, title = NULL, subtitle = NULL){
  data.set <- data.frame(x,y)
  ggplot(data.set) + 
    geom_point(aes(x = x1, y = y), color = color) +
    labs(
      title = (if(!is.null(title)){title}),
      subtitle = (if(!is.null(subtitle)){subtitle})
    ) +
    (if(!is.null(ylabs)){ylab(ylabs)})+
    (if(!is.null(xlabs)){xlab(xlabs)})->output
  return(output)
}