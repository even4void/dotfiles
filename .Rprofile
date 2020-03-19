options(menu.graphics=FALSE)
options(width=100, tibble.width=100)
options(stringsAsFactors=FALSE)
## options(max.print=100)
## options(prompt="R> ")
## options(continue="... ")
makeActiveBinding("._", function() .Last.value, .GlobalEnv)
s <- base::summary
h <- utils::head
n <- base::names
last <- function(x) tail(x, n = 1)
ht <- function(d) rbind(head(d,10), tail(d,10))
h5 <- function(d) d[1:5, 1:5]
