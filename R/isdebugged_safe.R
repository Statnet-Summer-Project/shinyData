isdebugged_safe <-
function(x,ns=NULL)  {
	g <- if (is.null(ns)) get(x) else getFromNamespace(x,ns)
	is.function(g) && isdebugged(g)
}
