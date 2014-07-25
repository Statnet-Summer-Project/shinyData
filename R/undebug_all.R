undebug_all <-
function(where=search()) {
	aa <- all_debugged(where)
	lapply(aa$env,undebug)
	## now debug namespaces
	invisible(mapply(function(ns,fun) {
						undebug(getFromNamespace(fun,ns))
					},names(aa$ns),aa$ns))
}
