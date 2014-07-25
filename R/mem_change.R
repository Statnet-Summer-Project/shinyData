mem_change <-
function(code) {
	start <- mem()
	
	expr <- substitute(code)
	eval(expr, parent.frame())
	rm(code, expr)
	
	round(mem() - start, 3)
}
