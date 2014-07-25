summaryRprof.kk <-
function (
				filename = "Rprof.out", 
				chunksize = 5000, 
				memory = c("none", 
						"both", "tseries", "stats"), 
				lines = c("hide", "show", "both"), 
				index = 2, 
				diff = TRUE, 
				exclude = NULL, 
				basenames = 1) 
{
	con <- file(filename, "rt")
	on.exit(close(con))
	firstline <- readLines(con, n = 1L)
	if (!length(firstline)) 
		stop(gettextf("no lines found in %s", sQuote(filename)), 
				domain = NA)
	sample.interval <- as.numeric(strsplit(firstline, "=")[[1L]][2L])/1e+06
	memory.profiling <- substr(firstline, 1L, 6L) == "memory"
	line.profiling <- grepl("line profiling", firstline)
	if (line.profiling) 
		filenames <- character(0)
	memory <- match.arg(memory)
	if (memory != "none" && !memory.profiling) 
		stop("profile does not contain memory information")
	if (memory == "tseries") 
		return(Rprof_memory_summary(filename = con, chunksize = chunksize, 
						label = index, diff = diff, exclude = exclude, sample.interval = sample.interval)) 	else if (memory == "stats") 
		return(Rprof_memory_summary(filename = con, chunksize = chunksize, 
						aggregate = index, diff = diff, exclude = exclude, 
						sample.interval = sample.interval))
	lines <- match.arg(lines)
	if (lines != "hide" && !line.profiling) 
		stop("profile does not contain line information")
	fnames <- NULL
	ucounts <- NULL
	fcounts <- NULL
	memcounts <- NULL
	umem <- NULL
	repeat ({
						chunk <- readLines(con, n = chunksize)
						if (line.profiling) {
							filenamelines <- grep("^#File [0-9]+: ", chunk)
							if (length(filenamelines)) {
								fnum <- as.integer(sub("^#File ([0-9]+): .*", 
												"\\1", chunk[filenamelines]))
								filenames[fnum] <- sub("^#File [0-9]+: ", "", 
										chunk[filenamelines])
								if (basenames) {
									dirnames <- dirname(filenames[fnum])
									filenames[fnum] <- basename(filenames[fnum])
									for (i in seq_len(basenames - 1)) {
										tail <- basename(dirnames)
										filenames[fnum] <- ifelse(tail == ".", filenames[fnum], 
												paste0(tail, "/", filenames[fnum]))
										dirnames <- dirname(dirnames)
									}
								}
								chunk <- chunk[-filenamelines]
							}
						}
						if (length(chunk) == 0L) 
							break
						if (memory.profiling) {
							memprefix <- attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:", 
											chunk), "match.length")
							if (memory == "both") {
								memstuff <- substr(chunk, 2L, memprefix - 1L)
								
								memcounts <- pmax(apply(sapply(strsplit(memstuff, 
																":"), as.numeric), 1, diff), 0)
								
								if(length(dim(memcounts))!=2)
									memcounts <- matrix(memcounts,nrow=1)
								
								memcounts <- c(0, rowSums(cbind(memcounts[, 1L:2L,drop=FALSE] * 
																8, memcounts[, 3L])))
								rm(memstuff)
							}
							chunk <- substr(chunk, memprefix + 1L, nchar(chunk, 
											"c"))
							if (any((nc <- nchar(chunk, "c")) == 0L)) {
								chunk <- chunk[nc > 0L]
								memcounts <- memcounts[nc > 0L]
							}
						}
						chunk <- strsplit(chunk, " ")
						if (line.profiling) 
							chunk <- lapply(chunk, function(x) {
										locations <- !grepl("^\"", x)
										if (lines != "hide") {
											fnum <- sub("#.*", "", x[locations])
											lnum <- sub(".*#", "", x[locations])
											x[locations] <- paste0(filenames[as.integer(fnum)], 
													"#", lnum)
										}
										switch(lines, hide = x <- x[!locations], show = x <- x[locations])
										if (length(x)) 
											x
										else "<no location>"
									})
						newfirsts <- sapply(chunk, "[[", 1L)
						newuniques <- lapply(chunk, unique)
						ulen <- sapply(newuniques, length)
						newuniques <- unlist(newuniques)
						new.utable <- table(newuniques)
						new.ftable <- table(factor(newfirsts, levels = names(new.utable)))
						if (memory == "both") 
							new.umem <- rowsum(memcounts[rep.int(seq_along(memcounts), ####
													ulen)], newuniques)
						fcounts <- rowsum(c(as.vector(new.ftable), fcounts), 
								c(names(new.ftable), fnames))
						ucounts <- rowsum(c(as.vector(new.utable), ucounts), 
								c(names(new.utable), fnames))
						if (memory == "both") 
							umem <- rowsum(c(new.umem, umem), c(names(new.utable), 
											fnames))
						fnames <- sort(unique(c(fnames, names(new.utable))))
					})
	firstnum <- fcounts * sample.interval
	uniquenum <- ucounts * sample.interval
	index1 <- order(-firstnum, -uniquenum)
	index2 <- order(-uniquenum, -firstnum)
	if (lines == "show") {
		filename <- sub("#.*$", "", fnames)
		linenum <- rep(0, length(filename))
		hasline <- filename != fnames
		linenum[hasline] <- as.numeric(sub("^.*#", "", fnames[hasline]))
		index3 <- order(filename, linenum)
	}
	firstpct <- round(100 * firstnum/sum(firstnum), 2)
	uniquepct <- round(100 * uniquenum/sum(firstnum), 2)
	digits <- ifelse(sample.interval < 0.01, 3L, 2L)
	firstnum <- round(firstnum, digits)
	uniquenum <- round(uniquenum, digits)
	if (memory == "both") 
		memtotal <- round(umem/1048576, 1)
	rval <- data.frame(firstnum, firstpct, uniquenum, uniquepct)
	names(rval) <- c("self.time", "self.pct", "total.time", "total.pct")
	rownames(rval) <- fnames
	if (memory == "both") 
		rval$mem.total <- memtotal
	by.self <- rval[index1, ]
	by.self <- by.self[by.self[, 1L] > 0, ]
	by.total <- rval[index2, c(3L, 4L, if (memory == "both") 5L, 
					1L, 2L)]
	result <- list(by.self = by.self, by.total = by.total)
	if (lines == "show") 
		result <- c(result, list(by.line = rval[index3, ]))
	c(result, sample.interval = sample.interval, sampling.time = sum(fcounts) * 
					sample.interval)
}
