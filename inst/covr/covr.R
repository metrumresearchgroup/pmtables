library(covr)

x <- package_coverage(file.path('.'), quiet=FALSE)

y <- coverage_to_list(x)

z <- zero_coverage(x)

write.csv(z, file = "inst/covr/zero.md")


df <- data.frame(file = names(y$filecoverage), coverage = y$filecoverage, row.names=NULL)
df <- df[order(as.numeric(df$coverage)),]

outfile <- "inst/covr/coverage.md"
cat(file=outfile, "# coverage: ",y$totalcoverage, "%\n\n", sep="")
cat(file=outfile, append = TRUE, knitr::kable(df,row.names=FALSE),sep="\n")

x
