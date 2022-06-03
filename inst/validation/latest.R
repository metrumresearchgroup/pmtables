library(yaml)

x <- yaml.load_file("inst/validation/pmtables-stories.yaml")

stories <- names(x)
stories <- sub("PMT-S", "", stories)
stories <- as.integer(stories)

tests <- unlist(lapply(x,  function(x) x$tests), use.names=FALSE)
dupt <- any(duplicated(tests))
tests <- tests[grepl("PMT-", tests)]
tests <- sub("PMT-(TEST|UTIL)-", "", tests)
tests <- as.integer(tests)
tests <- tests[!is.na(tests)]

message("last story: ", max(stories))
message("last test: ", max(tests))
