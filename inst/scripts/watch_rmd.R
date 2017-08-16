# Rscript watch_rmd.R working_directory interval server_pid

a = commandArgs(TRUE)

setwd(a[1])
blogdown:::watch_build(as.numeric(a[2]), as.integer(a[3]))
