library(testit)
# test from this source, not installed blogdown
devtools::load_all('..')
test_pkg('blogdown')
