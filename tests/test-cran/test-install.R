library(testit)

assert('is_version can detect hugo version number', {
  (is_version('1.1.2.3'))
  (is_version('2.1.2'))
  (is_version('10.1'))
  (is_version('12'))
  (!is_version('1.A'))
  (!is_version('Some text'))
  (!is_version('Some/path'))
})
