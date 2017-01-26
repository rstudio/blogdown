library(testit)

assert(
  'pkg_file() returns files/dirs from the package installation directory',
  dir_exists(pkg_file('resources'))
)

assert(
  'parse_toml() works',
  parse_toml(x = c('a = "foo"', '# a comment', 'b = false', 'c3 = 10')) %==%
    list(a = 'foo', b = FALSE, c3 = 10)
)

assert(
  'dash_filename() creates a filename by replacing non-alnum chars with -',
  dash_filename(c('foo Bar', 'foo/bar  !@ hi', '() foo/hello WORLD')) %==%
    c('foo-bar', 'foo-bar-hi', 'foo-hello-world')
)
