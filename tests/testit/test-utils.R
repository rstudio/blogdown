library(testit)

assert(
  'pkg_file() returns files/dirs from the package installation directory',
  dir_exists(pkg_file('resources'))
)

raw_list = function(x) {
  if (!is.list(x)) return(x)
  nms = names(x)
  attributes(x) = NULL
  names(x) = nms
  x
}

x1 = c('a = "foo"', '# a comment', 'b = false', 'c3 = 10')
x2 = list(a = 'foo', b = FALSE, c3 = 10L)
assert(
  'parse_toml() works',
  raw_list(parse_toml(x = x1)) %==% x2,
  parse_toml(x = x1, strict = FALSE) %==% x2
)

assert(
  'dash_filename() creates a filename by replacing non-alnum chars with -',
  dash_filename(c('foo Bar', 'foo/bar  !@ hi', '() foo/hello WORLD')) %==%
    c('foo-bar', 'foo-bar-hi', 'foo-hello-world')
)
