library(testit)

assert('pkg_file() returns files/dirs from the package installation directory', {
  (dir_exists(pkg_file('resources')))
})

raw_list = function(x) {
  if (!is.list(x)) return(x)
  nms = names(x)
  attributes(x) = NULL
  names(x) = nms
  x
}

x1 = c('a = "foo"', '# a comment', 'b = false', 'c3 = 10', 'd_e = "0.10"')
x2 = list(a = 'foo', b = FALSE, c3 = 10L, d_e = '0.10')
assert('read_toml() works', {
  (raw_list(read_toml(x = x1)) %==% x2)
  (read_toml(x = x1, strict = FALSE) %==% x2)
})

assert('dash_filename() creates a filename by replacing non-alnum chars with -', {
  dash_filename(c('foo Bar', 'foo/bar  !@ hi', '() foo/hello WORLD')) %==%
    c('foo-bar', 'foo-bar-hi', 'foo-hello-world')
})

assert('arg_string() turns a series of arguments to a single string', {
  (args_string('hi') %==% '"hi"')
  (args_string('"hi"') %==% '"hi"')
  (args_string('hi there') %==% '"hi there"')
  (args_string(a = 'hi') %==% 'a="hi"')
  (args_string(a = 'hi', b = 1) %==% 'a="hi" b=1')
})

assert('post_slug() extracts a slug from a filename', {
  (post_slug('foo-bar.md') %==% 'foo-bar')
  (post_slug('2015-07-23-foo-bar.md') %==% 'foo-bar')
  (post_slug('foo-bar.en.md') %==% 'foo-bar')
  (post_slug('foo-bar.zz.cn.md') %==% 'foo-bar.zz')
  (post_slug('2015-07-23-foo-bar/index.md') %==% 'foo-bar')
  (post_slug('2015-07-23-foo-bar/index.fr.md') %==% 'foo-bar')
  (post_slug('path/to/2015-07-23-foo-bar/index.Rmd') %==% 'foo-bar')
})

assert('bundle_index() checks if a file path points to the index page of a bundle', {
  (bundle_index(c('index.Rmd', 'index.Rmarkdown', 'index.html', 'index.md')))
  (bundle_index(c('index.en.Rmd', 'index.ja.html', 'index.fr.Rmarkdown')))
  (!bundle_index(c('index2.Rmd', 'index .md', 'abc.html', 'index.zzz.md')))
  (bundle_index(c('index', 'index.en'), ext = FALSE))
  (!bundle_index(c('index_', 'index.en_'), ext = FALSE))
})

test_rmd_file = tempfile()
test_rmd = "---
date: '2017-05-01'
string: text
empty_value: ~
empty_list: []
unit_list:
  - 1
multi_list:
  - 1
  - 2
links:
  - icon: images
    icon_pack: fas
    name: slides
    url: ~
  - icon: github
    icon_pack: fab
    name: code
    url: ~
---
"

assert('modify_yaml() perserves original values properly', {
  write(test_rmd, test_rmd_file)
  old_content = readLines(test_rmd_file)
  res = yaml_load(test_rmd)
  (!is.list(res$multi_list))  # should be flattened

  modify_yaml(test_rmd_file, .keep_empty = TRUE)
  (readLines(test_rmd_file) %==% old_content)
})

assert('is_example_url() detects example URLs', {
  '^https?://(www[.])?(example.(org|com)|replace-this-with-your-hugo-site.com)/?'
  (!is_example_url(NULL))
  (!is_example_url('https://www.rstudio.com'))
  (is_example_url('http://www.example.com'))
  (is_example_url('https://www.example.org'))
  (is_example_url('http://replace-this-with-your-hugo-site.com/'))
  (is_example_url('https://www.replace-this-with-your-hugo-site.com/'))
})

assert('is_domain_url() detects URLs that look like domain names', {
  (!is_domain_url(NULL))
  (!is_domain_url('https://example.org'))
  (!is_domain_url('/'))
  (!is_domain_url('foo/'))
  (is_domain_url('example.com'))
  (is_domain_url('www.example.com'))
  (is_domain_url('www.example.com/'))
  (is_domain_url('www.example.com/foo'))
  (!is_domain_url('-example.com'))
  (!is_domain_url('example-.com'))
})
