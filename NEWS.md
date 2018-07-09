# CHANGES IN blogdown VERSION 0.8

## BUG FIXES

- A bug was introduced when implementing #305: should append `draft: yes` to YAML only when `options(blogdown.draft.output = TRUE)` is set (thanks, @ConorIA, #312).

# CHANGES IN blogdown VERSION 0.7

## NEW FEATURES

- Added a new RStudio addin "Touch File" to update the modification time of a file, which can be useful when you want to force rebuilding a certain Rmd post after running `serve_site()` (#294).

- Added an RStudio addin "Quote Poem" to quote a poem using the Markdown syntax (https://yihui.name/en/2018/06/quote-poem-blogdown/).

- Added a new function `shortcodes()`, which is a vectorized version of `shortcode()`. For example, you can embed multiple tweets (thanks, @maelle, #307).

- Added an argument `ignore` to `build_dir()` to ignore output filenames when testing if the Rmd file is newer than its possible output files. By default, `*.Rproj` files are ignored (thanks, @chepec, #308).

- Added a global option `blogdown.draft.output`. If `options(blogdown.draft.output = TRUE)` is set, a field `draft: yes` will be appended to the YAML metadata of the HTML output file from an Rmd post, unless the Rmd post has already set the `draft` option. This means the output files are always drafts unless you explicitly set `draft: no` in the YAML metadata of Rmd posts (thanks, @mwaldstein, #305).

## BUG FIXES

- Applied a workaround for the bug in the `Rscript` command: https://stat.ethz.ch/pipermail/r-devel/2018-April/075897.html (reported from https://stackoverflow.com/q/50077412).

- The TOML metadata in new posts may fail to be converted to YAML (thanks, @apreshill, #301).

- When editing the `config.toml` file on Windows, `serve_site()` could fail with an error message `'---did you forget a '#'? at line 1>seImpl(path.expand(input), verbose, fromFile): Unidentified trailing character'` (thanks, @rhobis, #302).

# CHANGES IN blogdown VERSION 0.6

## NEW FEATURES

- Added a `title_case` argument to the `new_post()` function; if `TRUE`, the post title will be converted to title case. See `?blogdown::new_post` for details.

- Added a `hostname` argument to `install_theme()` and `new_site()`, as a complement to the `theme` argument. The default `hostname` is `'github.com'`; if you have access to GitHub Enterprise, you can use this to specify it instead (thanks, @ijlyttle, #264).

- The `new_post` addin now lets you choose an archetype. See https://gohugo.io/content-management/archetypes/ for more details (thanks, @lcolladotor, #173).

- Added a new RStudio addin (`insert_image`) for inserting external images into blog posts (thanks, @lcolladotor, #269). If you use `options(blogdown.insertimage.usebaseurl = TRUE)`, it adds the baseurl so that RSS feeds will include the images and be properly displayed in websites such as RBloggers (#275). You will need to publish the images so that they are displayed in a local preview and will need to keep in mind some drawbacks discussed in https://github.com/rstudio/blogdown/pull/275.

- The `theme` argument of `install_theme()` and `new_site()` now accepts a full URL to a theme's repository zip file. This can be used to install themes from other web-based git hosts, like GitLab and Bitbucket (thanks, @gadenbuie, #271).

- You may download the zip archive or tarball of the Hugo installer from Github by yourself, and pass the path to `install_hugo()` to install it, e.g., `blogdown::install_hugo('hugo_0.38_Windows-64bit.zip')` (thanks, @shrektan, #288).

## BUG FIXES

- The `kind` argument (i.e., the archetype) of `new_content()` now works with files that end in `.Rmd` and `.Rmarkdown`. The archetype still has to end in `.md` for Hugo to work with it (thanks, @lcolladotor, #261).

## MINOR CHANGES

- The Github repo `yihui/hugo-lithium-theme` was renamed to `yihui/hugo-lithiutm`, and the default `theme` argument value was changed accordingly (thanks, @rorynolan, #291).

# CHANGES IN blogdown VERSION 0.5

## BUG FIXES

- The bug #233 still exists on Windows (thanks, Sheng Luan).

# CHANGES IN blogdown VERSION 0.4

## BUG FIXES

- `install_theme()` may signal the error "The theme already exists" by mistake (thanks, @YizhouZheng, #230).

- A warning will be issued if two versions of Hugo are found (thanks, @mingsnu, #235).

- Plots are missing if a post filename contains multibyte characters (thanks, @dongzhuoer, #233).

# CHANGES IN blogdown VERSION 0.3

## MAJOR CHANGES

- The **later** package is a required dependency on Windows for `blogdown::serve_site()` now (#169).

# CHANGES IN blogdown VERSION 0.2

## BUG FIXES

- `serve_site()` failed to parse `baseurl` in config.toml when it contains comments (thanks, @ummel, https://github.com/rbind/support/issues/62).

- Three dashes in the beginning and/or end of config.yaml are ignored (thanks, @andrewheiss, #194).

- The R startup profile `.Rprofile` under the website project root directory should be respected when building R Markdown posts (thanks, @eisioriginal, #222).

# CHANGES IN blogdown VERSION 0.1

- The first CRAN release. For full documentation, please see the blogdown book: https://bookdown.org/yihui/blogdown.
