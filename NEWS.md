# CHANGES IN blogdown VERSION 0.20

## BUG FIXES

- Now `blogdown::install_theme()` downloads Hugo themes (from Github by default) as tarballs instead of zip archives, because `unzip()` is unable to extract files for certain themes (thanks, @jimvine, #433).

- Disallow running `blogdown::serve_site()` multiple times in the same R session due to an RStudio IDE issue (thanks, @jennybc @daczarne @denizCvrl @lopierra, #404).

# CHANGES IN blogdown VERSION 0.19

## NEW FEATURES

- The "Filename" field in the "New Post" addin in RStudio will use the "Slug" value (if provided) to create the base filename (thanks, @maelle, #448).

- For `blogdown::build_site(method = 'html', run_hugo =TRUE)`, if the R script `R/build2.R` exists, it will be executed after Hugo has built the site. This can be useful if you want to post-process the site (thanks, @chrisjake, #458).

- Added functions `shortcode_open()` and `shortcode_close()` so users can write the opening and closing shortcodes separately from the inner content, and the inner content can be processed by Pandoc (thanks, @tcwilkinson, #449).

## BUG FIXES

- **blogdown** no longer renders `.Rmd` documents within a packrat / renv library folder, for blog posts whose associated R libraries are managed by these packages (thanks, @kevinushey, #451).

- The "Language" field in the "New Post" addin in RStudio now shows up regardless of the capitalization of the `defaultContentLanguage` parameter in `config.toml` (thanks @mpaulacaldas, #442).

- Correctly identifies "Windows X.Y x64" as a 64bit operating system, so the extended version of Hugo can be installed (thanks, @anna-doizy, #461).

# CHANGES IN blogdown VERSION 0.18

## NEW FEATURES

- The "New Post" addin in RStudio uses the **whoami** package (if installed) to figure out the author name, in addition to using the global option `getOption('blogdown.author')`. The "Subdirectory" field is now a select input instead of a text input, so you can choose one item from a full list of subdirectories instead of manually typing the directory path (thanks, @maelle @gadenbuie, #432).

# CHANGES IN blogdown VERSION 0.17

## BUG FIXES

- The "New Post" addin in RStudio works with page bundle archetypes now (thanks, @malcolmbarrett and @apreshill, #414).

# CHANGES IN blogdown VERSION 0.16

## MINOR CHANGES

- Added tests for some utility functions (thanks, @novica, #405).

# CHANGES IN blogdown VERSION 0.15

## BUG FIXES

- _Insert Image_ addin now works correctly on windows (thanks, @filippogambarota @cderv, #397).

# CHANGES IN blogdown VERSION 0.14

## NEW FEATURES

- `new_site()` and `install_theme()` will check the minimal Hugo version specified by the theme, and automatically update Hugo if the current installed version of Hugo is not sufficient (thanks, @apreshill, #391).

# CHANGES IN blogdown VERSION 0.13

## NEW FEATURES

- Added a global option `blogdown.hugo.args`, which should be a character vector with additional flags to be passed to the `hugo` system command via `hugo_build()`. For example, `options(blogdown.hugo.args = '--minify')` will use [minification](https://gohugo.io/news/0.47-relnotes/) on the final rendered output. More available flags in the [hugo documentation](https://gohugo.io/commands/hugo#options) (thanks, @jozefhajnala, #382).

# CHANGES IN blogdown VERSION 0.12

## MINOR CHANGES

- When creating a post with a date in the future, a warning will be issued by default. To turn off this warning (if you are aware of the consequences of future dates), set `options(blogdown.warn.future = FALSE)` (thanks, @Chucheria on Twitter, #377).

# CHANGES IN blogdown VERSION 0.11

## NEW FEATURES

- Added a global option `blogdown.filename.pre_processor`, which can be a function with a single argument (the post title) that returns a pre-processed string to be used to generate the post filename. For example, if you set `options(blogdown.filename.pre_processor = function(x) stringi::stri_trans_general(x, "any-latin; nfd; [:nonspacing mark:] remove; nfc"))`, Cyrillic characters in a post title can be converted to ASCII, and the result string will be used for generating the post filename (thanks, @novica, #349).

## MAJOR CHANGES

- When previewing a **blogdown** website with Hugo on the RStudio Server, the Hugo configuration `relativeURLs` will be set to `true` automatically (thanks, @nwstephens, #124).

## MINOR CHANGES

- Added support to install the extended version of Hugo and enabled it as default via `install_hugo(..., extended = TRUE)` (thanks, @rgaiacs, #363).

# CHANGES IN blogdown VERSION 0.10

## NEW FEATURES

- The `slug` field in the "New Post" RStudio addin will no longer be automatically changed if the user has manually changed it once (thanks, @eliocamp, #347).

- Added multilingual support in the "New post" RStudio addin (thanks, @novica #344, @Guilz #323).

- You can create a new post as the index file of a Hugo [page bundle](https://gohugo.io/content-management/page-bundles/) via `blogdown::new_post()` or the RStudio addin "New Post" if you set `options(blogdown.new_bundle = TRUE)`. One benefit of using a page bundle instead of a normal page is that you can put resource files associated with the post (such as images) under the same directory of the post itself. This means you no longer have to put them under the `static/` directory, which has been quite confusing to Hugo beginners (thanks, @DavisVaughan @romainfrancois @apreshill, #351).

- Added an argument `empty_dirs` to `new_site()` so that you can preserve the empty directories via `blogdown::new_site(empty_dirs = TRUE)`. By default, empty directories will be deleted when a new site is created (thanks, @apreshill, rstudio-education/arm-workshop-rsc2019#8).

- Added a global option `blogdown.files_filter` to allow users to decide which Rmd files to be rebuilt (this option can be set in `.Rprofile`). The default filter is `blogdown:::timestamp_filter`, i.e., only Rmd files which are older than their output files will be recompiled when rebuilding a site. You can set `options(blogdown.files_filter = blogdown:::md5sum_filter)` to use a different filter based on MD5 checksums, i.e., only rebuild an Rmd file if its MD5 checksum has changed. The checksums of Rmd files are saved in the file `blogdown/md5sum.txt` under the website root directory (thanks, @jonathan-g, #341).

## MINOR CHANGES

- The default value of the argument `kind` of `new_post()` has been changed from `'default'` to `''`, which means this function will respect Hugo's default order of looking for the archetype. The `Archetype` dropdown menu of the RStudio addin "New Post" was changed accordingly, and this menu will also list archetypes in themes (thanks, David Daza, https://stackoverflow.com/questions/53309582/53341795#comment93989876_53341795).

# CHANGES IN blogdown VERSION 0.9

## NEW FEATURES

- Added an option in the "Update Metadata" addin to rename the post filename accordingly when the date is updated, e.g., when the date is changed from `2018-07-23` to `2018-07-28`, the post file `2018-07-23-hello-world.md` can be renamed to `2018-07-28-hello-world.md`.

# CHANGES IN blogdown VERSION 0.8

## BUG FIXES

- A bug was introduced when implementing #305: should append `draft: yes` to YAML only when `options(blogdown.draft.output = TRUE)` is set (thanks, @ConorIA, #312).

# CHANGES IN blogdown VERSION 0.7

## NEW FEATURES

- Added a new RStudio addin "Touch File" to update the modification time of a file, which can be useful when you want to force rebuilding a certain Rmd post after running `serve_site()` (#294).

- Added an RStudio addin "Quote Poem" to quote a poem using the Markdown syntax (https://yihui.org/en/2018/06/quote-poem-blogdown/).

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

- The Github repo `yihui/hugo-lithium-theme` was renamed to `yihui/hugo-lithium`, and the default `theme` argument value was changed accordingly (thanks, @rorynolan, #291).

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
