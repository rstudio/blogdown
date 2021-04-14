# CHANGES IN blogdown VERSION 1.4


# CHANGES IN blogdown VERSION 1.3

## NEW FEATURES

- `check_config()` will suggest ignoring the `renv` folder and the `renv.lock` file in the Hugo config if **renv** is used in the project (thanks, @solarchemist, #597).

- `check_content()` will also discover problematic YAML metadata of posts that is not a list (thanks, msmielak, https://stackoverflow.com/q/66857582/559676).

- New project menu in RStudio IDE now allows to select for keeping `config.toml` (thanks, @ogansser, #606).

## MAJOR CHANGES

- The `method` argument of `build_site()` has been removed (it was defunct in **blogdown** v1.2). Please set the build method in the global option `options(blogdown.method = )` instead.

- The `use_brew` argument of `install_hugo()` has been removed.

## BUG FIXES

- `check_content()` will ignore `renv` folders when looking for Rmd/md files to be checked (thanks, @solarchemist, #597).

- The RStudio addin "Update Metadata" does not work when the global option `blogdown.rename_file` is not set (thanks, @brshallo, #605).

- The RStudio addin "New Post" does not work when the YAML metadata of a post only contains a string. Normally the metadata should be a list (thanks, msmielak, https://stackoverflow.com/q/66857582/559676).

- When `.Rmd` is not ignored in the `ignoreFiles` field in the site config file (this can be detected by `blogdown::check_site()` and you should fix it), `.Rmd` files may be copied to the `public/` directory when building the site via `blogdown::build_site()`, which can cause problems when `blogdown::serve_site()` is running, i.e., `.Rmd` files will be rendered to the `rmarkdown::html_document()` format by `rmarkdown::render()`. As a result, the corresponding web pages will not be rendered by Hugo but only Pandoc, and they will lose the site style or shortcodes (thanks, @ogansser, #610 #608). Now `blogdown::check_site()` should detect this problem and recommend fixes.

# CHANGES IN blogdown VERSION 1.2

## NEW FEATURES

- Added the argument `force` back to `install_hugo()`. If the specified version of Hugo has been installed, `install_hugo()` will not reinstall it unless `force = TRUE` (thanks, @cderv, #575).

- The new option `options(blogdown.knit.serve_site = FALSE)` can be used to prevent **blogdown** from starting the web server automatically when the Knit button is clicked in RStudio and the site has not been served yet (thanks, @Athanasiamo, #572). By default, the web server will be started (if not already started) so the page being knitted can be previewed.

- Added a new global option `blogdown.site_root`, which can be used to specify the root directory of the website. This can be useful when the website source directory is not the root directory of a project but a subdirectory (thanks, @wjakethompson, #581).

- Added a new global option `blogdown.markdown.format` to allow users to customize Pandoc's Markdown output format. When using the file extension `.Rmarkdown` or `options(blogdown.method = 'markdown')`, an R Markdown file is first compiled to Markdown. This Markdown file needs to go through another conversion when it contains Markdown features that are only available to Pandoc but not other Markdown renderers such as Hugo/Goldmark, such as citations or fenced Divs. The new global option controls the Pandoc output format. By default, its value is `c('gfm', '+footnotes', '+tex_math_dollars')` for Pandoc 2.10.1 and later when the Markdown document contains bibliography or fenced Divs, otherwise it is `NULL`. With earlier versions of Pandoc, it will be `c('gfm')` only. If you want the conversion to be always performed, you may set this global option to a value that is not `NULL`, e.g., `options(blogdown.markdown.format = c('gfm', '+footnotes', '+tex_math_dollars', '+smart'))`.

## MAJOR CHANGES

- The `method` argument of `build_site()` is now defunct (it was deprecated in **blogdown** v1.0), and will be removed in a future version. Please set the build method in the global option `options(blogdown.method = )` instead.

- The `use_brew` argument of `install_hugo()` is defunct now, and will be removed in a future version.

- The `update_hugo()` function is defunct. Please use `install_hugo()` instead.

- The scripts `R/build.R` and `R/build2.R` are no longer executed when a document is compiled via the Knit button in RStudio. They will be executed only when building the whole site via `build_site()` (e.g., `Ctrl/Cmd + Shift + B` in RStudio).

## BUG FIXES

- `check_config()` will now correctly check for both missing and unneeded values in the `ignoreFiles` field in the config file.

- When serving the site with `blogdown::serve_site()` with `'blogdown.knit.on_save'` option being `TRUE`, Rmd files in `renv/` or `packrat/` folder are now correctly ignored and not rebuilt (#593).

- For `.Rmarkdown` posts, the Markdown extension `tex_math_dollars` should not be used when post-processing the `.markdown` output file with Pandoc < v2.10.1 (thanks, @lz100, #578).

- The `new_post()` function does not work with bundle archetypes (thanks, @maelle, #577).

- `install_theme()` will now remove the `.github/` folder if one exists in the theme repo as it is only useful to the theme developer (#584).

- Plots generated from R code chunks in posts cannot be previewed on RStudio Server (thanks, @cderv [@datawookie](https://datawookie.dev/blog/2021/02/setting-up-postref-shortcode-for-remote-blog/) #587).

- The error "RStudio is not running" should be suppressed when building the site in a non-interactive R session (thanks, @kcarnold, #596).

- The theme `gcushen/hugo-academic` is now correctly automatically redirected to `wowchemy/starter-academic` with correct default git branch when installing with `new_site()` or `install_theme()`.

- Also try to move `config/_default/config.yaml` to the root dir when installing a theme. Previously, only `config.toml` was moved (thanks, @andreashandel, #546).

- When installing a theme, delete `figure` shortcodes that uses `http` resources because the figure paths could be mangled on Windows (thanks, @andreashandel, #546).

## MINOR CHANGES

- When clicking the Knit button in RStudio to knit a post, the normal knitting process is shown (such as the progress bar) instead of being suppressed (thanks, @Athanasiamo, #572).

- The command `blogdown:::preview_site()` is no longer called or displayed in the R console when users click the Knit button after the server has been started (thanks, @apreshill, #543).

- The config option `ignoreErrors` will be set to `"error-remote-getjson"` when running `blogdown::serve_site()`. This is to prevent Hugo errors in fetching remote resources such as Tweets (thanks, Fan Yuan https://d.cosx.org/d/422065 and dhonda https://stackoverflow.com/q/64601786/559676).

# CHANGES IN blogdown VERSION 1.1

## NEW FEATURES

- Added new arguments `args`, `baseURL`, and `relativeURLs` to the `hugo_build()` function to allow users to pass more command-line arguments to Hugo and adjust the configurations `baseURL` and `relativeURLs` temporarily when building a site.

- Added the `...` argument to `build_site()`, to pass more arguments to the `hugo_build()` function, e.g., `blogdown::build_site(relativeURLs = TRUE)`.

- Added a global option `blogdown.server.verbose` to print the web server messages in real time when the server is running. Once enabled (via `options(blogdown.server.verbose = TRUE)`), you will see messages in the R console like "Change detected, rebuilding site" whenever you make changes to any files (thanks, @apreshill @cderv, #555).

- Added a new global option `blogdown.protect.math` (defaults to `TRUE`) to control whether to protect LaTeX math expressions in a pair of backticks when the post output format is Markdown instead of HTML. The reason to protect math expressions is to avoid the Markdown renderer's treatment of the math content as normal Markdown content, which may mangle the math expressions. If the math expression is protected, it needs to be unprotected later. See https://yihui.org/en/2018/07/latex-math-markdown/ for more information. Note that this option is only relevant to those who use the source format `.Rmarkdown` or the build method `options(blogdown.method = 'markdown')` (thanks, @bensoltoff #466, @mrkaye97 #567).

## BUG FIXES

- `bundle_site()` also moves the `.html` output files and the `*_files/`/`*_cache/` directories associated with `.Rmd` source posts to page bundles. Previously, only `.Rmd` files are moved (thanks, @llrs, #568).

- Fixed a bug of `install_theme()` when the theme archive contains theme files directly instead of a theme folder (thanks, Stefan Musch, https://stackoverflow.com/q/65702805/559676).

- Fixed a bug that causes HTML widgets to fail to render in the Markdown output files with **htmltools** >= 0.5.1.

- Fixed a bug on Windows that causes `check_gitignore()` to error when it shouldn't (#571).

## MAJOR CHANGES

- When the site is rendered via a call to `rsconnect::deploySite()` (e.g., when you call `rmarkdown::publish_site(render = TRUE)`), `blogdown::build_site()` will use the argument `relativeURLs = TRUE`, to make Hugo generate relative URLs that work with any base URL (note that this depends on how well a specific Hugo theme supports relative URLs).

## MINOR CHANGES

- `check_netlify()` and `check_config()` do not open files anymore in the IDE if no TODO items were found in them (#569).

- The internal functions `md5sum_filter()` and `timestamp_filter()` have been removed. They were renamed to `filter_md5sum()` and `filter_timestamp()`, respectively, and exported in **blogdown** 1.0. Please use these exported functions instead if you relied on the internal functions previously.

# CHANGES IN blogdown VERSION 1.0

## NEW FEATURES

- Added a function `check_site()` to provide diagnostics for a website project, which may help reveal potential problems in various places. It runs a series of checks on the config file (`config.yaml` or `config.toml`), the `.gitignore` file, the Hugo installation and version, the `netlify.toml` file, and content files. See the help page `?blogdown::check_site` for more info.

- Documented and exported the internal function `find_hugo()` to find the Hugo executable. If multiple versions of Hugo are installed, `find_hugo()` can also find a specific version of Hugo, e.g., `blogdown::find_hugo('0.25.1')`. You may use `blogdown::find_hugo('all')` to find all possible versions of Hugo currently installed.

- Added a function `remove_hugo()` to remove Hugo (thanks, @cderv, #504).

- The file format `.Rmarkdown` supports HTML widgets and citations now, just like the `.Rmd` format. If you are not familiar with the `.Rmarkdown` format, you may see https://bookdown.org/yihui/blogdown/output-format.html.

- Added a function `config_netlify()` to help users create the config file `netlify.toml` for Netlify. It sets the `build` commands properly for different deploy contexts, and writes the local Hugo version to the config file, so make sure Netlify uses the same version of Hugo as your local environment. See the help page `?blogdown::config_netlify` for details.

- Added a function `config_Rprofile()` to help create or modify the `.Rprofile` file. In particular, it will try to set the option `blogdown.hugo.version` in it, if the option has not been set. See the help page `?blogdown::config_Rprofile` for details.

- Added an argument `netlify = TRUE` to `new_site()` to create `netlify.toml` by default.

- Added an argument `.Rprofile = TRUE` to `new_site()` to create the `.Rprofile` file by default. This file contains a few sample global options that could affect **blogdown**'s behavior, e.g., the option `blogdown.hugo.version` is set to the current Hugo version, so that a site will not be affected by future Hugo updates and will always use the fixed version of Hugo. This file is provided so that users are better aware of some of these options, and can change them if they want.

- Exported the function `filter_newfile()`, which returns paths of source files that have not been knitted (i.e., their output files do not exist).

- The `build_rmd` argument of `build_site()` can take a function as its value now (thanks, [Tyler Smith](https://twitter.com/sedgeboy/status/1308511453129908225)). The function is expected to take a vector of paths of all R Markdown files under the `content/` directory, and returns a vector of paths of R Markdown files to be built. This argument can also take one of the aliases `"timestamp"`, which is equivalent to `blogdown::filter_timestamp`, `newfile` (equivalent to `blogdown::filter_newfile`), and `"md5sum"` (equivalent to `blogdown::filter_md5sum`). For example, `blogdown::build_site(build_rmd = "timestamp")` means to build all R Markdown files if they are older than their output files (by comparing modification times).

- When opening a **blogdown** website project in RStudio, you can specify a number of files to be automatically opened every time via the global option `blogdown.initial_files` in your `.Rprofile`. This option can take a vector of file paths, e.g., `options(blogdown.initial_files = c('config.yaml', '.Rprofile', 'content/post/my-first-post/index.Rmd'))` (files that do not exist will be ignored). Alternatively, this option can take a function that returns a vector of file paths, e.g., `options(blogdown.initial_files = blogdown:::initial_files)`.

- Added a new argument `.site_dir` to `serve_site()`, so users will be able to specify the site root directory (thanks, @Bijaelo, #527).

- Added a new argument `force` to `new_site()` to allow users to create a new site under a nonempty directory with `force = TRUE` if they are sure the site can be safely created under the directory (i.e., Hugo will not possibly override existing files). In an interactive R session, it will ask users if they want `force = TRUE` when the directory is not empty.

## MAJOR CHANGES

- `install_hugo()` no longer installs Hugo via Homebrew by default on macOS, but just downloads binaries from Hugo's Github releases, which gives you a stable version of Hugo. The `use_brew` argument of `install_hugo()` has been deprecated. Installing Hugo via Homebrew often leads to accidental updates of Hugo, which may break your existing sites. If you must install Hugo via Homebrew and want to fix its version, you can run `brew pin hugo`, so it will not be updated by accident in the future (e.g., via `brew upgrade`).

- By default, `install_hugo()` installs Hugo to `~/.local/share/Hugo` on Linux now, instead of `~/bin`.

- `install_hugo()` installs the `hugo` executable to a directory with the directory name being the Hugo version number, e.g., `~/Library/Application Support/Hugo/0.76.5`, or `~/.local/share/Hugo/0.25.1`. This makes it possible to install multiple versions of Hugo on the same computer.

- When starting to serve the site, `serve_site()` will compile Rmd files that do not have output files initially (thanks, Hannah Wang, https://stackoverflow.com/q/64420476/559676).

- The default value of the global option `blogdown.serve_site.startup` was changed from `TRUE` to `FALSE`, meaning that the site will not by served by default when the RStudio project is first opened. If you want the previous behavior, you may set `options(blogdown.serve_site.startup = TRUE)` in your `.Rprofile`.

- The `method` argument of `build_site()` was deprecated. The build method can only be specified via the global option `blogdown.method` now, e.g., you may set `options(blogdown.method = 'custom')` in `.Rprofile`. A new possible build method named `markdown` was added. When you set `options(blogdown.method = 'markdown')`, `.Rmd` posts will be compiled to `.md` (by default, they are compiled to `.html` since the default option is `blogdown.method = 'html'`). This provides another way to render R Markdown to Markdown instead of HTML. Previously, the only way to achieve this was to use the file extension `.Rmarkdown` (this way still works).

- The function `update_hugo()` and the argument `force` of `install_hugo()` have been deprecated. If you want to update Hugo to a newer version, you can call `install_hugo()` and specify a desired version.

- The functions `md5sum_filter` and `timestamp_filter` have been renamed to `filter_md5sum` and `filter_timestamp`, respectively.

## BUG FIXES

- `serve_site()` fails to start the server when the config file contains a `baseURL` value that includes a subpath in it (thanks, @giabaio #254, @ShixiangWang #494).

- `serve_site()` and `hugo_build()` fail to resolve URLs when `relativeURLs` is configured to `true` (thanks, @TianyiShi2001, #506).

- The "Insert Image" addin works with posts that are index pages of leaf bundles now. The images are added to the `images/` folder under the post directory by default instead of the top-level `static/` directory (thanks, @amssljc, #499).

- For a post that is the index page of a bundle, its images could not be displayed when the post content is displayed on other pages such as the home page (thanks, @andremrsantos #501, Fabio A. Cruz Sanchez https://stackoverflow.com/q/65097597/559676).

- The `_files/` and `_cache/` folders are not correctly moved for index pages of leaf bundles when the filename of an index page contains a language code such as `index.en.Rmd` (thanks, @cderv, #500).

- `install_hugo()` could not install the extended version of Hugo that was downloaded manually, e.g., `blogdown::install_hugo('~/Downloads/hugo_extended_0.78.2_Linux-64bit.tar.gz')` would fail (thanks, Stuart, https://stackoverflow.com/q/64962659/559676).

- `new_post(date = "")` or `new_post(date = NULL)` failed to work. In both cases, it should create a post without the `date` field in the YAML metadata (thanks, Caleb Stevens, https://stackoverflow.com/q/65067164/559676).

- `new_site()` no longer shows Hugo messages on Windows (#532).

- `new_site()` can figure out the default branch name of a theme repo now, instead of assuming the `master` branch is the default (thanks, @c1au6i0, #541).

## MINOR CHANGES

- For `new_site(to_yaml = TRUE)`, it will also convert `config.toml` to `config.yaml`.

- The default value of the `serve` argument in `new_site()` was changed from `TRUE` to `"ask"` in an interactive R session, which means it will ask if users want to serve the site after creating it.

- The default value for the `format` argument of `new_site()` was changed from `toml` to `yaml`, which means it will generate `config.yaml` instead of `config.toml` by default.

- `new_site()` will create two sample scripts `R/build.R` and `R/build2.R` (they can be deleted if you don't need them). See the help page `?blogdown::build_site` for their meanings.

- Autocomplete is supported for the names of important global options when typing inside `options()` in RStudio, e.g., when typing `options(blogdown.au)`, RStudio will show the candidate `blogdown.author`.

- `read_toml()` and `toml2yaml()` will try to preserve the original order of fields in the TOML data, instead of using the alphabetical order.

- When clicking the "Build Website" button in RStudio, it will no longer open the `index.html` file in the generated site folder, but emit a message telling users that this folder needs to be served before the web pages can be correctly previewed (#522).

- For Jekyll sites, the arguments `--watch`, `--incremental`, and `--livereload` are passed to `jekyll serve` by default. These arguments can be set via the global R option `blogdown.jekyll.server`.

- The meta variable `link-citations` is set to `true` for the `pandoc_args` argument of `blogdown::html_page()`, so that links can be generated on citation items by default.

- The internal function `Rscript()` was removed and it is publicly available in the **xfun** package now. If you need this function, please use `xfun::Rscript()` instead.

- Removed internal functions `is_windows()`, `is_osx()`, and `is_linux()`. They are available in the **xfun** package now (note that `is_osx()` has become `xfun::is_macos()`).

# CHANGES IN blogdown VERSION 0.21

## NEW FEATURES

- The `Knit` button in RStudio finally works with **blogdown** now. My apologies to those who have desperately clicked the `Knit` button or pressed `Ctrl + Shift + K` in vain over the years. I completely underestimated the power of your muscle memory.

- Added a global R option `blogdown.knit.on_save` to control whether to knit R Markdown documents on save. By default, it is `TRUE`. If you do not want to knit a document as you save it, you may set `options(blogdown.knit.on_save = FALSE)` in your `.Rprofile`. If this option is not set initially, it will be set to `FALSE` after you click the `Knit` button in RStudio.

- `blogdown::build_site()` no longer recompiles R Markdown files by default, because it may be expensive and often undesirable to compile Rmd files that have been compiled before. If you do want to recompile Rmd files, you may use `blogdown::build_site(build_rmd = TRUE)`. See the help page `?blogdown::build_site` for more information.

- Added a helper function `blogdown::bundle_site()` to move post files into leaf bundles in a website, e.g., from `content/foo/bar/hello-world.Rmd` to `content/foo/bar/hello-world/index.Rmd`.

- Exported the (previously internal) function `blogdown::filter_md5sum` function (#341). See its potential application on the help page `?blogdown::build_site`.

- Similarly, the function `blogdown::filter_timestamp()` has been exported and documented.

- If a theme contains Hugo modules (e.g., the former hugo-academic theme), the modules will be resolved at the time when a theme is installed, which means users will not need to install Go or GIT to work with themes that contain Hugo modules.

- Added a new function `hugo_available()` to check if Hugo with a minimal version is available.

- Added functions `read_toml()` and `write_toml()` to read/write TOML data, and functions `toml2yaml()` and `yaml2toml()` to convert data between TOML and YAML. See their help pages for details.

- Added the `keep_md` argument to `blogdown::html_page()` (thanks, @lazappi, #445).

## MAJOR CHANGES

- When creating a new site with `blogdown::new_site()`, the theme `gcushen/hugo-academic` is automatically redirected to `wowchemy/starter-academic`, because the original Github repo has moved and become a repo of Hugo modules.

- The default value of the global option `blogdown.new_bundle` was changed from `FALSE` to `TRUE` if the site is built through Hugo >= v0.32. This means new posts will be created as leaf bundles, i.e., of the form `path/post-filename/index.md` instead of `path/post-filename.md` (the extension `.md` may also be `.Rmd` or `.Rmarkdown`). If you are not familiar with Hugo's page bundles, please see the documentation at: https://gohugo.io/content-management/page-bundles/. Using page bundles makes it much easier to manage resources like images. Without page bundles, these resources have to be placed under the `static/` directory, and cannot live together with posts under the `content/` directory. If you do not like this change, you may still set `options(blogdown.new_bundle = FALSE)` in your `.Rprofile`. If you do like page bundles and want to convert old posts into bundles, the function `blogdown::bundle_site()` may be helpful.

- For page bundles, the `index_files/` and `index_cache/` folders are no longer moved to the `static/` directory (for other types of posts, these folders are still moved). Consequently, _you should not ignore `"_files$"` in the `ignoreFiles` field_ in your `config.toml` or `config.yaml` any more.

- When opening a **blogdown** project in RStudio, `blogdown::serve_site()` will be automatically called, so you will get the preview of the site immediately. If you do not like this behavior, you may set `options(blogdown.serve_site.startup = FALSE)` in your `.Rprofile`.

- The global option `blogdown.generator.server` has been deprecated. Now `blogdown::serve_site()` always use the Hugo server (which corresponds to `options(blogdown.generator.server = TRUE)` in previous version of **blogdown**), instead of the server created via the **servr** package (which corresponds to the default `options(blogdown.generator.server = FALSE)` before). The Hugo server is much faster, and also supports navigating to the output web page of which you are currently editing the source document. Note that the option `blogdown.hugo.server` is still supported (for setting command-line arguments for `hugo server`), and its default value is `c('-D', '-F', '--navigateToChanged')`. Also note that using Hugo's server means the website is not rendered to disk (i.e., it will not generate the `public/` folder) by default but served directly from memory. If you need the `public/` folder, you have to build the site explicitly via `blogdown::build_site()`, or if you use RStudio, press `Ctrl + Shift + B` or `Cmd + Shift + B` to build the website project (#495).

## MINOR CHANGES

- The command-line argument `--navigateToChanged` is passed to `hugo server` by default now if the Hugo version is not older than 0.25. If you start a Hugo server to serve and watch the site, it will automatically navigate to the page corresponding to the changed file.

- Images `tn.png` and `screenshot.png` under the `images/` directory of a theme will be deleted in `blogdown::install_theme()` because these are screenshots of a theme and don't affect the theme's function.

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

- Added a global option `blogdown.files_filter` to allow users to decide which Rmd files to be rebuilt (this option can be set in `.Rprofile`). The default filter is `blogdown:::filter_timestamp`, i.e., only Rmd files which are older than their output files will be recompiled when rebuilding a site. You can set `options(blogdown.files_filter = blogdown:::filter_md5sum)` to use a different filter based on MD5 checksums, i.e., only rebuild an Rmd file if its MD5 checksum has changed. The checksums of Rmd files are saved in the file `blogdown/md5sum.txt` under the website root directory (thanks, @jonathan-g, #341).

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
