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
