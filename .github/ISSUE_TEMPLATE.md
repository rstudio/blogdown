For general questions, please ask them on StackOverflow first, using at least the tags `r` and `blogdown`: http://stackoverflow.com/questions/ask Please come back here only if nobody answers your question there in at least 24 hours, and let us know the URL of your StackOverflow post. If you are not sure whether you should use StackOverflow or Github issues, use Stackoverflow first. We do watch StackOverflow questions as actively as Github issues.

For bug reports, please provide a minimal, self-contained, and reproducible example by reducing your example as much as possible right before the problem goes away. By doing this, you may be able to figure out what the problem really is before reporting to us. You can attach your example as a zip file here along with `devtools::session_info('blogdown')`, and screenshots are often very helpful to illustrate your issues.

To include a verbatim chunk of arbitrary text, wrap it in a pair of three backticks. When any line of your text contains N backticks (N >= 3), use N + 1 backticks to wrap the text, e.g. use four backticks to wrap three:

````
A sample document.

```{r}
1 + 1  # a line of code
```

Another paragraph.
````

If it is just a chunk of R code (or other languages) and you want syntax highlighting, you may use three backticks to format it, e.g.

```r
rnorm(10)
```

Usually your issue will be closed after it is fixed, but sometimes it is closed only because we are unable to offer any help. It does not mean your issue is not real or bad. You can propose a fix by yourself through a pull request. Your constructive feedback is always appreciated.
