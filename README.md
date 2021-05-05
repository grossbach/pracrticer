# praktikr
The R package praktikr contains functions to ease handling of subjective, and often sparse, deliberate practice time estimates, retrospectively collected from domain experts such as chess players and musicians. Mean daily hours of deliberate practice are used to calculate the cumulative practice hours someone has spent practice in their field of expertise.

To install praktikr, issue the following commands at the R prompt:


```r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("grossbach/praktikr")
```


praktikr's vignette has more information on how to use the package:

```r
vignette(praktikr)
```



