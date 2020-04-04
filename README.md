# Covid-19 Epidemic Modeling with R Shiny

<!-- badges: start -->
[![R build status](https://github.com/lemdt/CovidShinyModel/workflows/R-CMD-check/badge.svg)](https://github.com/lemdt/CovidShinyModel/actions)
<!-- badges: end -->

This is a shiny app that models the COVID-19 epidemic. This is based on the <a href="http://penn-chime.phl.io/">Penn Chime app</a> - all credit goes to the Penn Medicine and Code for Philly team for their idea. 

A beta version of the app is live <a href="https://jpspeng.shinyapps.io/COVIDModel/">here</a>.

## Development Environment Setup (R)

To run the app locally, load up R or Rstudio and run:

```
remotes::install_github('lemdt/CovidShinyModel')
covidshiny::start_app()
```

## Repo Organization

(James: the text for this section needs to be revised)

The live "production" version of the app is on <a href="https://jpspeng.shinyapps.io/COVIDModel/">https://jpspeng.shinyapps.io/COVIDModel/</a>. The code for the production version is in the [master branch](https://github.com/jpspeng/CovidShinyModel)

Pre-release versions of the app are on <a href="https://jpspeng.shinyapps.io/COVIDModel/">https://jpspeng.shinyapps.io/COVIDModel1Prerelease/</a> and <a href="https://jpspeng.shinyapps.io/COVIDModel/">https://jpspeng.shinyapps.io/COVIDModel2Prerelease/</a>. (Model 1 currently uses a deterministic model while Model 2 uses a Markov model.) The code for these versions are in the <b>prerelease</b> branch.


## Issues and bugs

If you find any issues or problems, please post an [issue](https://github.com/lemdt/CovidShinyModel/issues)


## License
 
The MIT License (MIT)

Copyright (c) 2020 LEMMA

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


## Code of Conduct

Please note that the 'CovidShinyModel' project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.