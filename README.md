# Covid-19 Epidemic Modeling with R Shiny

<!-- badges: start -->
[![R build status](https://github.com/karthik/CovidShinyModel/workflows/R-CMD-check/badge.svg)](https://github.com/karthik/CovidShinyModel)
<!-- badges: end -->

This is a shiny app that models the COVID-19 epidemic. This is based on the <a href="http://penn-chime.phl.io/">Penn Chime app</a> - all credit goes to the Penn Medicine and Code for Philly team for their idea. 

A beta version of the app is live <a href="https://jpspeng.shinyapps.io/COVIDModel/">here</a>.

## Development Environment Setup (R)

The shiny app is located in the folder /CovidModel. 

To run and test locally on your computer: 

1) Install <a href="https://www.r-project.org/">R</a> and <a href="https://rstudio.com/products/rstudio/download/">R Studio</a>.

2) Open R Studio and in the console, do the following to install the packages:
```
reqs <- c('shiny', 'ggplot2', 'shinyWidgets', 'data.table', 'DT', 'dplyr', 'shinyjs')
install.packages(reqs)
```

3) To run locally, open ui.R or server.R in R Studio and press "Run App" on the upper right hand corner. Or from the commandline run `shiny::runApp()`

## Repo Organization

The live "production" version of the app is on <a href="https://jpspeng.shinyapps.io/COVIDModel/">https://jpspeng.shinyapps.io/COVIDModel/</a>. The code for the production version is in the [master branch](https://github.com/jpspeng/CovidShinyModel)

Pre-release versions of the app are on <a href="https://jpspeng.shinyapps.io/COVIDModel/">https://jpspeng.shinyapps.io/COVIDModel1Prerelease/</a> and <a href="https://jpspeng.shinyapps.io/COVIDModel/">https://jpspeng.shinyapps.io/COVIDModel2Prerelease/</a>. (Model 1 currently uses a deterministic model while Model 2 uses a Markov model.) The code for these versions are in the <b>prerelease</b> branch.


Please note that the 'CovidShinyModel' project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.