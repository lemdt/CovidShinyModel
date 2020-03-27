# Covid-19 Epidemic Modeling with R Shiny
This is a shiny app that models the COVID-19 epidemic. This is based on the <a href="http://penn-chime.phl.io/">Penn Chime app</a> - all credit goes to the Penn Medicine and Code for Philly team for their idea. 

A beta version of the app is live <a href="https://jpspeng.shinyapps.io/COVIDModel/">here</a>.

## Development Environment Setup (R)

1) Install <a href="https://www.r-project.org/">R</a> and <a href="https://rstudio.com/products/rstudio/download/">R Studio</a>.

2) Open R Studio and in the console, do the following to install the packages:
```
reqs <- c('shiny', 'ggplot2', 'shinyWidgets', 'data.table', 'DT', 'dplyr', 'shinyjs')
install.packages(reqs)
```

3) To run locally, open ui.R or server.R in R Studio and press "Run App" on the upper right hand corner.


## Development Environment Setup (Python)
Some quick instructions for getting dev environment setup for Python parts of the pipeline.

Install pyenv virtual env
```
$ brew install pyenv-virtualenv
```

Add the following lines to your `~/.bash_profile` file
```
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi                                       
 if which pyenv-virtualenv-init > /dev/null; then
     eval "$(pyenv virtualenv-init -)";
fi
```

Install python
```
$ pyenv install 3.7.4
```

Create a new virtual environment and activate
```
pyenv virtualenv 3.7.4 covid
pyenv activate covid
```

Install python packages
```
pip install -r requirements.txt
```