# LEMMA (Local Epidemic Modeling for Management & Action) 

<!-- badges: start -->
[![R build status](https://github.com/lemdt/CovidShinyModel/workflows/R-CMD-check/badge.svg)](https://github.com/lemdt/CovidShinyModel/actions)
<!-- badges: end -->

LEMMA (Local Epidemic Modeling for Management & Action) is a tool designed to accept local and regional information about the SARS-CoV-2 (COVID-19) epidemic and provide relevant projections about epidemic trajectory to inform strategic planning and resource management. These projections are based on a modified Susceptible-Exposed-Infectious-Recovered (SEIR) model and are parameterized using local epidemic data on the number of hospitalizations. Outputs include daily number  hospitalizations, ICU use, ventilator use, as well as total cases, active cases, and resolved cases. 

## Development Environment Setup (R)

To run the app locally, load up R or Rstudio and run:

```
remotes::install_github('lemdt/CovidShinyModel')
covidshiny::start_app()
```

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
