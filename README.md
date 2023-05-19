# Fortinet Connection Analyzer

This project aims to improve the way System Administrators detect Security Flaw Patterns in their Firewall Connection Logs, by making easy to visualize this logs filtered by services or countries. 

<a href="https://www.buymeacoffee.com/carlesllobet" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee" height="41" width="174"></a>

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

To use this project the following R packages are needed: 
- shiny
- dplyr
- leaflet 
- DT

You can do it at once running the following command:

```
install.packages(c("shiny","dplyr","leaflet","DT"))
```

## Built With

* [R](https://www.r-project.org/) - The statistical computing framework used
* [RStudio](https://www.rstudio.com/) - The IDE used for R

## Authors

* **Carles Llobet** - *Initial work* - [Github](https://github.com/CarlesLlobet)
* **Aleix Hernandez** - *Initial work* - [Github](https://github.com/aleixhernandez)
* **Diego Martin** - *Initial work* - [Github](https://github.com/dmartinarc)
* **Luis Roberto** - *Initial work* - [Github](https://github.com/Luirro1)

See also the list of [contributors](https://github.com/CarlesLlobet/DataDrivenSecurity/contributors) who participated in this project.

## Acknowledgments

* Project inspired by https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
