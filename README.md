# ussesclimate

This is a package for sharing raw and processed soil moisture and climate data from the US Sheep Experiment Station.  This will be most useful to members of the Adler lab at Utah State University. Soil moisture data includes data collected between 2012 and 2016 in a drought shelter and irrigation experiment at the station. 

## Getting Started

Installing this package will give you access to the processed data files. Raw data and data processing steps are found in the package source in the "data-raw" folder. 

### Prerequisites

R, devtools 

### Installing

Install from github

```
devtools::install_github( 'akleinhesselink/ussesclimate' ) 
```

Check that data is attached: 

```
head( ussesclimate::clean_soil_moisture )
```

## Built With


## Contributing


## Versioning


## Authors

* **Andy Kleinhesselink** - *Initial work* - [akleinhesselink](https://github.com/akleinhesselink)

See also the list of [pbadler](https://github.com/pbadler) who participated in this project.

## License

This project is licensed under the

## Acknowledgments

