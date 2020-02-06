# Canadian-Recall-Api-in-R
API for Government of Canada recall based in R Language

<!-- badges: start -->
![Travis build status](https://travis-ci.org/derekreay/canrecall.svg?branch=master)
<!-- badges: end -->

## Overview

The aim of canrecall is to provide a wrapper in R for the recall API available on the healthycanadians.gc.ca website, and to add additional functionality such as searching by date of recall not present through the API.

- `recall_api()` lets you search for recalls based on a variety of criteria
-`recall_api_date()` - enables you to filter recalls based on a variety of criteria including dates which was not originally available through the webpage
-`recall_api_recent()`- returns the last 15 recalls by date along with the last 15 recalls in the 4 categories.  

## Key features:

* Functions for the main search functions in the API.  There are return the 15 latest recalls.  Or search recalls by set criteria.  As the api returned a date we added another function to screen recalls by date which is not available through the api on the website.

* Requests return an R dataframe that includes the return number, date, category along with some other information.

* a set of parameters can be passed into the functions based on the search criteria wanted.  

* The link data returned by two of the functions does not link to the website of the recall number.  This is an error with the API and not with our code.


## Getting help

If you encounter a bug, please file a reproducible example on Github.
