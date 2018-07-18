# rimr

:construction: 

[![Build Status](https://travis-ci.org/skvrnami/rimr.svg?branch=master)](https://travis-ci.org/skvrnami/rimr)

[![codecov](https://codecov.io/gh/skvrnami/rimr/branch/master/graph/badge.svg)](https://codecov.io/gh/skvrnami/rimr)

The rimr package serves to help with deterministic record linkage. 

## Installation

You can install rimr from github with:


``` r
# install.packages("devtools")
devtools::install_github("skvrnami/rimr")
```

## Example

See vignette for example.


# TO DO:

## Matching
based on:   
[*] exact equality  
[*] equality with tolerance (x - t <= x <= x + t)  
[ ] equality with tolerance with specified direction (e.g. x >= x & x <= x + t or x <= x & x <= x - t)  
[ ] equality with tolerance based on string distance
[*] higher/lower than  
[*] higher than or equal to/lower than or equal to  
[*] contains string  
[ ] contains string separated by word boundaries (for matching change in women's names after marriage + after divorce)  
[*] removing duplicites based on finding the most similar person (exact match on specified columns)  
[ ] allow methods other than strict equality for finding the most similar person (when there are more than 1 similar person, e.g. in the case of description of a person - using TF-IDF or something similar)  

## Workflow  
[*] Find all similar between 2 datasets  
[*] Find records not contained in the source dataset  
[*] Append missing records to the output  
[ ] Find all similar between sequence of datasets  

## Output  
[*] Create panel data  
[ ] Joint dataset (source dataset enhanced by additional variables from the target dataset)  

## Other

[ ] Check `dbplyr` for storing datasets
[ ] Check algorithmic complexity (could be source grouped?)

### Conceptual

[ ] specify predicate before running `find_all_similar` function  
[ ] connecting filters with OR?  

### Diss-related

[ ] check tolerance bounds (is it necessary to make +- tolerance for checking approximate year of birth?)  
[ ] check similarity of last names of women after getting married (could they be separated by dash?)  
[ ] https://developers.google.com/analytics/devguides/reporting/core/v3/reference#filters (filtering for substrings and regex)  


