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

:white_check_mark: exact equality  
:white_check_mark: equality with tolerance (x - t <= x <= x + t)  
:x: equality with tolerance with specified direction (e.g. x >= x & x <= x + t or x <= x & x <= x - t)  
:x: equality with tolerance based on string distance  
:white_check_mark: higher/lower than  
:white_check_mark: higher than or equal to/lower than or equal to  
:white_check_mark: contains string  
:x: contains string separated by word boundaries (for matching change in women's names after marriage + after divorce)  
:white_check_mark: removing duplicites based on finding the most similar person (exact match on specified columns)  
:white_check_mark: allow methods other than strict equality for finding the most similar person (when there are more than 1 similar person, e.g. in the case of description of a person - using Jaccard distance for comparison of words in occupation of a person, see `tests` for example)  

## Workflow  
:white_check_mark: Find all similar between 2 datasets  
:white_check_mark: Find records not contained in the source dataset  
:white_check_mark: Append missing records to the output  
:x: Find all similar between sequence of datasets  

## Output  
:white_check_mark: Create panel data  
:white_check_mark: Joint dataset (source dataset enhanced by additional variables from the target dataset)  

## Other

:x: Check `dbplyr` for storing datasets  
:x: Check algorithmic complexity (could be source grouped?)

### Conceptual

:x: specify predicate before running `find_all_similar` function  
:x: connecting filters with OR?  

### Diss-related

:x: check tolerance bounds (is it necessary to make +- tolerance for checking approximate year of birth?)  
:x: check similarity of last names of women after getting married (could they be separated by dash?)  
:x: https://developers.google.com/analytics/devguides/reporting/core/v3/reference#filters (filtering for substrings and regex)  


