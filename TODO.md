# TO DO:

## Particular stuff:  

* add tests
* check tolerance bounds (is it necessary to make +- tolerance for checking approximate year of birth?)
* check similarity of last names of women after getting married (could they be separated by dash?)

## Conceptual:

* How to compare across multiple election (recursive matching) 
* connecting filters with OR?
* to SQLite and back - create pivot table for identification of the same persons
and then extract data to tidy format (person_id, first_name, last_name, party_name, list_rank, ...)
* if there are more matches, select the person with more attributes that have the same
value (add finding similarity of descriptions)

## Graphical outputs [outside of package]:

* D3 animation (circles moving between groups) or [this](https://www.hvitfeldt.me/2018/03/recreate-sankey-flow-chart/)
* Sankey chart
* missing values (those who did not run) - pseudo heatmap

## Data
* API to retrieve data from SQLite
