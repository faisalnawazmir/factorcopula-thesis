## Testing for Structural Breaks in Factor Copula Models - Implementation and Application in Social Media Topic Analysis
## by Malte Bonart

A thesis written in R Markdown using a custom latex template. 
<!-- The following fonts are used: Bitter, Lato, Fira Mono. -->
### Source code and data locations

- factor copula examples: `source/fig1.R`.
- Monte Carlo simulation study: `source/fig2.R`.
- bloc-equidependence simulation study: `source/fig3.R` and `data/Y.rds`.
- creation of the Facebook data-set: `source/facebook_create.R`.
- factor copula analysis of Facebook data-set: `source/facebook_analysis.R` and `data/topics_residuals.rds`.
- asylum statistics: `source/asylum.R` and `data/asylum.csv`. 

### Requirements for re-running all analyses

- installation and set up of the development versions of the packages [`factorcopula`](https://github.com/bonartm/factorcopula) and [`cheopsr`](https://github.com/bonartm/cheopsr).
- access to the [CHEOPS cluster](https://rrzk.uni-koeln.de/cheops.html) at the University of Cologne.
- only for retrieving the original Facebook data-set: database login credentials in a file `./data/URI`. See [URI connection string format](https://docs.mongodb.com/manual/reference/connection-string/). 

