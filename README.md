## Testing for Structural Breaks in Factor Copula Models - Implementation and Application in Social Media Topic Analysis
## by Malte Bonart

A thesis written in RMarkdown using a custom latex template. 
<!-- The following fonts are used: Bitter, Lato, Fira Mono. -->
### Source code and data locations

- factor copula examples: `source/fig1.R`.
- monte carlo simulation study: `source/fig2.R`.
- bloc-equidependence simulation study: `source/fig3.R` and `data/Y.rds`.
- creation of the facebook dataset: `source/facebook_create.R`.
- factor copula analysis of facebook dataset: `source/facebook_analysis.R` and `data/topics_residuals.rds`.
- asylum statistics: `source/asylum.R` and `data/asylum.csv`. 

### Requirements for re-running all analyses

- instalation and set up of the developement versions of the packages [`factorcopula`](https://github.com/bonartm/factorcopula) and [`cheopsr`](https://github.com/bonartm/cheopsr).
- access to the [CHEOPS cluster](https://rrzk.uni-koeln.de/cheops.html) at the University of Cologne.
- only for retrieving the original facebook datasset: database login credentials in a file `./data/URI`. See [URI connection string format](https://docs.mongodb.com/manual/reference/connection-string/). 

