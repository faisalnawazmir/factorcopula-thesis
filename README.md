## Testing for Structural Breaks in Factor Copula Models - Implementation and Application in Social Media Topic Analysis
## by Malte Bonart

A thesis written in RMarkdown using a custom latex template. 
<!-- The following fonts are used: Bitter, Lato, Fira Mono. -->

### Source code locations

- factor copula examples: `source/fig1.R`.
- monte carlo simulation study: `source/fig2.R`.
- bloc-equidependence simulation study: `source/fig3.R`.
- creation of the facebook dataset: `source/facebook_create.R`.
- factor copula analysis of facebook dataset: `source/facebook_analysis.R`.

### Requirements for re-running all analyses

- instalation and set up of the developement versions of the packages [`factorcopula`](https://github.com/bonartm/factorcopula) and [`cheopsr`](https://github.com/bonartm/cheopsr).
- access to the [CHEOPS cluster](https://rrzk.uni-koeln.de/cheops.html) at the University of Cologne.
- only for facebook analysis: database login credentials in a file `./data/URI`. See [URI connection string format](https://docs.mongodb.com/manual/reference/connection-string/). 

