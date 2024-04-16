# Learning Tennis Shot Location Strategy through Bayesian Modeling
This repository contains work related to my graduate research in collaboration Dr. Garritt Page, Dr. Gilbert Fellingham, and Dr. Alejandro Jara analyzing tennis shot location strategy.

## Data
`AustrailianOpen/`, `FrenchOpen/`, `USOpen/`, and `Wimbledon/` contain raw datasets hand-collected from Major Profession Tennis matches between Roger Federer and Rafael Nadal, Federer and Juan Martin del Potro, and Nadal and Novak Djokovic.

## Cleaning, Wrangling, and Vizualization
The following files contain code to perform cleaning, wrangling, and visualization related to exploratory analysis, preparation for model fitting, and presentation of model results.

* `Cleaning.R`

* `MultinomialProbs.R`

## Bayesian Hierachical Multinomial-Dirichlet Model
See an example of model fit via Markov-chain Monte Carlo (MCMC) simulation using `nimble` in `bayesMultinomialDiscussion.R`.
