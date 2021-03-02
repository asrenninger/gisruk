# gisruk 2020
### analysis with spatial weights

This comes from a presentation on using geographically-weighted regression, with lasso and ridge variants, to reduce prediction error in modelling problems that exhibit strong patterns across space. These techniques, roughly, depress values farther away from an obversation compared to those closer by, leveraging Tobler's law. Geographically-weighted regression computes and then recomputes linear regression within a shifting window of analysis, allowing the beta coefficients to change each time. This allows us to see how they vary in different parts of the country. The diagram below explains: at each move of the window, we run a regression and store the results. This case explores healthy life expectancy in the United Kingdom, where it is high, where it is low, and where the associations between it and a set of covariates differ.

<p align="center">
  <img src="https://raw.githubusercontent.com/asrenninger/gisruk/master/viz/windows.gif" alt=""/>
</p>



