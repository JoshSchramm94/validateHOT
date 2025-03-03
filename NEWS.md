# validateHOT 1.0.5

* implemented suggestions and comments from JOSS review team
* release for JOSS submission
* fixes in the marksim() function (method and res arguments set to NULL)
* fixes in the kl() function (epsilon and log_base arguments set to NULL)

# validateHOT 1.0.4

* Implemented the suggestions from the JOSS review process, which makes functions faster
* changed name from createHOT() to create_hot()
* changed piece.p to a nested list to allow for multiple piecewise-coded attributes
* added examples for part-worth coded attributes but later treat this attribute continuously
* added turf_ladder() function
* bug fixes
* adjusted DESCRIPTION file title slightly

# validateHOT 1.0.3

* got rid of the base:: calls in all functions

# validateHOT 1.0.2

* Updated tidyverse logic and switched from summarise() to reframe()
* updated prob_scores() function for individual probability scores
