[![Build Status](https://travis-ci.com/hrvg/sturdy-umbrella.svg?token=Dx1gYTrTiuxgW9Sq3s3q&branch=master)](https://travis-ci.com/hrvg/sturdy-umbrella)

# Literature Review Finds Opportunities for Water Resources Management Research in Latin America and the Carribean

Alyssa J. DeVincentis (1), Hervé Guillon (1), Romina Díaz Gómez (1), Noelle K. Patterson (1), Francine van den Brandeler (2), Arthur Koehl (1), J. Pablo Ortiz-Partida (3), Laura E. Garza-Díaz (1), Jennifer Gamez-Rodríguez (1), Erfan Goharian (5) and Samuel Sandoval Solis (1)

(1): University of California, Davis;
(2): University of Amsterdam;
(3): Union of Concerned Scientists;
(4): University of South Carolina

Global health, ecosystem function, and economic prosperity are unquestionably linked to successful management of water resources, which requires sufficient and equitably distributed research across spatial, time, and climatic scales. This research identifies research opportunities for water resources management in Latin America. We conducted an unprecedented literature review of over 30,000 multilingual articles on water resources research through the use of Latent Dirichlet Association (LDA). Topic modeling was validated through cross-validation with a training set of human-derived topics. A database was built to include results from the LDA analysis and biophysical and economic country descriptors in order to correlate countries’ populations, economies, natural resources, climate, and governmental structures to the state of their water resources research. Georeferenced relationships between the LDA results and country descriptors revealed bright spots and knowledge gaps, and a network model identified the strongest and weakest connections between topics in water resources research. A systems dynamics model identified country characteristics that are driving the prevalence or lack of research in certain water resources topics. All of these results were ground-truthed with an electronic survey sent to academic researchers throughout Latin America. The results from this research emphasize the need for more equitable distribution of resources, both financial and human, throughout Latin America to ensure the future safety and success of the region.

---

## File map

- `./data` : data-source
	+ `./data/README.md` : indicate the data source and link to them
- `./docs` : Package reference documentation.	
- `./R` : contains the `R` code
	+ `./R/sub_project_name` : code to run a sub-project
		- `./R/sub_project_name/helpers.R` : sub-routines for the sub-project
	+ `./R/utils` : shared functions between sub-projects
	+ `./R/misc` : unconsolidated code snippets
- `src` : `python` code
	+ `BERTpy`: various versions of `python` scripts to run [Google BERT](https://github.com/google-research/bert); as this development was abandonned, code has not been refactored to production-level
- `./requirements.txt` : dependencies

## About

This project uses:

- [R](https://www.r-project.org/)
- [Python](https://www.python.org/)
- [Google BERT](https://github.com/google-research/bert)
- [Natural Earth Data](https://www.naturalearthdata.com)


Need to add here a path towards the entire datasets except `pdf` files. Review vignettes for paths:

- `F:\hguillon\research\data\latin_america\corpus_pdf`
- `F:\hguillon\research\data\latin_america\corpus_csv`