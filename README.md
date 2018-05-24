## About
This project uses [R](https://www.r-project.org/) and [Python](https://www.python.org/).

## File map

- ```./data``` : data-source
	+ ```./data/README.md``` : indicate the data source and link to them
- ```./docs``` : Package reference documentation.	
- ```./R``` : contains the ```R``` code
	+ ```./R/sub_project_name``` : code to run a sub-project
		- ```./R/sub_project_name/helpers.R``` : little sub-routines for the sub-project
	+ ```./R/utils``` : shared functions between sub-projects
	+ ```./R/misc``` : code snippets that are not consolidated into a subproject
- ```src``` : ```C``` functions (if needed)
- ```./requirements.txt``` : Development dependencies.

## License

Copyright (c) 2018

Licensed under the [MIT license](LICENSE).