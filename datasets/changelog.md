0.4
	* Get rid of dependency on 'data-default' (introduced by previous versions of 'req')
	
	* Bump 'req' dependency to 2.0.0 
0.3
	* 'datasets' hosted within the DataHaskell/dh-core project

	* use 'req' for HTTP and HTTPS requests, instead of 'wreq'

	* Mushroom and Titanic datasets

	* Restructured top-level documentation

	* Removed 'csvDatasetPreprocess' and added 'withPreprocess'. Now bytestring preprocessing is more compositional, i.e. 'withPreprocess' can be used with JSON datasets as well.
	

0.2.5

	* Old Faithful matches R dataset

0.2.4

	* Netflix dataset

0.2.3	

	* Coal dataset

	* New internal API

	* Ord instance for IrisClass

0.2.2

	* Enum, Bounded instances for IrisClass

	* Gapminder dataset

	* Use wreq for HTTP and HTTPS requests

0.2.1

	* Wine quality datasets

	* Vocabulary, UN, States datasets

	* CO2, Sunspots and Quakes datasets

0.2.0.3

	* Further GHC portability

0.2.0.2

	* Improve GHC portability

0.2.0.1

	* Bugfix: include embedded data files in cabal extra-source-files

0.2

	* iris dataset is a pure value (with file-embed)

	* Michelson, Nightingale and BostonHousing datasets
