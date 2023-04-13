EvdoGraph: A Knowledge Graph for the EVDOXUS textbook management service

A Prolog application for creating a Knowledge Graph of textbooks for Greek Universities from the Evdoxus service
================================================================================================================

- (1) When you run the Program for the first time, you should initialize the cache by running (it takes some time):


	?- init_cache.

This initializes the cache, downloading all course pages.

- (2) Each new academic year, the cache should be updated with the new courses by running: 


	?- update_cache.

This incrementally caches the course pages of the new academic year.

- (3) When you want to extract all the information from the Evdoxus web service about books, courses, modules, Department and Universities, 
and generate thw Knowledge Graph in RDF internally inside SWI-Prolog, you should run:


	?- extract_courses.


- (4) After creating the KG, you can save it in a local file, in Turtle format, using thew following:


	?- rdf_save_turtle('evdoxus-all-20230404.ttl',[]).

Of course, the corrrect filename should be used.

After this, the .ttl file can be uploaded in a Triplestore, such as GraphDB, or Virtuoso.

You could use the following installation:

	http://lod.csd.auth.gr:7200/sparql
	
You should select the "Evdoxus" repository (pull-down menu at upper-right corner).
