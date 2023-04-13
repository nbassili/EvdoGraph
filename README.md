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


SPARQL queries
==============

The following SPARQL queries implement several Competency Questions for the Evdoxus Ontology-Knowledge Graph.

- Return all modules that the book is used, along with the Department and the University


	PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
	select (?un as ?Πανεπιστήμιο) (?dn as ?Τμήμα) (?mt as ?Μάθημα) 
	where { 
		?s a evdx:Book .
		?s evdx:hasCode "94700120" .
		?m a evdx:Module .
		?m evdx:title ?mt .
		?m evdx:hasBook ?s .
		?c a evdx:Course .
		?c evdx:year 2022 .
		?c evdx:hasModule ?m .
		?d a evdx:Department .
		?d evdx:hasCourse ?c .
		?d evdx:name ?dn .
		?u a evdx:University .
		?u evdx:hasDepartment ?d .
		?u evdx:name ?un .
	}


- Return how many modules and all module names (in a string), that the book is used, along with the Department and the University, group by Department


	PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
	select (?un as ?Πανεπιστήμιο) (?dn as ?Τμήμα) (count(?mt) as ?ΑριθμόςΜαθημάτων) (group_concat(?mt;separator=", ") as ?Μαθήματα) 
	where { 
		?s a evdx:Book .
		?s evdx:hasCode "94700120" .
		?m a evdx:Module .
		?m evdx:title ?mt .
		?m evdx:hasBook ?s .
		?c a evdx:Course .
		?c evdx:year 2022 .
		?c evdx:hasModule ?m .
		?d a evdx:Department .
		?d evdx:hasCourse ?c .
		?d evdx:name ?dn .
		?u a evdx:University .
		?u evdx:hasDepartment ?d .
		?u evdx:name ?un .
	} group by ?un ?dn


- Return in how many modules, of how many Departments and how many Universities the book is used


	PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
	select (count(DISTINCT ?u) as ?Πανεπιστήμια) (count(DISTINCT ?d) as ?Τμήματα) (count(?m) as ?ΑριθμόςΜαθημάτων) 
	where { 
		?s a evdx:Book .
		?s evdx:hasCode "94700120" .
		?m a evdx:Module .
		?m evdx:hasBook ?s .
		?c a evdx:Course .
		?c evdx:year 2022 .
		?c evdx:hasModule ?m .
		?d a evdx:Department .
		?d evdx:hasCourse ?c .
		?u a evdx:University .
		?u evdx:hasDepartment ?d .
	}


- Return in how many modules, of how many Departments and how many Universities the book is used, per year, for a range of years


	PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
	select ?year (count(DISTINCT ?u) as ?Πανεπιστήμια) (count(DISTINCT ?d) as ?Τμήματα) (count(?m) as ?ΑριθμόςΜαθημάτων) 
	where { 
		?s a evdx:Book .
		?s evdx:hasCode "94700120" .
		?m a evdx:Module .
		?m evdx:hasBook ?s .
		?c a evdx:Course .
		?c evdx:year ?year.
		FILTER (?year>= 2019 && ?year < 2023) .
		?c evdx:hasModule ?m .
		?d a evdx:Department .
		?d evdx:hasCourse ?c .
		?u a evdx:University .
		?u evdx:hasDepartment ?d .
	} group by ?year
	order by ?year


The same query, for multiple books:

	PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
	select ?year (count(DISTINCT ?u) as ?Πανεπιστήμια) (count(DISTINCT ?d) as ?Τμήματα) (count(?m) as ?ΑριθμόςΜαθημάτων) 
	where { 
		?s a evdx:Book .
		VALUES ?code { "94700120" "12867416" }
		?s evdx:hasCode ?code .
		?m a evdx:Module .
		?m evdx:hasBook ?s .
		?c a evdx:Course .
		?c evdx:year ?year.
		FILTER ((?year>=2019) && (?year<2023)) .
		?c evdx:hasModule ?m .
		?d a evdx:Department .
		?d evdx:hasCourse ?c .
		?u a evdx:University .
		?u evdx:hasDepartment ?d .
} group by ?year
order by ?year

- Which Departments (including details about University/Modules) have been added in the second academic year compared to the first


	PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
	select (?un as ?Πανεπιστήμιο) (?dn as ?Τμήμα) (count(?mt) as ?ΑριθμόςΜαθημάτων) (group_concat(?mt;separator=", ") as ?Μαθήματα) 
	where { 
		?s a evdx:Book .
		?s evdx:hasCode "94700120" .
		?m a evdx:Module .
		?m evdx:title ?mt .
		?m evdx:hasBook ?s .
		?c a evdx:Course .
		?c evdx:year 2022 .
		?c evdx:hasModule ?m .
		?d a evdx:Department .
		?d evdx:hasCourse ?c .
		?d evdx:name ?dn .
		?u a evdx:University .
		?u evdx:hasDepartment ?d .
		?u evdx:name ?un .
		FILTER NOT EXISTS {
			?m1 a evdx:Module .
			?m1 evdx:hasBook ?s .
			?c1 a evdx:Course .
			?c1 evdx:year 2021 .
			?c1 evdx:hasModule ?m1 .
			?d evdx:hasCourse ?c1 .
		}
	} group by ?un ?dn


In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Departments (including details about University/Modules) have been deleted from the first academic year compared to the second one.

- Which Universities (including details about Departments/Modules) have been added in the second academic year compared to the first


	PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
	select (?un as ?Πανεπιστήμιο) (?dn as ?Τμήμα) (count(?mt) as ?ΑριθμόςΜαθημάτων) (group_concat(?mt;separator=", ") as ?Μαθήματα) 
	where { 
		?s a evdx:Book .
		?s evdx:hasCode "94700120" .
		?m a evdx:Module .
		?m evdx:title ?mt .
		?m evdx:hasBook ?s .
		?c a evdx:Course .
		?c evdx:year 2022 .
		?c evdx:hasModule ?m .
		?d a evdx:Department .
		?d evdx:hasCourse ?c .
		?d evdx:name ?dn .
		?u a evdx:University .
		?u evdx:hasDepartment ?d .
		?u evdx:name ?un .
		FILTER NOT EXISTS {
			?m1 a evdx:Module .
			?m1 evdx:hasBook ?s .
			?c1 a evdx:Course .
			?c1 evdx:year 2021 .
			?c1 evdx:hasModule ?m1 .
			?d1 a evdx:Department .
			?d1 evdx:hasCourse ?c1 .
			?u evdx:hasDepartment ?d1 .
		}
	} group by ?un ?dn


In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Universities (including details about Departments/Modules) have been deleted from the first academic year compared to the second one.

- Which Modules (including details about Departments/Universities) have been added in the second academic year compared to the first


		PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
		select (?un as ?Πανεπιστήμιο) (?dn as ?Τμήμα) (count(?mt) as ?ΑριθμόςΜαθημάτων) (group_concat(?mt;separator=", ") as ?Μαθήματα) 
		where { 
			?s a evdx:Book .
			?s evdx:hasCode "94700120" .
			?m a evdx:Module .
			?m evdx:title ?mt .
			?m evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year 2022 .
			?c evdx:hasModule ?m .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
			?d evdx:name ?dn .
			?u a evdx:University .
			?u evdx:hasDepartment ?d .
			?u evdx:name ?un .
			FILTER NOT EXISTS {
				?c1 evdx:year 2021 .
				?c1 evdx:hasModule ?m1 .
				?m1 a evdx:Module .
				?m1 evdx:hasBook ?s .
				?m1 evdx:title ?mt .
				?c1 a evdx:Course .
				?d evdx:hasCourse ?c1 .
			}
		} group by ?un ?dn



In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Modules (including details about University/Department) have been deleted from the first academic year compared to the second one.

- Return comparison details and statistics for multiple books for a specific academic year.


		PREFIX evdx: <http://lpis.csd.auth.gr/ontologies/evdoxus#>
		select (group_concat(DISTINCT ?code;separator=", ") as ?Βιβλίο) (count(DISTINCT ?u) as ?Πανεπιστήμια) (count(DISTINCT ?d) as ?Τμήματα) (count(?m) as ?ΑριθμόςΜαθημάτων) 
		where {
			?s a evdx:Book .
			VALUES (?book ?code) { (1 "94700120") (1 "12867416") (2 "102070469") (2 "13909")}
			?s evdx:hasCode ?code .
			?m a evdx:Module .
			?m evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year 2022 .
			?c evdx:hasModule ?m .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
			?u a evdx:University .
			?u evdx:hasDepartment ?d .
		} group by ?book


