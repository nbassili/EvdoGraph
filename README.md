EvdoGraph: A Knowledge Graph for the EVDOXUS textbook management service
========================================================================

### A Prolog application for creating a Knowledge Graph of textbooks for Greek Universities from the Evdoxus service
--------------------------------------------------------------------------------------------------------------------

[Evdoxus](https://eudoxus.gr/) is a web information system for the management of the total ecosystem for the free provision of textbooks 
to the undergraduate students at the Greek Universities. Among its users are book publishers that register textbooks, faculty members 
that search for appropriate textbooks for their courses, administration of university departments that register the relevant textbooks 
for each course of the curricula, and finally, students that select one book per course that they attend. All the above information 
(except for which students selected which books) is freely available at the Evdoxus site. 

The EvdoGraph project retrieves this information, through the Evdoxus APIs, and converts it into an open Knowledge Graph in RDF that can be used 
to generate several interesting reports and answer statistical analysis questions in SPARQL, included below. The KG is backed by a simple ontology 
which is aligned with some well-known ontologies (VIVO, AIISO, TEACH, Bowlogna, Schema.org, DBpedia, Wikidata). 
The KG encompasses the Linked Open Data initiative by linking University entities with their corresponding DBpedia entries, 
employing the Wikipedia search engine and the DBpedia SPARQL endpoint.

The retrieval / conversion application has been developed using SWI-Prolog's http, json, XPath and Semantic Web libraries. 

The possible uses for the KG are countless. Appart from the SPARQL queries shown below, the KG could be used for marketing purposes, 
i.e., publishers could have an instant clear picture of the University market in order to strategically decide for new books or promotion targets, 
or faculty researchers could analyse the Greek Higher Education landscape, i.e., analyse what kind of courses are taught at various disciplines, 
or compare study programs at different Universities / Departments.

Ideas for future work could include the more fine-grained treatment of various information of textbooks. For example, currently all authors are contained in a single string. The same is true about the professors that teach a course. So, separating reliably this information into lists of persons would statistics about authors and professors. Furthermore, based on the ISBN and/or other book information there is a possibility to further link the KG to external bibliographic LOD datasets. Finally, another option would be to link courses to their syllabus description at various University repositories or open data APIs, such as the one of the [Aristotle University of Thessaloniki](https://ws-ext.it.auth.gr/swagger/).

Publication
-----------
N. Bassiliades, [“EvdoGraph: A Knowledge Graph for the EVDOXUS Textbook Management Service for Greek Universities”](https://intelligence.csd.auth.gr/publication/conference-papers/evdograph-a-knowledge-graph-for-the-evdoxus-textbook-management-service-for-greek-universities/), accepted for presentation at, 15th International Conference on Knowledge Engineering and Ontology Development (KEOD 2023), 13-15 Nov 2023, Rome, Italy.   

This publication describes a previous version of EvdoGraph, where information about books and courses was extracted via scrapping. Also, the described ontology is simpler
than the current one.

Running EvdoGraph
-----------------

The main file that should be consulted is 'evdoxusKG.pl'. It consults all the other necessary files.


		?- consult('evdoxusKG.pl').


1. When you run the Program for the first time, you should initialize the cache by running (it takes some time):


		?- init_cache.

	This initializes the cache, downloading all information about universities, departments, courses (per year), books and books per course.
	These are saved in the following files at the ./cache/ subfolder:
		- years.pl
		- universities.pl
		- departments.pl
		- courses.pl
		- books.pl
		- books_per_course.pl

1. Each new academic year, the cache should be updated with the new courses by running: 


		?- update_cache.

	This incrementally caches the course pages of the new academic year. Sometimes, in the middle of an academic year, this refreshes the cache of the current academic year,
	since during the year information about book per course is usually updated.

1. When you want to convert all the cached information from the Evdoxus web service about books, courses, Department, Universities and Publishers, 
into the Evdoxus Knowledge Graph (in RDF), you should run:


		?- generate_graph.

	The KG is saved in the local file 'evdoxus-all.ttl', in Turtle format.
	After this, the .ttl file can be uploaded in a Triplestore, such as GraphDB, or Virtuoso.

1. When you want to update the RDF Evdoxus Graph from the updated cached information, you should run:


		?- update_graph.

	The KG is saved in the local file 'evdoxus-all-new.ttl', in Turtle format.
	After this, the .ttl file can be uploaded in a Triplestore, such as GraphDB, or Virtuoso.
	Since some of the triples in the old KG may not be present in the new graph, it is advised to recreate the KG at the triplestore;
	not to just incrementally load it.

You could use the following GraphDB installation: http://lod.csd.auth.gr:7200/sparql
	
You should select the "EvdoGraph" repository (pull-down menu at upper-right corner).


SPARQL queries
--------------

The following SPARQL queries implement several Competency Questions for the Evdoxus Ontology-Knowledge Graph.
These competency questions have been derived from the various predicates of the [EvdoStats](https://github.com/nbassili/EvdoStats) project.
Run the following SPARQL queries not including the inferred data at the results, since the linked DBPedia entities for Universities will 
inflate the results. (Press the fourth-from-the-top icon at the right side of the SPARQL textbox).

1. Return all courses that the book is used, along with the Department and the University


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select DISTINCT (?un as ?University) (?dn as ?Department) (?t as ?Course) 
		where { 
			?s a evdx:Book .
			VALUES ?code {94700120}
			?s evdx:hasCode ?code .
			?c a evdx:Course .
			?c evdx:title ?t .
			?c evdx:hasBook ?s .
			?c evdx:year 2022 .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
			?d evdx:name ?dn .
			?u a evdx:University .
			?u evdx:hasDepartment ?d .
			?u evdx:name ?un .
		}

	Inside VALUES multiple book IDs can be used (e.g. various editions of the same book).

1. Return how many courses and all course names (in a string), that the book is used, along with the Department and the University, group by Department


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department)  (count(?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses) 
		where {
			?c evdx:title ?t .
			{
				select DISTINCT ?un ?dn ?c 
				where { 
					?s a evdx:Book .
					VALUES ?code { 102070469 13909 }
					?s evdx:hasCode ?code .
					?c evdx:hasBook ?s .
					?c a evdx:Course .
					?c evdx:year 2022 .
					?d a evdx:Department .
					?d evdx:hasCourse ?c .
					?d evdx:name ?dn .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
					?u evdx:name ?un .
				}
			}
		} group by ?un ?dn


1. Return in how many courses, of how many Departments and how many Universities the book is used


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (count(DISTINCT ?u) as ?Universities) (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?c) as ?NoOfCourses) 
		where { 
			?s a evdx:Book .
			VALUES ?code { 102070469 13909 }
			?s evdx:hasCode ?code .
			?c evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year 2022 .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
			?u a evdx:University .
			?u evdx:hasDepartment ?d .
		}


1. Return in how many courses, of how many Departments and how many Universities the book is used, per year, for a range of years


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?year (count(DISTINCT ?u) as ?Universities) (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?c) as ?NoOfCourses) 
		where { 
			?s a evdx:Book .
			VALUES ?code { 94700120 12867416 }
			?s evdx:hasCode ?code .
			?c evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year ?year.
			FILTER ((?year>=2019) && (?year<2023)) .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
			?u a evdx:University .
			?u evdx:hasDepartment ?d .
		} group by ?year
		  order by ?year


1. Which Departments (including details about University/Courses) have been added in the second academic year compared to the first


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(DISTINCT ?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses) 
		where {
			?c evdx:title ?t .
			{
				select DISTINCT ?un ?dn ?c 
				where { 
					?s a evdx:Book .
					VALUES ?code {102070469 13909}
					?s evdx:hasCode ?code .
					?c evdx:hasBook ?s .
					?c a evdx:Course .
					?c evdx:year 2022 .
					?d a evdx:Department .
					?d evdx:hasCourse ?c .
					?d evdx:name ?dn .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
					?u evdx:name ?un .
					FILTER NOT EXISTS {
						?c1 a evdx:Course .
						?s1 a evdx:Book .
						VALUES ?code1 { 102070469 13909 }
						?s1 evdx:hasCode ?code1 .
						?c1 evdx:hasBook ?s1 .
						?c1 evdx:year 2021 .
						?d evdx:hasCourse ?c1 .
					}
				}
			}
		} group by ?un ?dn



	In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Departments (including details about University/Courses) have been deleted from the first academic year compared to the second one.

1. Which Universities (including details about Departments/Courses) have been added in the second academic year compared to the first


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses) 
		where {
			?c evdx:title ?t .
			{
				select DISTINCT ?un ?dn ?c
				where { 
					?s a evdx:Book .
					VALUES ?code { 102070469 13909 }
					?s evdx:hasCode ?code .
					?c evdx:hasBook ?s .
					?c a evdx:Course .
					?c evdx:year 2022 .
					?d a evdx:Department .
					?d evdx:hasCourse ?c .
					?d evdx:name ?dn .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
					?u evdx:name ?un .
					FILTER NOT EXISTS {
                		?c1 a evdx:Course .
						?s1 a evdx:Book .
						VALUES ?code1 { 102070469 13909 }
						?s1 evdx:hasCode ?code1 .
						?c1 evdx:hasBook ?s1 .
						?c1 evdx:year 2021 .
						?d1 a evdx:Department .
						?d1 evdx:hasCourse ?c1 .
						?u evdx:hasDepartment ?d1 .
					}
				}
			}
		} group by ?un ?dn



	In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Universities (including details about Departments/Courses) have been deleted from the first academic year compared to the second one.

1. Which Courses (including details about Departments/Universities) have been added in the second academic year compared to the first


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses)  
		where {
			?c evdx:title ?t .
			{
				select DISTINCT ?un ?dn ?c
				where { 
					?s a evdx:Book .
					VALUES ?code { 102070469 13909 }
					?s evdx:hasCode ?code .
					?c evdx:hasBook ?s .
					?c evdx:title ?title .
					?c a evdx:Course .
					?c evdx:year 2022 .
					?d a evdx:Department .
					?d evdx:hasCourse ?c .
					?d evdx:name ?dn .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
					?u evdx:name ?un .
					FILTER NOT EXISTS {
						?c1 a evdx:Course .
						?s1 a evdx:Book .
						?c1 evdx:year 2021 .
						VALUES ?code1 { 102070469 13909 }
						?s1 evdx:hasCode ?code1 .
						?c1 evdx:hasBook ?s1 .
						?c1 evdx:title ?title .
						?d evdx:hasCourse ?c1 .
					}
				}
			}
		} group by ?un ?dn



	In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Courses (including details about University/Department) have been deleted from the first academic year compared to the second one.

1. Return comparison details and statistics for multiple books for a specific academic year.


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (group_concat(DISTINCT ?code;separator=", ") as ?Book) (count(DISTINCT ?u) as ?Universities) (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?c) as ?NoOfCourses) 
		where {
			?s a evdx:Book .
			VALUES (?book ?code) { (1 94700120) (1 12867416) (2 102070469) (2 13909)}
			?s evdx:hasCode ?code .
			?c evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year 2022 .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
			?u a evdx:University .
			?u evdx:hasDepartment ?d .
		} group by ?book


1. Which courses (including details about Departments/Universities) use only the first book and not the second


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses) 
		where {
			?c evdx:title ?t .
			{
				select DISTINCT ?un ?dn ?c
				where { 
					?s a evdx:Book .
					VALUES ?code {102070469 13909}
					?s evdx:hasCode ?code .
					?c evdx:hasBook ?s .
					?c a evdx:Course .
					?c evdx:year 2022 .
					?d a evdx:Department .
					?d evdx:hasCourse ?c .
					?d evdx:name ?dn .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
					?u evdx:name ?un .
					FILTER NOT EXISTS {
						VALUES ?code1 {94700120}
						?s1 evdx:hasCode ?code1 .
						?c evdx:hasBook ?s1 .
					}
				}
			}
		} group by ?un ?dn


1. Which Departments (including details about University/Courses) use only the first book and not the second

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses) 
		where {
			?c evdx:title ?t .
			{
				select DISTINCT ?un ?dn ?c
				where { 
					?s a evdx:Book .
					VALUES ?code {94700120}
					?s evdx:hasCode ?code .
					?c evdx:hasBook ?s .
					?c a evdx:Course .
					?c evdx:year 2022 .
					?d a evdx:Department .
					?d evdx:hasCourse ?c .
					?d evdx:name ?dn .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
					?u evdx:name ?un .
					FILTER NOT EXISTS {
						VALUES ?code1 {102070469 13909}
						?s1 evdx:hasCode ?code1 .
						?c evdx:hasBook ?s1 .
					}
				}
			}
		} group by ?un ?dn


1. Which Universities (including details about Departments/Courses) use only the first book and not the second

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses)  
		where {
			?c evdx:title ?t .
			{
				select DISTINCT ?un ?dn ?c
				where { 
					?s a evdx:Book .
					VALUES ?code {94700120}
					?s evdx:hasCode ?code .
					?c evdx:hasBook ?s .
					?c a evdx:Course .
					?c evdx:year 2022 .
					?d a evdx:Department .
					?d evdx:hasCourse ?c .
					?d evdx:name ?dn .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
					?u evdx:name ?un .
					FILTER NOT EXISTS {
						VALUES ?code1 {102070469 13909}
						?s1 evdx:hasCode ?code1 .
						?c1 evdx:hasBook ?s1 .
						?c1 a evdx:Course .
						?c1 evdx:year 2022 .
						?d1 evdx:hasCourse ?c1 .
						?u evdx:hasDepartment ?d1 .
					}
				}
			}
		} group by ?un ?dn


1. Which courses (including details about Departments/Universities) use both books 

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?t as ?Course) (?dn as ?Department) (?un as ?University) 
		where {
			?c evdx:title ?t .
    		?u evdx:name ?un .
    		?d evdx:name ?dn .
			{
				select DISTINCT ?u ?d ?c
				where { 
					?s a evdx:Book .
					VALUES ?code {102070469 13909}
					?s evdx:hasCode ?code .
					?c evdx:hasBook ?s .
					?c a evdx:Course .
					?c evdx:year 2022 .
					?s1 a evdx:Book .
					VALUES ?code1 {94700120}
					?s1 evdx:hasCode ?code1 .
					?c evdx:hasBook ?s1 .
					?d a evdx:Department .
					?d evdx:hasCourse ?c .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
				}
			}
		} 


1. Which Departments (including details about University/Courses) use both books 

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?dn as ?Department) (?un as ?University) (count(?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses)  
		where {
			?c evdx:title ?t .
			?u evdx:name ?un .
    		?d evdx:name ?dn .
			{
				select DISTINCT ?u ?d ?c
				where { 
					?s a evdx:Book .
					VALUES ?code {102070469 13909}
					?s evdx:hasCode ?code .
					?s1 a evdx:Book .
					VALUES ?code1 {94700120}
					?s1 evdx:hasCode ?code1 .
					{
						?c evdx:hasBook ?s .
					}
					UNION
					{
						?c evdx:hasBook ?s1 .
					}
					?c a evdx:Course .
					?c evdx:year 2022 . 
					?d evdx:hasCourse ?c .
					?d a evdx:Department .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
				}
			}
		} group by ?dn ?un


1. Which Universities (including details about Department/Courses) use both books

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (count(distinct ?d) as ?NoOfDepartments) (group_concat(distinct ?dn;separator=", ") as ?Departments) (count(?c) as ?NoOfCourses) (group_concat(?t;separator=", ") as ?Courses)  
		where {
			?c evdx:title ?t .
			?u evdx:name ?un .
    		?d evdx:name ?dn .
			{
				select DISTINCT ?u ?d ?c
				where { 
					?s a evdx:Book .
					VALUES ?code {102070469 13909}
					?s evdx:hasCode ?code .
					?s1 a evdx:Book .
					VALUES ?code1 {94700120}
					?s1 evdx:hasCode ?code1 .
					{
						?c evdx:hasBook ?s .
						?d evdx:hasCourse ?c .
					}
					UNION
					{
						?c evdx:hasBook ?s1 .
						?d evdx:hasCourse ?c .
					}
					?c a evdx:Course .
					?c evdx:year 2022 .
					?d a evdx:Department .
					?u a evdx:University .
					?u evdx:hasDepartment ?d .
				}
			}
		} group by ?un


1. Which Departments (and their University) are not active during a specific academic year (they do not have any course during that year)

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select DISTINCT ?dn ?un where { 
			?d rdf:type evdx:Department .		
    		?d evdx:name ?dn .
			?u evdx:hasDepartment ?d .
			?u evdx:name ?un .
			FILTER NOT EXISTS {
				?c a evdx:Course .
				?c evdx:year 2022 .
				?d evdx:hasCourse ?c .
			}
		}


1. Which Universities are not active during a specific academic year (all their departments do not have any course during that year) 

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?u ?un where { 
			?u a evdx:University .
			?u evdx:name ?un .
			FILTER NOT EXISTS {
				?u evdx:hasDepartment ?d .
				?d evdx:hasCourse ?c .
				?c evdx:year 2022 .
			}
		}


1. Complete list of all books, with their title and how many courses, departments and universities use it, for a specific year. Order in descending order of the University count. 

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?bt (count(DISTINCT ?u) as ?Universities) (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?c) as ?Courses) 
		where { 
			?s a evdx:Book .
			?s evdx:title ?bt .
			?c evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year 2022 .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
			?u a evdx:University .
			?u evdx:hasDepartment ?d .
		} group by ?s ?bt
		order by desc(?Universities)
		
	
	
	The above query might need to increase the heap size in order to run.
	

1. Complete list of all books, with their title and how many courses, departments and universities use it, for a specific year. Order in descending order of the Department and Course count.

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?bt  (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?c) as ?Courses) 
		where { 
			?s a evdx:Book .
			?s evdx:title ?bt .
			?c evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year 2022 .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
		} group by ?s ?bt
		order by desc(?Departments) desc(?Courses)
		
	
	
	The above query might need to increase the heap size in order to run.

