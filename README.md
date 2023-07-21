EvdoGraph: A Knowledge Graph for the EVDOXUS textbook management service
================================================================================================================

### A Prolog application for creating a Knowledge Graph of textbooks for Greek Universities from the Evdoxus service
----------------------------------------------------------------------------------------------------------------

[Evdoxus](https://eudoxus.gr/) is a web information system for the management of the total ecosystem for the free provision of textbooks 
to the undergraduate students at the Greek Universities. Among its users are book publishers that register textbooks, faculty members 
that search for appropriate textbooks for their courses, administration of university departments that register the relevant textbooks 
for each module of the curricula (course), and finally, students that select one book per module that they attend. All the above information 
(except for which students selected which books) is freely available at the Evdoxus site in the form of HTML web pages. 

The EvdoGraph project extracts this information and converts it into an open Knowledge Graph in RDF that can be used to generate several 
interesting reports and answer statistical analysis questions in SPARQL, included below. The KG is backed by a simple ontology which is aligned 
with some well-known ontologies. The extraction / conversion application has been developed using SWI-Prolog's XPath and Semantic Web libraries. 
The KG encompasses the Linked Open Data initiative by linking University entities with their corresponding DBpedia entries, 
employing the Wikipedia search engine and the DBpedia SPARQL endpoint.


Running EvdoGraph
-----------------

The main file that should be consulted is 'evdoxusKG.pl'. It consults all the other necessary files.


		?- consult('evdoxusKG.pl').


1. When you run the Program for the first time, you should initialize the cache by running (it takes some time):


		?- init_cache.

	This initializes the cache, downloading all course pages.

1. Each new academic year, the cache should be updated with the new courses by running: 


		?- update_cache.

	This incrementally caches the course pages of the new academic year.

1. When you want to extract all the information from the Evdoxus web service about books, courses, modules, Department and Universities, 
and generate thw Knowledge Graph in RDF internally inside SWI-Prolog, you should run:


		?- extract_courses.


1. After creating the KG, you can save it in a local file, in Turtle format, using thew following:


		?- rdf_save_turtle('evdoxus-all-20230404.ttl',[]).

	Of course, the corrrect filename should be used.

After this, the .ttl file can be uploaded in a Triplestore, such as GraphDB, or Virtuoso.

You could use the following installation: http://lod.csd.auth.gr:7200/sparql
	
You should select the "Evdoxus" repository (pull-down menu at upper-right corner).


SPARQL queries
--------------

The following SPARQL queries implement several Competency Questions for the Evdoxus Ontology-Knowledge Graph.
These competency questions have been derived from the various predicates of the [EvdoStats](https://github.com/nbassili/EvdoStats) project.

- Return all modules that the book is used, along with the Department and the University


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (?mt as ?Module) 
		where { 
			?s a evdx:Book .
			VALUES ?code {"94700120"}
			?s evdx:hasCode ?code .
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

Inside VALUES multiple book IDs can be used (e.g. various editions of the same book).

- Return how many modules and all module names (in a string), that the book is used, along with the Department and the University, group by Department


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department)  (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code { "102070469" "13909" }
					?s evdx:hasCode ?code .
					?m a evdx:Module .
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
			}
		} group by ?un ?dn


- Return in how many modules, of how many Departments and how many Universities the book is used


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (count(DISTINCT ?u) as ?Universities) (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?m) as ?NoOfModules) 
		where { 
			?s a evdx:Book .
			VALUES ?code { "102070469" "13909" }
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
		}


- Return in how many modules, of how many Departments and how many Universities the book is used, per year, for a range of years


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?year (count(DISTINCT ?u) as ?Universities) (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?m) as ?NoOfModules) 
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


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code {"102070469" "13909"}
					?s evdx:hasCode ?code .
					?m a evdx:Module .
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
						VALUES ?code1 { "102070469" "13909" }
						?s1 evdx:hasCode ?code1 .
						?m1 evdx:hasBook ?s1 .
						?c1 a evdx:Course .
						?c1 evdx:year 2021 .
						?c1 evdx:hasModule ?m1 .
						?d evdx:hasCourse ?c1 .
					}
				}
			}
		} group by ?un ?dn



In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Departments (including details about University/Modules) have been deleted from the first academic year compared to the second one.

- Which Universities (including details about Departments/Modules) have been added in the second academic year compared to the first


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code { "102070469" "13909" }
					?s evdx:hasCode ?code .
					?m a evdx:Module .
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
						VALUES ?code1 { "102070469" "13909" }
						?s1 evdx:hasCode ?code1 .
						?m1 evdx:hasBook ?s1 .
						?c1 a evdx:Course .
						?c1 evdx:year 2021 .
						?c1 evdx:hasModule ?m1 .
						?d1 a evdx:Department .
						?d1 evdx:hasCourse ?c1 .
						?u evdx:hasDepartment ?d1 .
					}
				}
			}
		} group by ?un ?dn



In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Universities (including details about Departments/Modules) have been deleted from the first academic year compared to the second one.

- Which Modules (including details about Departments/Universities) have been added in the second academic year compared to the first


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules)  
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code { "102070469" "13909" }
					?s evdx:hasCode ?code .
					?m a evdx:Module .
					?m evdx:hasBook ?s .
					?m evdx:title ?mtitle .
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
						VALUES ?code1 { "102070469" "13909" }
						?s1 evdx:hasCode ?code1 .
						?m1 evdx:hasBook ?s1 .
						?m1 evdx:title ?mtitle .
						?c1 a evdx:Course .
						?d evdx:hasCourse ?c1 .
					}
				}
			}
		} group by ?un ?dn



In the above query if we change the years inside and outside FILTER NOT EXISTS, then we get which Modules (including details about University/Department) have been deleted from the first academic year compared to the second one.

- Return comparison details and statistics for multiple books for a specific academic year.


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (group_concat(DISTINCT ?code;separator=", ") as ?Book) (count(DISTINCT ?u) as ?Universities) (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?m) as ?NoOfModules) 
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


- Which modules (including details about Departments/Universities) use only the first book and not the second


		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code {"102070469" "13909"}
					?s evdx:hasCode ?code .
					?m a evdx:Module .
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
						VALUES ?code1 {"94700120"}
						?s1 evdx:hasCode ?code1 .
						?m evdx:hasBook ?s1 .
					}
				}
			}
		} group by ?un ?dn


- Which Departments (including details about University/Modules) use only the first book and not the second

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code {"94700120"}
					?s evdx:hasCode ?code .
					?m a evdx:Module .
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
						VALUES ?code1 {"102070469" "13909"}
						?s1 evdx:hasCode ?code1 .
						?m1 evdx:hasBook ?s1 .
						?c evdx:hasModule ?m1 .
					}
				}
			}
		} group by ?un ?dn


- Which Universities (including details about Departments/Modules) use only the first book and not the second

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code {"94700120"}
					?s evdx:hasCode ?code .
					?m a evdx:Module .
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
						VALUES ?code1 {"102070469" "13909"}
						?s1 evdx:hasCode ?code1 .
						?m1 evdx:hasBook ?s1 .
						?c1 a evdx:Course .
						?c1 evdx:year 2022 .
						?c1 evdx:hasModule ?m1 .
						?d1 evdx:hasCourse ?c1 .
						?u evdx:hasDepartment ?d1 .
					}
				}
			}
		} group by ?un ?dn


- Which modules (including details about Departments/Universities) use both books 

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code {"102070469" "13909"}
					?s evdx:hasCode ?code .
					?m a evdx:Module .
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
					FILTER EXISTS {
						VALUES ?code1 {"94700120"}
						?s1 evdx:hasCode ?code1 .
						?m evdx:hasBook ?s1 .
					}
				}
			}
		} group by ?un ?dn


- Which Departments (including details about University/Modules) use both books

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (?dn as ?Department) (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			{
				select DISTINCT ?un ?dn ?m 
				where { 
					?s a evdx:Book .
					VALUES ?code {"102070469" "13909"}
					?s evdx:hasCode ?code .
					?m a evdx:Module .
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
					FILTER EXISTS {
						?m1 a evdx:Module .
						VALUES ?code1 {"94700120"}
						?s1 evdx:hasCode ?code1 .
						?m1 evdx:hasBook ?s1 .
						?c evdx:hasModule ?m1 .
					}
				}
			}
		} group by ?un ?dn


- Which Universities (including details about Department/Modules) use both books

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select (?un as ?University) (count(distinct ?d) as ?ΑριθμόςΤμημάτων)(group_concat(?dn;separator=", ") as ?Departments)  (count(?m) as ?NoOfModules) (group_concat(?mt;separator=", ") as ?Modules) 
		where {
			?m evdx:title ?mt .
			?d evdx:name ?dn .
			{
				select DISTINCT ?un ?d ?m
				where { 
					?s a evdx:Book .
					VALUES ?code {"102070469" "13909"}
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
					?u evdx:name ?un .
					FILTER EXISTS {
						?m1 a evdx:Module .
						VALUES ?code1 {"94700120"}
						?s1 evdx:hasCode ?code1 .
						?m1 evdx:hasBook ?s1 .
						?c1 a evdx:Course .
						?c1 evdx:year 2022 .
						?c1 evdx:hasModule ?m1 .
						?d1 evdx:hasCourse ?c1 .
						?u evdx:hasDepartment ?d1 .
					}
				}
			}
		} group by ?un


- Which Departments (and their university) are not active during a specific academic year (they have a course with no modules)

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?dn ?un where { 
			?c a evdx:Course .
			?c evdx:year 2022 .
			?d evdx:hasCourse ?c .
			?d evdx:name ?dn .
			?u evdx:hasDepartment ?d .
			?u evdx:name ?un .
			FILTER NOT EXISTS {
				?c evdx:hasModule ?m .
			}
		}


- Which Universities are not active during a specific academic year (the courses of all their departments have no modules)

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?u ?un where { 
			?u a evdx:University .
			?u evdx:name ?un .
			FILTER NOT EXISTS {
				?u evdx:hasDepartment ?d .
				?d evdx:hasCourse ?c .
				?c evdx:year 2022 .
				?c evdx:hasModule ?m .
			}
		}


- Complete list of all books, with their title and how many modules, departments and universities use it, for a specific year. Order in descending order of the University count.

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?bt (count(DISTINCT ?u) as ?Universities) (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?m) as ?Modules) 
		where { 
			?s a evdx:Book .
			?s evdx:title ?bt .
			?m a evdx:Module .
			?m evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year 2022 .
			?c evdx:hasModule ?m .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
			?u a evdx:University .
			?u evdx:hasDepartment ?d .
		} group by ?s ?bt
		order by desc(?Universities)


- Complete list of all books, with their title and how many modules, departments and universities use it, for a specific year. Order in descending order of the Department and Module count.

		PREFIX evdx: <https://w3id.org/evdoxus#>
		select ?bt  (count(DISTINCT ?d) as ?Departments) (count(DISTINCT ?m) as ?Modules) 
		where { 
			?s a evdx:Book .
			?s evdx:title ?bt .
			?m a evdx:Module .
			?m evdx:hasBook ?s .
			?c a evdx:Course .
			?c evdx:year 2022 .
			?c evdx:hasModule ?m .
			?d a evdx:Department .
			?d evdx:hasCourse ?c .
		} group by ?s ?bt
		order by desc(?Departments) desc(?Modules)

