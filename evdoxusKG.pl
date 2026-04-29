:- set_prolog_flag(encoding,utf8).

:- [caching].

:- ensure_loaded('cache/years.pl').
:- ensure_loaded('cache/universities.pl').
:- ensure_loaded('cache/departments.pl').
:- ensure_loaded('cache/courses.pl').
:- ensure_loaded('cache/books.pl').
:- ensure_loaded('cache/books_per_course.pl').


:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/sparql_client)).

%:- [cache_courses].


:- rdf_register_prefix(dbo,'http://dbpedia.org/ontology/'),
   rdf_register_prefix(dbr,'http://dbpedia.org/resource/'),
   rdf_register_prefix('dbpedia-el','http://el.dbpedia.org/resource/').

:- rdf_load('evdoxus.ttl',[register_namespaces(true)]).

:- set_prolog_flag(stack_limit, 8_589_934_592).

% Initialize cache when you run the program for the first time by running:
%    :- init_cache.
% Each new academic year, the cache should be updated with the new year coursesm by running:
%    :- update_cache.
% You can extract courses to RDF inside SWI-Prolog by running:
%    :- extract_courses.
% Time stats @ Univeristy PC:
% 5,913,864,123 inferences, 230.375 CPU in 450.964 seconds (51% CPU, 25670598 Lips)
% Afterwards, you run the following to save the triples (using correct filename).
%    :- rdf_save_turtle('evdoxus-all-20230420.ttl',[]).
% Saved 3,887,410 triples about 585,984 subjects into 'evdoxus-all-20230510.ttl' (27.984 sec)
% in GraphDB you must CLEAR DEFAULT
% Imported successfully in 10m 49s.. (GRAPHDB)

extract_courses :-
	(exists_file('evdoxus-universities.ttl') ->
		true;
		extract_universities
	),
	extract_courses_u.

extract_universities :-
	write('Extracting and linking Universities...'), nl, nl,
	findall(UnivURI-University,university(UnivURI,University),UnivList),
	generate_universities(UnivList),
	rdf_save_turtle('evdoxus-universities.ttl',[]).

extract_courses_u :-
	rdf_load('evdoxus-universities.ttl',[register_namespaces(true)]),
	nl, write('Extracting Departments, Courses and Books...'), nl, nl,
	extract_departments_courses(DC_List),
	generate_departments_courses(DC_List),
	rdf_save_turtle('evdoxus-all.ttl',[]).


generate_universities(UnivList) :-
	length(UnivList,N),
	generate_universities_aux(UnivList,1,N).

generate_universities_aux([],_,_).
generate_universities_aux([UnivNo-UnivName|RestUniversities],X,N) :-
	%(UnivNo == 10 -> stop_here; true),
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,university_,UnivNo],UnivURI),
	rdf_assert(UnivURI, rdf:type, evdx:'University'),
	rdf_assert(UnivURI,evdx:'ID',UnivNo^^xsd:integer),
	rdf_assert(UnivURI, evdx:name, UnivName^^xsd:string),
	write("Searching links for University: "), write(UnivNo-UnivName), write(" ... "),
	search_university(UnivName,ELDBPediaURL,ENDBPediaURL), !,
	write("found!"), nl,
	(ENDBPediaURL \= null -> rdf_assert(UnivURI, owl:sameAs, ENDBPediaURL); true),
	(ELDBPediaURL \= null -> rdf_assert(UnivURI, owl:sameAs, ELDBPediaURL); true),
	X1 is X + 1,
	generate_universities_aux(RestUniversities,X1,N).

stop_here.

extract_departments_courses(DC_List) :-
	findall(UnivID-DeptName-DeptCode-SchoolName-DeptCourses,
		(   university(UnivID,_UnivName),
			department(_DeptID,DeptCode,DeptName,SchoolName,UnivID),
		    extract_courses(DeptCode,DeptCourses)
		),
		DC_List).

extract_courses(DeptCode,DeptCourses) :-
	findall(CourseID-CourseCode-CourseTitle-Semester-Professors-Year-CourseBooks,
		(   course(CourseID,CourseCode,CourseTitle,Semester,Professors,DeptCode,Year),
			findall(BookID,course_book(BookID,CourseID),CourseBooks)
		),
		DeptCourses).


%generate_departments_courses(DC_List).
generate_departments_courses([]).
generate_departments_courses([UnivID-DeptName-DeptCode-SchoolName-DeptCourses|RestDC_List]) :-
	discover_univ_uri(UnivID,UnivURI),
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,dept_,DeptCode],DeptURI),
	rdf_assert(UnivURI, evdx:hasDepartment, DeptURI),
	rdf_assert(DeptURI, rdf:type, evdx:'Department'),
	rdf_assert(DeptURI, evdx:name, DeptName^^xsd:string),
	rdf_assert(DeptURI, evdx:'ID', DeptCode^^xsd:integer), 
	write("Department: "), writeln(DeptCode-DeptName),
	my_rdf_assert(DeptURI, evdx:hasSchool, SchoolName^^xsd:string),
	generate_courses(DeptURI,DeptCode,DeptCourses),
	generate_departments_courses(RestDC_List).

%generate_courses(DeptURI,DeptCode,DeptCourses),
generate_courses(_DeptURI,_DeptCode,[]).
generate_courses(DeptURI,DeptCode,[CourseID-CourseCode-CourseTitle-Semester-Professors-Year-CourseBooks|RestDeptCourses]) :-
	%split_string(CourseURL,"/","",L),
	%last(L,CourseYear),
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,course_,CourseID],CourseURI),
	rdf_assert(DeptURI, evdx:hasCourse, CourseURI),
	rdf_assert(CourseURI, rdf:type, evdx:'Course'),
	rdf_assert(CourseURI, rdf:'ID', CourseID^^xsd:integer),
	my_rdf_assert(CourseURI, evdx:title, CourseTitle^^xsd:string),
	rdf_assert(CourseURI, evdx:year, Year^^xsd:integer),
	my_rdf_assert(CourseURI, evdx:semester, Semester^^xsd:integer),
	my_rdf_assert(CourseURI, evdx:hasCode, CourseCode^^xsd:string),
	my_rdf_assert(CourseURI, evdx:professors, Professors^^xsd:string),
	atomic_list_concat(['https://service.eudoxus.gr/coursebooks/rest/courses-books/course/',CourseID,'/books'],CourseURL),
	rdf_assert(CourseURI, evdx:hasURL, CourseURL^^xsd:anyURI),
	write("            "), writeln(CourseID-CourseTitle),
	generate_books(CourseURI,CourseBooks), 
	generate_courses(DeptURI,DeptCode,RestDeptCourses).

%generate_books(CourseURI,CourseBooks),
generate_books(_CourseURI,[]) :- nl.
generate_books(CourseURI,[BookID|RestCourseBooks]) :-
	find_or_create_book(BookID,BookURI), !,
	rdf_assert(CourseURI, evdx:hasBook, BookURI),
	write("                        "), write(BookID), write(" "),
	generate_books(CourseURI,RestCourseBooks).

find_or_create_book(BookID,BookURI) :-
	rdf(BookURI,evdx:'ID',BookID^^xsd:integer), 
	rdf(BookURI,rdf:type,evdx:'Book'), !.
find_or_create_book(BookID,BookURI) :-
	% book(94700120,"Published","ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ - 4η ΕΚΔΟΣΗ","ΒΛΑΧΑΒΑΣ Ι./ΚΕΦΑΛΑΣ Π. / ΒΑΣΙΛΕΙΑΔΗΣ Ν. / ΚΟΚΚΟΡΑΣ Φ./ ΣΑΚΕΛΛΑΡΙΟΥ Η.","9786185196448","4","ΕΤΑΙΡΕΙΑ ΑΞΙΟΠΟΙΗΣΗΣ ΚΑΙ ΔΙΑΧΕΙΡΙΣΗΣ ΠΕΡΙΟΥΣΙΑΣ ΠΑΝΕΠΙΣΤΗΜΙΟΥ ΜΑΚΕΔΟΝΙΑΣ","1441","ΕΤΑΙΡΙΑ ΑΞΙΟΠΟΙΗΣΗΣ ΚΑΙ ΔΙΑΧΕΙΡΙΣΗΣ ΠΕΡΙΟΥΣΙΑΣ ΤΟΥ ΠΑΝΕΠΙΣΤΗΜΙΟΥ ΜΑΚΕΔΟΝΙΑΣ ΟΙΚΟΝΟΜΙΚΩΝ ΚΑΙ ΚΟΙΝΩΝΙΚΩΝ ΕΠΙΣΤΗΜΩΝ",2020,["εφαρμογες στην πληροφορική","εφαρμογες υπολογιστων","πληροφοριακά συστήματα","πληροφορικά συστήματα","πληροφορικη","τεχνητη νοημοσυνη","υπολογιστές","υπολογιστική νοημοσύνη"],1000,"Soft","[17 x 24]","https://static.eudoxus.gr/books/20/cover-94700120.jpg","https://static.eudoxus.gr/books/20/backcover-94700120.jpg","https://static.eudoxus.gr/books/20/toc-94700120.pdf","https://static.eudoxus.gr/books/20/chapter-94700120.pdf","https://static.eudoxus.gr/books/http://www.uompress.gr ","https://service.eudoxus.gr/coursebooks/rest/courses-books/book/eudoxus/info?bookId=94700120").
	book(BookID,BookType,Title,Authors,ISBN,Edition,Distributor,PublisherID,Publisher,Year,Keywords,Pages,CoverType,BookSize,FrontCover,Backcover,Contents,Excerpt,PublisherWebPage,BookURL),
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,book_,BookID],BookURI),
	rdf_assert(BookURI, rdf:type, evdx:'Book'),
	rdf_assert(BookURI, evdx:'ID', BookID^^xsd:integer),
	my_rdf_assert(BookURI, evdx:authors, Authors^^xsd:string),
	my_rdf_assert(BookURI, evdx:backCover, Backcover^^xsd:anyURI),
	my_rdf_assert(BookURI, evdx:bookSize, BookSize^^xsd:string),
	my_rdf_assert(BookURI, evdx:bookType, BookType^^xsd:string),
	my_rdf_assert(BookURI, evdx:contents, Contents^^xsd:anyURI),
	my_rdf_assert(BookURI, evdx:coverType, CoverType^^xsd:string),
	my_rdf_assert(BookURI, evdx:distributor, Distributor^^xsd:string),
	my_rdf_assert(BookURI, evdx:edition, Edition^^xsd:string),
	my_rdf_assert(BookURI, evdx:excerpt, Excerpt^^xsd:anyURI),
	my_rdf_assert(BookURI, evdx:frontCover, FrontCover^^xsd:anyURI),
	my_rdf_assert(BookURI, evdx:isbn, ISBN^^xsd:string),
	my_rdf_assert(BookURI, evdx:pages, Pages^^xsd:integer),
	my_rdf_assert(BookURI, evdx:publicationYear, Year^^xsd:integer),
	my_rdf_assert(BookURI, evdx:publisherWebPage, PublisherWebPage^^xsd:anyURI),
	my_rdf_assert(BookURI, evdx:publisher, Publisher^^xsd:string),
	my_rdf_assert(BookURI, evdx:publisherID, PublisherID^^xsd:string),
	multi_rdf_assert(BookURI, evdx:keyword, Keywords,xsd:string),
	my_rdf_assert(BookURI, evdx:title, Title^^xsd:string),
	rdf_assert(BookURI, evdx:hasCode, BookID^^xsd:integer),
	my_rdf_assert(BookURI, evdx:hasURL, BookURL^^xsd:anyURI), !.

%multi_rdf_assert(Subject,Predicate,ListofObjects)
multi_rdf_assert(_,_Predicate,[],_).
multi_rdf_assert(Subject,Predicate,[Object|RestofObjects],Datatype) :-
	my_rdf_assert(Subject,Predicate,Object^^Datatype),
	multi_rdf_assert(Subject,Predicate,RestofObjects,Datatype).

% Do not store empty strings or zero values
my_rdf_assert(Subject, Predicate, Data^^xsd:integer) :- !,
	((Data == 0 ; Data == null) ->
		true;
		rdf_assert(Subject, Predicate, Data^^xsd:integer)
	).
my_rdf_assert(Subject, Predicate, Data^^xsd:string) :-
	((Data == ""; Data == null; Data == "null"; Data == '')->
		true;
		rdf_assert(Subject, Predicate, Data^^xsd:string)
	).
my_rdf_assert(Subject, Predicate, Data^^xsd:anyURI) :-
	((Data == ""; Data == null; Data == "null"; Data == '')->
		true;
		rdf_assert(Subject, Predicate, Data^^xsd:anyURI)
	).

discover_univ_uri(UnivID,UnivURI) :-
	rdf(UnivURI, evdx:'ID', UnivID^^xsd:integer), 
	rdf(UnivURI,rdf:type,evdx:'University'), !.


test1(A,B) :- search_university("ΑΣΠΑΙΤΕ",A,B).

% //li[contains(@class,'interlanguage-link')]/a[@hreflang='en']/@href
search_university(UnivName,GRDBPediaURL,GBDBPediaURL) :-
	string_replace("&","ΚΑΙ",UnivName,UnivName1),
	atomic_list_concat(['https://el.wikipedia.org/w/index.php?fulltext=1&search=',UnivName1,'&ns0=1'],WikipediaURL),
	%uri_normalized(WikipediaURL1,WikipediaURL),
	%rnd_delay(5000,10000),
	catch(http_open(WikipediaURL,In,[]),
	      _,
	      (uri_normalized(WikipediaURL,WikipediaURL1),
	       http_open(WikipediaURL1,In,[]))),
	set_stream(In, encoding(utf8)),
	load_structure(In, HTML,  [ dialect(xml), max_errors(-1),syntax_errors(quiet) ]),
	close(In),
	xpath(HTML,//ul(@class='mw-search-results'),UL),
	xpath(UL,li(1),LI),
	xpath(LI,//div(1),DIV),
	xpath(DIV,a(@href),LocalWikiURL),
	xpath(DIV,a(@title),ResultTitle),
	((xpath(DIV,span(@class='searchalttitle'),Span),
	  xpath(Span,a(@class='mw-redirect',@title),AltTitle))
	-> true; AltTitle=""),
	global_url(LocalWikiURL,'http://el.wikipedia.org',WikiURL),
	wikipedia_to_dbpedia_url(el,WikiURL,ELDBPediaURL1),
	uri_iri(ELDBPediaURL1,ELDBPediaURL),
	(find_en_dbpedia(WikiURL,ELDBPediaURL,ENDBPediaURL) -> true; ENDBPediaURL = null),  % Try to find Enlish DBpedia by alternative means
	find_alternative_names(ENDBPediaURL,AltNames),
	( compare_names(UnivName1,[ResultTitle,AltTitle|AltNames]) ->
		(GRDBPediaURL=ELDBPediaURL, GBDBPediaURL = ENDBPediaURL);
		(GRDBPediaURL = null, GBDBPediaURL = null)
	).

%find_alternative_names(_ENDBPediaURL,[]).
find_alternative_names(null,[]) :- !.
find_alternative_names(ENDBPediaURL,AltNames) :-
	atomic_list_concat(['select distinct str(?l) where { <',ENDBPediaURL,'> (rdfs:label|foaf:name|dbp:nativeName) ?l .}'],Sparql_Query),
	run_sparql_query('dbpedia.org',Sparql_Query,Results),
	findall(Str,member(row(literal(Str)),Results),AltNames).

/*
select distinct str(?l)
where {
{<http://dbpedia.org/resource/Panteion_University> rdfs:label ?l .}
UNION
{<http://dbpedia.org/resource/Panteion_University> foaf:name ?l .}
union
{ <http://dbpedia.org/resource/Panteion_University> dbp:nativeName ?l }
}

select distinct str(?l) where { <http://dbpedia.org/resource/Panteion_University> (rdfs:label|foaf:name|dbp:nativeName) ?l .}
*/

wikipedia_to_dbpedia_url(el,WikiURL,DBPediaURL) :- !,
	sub_atom(WikiURL,0,B,C,'http://el.wikipedia.org/wiki/'),
	sub_atom(WikiURL,B,C,0,D),
	atom_concat('http://el.dbpedia.org/resource/',D,DBPediaURL1),
	uri_normalized(DBPediaURL1,DBPediaURL).
wikipedia_to_dbpedia_url(en,WikiURL,DBPediaURL) :-
	sub_atom(WikiURL,0,B,C,'https://en.wikipedia.org/wiki/'),
	sub_atom(WikiURL,B,C,0,D),
	atom_concat('http://dbpedia.org/resource/',D,DBPediaURL1),
	uri_normalized(DBPediaURL1,DBPediaURL).
	
find_en_dbpedia(_WikipediaURL,ELDBPediaURL,ENDBPediaURL) :- 
	atomic_list_concat(['select ?u where { ?u rdf:type dbo:EducationalInstitution ; owl:sameAs <',ELDBPediaURL,'> . }'],Sparql_Query),
	run_sparql_query('dbpedia.org',Sparql_Query,Results),
	Results = [row(ENDBPediaURL)].
% //li[contains(@class,'interlanguage-link')]/a[@hreflang='en']/@href
%//a[@class="interlanguage-link-target" and @hreflang='en']/@href
find_en_dbpedia(WikipediaURL,_ELDBPediaURL,ENDBPediaURL) :- 
	catch(http_open(WikipediaURL,In,[]),
	      _,
	      (uri_normalized(WikipediaURL,WikipediaURL1),
	       http_open(WikipediaURL1,In,[]))),
	set_stream(In, encoding(utf8)),
	load_structure(In, HTML,  [ dialect(xml), max_errors(-1),syntax_errors(quiet) ]),
	close(In),
	xpath(HTML,//a(@class='interlanguage-link-target', @hreflang='en', @href),ENWikiURL),
	wikipedia_to_dbpedia_url(en,ENWikiURL,ENDBPediaURL).


string_replace(Str1,Str2,StringIn,StringOut) :-
	sub_string(StringIn, Before, Length, After, Str1), !,
	sub_string(StringIn, 0, Before, _, Prefix),
	P is Before + Length,
	sub_string(StringIn, P, After, 0, Suffix),
	atomics_to_string([Prefix,Str2,Suffix], StringOut).
string_replace(_Str1,_Str2,StringIn,StringIn).


run_sparql_query(Server,Sparql_Query,Results) :-
       rnd_delay(1000,5000),
	findall(Row,catch(sparql_query(Sparql_Query,Row,[ host(Server),path('/sparql'),search([entailment=rdfs,timeout=2000])]),
				  Error,
				  ( print_message(warning, Error),
				    assert(failed_query(sparql_query,Sparql_Query)),
				    fail
				  )
				 ),
		    Results).

rnd_delay(X,Y) :-
	random(X,Y,Dl),
	Dl1 is Dl * 50,
	delay(Dl1).

delay(0) :- !.
delay(N) :-
	N1 is N - 1,
	delay(N1).


%compare_names(UnivName1,[ResultTitle,AltTitle|AltNames])
compare_names(UnivName,UnivAltNames) :-
	score_names(UnivName,UnivAltNames,UnivAltScores),
	max_list(UnivAltScores,MaxScore),
	MaxScore > 0.75.

score_names(_UnivName,[],[]).
score_names(UnivName,[UnivAltName|RestAltNames],[Score|RestScores]) :-
	string_replace("ΤΕΙ ","Τεχνολογικό Εκπαιδευτικό Ίδρυμα ",UnivName,UnivName1),
	string_replace("ΤΕΙ ","Τεχνολογικό Εκπαιδευτικό Ίδρυμα ",UnivAltName,UnivAltName1),
	downcase_atom(UnivName1,UnivName2),
	downcase_atom(UnivAltName1,UnivAltName2),
	isub(UnivName2,UnivAltName2,Score,[zero_to_one(true)]),
	score_names(UnivName,RestAltNames,RestScores).


/* Checks 

find_null_values_book_args(List) :-
	setof(ArgName,find_empty_book_arg(ArgName),List).
	
find_empty_book_arg(ArgName) :-
	BookFact = book(BookID,BookType,Title,Authors,ISBN,Edition,Distributor,PublisherID,Publisher,Year,Keywords,Pages,CoverType,BookSize,FrontCover,Backcover,Contents,Excerpt,PublisherWebPage,BookURL),
	ArgsNames = ["BookID","BookType","Title","Authors","ISBN","Edition","Distributor","PublisherID","Publisher","Year","Keywords","Pages","CoverType","BookSize","FrontCover","Backcover","Contents","Excerpt","PublisherWebPage","BookURL"],
	call(BookFact),
	BookFact =.. [book|BookArgs],
	nth1(1,BookArgs,BookID),
	nth1(N,BookArgs,""),
	nth1(N,ArgsNames,ArgName),
	write("Book with ID: "), write(BookID), write(" has empty string in argument: "), write(ArgName), nl.

find_zero_values_book_args(List) :-
	setof(ArgName,find_zero_book_int(ArgName),List).	

find_zero_book_int(ArgName) :-
	BookFact = book(BookID,BookType,Title,Authors,ISBN,Edition,Distributor,PublisherID,Publisher,Year,Keywords,Pages,CoverType,BookSize,FrontCover,Backcover,Contents,Excerpt,PublisherWebPage,BookURL),
	ArgsNames = ["BookID","BookType","Title","Authors","ISBN","Edition","Distributor","PublisherID","Publisher","Year","Keywords","Pages","CoverType","BookSize","FrontCover","Backcover","Contents","Excerpt","PublisherWebPage","BookURL"],
	call(BookFact),
	BookFact =.. [book|BookArgs],
	nth1(1,BookArgs,BookID),
	nth1(N,BookArgs,0),
	nth1(N,ArgsNames,ArgName),
	write("Book with ID: "), write(BookID), write(" has zero value in argument: "), write(ArgName), nl.

find_null_values_course_args(List) :-
	setof(ArgName,find_empty_course_arg(ArgName),List).
	
find_empty_course_arg(ArgName) :-
	CourseFact = course(CourseID,CourseCode,CourseTitle,Semester,Professors,DeptCode,Year),
	ArgsNames = ["CourseID","CourseCode","CourseTitle","Semester","Professors","DeptCode","Year"],
	call(CourseFact),
	CourseFact =.. [course|CourseArgs],
	nth1(1,CourseArgs,CourseID),
	nth1(N,CourseArgs,""),
	nth1(N,ArgsNames,ArgName),
	write("Course with ID: "), write(CourseID), write(" has empty string in argument: "), write(ArgName), nl.

find_zero_values_course_args(List) :-
	setof(ArgName,find_zero_course_int(ArgName),List).	

find_zero_course_int(ArgName) :-
	CourseFact = course(CourseID,CourseCode,CourseTitle,Semester,Professors,DeptCode,Year),
	ArgsNames = ["CourseID","CourseCode","CourseTitle","Semester","Professors","DeptCode","Year"],
	call(CourseFact),
	CourseFact =.. [course|CourseArgs],
	nth1(1,CourseArgs,CourseID),
	nth1(N,CourseArgs,0),
	nth1(N,ArgsNames,ArgName),
	write("Course with ID: "), write(CourseID), write(" has empty string in argument: "), write(ArgName), nl.
	*/