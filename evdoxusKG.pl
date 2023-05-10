:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/sparql_client)).

:- [cache_courses].

:- dynamic extracted_info/2.

:- rdf_register_prefix(dbo,'http://dbpedia.org/ontology/'),
   rdf_register_prefix(dbr,'http://dbpedia.org/resource/'),
   rdf_register_prefix('dbpedia-el','http://el.dbpedia.org/resource/').

:- rdf_load('evdoxus.ttl',[register_namespaces(true)]).

:- set_prolog_flag(stack_limit, 4_294_967_296).

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
	write('Extracting and linking Universities...'), nl, nl,
	extract_universities(UnivList,HTML),
	generate_universities(UnivList),
	nl, write('Extracting Departments, Courses, Modules and Books...'), nl, nl,
	extract_departments_courses(HTML,DC_List),
	generate_departments_courses(DC_List).

% You can run extract_courses in two phases. First use extract_courses_c to do all the extraction from HTML pages (it takes a long time)
% Then you can use generate_courses_c to generate the RDF graph. This separation is usefull for debugging purposes on the second phase.
extract_courses_c :-
	write('Extracting Universities...'), nl, nl,
	extract_universities(UnivList,HTML),
	nl, write('Extracting Departments, Courses, Modules and Books...'), nl, nl,
	extract_departments_courses(HTML,DC_List),
	assert(extracted_info(UnivList,DC_List)).

generate_courses_c :-
	extracted_info(UnivList,DC_List),
	write('Linking Universities...'), nl, nl,
	generate_universities(UnivList),
	nl, write('Extracting Departments, Courses, Modules and Books...'), nl, nl,
	generate_departments_courses(DC_List).

extract_universities(UnivList,HTML) :-
	dtd(html, DTD),
	http_open('https://service.eudoxus.gr/public/departments',In,[]),
	load_structure(In, HTML, [ dtd(DTD), dialect(sgml), shorttag(false), max_errors(-1),syntax_errors(quiet),encoding('utf-8') ]),
	close(In),
	xpath(HTML,//body//table, Universities),
	findall(UnivURI-University,
		(xpath(Universities, tr/td/a, UnivEntry),
		 xpath(UnivEntry, /self(@href), UnivURI),
		 xpath(UnivEntry, /self(normalize_space), University)
		),
		UnivList).

generate_universities(UnivList) :-
	length(UnivList,N),
	generate_universities_aux(UnivList,1,N).

generate_universities_aux([],_,_).
generate_universities_aux([UnivNo-UnivName|RestUniversities],X,N) :-
	split_string(UnivNo,'#','#',[NS]),
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,university_,NS],UnivURI),
	rdf_assert(UnivURI, rdf:type, evdx:'University'),
	rdf_assert(UnivURI, evdx:name, UnivName^^xsd:string),
	search_university(UnivName,ELDBPediaURL,ENDBPediaURL),
	(ENDBPediaURL \= null -> rdf_assert(UnivURI, owl:sameAs, ENDBPediaURL); true),
	(ELDBPediaURL \= null -> rdf_assert(UnivURI, owl:sameAs, ELDBPediaURL); true),
	write(UnivName), nl,
	X1 is X + 1,
	generate_universities_aux(RestUniversities,X1,N).


extract_departments_courses(HTML,DC_List) :-
	findall(UnivName-DeptName-DeptCode-DeptCourses,
		(   xpath(HTML,//body//ul,Dept),
		    extract_courses(Dept,UnivName,DeptName,DeptCode,DeptCourses)),
		DC_List).

extract_courses(Dept,UnivName,DeptName,DeptCode,DeptCourses) :-
	findall(CourseURL-CourseTitle-CourseModulesBooks,
		(   xpath(Dept,li,Course),
		    xpath(Course,a(@href),CourseURL1),
		    atom_concat('https://service.eudoxus.gr',CourseURL1,CourseURL),
		    xpath(Course,a(text),CourseTitle),
		    extract_modules(CourseURL,CourseModulesBooks)),
		DeptCourses),
	DeptCourses = [CourseURL-_-_|_],
	sub_string(CourseURL,54,_L,0,LastPart),
	split_string(LastPart,"/","",List),
	List=[DeptCode|_],
	atomics_to_string(List,'_',StringLastPart),
	string_concat(StringLastPart,".html",LocalCourseFile),
	string_concat("cache/",LocalCourseFile,LocalCourseFilePath),
	dtd(html, DTD),
	open(LocalCourseFilePath,read,In,[encoding(utf8)]),
	load_structure(In, HTML, [ dtd(DTD), dialect(sgml), shorttag(false), max_errors(-1),syntax_errors(quiet),encoding('utf-8') ]),
	close(In),
	xpath(HTML,//body//h2(1,text),UnivName),
	xpath(HTML,//body//h2(2,text),DeptName), !,
	write(UnivName), write(' - '), write(DeptName), nl.



%generate_departments_courses(DC_List).
generate_departments_courses([]).
generate_departments_courses([UnivName-DeptName-DeptCode-DeptCourses|RestDC_List]) :-
	discover_univ_uri(UnivName,UnivURI),
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,dept_,DeptCode],DeptURI),
	rdf_assert(UnivURI, evdx:hasDepartment, DeptURI),
	rdf_assert(DeptURI, rdf:type, evdx:'Department'),
	rdf_assert(DeptURI, evdx:name, DeptName^^xsd:string),
	rdf_assert(DeptURI, evdx:hasCode, DeptCode^^xsd:string),
	generate_courses(DeptURI,DeptCode,DeptCourses),
	generate_departments_courses(RestDC_List).

%generate_courses(DeptURI,DeptCode,DeptCourses),
generate_courses(_DeptURI,_DeptCode,[]).
generate_courses(DeptURI,DeptCode,[CourseURL-CourseTitle-CourseModulesBooks|RestDeptCourses]) :-
	split_string(CourseURL,"/","",L),
	last(L,CourseYear),
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,course_,DeptCode,'_',CourseYear],CourseURI),
	rdf_assert(DeptURI, evdx:hasCourse, CourseURI),
	rdf_assert(CourseURI, rdf:type, evdx:'Course'),
	rdf_assert(CourseURI, evdx:title, CourseTitle^^xsd:string),
	rdf_assert(CourseURI, evdx:year, CourseYear^^xsd:integer),
	rdf_assert(CourseURI, evdx:hasURL, CourseURL^^xsd:anyURI),
	generate_modules(CourseURI, DeptCode, CourseYear, CourseModulesBooks),
	generate_courses(DeptURI,DeptCode,RestDeptCourses).

generate_modules(CourseURI, DeptCode, CourseYear, CourseModulesBooks) :-
	generate_modules_aux(1, CourseURI, DeptCode, CourseYear, CourseModulesBooks).


%generate_modules(X, CourseURI, DeptCode, CourseYear, CourseModulesBooks),
generate_modules_aux(_, _CourseURI, _DeptCode, _CourseYear, []).
generate_modules_aux(X, CourseURI, DeptCode, CourseYear, [ModuleName-ModuleCode-Semester-ModuleBooks|RestCourseModulesBooks]) :-
	find_or_create_module(X,CourseURI,DeptCode,CourseYear,ModuleName,ModuleCode,ModuleURI),
	rdf_assert(ModuleURI, evdx:semester, Semester^^xsd:integer),
	generate_books(ModuleURI,ModuleBooks),   % CHECK HERE
	X1 is X + 1,
	generate_modules_aux(X1, CourseURI, DeptCode, CourseYear, RestCourseModulesBooks).

find_or_create_module(_X,CourseURI,_DeptCode,_CourseYear,ModuleName,ModuleCode,ModuleURI) :-
	rdf(ModuleURI,evdx:hasCode,ModuleCode^^xsd:string), 
	rdf(ModuleURI,evdx:title,ModuleName^^xsd:string),
	rdf(CourseURI,evdx:hasModule,ModuleURI), !.
find_or_create_module(X,CourseURI,DeptCode,CourseYear,ModuleName,ModuleCode,ModuleURI) :-
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,module_,DeptCode,'_',CourseYear,'_',X],ModuleURI),
	rdf_assert(CourseURI, evdx:hasModule, ModuleURI),
	rdf_assert(ModuleURI, rdf:type, evdx:'Module'),
	rdf_assert(ModuleURI, evdx:title, ModuleName^^xsd:string),
	rdf_assert(ModuleURI, evdx:hasCode, ModuleCode^^xsd:string).

%generate_books(ModuleURI,ModuleBooks),
generate_books(_ModuleURI,[]).
generate_books(ModuleURI,[BookName-BookCode-BookURL|RestModuleBooks]) :-
	find_or_create_book(BookName,BookCode,BookURL,BookURI),
	rdf_assert(ModuleURI, evdx:hasBook, BookURI),
	generate_books(ModuleURI,RestModuleBooks).

find_or_create_book(_BookName,BookCode,_BookURL,BookURI) :-
	rdf(BookURI,evdx:hasCode,BookCode^^xsd:string), 
	rdf(BookURI,rdf:type,evdx:'Book'), !.
find_or_create_book(BookName,BookCode,BookURL,BookURI) :-
	rdf_current_prefix(evdx,EvdxURI),
	atomic_list_concat([EvdxURI,book_,BookCode],BookURI),
	rdf_assert(BookURI, rdf:type, evdx:'Book'),
	rdf_assert(BookURI, evdx:title, BookName^^xsd:string),
	rdf_assert(BookURI, evdx:hasCode, BookCode^^xsd:string),
	rdf_assert(BookURI, evdx:hasURL, BookURL^^xsd:anyURI).


writelist([]).
writelist([H|T]) :-
	write(H), nl,
	writelist(T).

discover_univ_uri(UnivName,UnivURI) :-
	rdf(UnivURI, evdx:name, UnivName^^xsd:string), !.
discover_univ_uri(UnivName,UnivURI) :-
	rdf(UnivURI, evdx:name, UnivName1^^xsd:string),
	isub(UnivName,UnivName1,Score,[zero_to_one(true)]),
	Score > 0.9, !.

extract_modules(CourseURL,CourseModulesBooks) :-
	sub_string(CourseURL,54,_L,0,LastPart),
	split_string(LastPart,"/","",List),
	atomics_to_string(List,'_',StringLastPart),
	string_concat(StringLastPart,".html",LocalCourseFile),
	string_concat("cache/",LocalCourseFile,LocalCourseFilePath),
	dtd(html, DTD),
	open(LocalCourseFilePath,read,In,[encoding(utf8)]),
	load_structure(In, HTML, [ dtd(DTD), dialect(sgml), shorttag(false), max_errors(-1),syntax_errors(quiet),encoding('utf-8') ]),
	close(In),
	xpath(HTML,//body//div(@id='main'),Main), !,
	findall(ModuleName-ModuleCode-Semester-ModuleBooks,
		(xpath(Main,h2(index(Ind),normalize_space),FullModuleName),
		 get_module_name(FullModuleName,ModuleName,ModuleCode),
		 xpath(Main,h3(index(Ind),text(string)),FullSemester),
		 split_string(FullSemester," ","",[_,Semester|_]),
		 xpath(Main,ol(index(Ind)),BookList),
		 findall(BookName-BookCode-BookURL,
			 (xpath(BookList,li/ul/li,Book),
			  xpath(Book,/self(normalize_space),FullBookName),
			  get_book_name(FullBookName,BookName,BookCode),
			  xpath(Book,/self/a(@href),BookURL)),
			ModuleBooks)),
		CourseModulesBooks).

get_module_name(FullModuleName,ModuleName,ModuleCode) :-
	sub_string(FullModuleName,B,_L,A,":"),
	B1 is B+2,
	A1 is A - 1,
	A1 >= 0,
	sub_string(FullModuleName,B1,A1,0,ModuleName),
	A2 is A + 2,
	sub_string(FullModuleName,8,_,A2,ModuleCode), !.

get_book_name(FullBookName,BookName,BookCode) :-
	sub_string(FullBookName,B,_L,A,":"),
	B1 is B+2,
	sub_string(FullBookName,B1,_,13,BookName),
	A2 is A + 2,
	sub_string(FullBookName,8,_,A2,BookCode), !.


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
	%<span class='searchalttitle'>(ανακατεύθυνση από <a href="/wiki/%CE%A4%CE%95%CE%99_%CE%A7%CE%B1%CE%BB%CE%BA%CE%AF%CE%B4%CE%B1%CF%82" class="mw-redirect" title="ΤΕΙ Χαλκίδας">
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

	
uri_replace(URIIn,LangIN,LangOUT,URIOut) :-
	split_string(URIIn,"./","./",ListIn),
	list_replace(LangIN,LangOUT,ListIn,ListOut),
	atomics_to_string(ListOut,URIOutS),
	atom_string(URIOut, URIOutS).

list_replace(_X,_Y,[],[]).
list_replace(X,Y,[X|T],[Y|T1]) :- !,
	list_replace(X,Y,T,T1).
list_replace(X,Y,[H|T],[H|T1]) :-
	list_replace(X,Y,T,T1).

string_replace(Str1,Str2,StringIn,StringOut) :-
	sub_string(StringIn, Before, Length, After, Str1), !,
	sub_string(StringIn, 0, Before, _, Prefix),
	P is Before + Length,
	sub_string(StringIn, P, After, 0, Suffix),
	atomics_to_string([Prefix,Str2,Suffix], StringOut).
string_replace(_Str1,_Str2,StringIn,StringIn).


/*
select ?s where {
?s rdf:type dbo:University .
?s owl:sameAs <http://el.dbpedia.org/resource/Γεωπονικό_Πανεπιστήμιο_Αθηνών>.
}

}
*/

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


