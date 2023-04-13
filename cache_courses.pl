:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- dynamic course/4, cached_course/4.


init_cache :-
	( exists_file('courses.pl') ->
		copy_file('courses.pl','courses-old.pl');
		true),
	( exists_file('cached_courses.pl') ->
		copy_file('cached_courses.pl','cached_courses-old.pl');
		true),
	extract_course_list(List),
	transform_univ_data(List,A),
	flatten(A,Courses),
	convert_to_courses(Courses),
	abolish(course/4),
	consult('courses.pl'),
	cache_courses,
	abolish(cached_course/4),
	consult('cached_courses.pl').

update_cache :-
	( exists_file('courses.pl') ->
		copy_file('courses.pl','courses-old.pl');
		true),
	( exists_file('cached_courses.pl') ->
		copy_file('cached_courses.pl','cached_courses-old.pl');
		true),
	extract_course_list(List),
	transform_univ_data(List,A),
	flatten(A,Courses),
	convert_to_courses(Courses),
	abolish(course/4),
	consult('courses.pl'),
	inc_cache_courses,
	abolish(cached_course/4),
	consult('cached_courses.pl').
	
extract_course_list(List) :-
	dtd(html, DTD),
	http_open('https://service.eudoxus.gr/public/departments',In,[]),
	load_structure(In, HTML, [ dtd(DTD), dialect(sgml), shorttag(false), max_errors(-1),syntax_errors(quiet),encoding('utf-8') ]),
	close(In),
	findall(GlobInd-ElName-Content,
		(   xpath(HTML,//body/div/div/'*'(index(GlobInd)),X),
		    GlobInd >= 5,
		    extract_content(X,ElName,Content)
		),
		List).

extract_content(element(h2,[],SubTree),h2,UnivName) :- !,
	xpath(element(h2,[],SubTree),/self(normalize_space),UnivName).
extract_content(element(p,[],SubTree),p,DeptName) :- !,
	xpath(element(p,[],SubTree),/self(normalize_space),DeptName).
extract_content(element(ul,[],SubTree),ul,DeptCourses) :- !,
	extract_courses(element(ul,[],SubTree),DeptCourses).


transform_univ_data(List,[UList|ARest]) :-
	append([_N-h2-U|UData],[N1-h2-U1|L3],List), !,
	transform_univ_data_aux(U,UData,UList),
	transform_univ_data([N1-h2-U1|L3],ARest).
transform_univ_data(List,[UList]) :-
	append([_N-h2-U|UData],[],List), !,
	transform_univ_data_aux(U,UData,UList).
transform_univ_data([],[]).

transform_univ_data_aux(_U,[],[]) :- !.
transform_univ_data_aux(U,[_N-p-DeptName,_M-ul-DeptCourses|RestUData],[U-DeptName-DeptCourses|RestUList]) :-
	transform_univ_data_aux(U,RestUData,RestUList).
	
convert_to_courses(List) :-
	tell('courses.pl'),
	convert_to_courses_aux(List),
	told.

convert_to_courses_aux([]).
convert_to_courses_aux([Univ-DeptName-DeptCourses|Rest]) :-
	convert_to_courses_aux2(Univ,DeptName,DeptCourses),
	convert_to_courses_aux(Rest).

convert_to_courses_aux2(_Univ,_DeptName,[]).
convert_to_courses_aux2(Univ,DeptName,[CourseURL-CourseTitle|Rest]) :-
	Fact =.. [course,Univ,DeptName,CourseURL,CourseTitle],
	write_term(Fact,[quoted(true)]), write('.'), nl,
	convert_to_courses_aux2(Univ,DeptName,Rest).

extract_courses(Dept,DeptCourses) :-
	findall(CourseURL-CourseTitle,
		(   xpath(Dept,li,Course),
		    xpath(Course,a(@href),CourseURL1),
		    atom_concat('https://service.eudoxus.gr',CourseURL1,CourseURL),
		    xpath(Course,a(text),CourseTitle)
		),
		DeptCourses).

cache_one_course(CourseURL,LocalCourseFile) :-
%	'https://service.eudoxus.gr/public/departments/courses/2381/2010'
	sub_string(CourseURL,54,_L,0,LastPart),
	split_string(LastPart,"/","",List),
	atomics_to_string(List,'_',StringLastPart),
	string_concat(StringLastPart,".html",LocalCourseFile),
	string_concat("cache/",LocalCourseFile,LocalCourseFilePath),
	http_open(CourseURL,In,[]),
	open(LocalCourseFilePath,write,Out,[encoding(iso_latin_1)]),
	copy_stream_data(In, Out),
	close(In),
	close(Out), !.

inc_cache_one_course(CourseURL,LocalCourseFile,Cached) :-
	sub_string(CourseURL,54,_L,0,LastPart),
	split_string(LastPart,"/","",List),
	atomics_to_string(List,'_',StringLastPart),
	string_concat(StringLastPart,".html",LocalCourseFile),
	string_concat("cache/",LocalCourseFile,LocalCourseFilePath),
	(exists_file(LocalCourseFilePath)
	  ->
		Cached=false ;
		( http_open(CourseURL,In,[]),
		  open(LocalCourseFilePath,write,Out,[encoding(iso_latin_1)]),
		  copy_stream_data(In, Out),
		  close(In),
		  close(Out),
		  Cached=true
		 )
	),
	!.

cache_courses :-
	(exists_directory(cache) ->
		true;
		make_directory(cache)),
	tell('cached_courses.pl'),
	cache_courses_aux,
	told.

cache_courses_aux :-
	course(University,Department,CourseURL,CourseName),
	write(user,"Caching: "),
	write(user,University-Department-CourseName),
	nl(user),
	cache_one_course(CourseURL,LocalCourseFile),
	write_term(cached_course(University,Department,LocalCourseFile,CourseName),[quoted(true)]),
	write('.'), nl,
	fail; true.

inc_cache_courses :-
	tell('cached_courses.pl'),
	inc_cache_courses_aux,
	told.

inc_cache_courses_aux :-
	course(University,Department,CourseURL,CourseName),
	inc_cache_one_course(CourseURL,LocalCourseFile,Cached),
	(Cached == true ->
		(write(user,"Cached: "),
		 write(user,University-Department-CourseName),
		 nl(user));
		true),
	write_term(cached_course(University,Department,LocalCourseFile,CourseName),[quoted(true)]),
	write('.'), nl,
	fail; true.
	