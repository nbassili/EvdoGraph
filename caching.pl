:- set_prolog_flag(encoding,utf8).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(json)).

:- dynamic university/2.
:- dynamic department/5.
:- dynamic course/7.
:- dynamic book/20.
:- dynamic course_book/2.
:- dynamic active_year/1.
:- dynamic year/1.


init_cache:-
    cache_years,
    cache_universities,
    cache_departments,
    cache_courses,
    cache_books,
    check_no_books("no-books.txt").

update_cache:-
    ensure_loaded('./cache/years.pl'),
    get_years(_Years, ActiveYear),
    active_year(PrevActiveYear),
    (ActiveYear == PrevActiveYear ->
        UpdatedYears=[ActiveYear];
        (UpdatedYears=[PrevActiveYear,ActiveYear], retractall(year(_)), retractall(active_year(_)), cache_years)
    ),
    update_cache_courses(UpdatedYears,UpdatedCourses),
    update_cache_books(ActiveYear,UpdatedCourses),
    atomic_list_concat(['no-books-', ActiveYear,',txt'], NoBooksFile),
    check_no_books(NoBooksFile), !.


cache_years:-
    get_years(Years, ActiveYear),
    open('./cache/years.pl', write, Stream),
    cache_facts(Stream, year, Years),
    nl(Stream),
    write(Stream, active_year(ActiveYear)),
    writeln(Stream, '.'),
    close(Stream).

get_years(Years, ActiveYear) :-
    get_json_from_url("https://service.eudoxus.gr/coursebooks/rest/courses-books/academic-years", JSON),
    process_academic_years(JSON, Years),
    get_dict(activeYear, JSON, ActiveYear).

process_academic_years(JSON, Years) :-
    get_dict(yearsList, JSON, YearsJson),
    findall(Year, (member(YearDict, YearsJson), get_dict(year, YearDict, Year)), Years).

get_universities(Universities) :-
    get_json_from_url("https://service.eudoxus.gr/coursebooks/rest/courses-books/secretariat-academics", JSON),
    get_dict(institutions, JSON, UniversitiesJson),
    setof(ID-University, UniversityDict^(member(UniversityDict, UniversitiesJson), get_dict(id, UniversityDict, ID), get_dict(name, UniversityDict, University)), Universities).

cache_universities:-
    get_universities(Universities),
    open('./cache/universities.pl', write, Stream),
    cache_facts(Stream, university, Universities),
    close(Stream).

cache_departments:-
    get_departments(Departments),
    open('./cache/departments.pl', write, Stream),
    cache_facts(Stream, department, Departments),
    close(Stream).

cache_courses:-
    ensure_loaded('./cache/departments.pl'),
    ensure_loaded('./cache/universities.pl'),
    findall(DepartmentID, department(_,DepartmentID,_,_,_), Departments),
    get_years(Years, _ActiveYear),
    cache_courses_per_department(Departments, Years),
    save_facts(course/7, './cache/courses.pl').

update_cache_courses([Year1],RetractedCourses):-
    ensure_loaded('./cache/departments.pl'),
    ensure_loaded('./cache/universities.pl'),
    ensure_loaded('./cache/courses.pl'),
    findall(DepartmentID, department(_,DepartmentID,_,_,_), Departments),
    retract_courses_for_years([Year1],RetractedCourses),
    cache_courses_per_department(Departments, [Year1]),
    save_facts(course/7, './cache/courses.pl').
update_cache_courses([Year1,Year2],UpdatedCourses):-
    ensure_loaded('./cache/departments.pl'),
    ensure_loaded('./cache/universities.pl'),
    ensure_loaded('./cache/courses.pl'),
    findall(DepartmentID, department(_,DepartmentID,_,_,_), Departments),
    retract_courses_for_years([Year1,Year2],RetractedCourses),
    cache_courses_per_department(Departments, [Year1,Year2]),
    findall(CourseID, course(CourseID,_,_,_,_,_,Year2), InsertedCourses),
    append(RetractedCourses, InsertedCourses, UpdatedCourses),
    save_facts(course/7, './cache/courses.pl').

update_cache_books(ActiveYear, UpdatedCourses):-
    ensure_loaded('./cache/books.pl'),
    ensure_loaded('./cache/books_per_course.pl'),
    retract_books_for_courses(UpdatedCourses),
    atomic_list_concat(['no-books-', ActiveYear,',txt'], NoBooksFile),
    open(NoBooksFile, write, Stream),
    cache_books_per_course(UpdatedCourses,Stream,1),
    close(Stream),
    save_facts(book/20, './cache/books.pl'),
    save_facts(course_book/2, './cache/books_per_course.pl').

retract_books_for_courses([]).
retract_books_for_courses([CourseID|Rest]) :-
    retractall(course_book(_,CourseID)),
    retract_books_for_courses(Rest).

retract_courses_for_years([],[]).
retract_courses_for_years([Year|Rest], RetractedCourses) :-
    findall(CourseID, course(CourseID,_,_,_,_,_,Year), CourseIDs),
    retract_courses_for_years(Rest, RestRetractedCourses),
    append(CourseIDs, RestRetractedCourses, RetractedCourses),
    retractall(course(_,_,_,_,_,_,Year)).


cache_courses_per_department([],_).
cache_courses_per_department([DepartmentID|RestDepartments], Years) :-
    department(_, DepartmentID, DepartmentName, _, UniversityID),
    university(UniversityID, UniversityName),
    write("Fetching courses for department "), write(DepartmentName), write(" at "), write(UniversityName), write(" for years: "),
    cache_courses_for_department(DepartmentID, Years),
    cache_courses_per_department(RestDepartments, Years).

cache_courses_for_department(_, []) :- nl.
cache_courses_for_department(DepartmentID, [Year|Rest]) :-
    write(Year), write(" "),
    get_courses(DepartmentID, Year, Courses),
    assert_facts(course,Courses,[DepartmentID,Year]),
    cache_courses_for_department(DepartmentID, Rest).

cache_books:-
    ensure_loaded('./cache/courses.pl'),
    findall(CourseID, course(CourseID,_,_,_,_,_,_), Courses),
    open('no-books.txt', write, Stream),
    cache_books_per_course(Courses,Stream,1),
    close(Stream),
    save_facts(book/20, './cache/books.pl'),
    save_facts(course_book/2, './cache/books_per_course.pl').


% Auxiliari predicate that is used when the courses.pl file is split
% into multiple files (because it takes too much time to run it at once
% for all courses!)
%
cache_books(Part):-
    atomic_list_concat(['./cache/courses-', Part, '.pl'], File1),
    ensure_loaded(File1),
    ensure_loaded('./cache/books.pl'),
    ensure_loaded('./cache/books_per_course.pl'),
    atomic_list_concat(['no-books-', Part, '.txt'], File2),
    findall(CourseID, course(CourseID,_,_,_,_,_,_), Courses),
    open(File2, write, Stream),
    cache_books_per_course(Courses,Stream,1),
    close(Stream),
    save_facts(book/20, './cache/books.pl'),
    save_facts(course_book/2, './cache/books_per_course.pl').

cache_books_per_course([], _, _).
cache_books_per_course([CourseID|RestCourses], Stream, Count) :-
    write(Count), write(": "),
    write("Fetching books for course "), write(CourseID), write(": "),
    get_books_of_course(CourseID, Books),
    (Books == [] -> (write(Stream,CourseID), nl(Stream)); true),
    assert_facts(book, Books, []),
    findall(BookID, (member(Book, Books), dashes_to_list(Book,[BookID|_]), write(BookID), write(" ")), BookIDs),
    nl,
    assert_facts(course_book, BookIDs, [CourseID]),
    Count1 is Count + 1,
    cache_books_per_course(RestCourses, Stream, Count1).


assert_facts(_, [], _).
assert_facts(Predicate, [Head|Rest], ExtraArgs) :-
    dashes_to_list(Head, Args),
    append(Args, ExtraArgs, AllArgs),
    Fact=..[Predicate|AllArgs],
    assertif(Fact),
    assert_facts( Predicate, Rest, ExtraArgs).

assertif(Fact) :-
    Fact, !.
assertif(Fact) :-
    assert(Fact).

cache_facts(_, _, []).
cache_facts(Stream, Predicate, [Head|Rest]) :-
    dashes_to_list(Head, Args),
    Fact=..[Predicate|Args],
    writeq(Stream, Fact),
    write(Stream, '.\n'),
    cache_facts(Stream, Predicate, Rest).

save_facts(Predicate/Arity,File) :-
    functor(Fact, Predicate, Arity),
    open(File, write, Stream),
    save_facts_aux(Stream, Fact),
    close(Stream).

save_facts_aux(Stream, Fact) :-
    call(Fact),
    writeq(Stream, Fact),
    write(Stream, '.\n'),
    fail.
save_facts_aux(_, _).

dashes_to_list(Dashes, List) :-
    dashes_to_list_aux(Dashes, [], List).

dashes_to_list_aux(A, List, [A|List]) :-
    \+ compound(A),
    !.
dashes_to_list_aux(A-B, Acc, List) :-
    dashes_to_list_aux(A, [B|Acc], List).


get_departments(Departments) :-
    get_json_from_url("https://service.eudoxus.gr/coursebooks/rest/courses-books/secretariat-academics", JSON),
    get_dict(institutionAcademics, JSON, UniversitiesDepartmentsJson),
    get_dict(academicSecretariats, JSON, UniversitiesDepartmentsSecretariatsJson),
    dict_keys(UniversitiesDepartmentsJson, UniversityIDs),
    setof(ID-SID-Department-School-UniversityID,
          UniversityDepartmentsJson^StrID^DepartmentSecretariatJson^DepartmentDict^AUniversityID^(
            member(AUniversityID, UniversityIDs),
            get_dict(AUniversityID, UniversitiesDepartmentsJson, UniversityDepartmentsJson),
            atom_number(AUniversityID, UniversityID),
            member(DepartmentDict, UniversityDepartmentsJson),
            get_dict(id, DepartmentDict, ID),
            atom_number(StrID, ID),
            get_dict(StrID, UniversitiesDepartmentsSecretariatsJson, DepartmentSecretariatJson),
            get_dict(id, DepartmentSecretariatJson, SID),
            get_dict(department, DepartmentDict, Department),
            get_dict(school, DepartmentDict, School)
            ),
        Departments).


get_courses(Department,Year,Courses) :-
    atomic_list_concat( ["https://service.eudoxus.gr/coursebooks/rest/courses-books/get-semesters-courses?secretariatId=", Department, "&year=", Year], URL),
    get_json_from_url(URL, JSON),
    dict_keys(JSON, Semesters),
   findall(CourseID-CourseCode-CourseName-CourseSemester-Professors,
        (
            member(Semester, Semesters),
            get_dict(Semester, JSON, SemesterJson),
            member(CourseDict, SemesterJson),
            get_dict(id, CourseDict, CourseID),
            get_dict(title, CourseDict, CourseName),
            get_dict(code, CourseDict, CourseCode),
            get_dict(semester, CourseDict, CourseSemester),
            get_dict(professor, CourseDict, Professors)
            ),
        Courses).



get_books_of_course(CourseID, Books) :-
    atomic_list_concat( ["https://service.eudoxus.gr/coursebooks/rest/courses-books/course/", CourseID, "/books"], URL),
    get_json_from_url(URL, JSON),
    get_dict(bookgroups, JSON, Books1),
    findall(BookID-BookType-Title-Authors-ISBN-Edition-Distributor-PublisherID-Publisher-Year-Keywords-Pages-CoverType-BookSize-FrontCover-Backcover-Contents-Excerpt-PublisherWebPage-BookURL,
        (member(BookGroup, Books1),
         get_dict(books, BookGroup, BooksInGroup),
         member(BookInGroup, BooksInGroup),
         get_dict(book,BookInGroup,Book),
         get_dict(id, Book, BookID),
         string_concat("https://service.eudoxus.gr/coursebooks/rest/courses-books/book/eudoxus/info?bookId=", BookID, BookURL),
         get_dict(type, Book, BookType),
         get_dict(title, Book, Title),
         get_dict(authors, Book, Authors),
         get_dict(isbn, Book, ISBN),
         get_dict(editionNumber, Book, Edition),
         get_dict(editorialHouse, Book, Distributor),
         get_dict(publisherId, Book, PublisherID),
         get_dict(publisherName, Book, Publisher),
         get_dict(publicationYear, Book, Year),
         get_dict(keywords, Book, Keywords),
         get_dict(pages, Book, Pages),
         get_dict(binding, Book, CoverType),
         get_dict(dimensions, Book, BookSize),
         get_dict(pathToCover, Book, PathToCover),
         path_to_URL(PathToCover, FrontCover),
         get_dict(pathToBackcover, Book, PathToBackcover),
         path_to_URL(PathToBackcover, Backcover),
         get_dict(pathToTOC, Book, PathToTOC),
         path_to_URL(PathToTOC, Contents),
         get_dict(pathToChapter, Book, PathToChapter),
         path_to_URL(PathToChapter, Excerpt),
         get_dict(linkToPublisher, Book, LinkToPublisher),
         path_to_URL(LinkToPublisher, PublisherWebPage)
        ),
        BookList1),
    get_dict(freeBooks, JSON, Books2),
    findall(BookID-BookType-Title-Authors-ISBN-Edition-Distributor-PublisherID-Publisher-Year-Keywords-Pages-CoverType-BookSize-FrontCover-Backcover-Contents-Excerpt-PublisherWebPage-BookURL,
        (member(Book, Books2),
         get_dict(id, Book, BookID),
         string_concat("https://service.eudoxus.gr/coursebooks/rest/courses-books/book/eudoxus/info?bookId=", BookID, BookURL),
         get_dict(type, Book, BookType),
         get_dict(title, Book, Title),
         get_dict(authors, Book, Authors),
         get_dict(isbn, Book, ISBN),
         get_dict(editionNumber, Book, Edition),
         get_dict(editorialHouse, Book, Distributor),
         get_dict(publisherId, Book, PublisherID),
         get_dict(publisherName, Book, Publisher),
         get_dict(publicationYear, Book, Year),
         get_dict(keywords, Book, Keywords),
         get_dict(pages, Book, Pages),
         get_dict(binding, Book, CoverType),
         get_dict(dimensions, Book, BookSize),
         get_dict(pathToCover, Book, PathToCover),
         path_to_URL(PathToCover, FrontCover),
         get_dict(pathToBackcover, Book, PathToBackcover),
         path_to_URL(PathToBackcover, Backcover),
         get_dict(pathToTOC, Book, PathToTOC),
         path_to_URL(PathToTOC, Contents),
         get_dict(pathToChapter, Book, PathToChapter),
         path_to_URL(PathToChapter, Excerpt),
         get_dict(linkToPublisher, Book, LinkToPublisher),
         path_to_URL(LinkToPublisher, PublisherWebPage)
        ),
        BookList2),
    append(BookList1, BookList2, Books).

path_to_URL(null, "") :- !.
path_to_URL("", "") :- !.
path_to_URL(URL, URL) :-
    string_concat("https://", _, URL), !.
path_to_URL(Path, URL) :-
    string_concat("https://static.eudoxus.gr/books/", Path, URL).


check_no_books_course(CourseID) :-
    atomic_list_concat( ["https://service.eudoxus.gr/coursebooks/rest/courses-books/course/", CourseID, "/books"], URL),
    get_json_from_url(URL, JSON),
    check_no_books_json(JSON).

check_no_books_json(JSON) :-
    get_dict(bookgroups, JSON, []),
    get_dict(freeBooks, JSON, []), !.
check_no_books_json(JSON) :-
    get_dict(bookgroups, JSON, BookGroups),
    check_no_books_bookgroups(BookGroups),
    get_dict(freeBooks, JSON, []), !.

check_no_books_bookgroups([]).
check_no_books_bookgroups([BookGroup|Rest]) :-
    get_dict(books, BookGroup, []),
    check_no_books_bookgroups(Rest).


% check_no_books("no-books.txt").

check_no_books(File):-
    open(File, read, Stream),
    check_no_books_stream(Stream),
    close(Stream).
check_no_books_stream(Stream) :-
    read_line_to_string(Stream, Line),
    (Line == end_of_file -> true;
        (atom_number(Line, CourseID),
         (check_no_books_course(CourseID) -> true; (write("Course with ID "), write(CourseID), writeln(" might have books!"))),
         check_no_books_stream(Stream))).

find_missing_courses(Missing):-
    ensure_loaded('./cache/courses.pl'),
    ensure_loaded('./cache/books_per_course.pl'),
    findall(Course,course(Course,_,_,_,_,_,_), Courses),
    list_to_ord_set(Courses, CoursesSet),
    setof(Course_w_Books, Book^course_book(Book,Course_w_Books), CoursesWithBooks),
    open("no-books.txt", read, Stream),
    get_all_no_books(Stream, CoursesWithNoBooks),
    close(Stream),
    list_to_ord_set(CoursesWithNoBooks, CoursesWithNoBooksSet),
    ord_union(CoursesWithNoBooksSet,CoursesWithBooks, FoundCourses),
    ord_subtract(CoursesSet, FoundCourses, Missing).

% :- save_missing_courses("missing-courses.txt").
% Auxiliary predicate - it is used to find if there are courses that are
% neither found with books nor they have been recorded as no-book courses
save_missing_courses(File):-
    find_missing_courses(Missing),
    open(File, write, Stream),
    save_missing_courses_stream(Stream, Missing),
    close(Stream).

save_missing_courses_stream(_Stream, []).
save_missing_courses_stream(Stream, [Missing|Rest]) :-
    writeln(Stream, Missing),
    save_missing_courses_stream(Stream, Rest).

get_all_no_books(Stream,Result):-
    read_line_to_string(Stream, Line),
    (Line \== end_of_file ->
        (atom_number(Line, CourseID),
         get_all_no_books(Stream, RestCourseIDs),
         Result=[CourseID|RestCourseIDs]);
        Result=[]).


get_json_from_url(URL, JSON) :-
    http_get(URL, A, [json_object(dict),input_encoding(utf8)]),
    atom_json_dict(A,JSON,[]).
