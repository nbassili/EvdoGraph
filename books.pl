
:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

% :- cache_one_book('https://service.eudoxus.gr/search/#a/id:94700120/0', BookCode, LocalBookFile).

cache_one_book(BookURL, BookCode, LocalBookFile) :-
%%https://service.eudoxus.gr/search/#a/id:94700120/0
	dtd(html, DTD),
	sub_string(BookURL,40,_L,2,BookCode),
	%split_string(LastPart,"/","",List),
	%atomics_to_string(List,'_',StringLastPart),
	string_concat(BookCode,".html",LocalBookFile),
	string_concat("book_cache/",LocalBookFile,LocalBookFilePath),
	http_open(BookURL,In,[]),
		load_structure(In, HTML, [ dtd(DTD), dialect(sgml), shorttag(false), max_errors(-1),syntax_errors(quiet),encoding('utf-8') ]),
		write(HTML), nl,
	open(LocalBookFilePath,write,Out,[encoding(utf8)]),
	%html_write(Out, HTML, [ dtd(DTD) ]),
	copy_stream_data(In, Out),
	close(In),
	close(Out), !.
	
	
	
%%/html/body/table/tbody/tr[2]/td/table/tbody/tr[3]/td/table/tbody/tr/td[2]/table/tbody

cache_book(BookCode, LocalBookFile) :-
	process_create(path('python'),['cache_book.py', BookCode],[stdout(pipe(In))]),
	read_string(In, "\n", "\r\t ", _Sep, LocalBookFile).
    
