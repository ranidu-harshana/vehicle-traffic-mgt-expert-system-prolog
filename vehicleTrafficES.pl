isa(pedestrian_cross, symbol).
isa(slow_down, symbol).

start :-sleep(0.4),
    write('-----------------------------------------------------------------'),nl,
    sleep(0.4),
    write('*****************************************************************'),nl,
    sleep(0.2),
    write("###############||| VEHICLE EXPERT SYSTEM |||#####################"),nl,
    sleep(0.4),
    write('*****************************************************************'),nl,
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl,nl,nl,
    write("Hi. How are you? First of all tell me your Vehicle number please : "),nl,
    read(User),nl,pt(User).



    ocation(User,red_light) :- verify(User," Is the signal Red (y/n) ?").

    ocation(User,green_light) :- verify(User," Is the signal Green (y/n) ?").

    ocation(User,yellow_light) :- verify(User," Is the signal Yellow (y/n) ?").

    ocation(User,go_straight) :- verify(User," Do you need to go straight (y/n) ?").

    ocation(User,need_to_turn_L) :- verify(User," Do you need to turn left (y/n) ?").

    ocation(User,left_light_on) :- verify(User," Did you turn on the left signal light (y/n) ?").

    ocation(User,need_to_turn_R) :- verify(User," Do you need to turn right (y/n) ?").

    ocation(User,right_light_on) :- verify(User," Did you turn on the right signal light (y/n) ?").

    ocation(User,pedestrians) :- verify(User," Are there any pedestrians passing (y/n) ?").

    ocation(User,immergency_vehicles) :- verify(User," Are there any immergency vehicles passing (y/n) ?").

    ocation(User,green_to_yellow) :- verify(User," Is the signal from green to yellow (y/n) ?").

    ocation(User,red_to_yellow) :- verify(User," Is the signal from red to yellow (y/n) ?").

    ocation(User,seeing_symbol) :- verify(User," Do you see any symbol (y/n) ?").

    ocation(_,"Sorry, I don't seem to be able to answer your questions.").


    hypothesis(User,'You probably have to go straight now') :-
        ocation(User,green_light),
        not(ocation(User,immergency_vehicles)),
        not(ocation(User,pedestrians)).

    hypothesis(User,'You probably have to side your vehicle and give them priority') :-
        ocation(User,green_light),
        ocation(User,immergency_vehicles).

    hypothesis(User,'You probably have to wait till they cross') :-
        ocation(User,green_light),
        ocation(User,pedestrians).

    hypothesis(User,'You probably have to turn left <--') :-
        ocation(User,green_light),
        ocation(User,need_to_turn_L),
        ocation(User,left_light_on).

    hypothesis(User,'You probably have to turn right -->') :-
        ocation(User,green_light),
        ocation(User,need_to_turn_R),
        ocation(User,right_light_on).

    hypothesis(User,'You probably have to stop the vehicle') :-
        ocation(User,red_light).

    hypothesis(User,'Be ready to stop') :-
        ocation(User,green_to_yellow).

    hypothesis(User,'Be ready to Go') :-
        ocation(User,red_to_yellow).

    hypothesis(User,'Tell me the symbol you see') :-
        ocation(User,seeing_symbol).

    hypothesis(_,"Sorry I don't seem to be able to answer your questions").


ask(User,Question) :-
	write(User),write(', '),write(Question),

        read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail),

	write('Loading.'),nl,
	sleep(1),
	write('Loading..'),nl,
	sleep(1),
	write('Loading...'),nl,
	sleep(1),
    nl.

:- dynamic yes/1,no/1.

verify(P,S) :-
   (yes(S)
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(P,S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.

symbolAction(User,Symbol):-
        (   Symbol = 'pedestrian_cross' -> nl,write("Instruction: Please slow down your vehocle."),undo,end(User,'Please slow down your vehocle.')
        ;   Symbol = 'slow_down' -> nl,write("Instruction: Slow down your vehicle."),undo,end(User,'Slow down your vehicle.')
        ;   write("Sorry I don't seem to be able to answer your questions")).

checkSymbol(User):-
        nl,nl,write("Symbol: "),read(Symbol),
        (   isa(Symbol, symbol) ->  symbolAction(User,Symbol)
        ;   write("Instruction: We cannot identify the symbol you see."),undo,end(User,'We cannot identify the symbol you see.')
        ).

pt(User):-
    hypothesis(User,Instruction),nl,

        write('Instruction: '),write(User),write(', '),write(Instruction),write('.'),
        (   Instruction = 'Tell me the symbol you see'
        ->  checkSymbol(User)
        ;   undo,end(User, Instruction)
    ).


end(User, Instruction) :-
    nl,nl,nl,
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl,
    sleep(0.7),
    write('*****************************************************************'),nl,
    sleep(0.4),
    write("###############||| THANK YOU FOR USING ME |||####################"),nl,
    sleep(0.4),
    string_concat("Vehicle Number: " ,User, FinalString),
    string_concat(FinalString, " ====> Output: ", FinalString1),
    string_concat(FinalString1, Instruction, FinalString2),
    string_concat(FinalString2, '\n', FinalString3),
    write('*****************************************************************'),nl, write_to_file('vehicledb.txt', FinalString3),
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl.

write_to_file(File, Text):-
    open(File, append, Stream),
    write(Stream, Text),nl,nl,
    close(Stream).


read_file(File):-
    open(File, read, Stream),
    % Get char from the data stream
    get_char(Stream, Char1),
    % Output all the chars until end of file
    proc_stream(Char1, Stream),
    close(Stream).

proc_stream(end_of_file,_):-!. % Terminating Condition for

proc_stream(Char, Stream):-
    write(Char),
    get_char(Stream, Char2),
    proc_stream(Char2, Stream). % Recursion



help :- write("To start the expert system please type 'start.' and press Enter key").
admin:-
    write('-----------------------------------------------------------------'),nl,
    sleep(0.4),
    write('*****************************************************************'),nl,
    sleep(0.2),
    write("####################||| ADMIN PANEL |||##########################"),nl,
    sleep(0.4),
    write('*****************************************************************'),nl,
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl,nl,nl,

    write('Reading Database.'),nl,
	sleep(1),
	write('Reading Database..'),nl,
	sleep(1),
	write('Reading Database...'),nl,
	sleep(1),nl,nl,
    write("######################||| HISTORY |||############################"),nl,nl,
    read_file('vehicledb.txt'),
    nl,nl,nl,
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl,
    sleep(0.7),
    write('*****************************************************************'),nl,
    sleep(0.4),
    write("###############||| THANK YOU FOR USING ME |||####################"),nl,
    sleep(0.4),
    write('*****************************************************************'),nl,
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl.
