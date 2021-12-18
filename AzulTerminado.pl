% Autor: Carlos Yanguas Durán, Carlos Golvano Díaz y Lucas García de Viedma
% Fecha: 11/03/2020

%----------------------------------------------------------------------------------------------
%---------------------------Funcion de inicio de juego y menu----------------------------------
%----------------------------------------------------------------------------------------------

jugar:-
       writeln(''),writeln(''),writeln(''),writeln(''),writeln(''),
       writeln(''),writeln(''),writeln(''),writeln(''),writeln(''),
       writeln('==========================================='),
       writeln('|           BIENVENIDO A AZUL'),
       writeln('==========================================='),
       write('Introduce un numero de jugadores (2,3 o 4): '),
       read(X),numJugadores(X),writeln('Fin de programa').

bucleError:-
            writeln('Por favor introduzca un numero de jugadores correcto (2,3 o 4): '),
            read(X), numJugadores(X).

numJugadores(2):-preparacion(2,Tableros,Mesa),!.
numJugadores(3):-preparacion(3,Tableros,Mesa),!.
numJugadores(4):-preparacion(4,Tableros,Mesa),!.
numJugadores(X):-writeln('El numero de jugadores debe ser 2,3 o 4'), bucleError.

preparacion(Jugadores,Tableros,Mesa):-
  generarTableros(Jugadores,Tableros),
  generarMesa(Jugadores,Mesa),
  turno(0,Jugadores,Mesa,Tableros).

turno(Jugador,NumJugadores,Mesa,Tableros):-
  JugadorNumerico is Jugador+1,
  write('\n               Turno del Jugador '),write(JugadorNumerico),
  nth1(JugadorNumerico,Tableros,TableroJugador),
  mostrarTableroGeneral(NumJugadores,Tableros),
  mostrarFactorias(Mesa,FactoriaElegida),
  comprobarColocarFichas(Mesa, TableroJugador, FactoriaElegida, Cantidad, FichaElegida, FilaElegida),
  actualizaTablero(TableroJugador,FilaElegida,FichaElegida,Cantidad,TableroJugadorActualizado0,NumFichasCaja),
  pasarACaja(Mesa,NumFichasCaja,FichaElegida,MesaActualizada),
  moverFichasCentro(MesaActualizada, FactoriaElegida, FichaElegida, TableroJugadorActualizado0, MesaResultante, TableroJugadorActualizado1),
  nth1(JugadorNumerico,Tableros,_,TableroAActualizar),
  nth1(JugadorNumerico,TablerosActualizados,TableroJugadorActualizado1,TableroAActualizar),
  Siguiente1 is Jugador + 1,
  finRonda(NumJugadores,MesaResultante,TablerosActualizados,Siguiente1,MesaFinal,TablerosFinal,Sig),
  SiguienteJugador is Sig mod NumJugadores,
  finPartida(SiguienteJugador,NumJugadores,MesaFinal,TablerosFinal,TableroJugadorActualizado1).



finPartida(SiguienteJugador,NumJugadores,MesaFinal,TablerosFinal,TableroJugadorActualizado):-
  comprobarFinPartida(TableroJugadorActualizado,Fin),
  Fin==1,!,write('\n\n                           FIN DEL JUEGO').

finPartida(SiguienteJugador,NumJugadores,MesaFinal,TablerosFinal,TableroJugadorActualizado):-
  turno(SiguienteJugador,NumJugadores,MesaFinal,TablerosFinal).
%----------------------------------------------------------------------------------------------
%-----------------------------Funcion de comprobacion de fin-----------------------------------
%----------------------------------------------------------------------------------------------

comprobarFinPartida(TableroJugador, Fin):-
  nth1(3,TableroJugador, Mosaico),
  comprobarFinPartidaAux(Mosaico, 1, Fin).

comprobarFinPartidaAux(Mosaico, 6, Final):-
  Fin = 0, !.

comprobarFinPartidaAux(Mosaico, NumFila, Final):-
  nth1(NumFila, Mosaico, Fila),
  comprobarFilaCompleta(Fila, 1, Final),
  Final==1, !.

comprobarFinPartidaAux(Mosaico,NumFila,Final):-
  NumFila1 is NumFila+1,
  comprobarFinPartidaAux(Mosaico, NumFila1, Final).

comprobarFilaCompleta(Fila, 6,  Fin):-
  Fin=1, !.

comprobarFilaCompleta(Fila, NumFichas, Fin):-
  nth1(NumFichas, Fila, Color),
  Color \= '   ',!,
  NumFichas1 is NumFichas+1,
  comprobarFilaCompleta(Fila, NumFichas1, Fin).

comprobarFilaCompleta(Fila,NumFichas,Fin):-
  Fin=0.

%----------------------------------------------------------------------------------------------
%---------------------------Funcion de generacion de tableros----------------------------------
%----------------------------------------------------------------------------------------------

generarTableros(2,Tableros):-
Tableros = [[0,[['   '],['   ','   '],['   ','   ','   '],['   ','   ','   ','   '],['   ','   ','   ','   ','   ']],
             [['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   ']],[]],
            [0,[['   '],['   ','   '],['   ','   ','   '],['   ','   ','   ','   '],['   ','   ','   ','   ','   ']],
             [['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   ']],[]]].

generarTableros(3,Tableros):- generarTableros(2,Tableros2),
  append(Tableros2,[[0,[['   '],['   ','   '],['   ','   ','   '],['   ','   ','   ','   '],['   ','   ','   ','   ','   ']],
  [['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   ']],[]]],Tableros).

generarTableros(4,Tableros):- generarTableros(3,Tableros3),
  append(Tableros3,[[0,[['   '],['   ','   '],['   ','   ','   '],['   ','   ','   ','   '],['   ','   ','   ','   ','   ']],
  [['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   '],['   ','   ','   ','   ','   ']],[]]],Tableros).

%----------------------------------------------------------------------------------------------
%---------------------------Funcion de generacion de reservas----------------------------------
%----------------------------------------------------------------------------------------------

generarReserva(0,Y,Reserva):-                                                                                                    %Genera las reservas del tablero de un jugador fin
  Reserva = Y,!.

generarReserva(X,Y,Reserva):-                                                                                                    %Genera las reservas del tablero de un jugador
   R = ['   '],
   append(Y,R,Y2),
   X1 is X-1,
   generarReserva(X1,Y2,Reserva).

%----------------------------------------------------------------------------------------------
%-------------------------------Funcion de generacion de mesa----------------------------------
%----------------------------------------------------------------------------------------------

generarMesa(Jugadores, Mesa):-                                                                                                   %Genera una mesa con los elementos pertinentes
generarBolsa(Bolsa),
generarFactorias(Jugadores,Bolsa,Factorias,BolsaRestante),
Mesa = [1,Factorias,[],BolsaRestante,[0,0,0,0,0]]. %Marcador y factorías, Centro, Bolsa, Caja

%----------------------------------------------------------------------------------------------
%---------------------------Funcion de generacion de factorias---------------------------------
%----------------------------------------------------------------------------------------------

generarFactoriasAux(0,ListaActual,Bolsa,ListaFactorias,BolsaRestante):-                                                          %Genera las factorias necesarias segun los jugadores
  ListaFactorias=ListaActual,                                                                                                    %(y las rellena) fin AUX
  BolsaRestante=Bolsa,!.

generarFactoriasAux(ContadorFactorias,ListaActual,Bolsa,ListaFactorias,BolsaRestante):-                                          %Genera las factorias necesarias segun los jugadores
  obtenerFichasFactoria(Bolsa, Factoria, BolsaRestanteActual),                                                                   %(y las rellena) AUX
  append(ListaActual,[Factoria],ListaFactoriasActual),
  ContadorFactoriasActualizado is ContadorFactorias - 1,
  generarFactoriasAux(ContadorFactoriasActualizado,ListaFactoriasActual,BolsaRestanteActual,ListaFactorias,BolsaRestante).

generarFactorias(Jugadores,Bolsa,Factorias,BolsaRestante):-                                                                      %Genera las factorias necesarias segun los jugadores.
  NumFactorias is (Jugadores * 2) + 1,
  generarFactoriasAux(NumFactorias,[],Bolsa,Factorias,BolsaRestante).

%----------------------------------------------------------------------------------------------
%------------------------------Funcion de generacion de bolsa----------------------------------
%----------------------------------------------------------------------------------------------

generarBolsa(Bolsa):- Bolsa = [20,20,20,20,20].                                                                    %Genera una bolsa con 100 fichas (20 de cada color)

%----------------------------------------------------------------------------------------------
%---------------------------Funcion de rellenado de factorias----------------------------------
%----------------------------------------------------------------------------------------------

obtenerFichasFactoria(Bolsa, Factoria, BolsaResultante):-                                                          %Rellena una factoria con 4 fichas, todas sacadas de la bolsa
  obtenerFichasFactoriaAux(Bolsa,[],0,Factoria,BolsaResultante).

obtenerFichasFactoriaAux(Bolsa,Factoria,4,FactoriaResultado,BolsaResultante):-                                     %Obtiene una ficha aleatoria de la bolsa y la añade a una
  FactoriaResultado=Factoria,                                                                                      %factoria, la ficha se resta de la bolsa AUX fin
  BolsaResultante=Bolsa.

obtenerFichasFactoriaAux(Bolsa,Factoria,Cont,FactoriaResultado,BolsaResultante):-                                  %Obtiene una ficha aleatoria de la bolsa y la añade a una
  length(Bolsa, X),                                                                                                %factoria, la ficha se resta de la bolsa AUX
  elegirPieza(X,Bolsa,Valor,Pos),
  NuevoValor is Valor-1,
  nth1(Pos,Bolsa,_,BolsaResultante1),
  nth1(Pos, BolsaResultante2, NuevoValor, BolsaResultante1),
  append(Factoria,[Pos],NuevaFactoria),
  Cont1 is Cont+1,
  obtenerFichasFactoriaAux(BolsaResultante2,NuevaFactoria,Cont1,FactoriaResultado,BolsaResultante).

elegirPieza(X,Bolsa,Valor,Pos):-                                                                                   %Elige pieza aleatoria de la bolsa, mediante el bucle
  random_between(1,X,Pos),                                                                                         %inferior siempre obtiene una ficha valida
  nth1(Pos,Bolsa,Valor),
  Valor>0,!.

elegirPieza(X,Bolsa,Valor,Pos):- elegirPiezaBuc(X,Bolsa,Valor,Pos).
elegirPiezaBuc(X,Bolsa,Valor,Pos):- elegirPieza(X,Bolsa,Valor,Pos).

%----------------------------------------------------------------------------------------------
%--------------------------------Funcion de muestreo de factorias------------------------------
%----------------------------------------------------------------------------------------------

mostrarFactorias(Mesa,Resultado):-                                                                                         %Muestra las factorias y pide al usuario que elija una
  nth1(2, Mesa, Factorias),
  nth1(3, Mesa, Centro),
  length(Factorias,Num_Factorias),
  write('\n'),
  mostrarFactoriasAux(Factorias,Num_Factorias),
  write('\n'),
  write('Centro (0): '),write(Centro),write('\n'),
  nth0(0,CasillasDeObtencion,Centro,Factorias),
  elegirFactoria(Num_Factorias,CasillasDeObtencion,Resultado).

mostrarFactoriasAux(Factorias, 1):-                                                                                        %Muestra una factoria AUX fin
  nth1(1, Factorias, Factoria1),
  write('Factoria 1: '),
  write(Factoria1),
  write('  ').

mostrarFactoriasAux(Factorias, Num_Factoria):-                                                                             %Muestra una factoria AUX
  Sig_Factoria is Num_Factoria-1,
  mostrarFactoriasAux(Factorias,Sig_Factoria),
  nth1(Num_Factoria, Factorias, Factoria),
  write('Factoria '),
  write(Num_Factoria),
  write(': '),
  write(Factoria),
  write('  ').


elegirFactoria(Pos_Factoria,Factorias,Resultado):-                                                                         %Solicita al usuario una factoria hasta que le da
  write('\nElija el numero de la factoria/centro que desea: '),                                                            %un numero de factoria/centro correcto (con el bucle
  read(Eleccion),                                                                                                          %inferior)
  Pos_Factoria>=Eleccion,
  Eleccion >= 0,
  Limite is -1,
  Eleccion>Limite,
  nth0(Eleccion,Factorias,Factoria,_),
  Factoria \= [],
  !,
  Resultado = Eleccion.

elegirFactoria(Pos_Factoria,Factorias,Resultado):- write('\nEsa factoria no esta disponible.'),elegirFactoriaBuc(Pos_Factoria,Factorias,Resultado).
elegirFactoriaBuc(Pos_Factoria,Factorias,Resultado):- elegirFactoria(Pos_Factoria,Factorias,Resultado).

%----------------------------------------------------------------------------------------------
%--------------------------------Funcion de impresion de juego---------------------------------
%----------------------------------------------------------------------------------------------

mostrarTablero(Jugador,Tableros):-                                                                                         %Muestra el tablero de un jugador concreto
nth1(Jugador,Tableros,TableroJugador),
nth1(1,TableroJugador,Ini),
nth1(2,TableroJugador,Reserva),
nth1(3,TableroJugador,Mosaico),
nth1(4,TableroJugador,Resto),
nth1(1,Reserva,Reserva1),
nth1(2,Reserva,Reserva2),
nth1(3,Reserva,Reserva3),
nth1(4,Reserva,Reserva4),
nth1(5,Reserva,Reserva5),
nth1(1,Mosaico,Mosaico1),
nth1(2,Mosaico,Mosaico2),
nth1(3,Mosaico,Mosaico3),
nth1(4,Mosaico,Mosaico4),
nth1(5,Mosaico,Mosaico5),
write(  '\n\n Reserva del Jugador '),write(Jugador),write('   Tablero del Jugador '),write(Jugador),
write('\n ---------------------   ---------------------\n'),
write('                           1   2   3   4   5  \n                 '),
write(Reserva1), write(' 1 '),write(Mosaico1),write('\n             '),
write(Reserva2), write(' 2 '),write(Mosaico2),write('\n         '),
write(Reserva3), write(' 3 '),write(Mosaico3),write('\n     '),
write(Reserva4), write(' 4 '),write(Mosaico4),write('\n '),
write(Reserva5), write(' 5 '),write(Mosaico5),write('\n'),
write('\n Suelo del jugador '),write(Jugador),write(': '),write(Resto),write(' --> '),write(Ini),write('\n').

mostrarTableroGeneral(1,Tableros):-                                                                                        %Muestra los tableros de los jugadores fin
 mostrarTablero(1,Tableros),!.

mostrarTableroGeneral(Jugadores,Tableros):-                                                                                %Muestra los tableros de los jugadores
 TablerosAMostrar is Jugadores-1,
 mostrarTableroGeneral(TablerosAMostrar,Tableros),
 mostrarTablero(Jugadores,Tableros).

%----------------------------------------------------------------------------------------------
%---------------------------Funcion de fichas descartadas al centro----------------------------
%----------------------------------------------------------------------------------------------

moverFichasCentro(Mesa, 0, ColorFichasSeleccionadas, Tablero, MesaResultante, TableroResultante):-                         %Jugador elige coger las fichas centro, se le dan las fichas
  nth1(3, Mesa,Centro, MesaAActualizar),                                                                                   %y las restantes van a la caja. Si es el primero se lleva la
  nth1(3, MesaActualizada1,[],MesaAActualizar),                                                                            %ficha de primer jugador. Centro se vacia.
  length(Centro,Max),
  moverFichasCentroAux(Mesa, Centro,[], ColorFichasSeleccionadas, 1, Max, CajaResultante),
  nth1(5, MesaActualizada1,CajaPrevia,MesaAActualizar2),
  append(CajaPrevia,CajaResultante,CajaTotal),
  nth1(5, MesaResultante0,CajaTotal,MesaAActualizar2),
  primerJugadorCentro(MesaResultante0,Tablero,MesaResultante,TableroResultante).

moverFichasCentro(Mesa, NumFactoria, ColorFichasSeleccionadas, Tablero, MesaResultante, TableroResultante):-               %Jugador elige coger las fichas factoria, se le dan las fichas
  nth1(2, Mesa, Factorias),                                                                                                %y las restantes van al centro. La factoria se vacia.
  nth1(NumFactoria, Factorias, FactoriaSeleccionada),
  moverFichasCentroAux(Mesa, FactoriaSeleccionada,[], ColorFichasSeleccionadas, 1, 5, CentroResultante),
  nth1(3, Mesa, _, Mesa1),
  nth1(3, MesaResultanteAux, CentroResultante, Mesa1),
  FactoriaResultante = [],
  nth1(NumFactoria,Factorias,_,FactoriasResultantes),
  nth1(NumFactoria, FactoriasResultantes1, FactoriaResultante, FactoriasResultantes),
  nth1(2,MesaResultanteAux,_,Mesa2),
  nth1(2,MesaResultante,FactoriasResultantes1, Mesa2),
  TableroResultante = Tablero.

moverFichasCentroAux(Mesa, FactoriaSeleccionada, FichasCentro, ColorSeleccionado, Cont, Max, CentroResultante):-                %Añade las fichas de la factoria al centro
  FactoriaSeleccionada \= 0,
  Cont==Max,
  nth1(3, Mesa, CentroActual),
  append(CentroActual, FichasCentro, CentroResultante), !.

moverFichasCentroAux(Mesa, FactoriaSeleccionada, FichasCentro, ColorSeleccionado, Cont, Max, CentroResultante):-                %Añade las fichas del centro a la caja
  FactoriaSeleccionada == 0,
  Cont==Max,
  CentroResultante is FichasCentro, !.

moverFichasCentroAux(Mesa, FactoriaSeleccionada, FichasCentro, ColorSeleccionado, Cont, Max, CentroResultante):-                %Comprueba que fichas van al centro/caja y cuales
  nth1(Cont, FactoriaSeleccionada, ColorFicha),                                                                                 %al tablero del jugador (al centro/caja)
  ColorFicha \= ColorSeleccionado, !,
  append(FichasCentro, [ColorFicha], FichasCentroResultante),
  Cont1 is Cont+1,
  moverFichasCentroAux(Mesa, FactoriaSeleccionada, FichasCentroResultante, ColorSeleccionado, Cont1, Max, CentroResultante).

moverFichasCentroAux(Mesa, FactoriaSeleccionada, FichasCentro, ColorSeleccionado, Cont, Max, CentroResultante):-                %Comprueba que fichas van al centro/caja y cuales
  Cont1 is Cont + 1,                                                                                                            %al tablero del jugador (al jugador)
  moverFichasCentroAux(Mesa, FactoriaSeleccionada, FichasCentro, ColorSeleccionado, Cont1, Max, CentroResultante).

primerJugadorCentro(Mesa,Tablero,NewMesa,NewTablero):-                                                                          %Si el jugador es el primero en coger del centro coje
  nth1(1,Mesa,JugadorInicial,MesaSinJI),                                                                                        %la ficha de primer jugador
  JugadorInicial == 1,!,
  nth1(1,NewMesa,0,MesaSinJI),
  nth1(1,Tablero,_,TableroSinJI),
  nth1(1,NewTablero,1,TableroSinJI).

primerJugadorCentro(Mesa,Tablero,NewMesa,NewTablero):-                                                                          %Si no es el primero no cambia nada
  NewMesa=Mesa,
  NewTablero=Tablero.

%----------------------------------------------------------------------------------------------
%--------------------------------Funcion de añadido a tablero----------------------------------
%----------------------------------------------------------------------------------------------

añadirReservaAMosaico(Jugador,Tableros,TablerosSiJ,Resto):-                                                                          %Añade las fichas de filas de reserva completas al mosaico
  nth1(Jugador,Tableros,TableroJugador,TablerosNoJ),
  nth1(2,TableroJugador,ReservaJugador,TableroNoR),
  nth1(2,TableroNoR,MosaicoJugador,TableroNoRNoM),
  añadirFilaReservaAMosaico(1,ReservaJugador,MosaicoJugador,[],NuevaReserva,NuevoMosaico,Resto),
  nth1(2,NuevoTableroSiR,NuevaReserva,TableroNoRNoM),
  nth1(3,NuevoTableroSiRSiM,NuevoMosaico,NuevoTableroSiR),
  nth1(Jugador,TablerosSiJ,NuevoTableroSiRSiM,TablerosNoJ).

añadirFilaReservaAMosaico(6,Reserva,Mosaico,RestoI,NuevaReserva,NuevoMosaico,RestoF):-                                         %Comprueba la fila de reserva y la añade si es correspondiente fin
  NuevaReserva=Reserva,
  NuevoMosaico=Mosaico,
  RestoF=RestoI,!.

añadirFilaReservaAMosaico(X,Reserva,Mosaico,RestoI,NuevaReserva,NuevoMosaico,RestoF):-                                         %Comprueba la fila de reserva y la añade si es correspondiente
  nth1(X,Reserva,Fila),                                                                                                        %las fichas sobrantes van a la caja
  nth1(X,Fila,Num,RestoFila),
  Num \= '   ',
  comprobarFilaReserva(X,Fila,Num),!,
  nth1(X,Mosaico,FilaAActualizar,MosaicoAActualizar),
  Pos0 is Num+X-2,
  Pos1 is Pos0 mod 5,
  Pos2 is Pos1+1,
  nth1(Pos2,FilaAActualizar,_,FilaPreparada),
  nth1(Pos2,FilaActualizada,Num,FilaPreparada),
  nth1(X,MosaicoActualizado,FilaActualizada,MosaicoAActualizar),
  X1 is X+1,
  generarReserva(X,[],NuevaFilaReserva),
  nth1(X,Reserva,_,ReservaAActualizar),
  nth1(X,ReservaActualizada,NuevaFilaReserva,ReservaAActualizar),
  append(RestoFila,RestoI,RestoActualizado),
  añadirFilaReservaAMosaico(X1,ReservaActualizada,MosaicoActualizado,RestoActualizado,NuevaReserva,NuevoMosaico,RestoF).

añadirFilaReservaAMosaico(X,Reserva,Mosaico,RestoI,NuevaReserva,NuevoMosaico,RestoF):-                                         %Si la fila no esta completa pasa a la siguiente
  X \= 6,
  X1 is X+1,
  añadirFilaReservaAMosaico(X1,Reserva,Mosaico,RestoI,NuevaReserva,NuevoMosaico,RestoF).

comprobarFilaReserva(0,Fila,Num):- true.                                                                                       %Comprueba si la fila esta completa fin

comprobarFilaReserva(X,Fila,Num):-                                                                                             %Comprueba si la fila esta completa
  nth1(X,Fila,Num2),
  Num == Num2,
  X1 is X-1,
  comprobarFilaReserva(X1,Fila,Num).

%-------------------------------------------------------------------------------
%-------------Devuelve una lista con las fichas colocadas en una fila-----------
%--------Fila: Fila donde se va a meter; Cantidad: número de fichas-------------
%-------------------------------------------------------------------------------

ponerFichasFila(Fila, Cantidad, Ficha, NewFila):-                               %Dada una fila, una ficha y la cantidad de fichas. Devuelve la fila con las fichas insertadas
count(Ficha, Fila, CF, LF),
count('   ', Fila, CB, LB),
ponerFichasFilaAux(0, CB, Ficha, Cantidad, NewFila, LF).

ponerFichasFilaAux(Cont, Libres, Ficha, Cantidad, NewFila, FilaAux):-           %Función recursiva para realizar la tarea de ponerFichasFila()
Cont < Libres,
Cont < Cantidad,
!,
append([Ficha], FilaAux, FilaAux2),
ContAux is Cont + 1,
ponerFichasFilaAux(ContAux, Libres, Ficha, Cantidad, NewFila, FilaAux2).

ponerFichasFilaAux(Cont, Libres,_, Cantidad, NewFila, NewFilaAux):-             %Añade espacios libres a la fila, en caso de no haberse llenado
Cont < Libres,
!,
append(NewFilaAux, ['   '], NewFilaAux2),
ContAux is Cont + 1,
ponerFichasFilaAux(ContAux, Libres,_,Cantidad, NewFila, NewFilaAux2).

ponerFichasFilaAux(_,_,_,_,NewFila,NewFila).

%-------------------------------------------------------------------------------
%---------------Comprueba si se puede colocar una ficha en una fila-------------
%---------------Lugar = 0: factoría ; Lugar = 1: centro-------------------------
%-------------------------------------------------------------------------------


comprobarColocarFichas(Mesa, Tablero, 0, Cantidad, FichaElegida, FilaElegida):-              %Se ejecuta en el caso de haber cogido fichas del centro
write('Que ficha desea coger?:'),
read(Ficha),
cogerFichas(Mesa, Ficha, Cantidad, FichaElegida),
comprobarFilas(Tablero,Ficha, FilaElegida),!.

comprobarColocarFichas(Mesa, Tablero, NumFactoria, Cantidad, FichaElegida, FilaElegida):-    %Se ejecuta en el caso de hacer codigo fichas de una factoría
write('Que ficha desea coger?:'),
read(Ficha),
cogerFichas(Mesa, NumFactoria, Ficha, Cantidad, FichaElegida),
comprobarFilas(Tablero,Ficha, FilaElegida).

cogerFichas(Mesa, NumFactoria, Ficha, Cantidad, FichaElegida):-                              %Coge las fichas elegidas de una factoría
nth1(2, Mesa, Factorias),
nth1(NumFactoria, Factorias, FactoriaElegida),
count(Ficha, FactoriaElegida, Cantidad, ListaRes),
ListaRes \= [],!,
FichaElegida is Ficha.

cogerFichas(Mesa, Ficha, Cantidad, FichaElegida):-                                           %Coge las fichas del centro
  nth1(3, Mesa, Centro),
  count(Ficha, Centro, Cantidad, ListaRes),
  ListaRes \= [],!,
  FichaElegida is Ficha.

cogerFichas(Mesa, NumFactoria, Ficha, Cantidad, FichaElegida):-                              %Excepción para el caso en el que no hay fichas en la factoría elegida
write('En la factoría '),
write(NumFactoria),
write(' no hay fichas del tipo '),
writeln(Ficha),
write('Que ficha desea coger?:'),
read(NewFicha),
cogerFichasBuc(Mesa, NumFactoria, NewFicha, Cantidad, FichaElegida).

cogerFichasBuc(Mesa, Ficha, Cantidad, FichaElegida):-
cogerFichas(Mesa, Ficha, Cantidad, FichaElegida).

cogerFichasBuc(Mesa, NumFactoria, Ficha, Cantidad, FichaElegida):-
cogerFichas(Mesa, NumFactoria, Ficha, Cantidad, FichaElegida).

cogerFichas(Mesa, Ficha, Cantidad, FichaElegida):-                                           %Excepción para el caso de que no haya fichas en el centro
  write('En el centro no hay fichas del tipo '),
  writeln(Ficha),
  write('Que ficha desea coger?:'),
  read(NewFicha),
  cogerFichasBuc(Mesa, NewFicha, Cantidad, FichaElegida).


%-------------------------------------------------------------------------------
%-------------Funciones de actualización de reserva del tablero-----------------
%-------------------------------------------------------------------------------
                                                                                                              %Actualiza las filas de la reserva de un tablero dado.
                                                                                                              %Se realizan todas las comprobaciones pertinentes
actualizaTablero(TableroJugador,FilaElegida,FichaElegida,Cantidad,TableroJugadorActualizado,NumFichasCaja):-
  nth1(2,TableroJugador,Reserva,RestoTablero),
  nth1(FilaElegida,Reserva,FilaACambiar,RestoReserva),
  count('   ',FilaACambiar,CActualFila,_),
  nth1(1,TableroJugador,FichaInicio),
  nth1(4,TableroJugador,Suelo),
  length(Suelo, Len),
  PosLibres is 8 - Len - FichaInicio,
  NumFichasSuelo is Cantidad - CActualFila,
  NumFichasCaja is NumFichasSuelo - PosLibres,
  ponerFichasFila(FilaACambiar,Cantidad,FichaElegida,NewFila),
  pasarASuelo(0,PosLibres,FichaElegida,NumFichasSuelo,Suelo,SueloActualizado),
  nth1(FilaElegida,ReservaActualizada,NewFila,RestoReserva),
  nth1(2,TableroJugadorActualizadoAux,ReservaActualizada,RestoTablero),
  nth1(4,TableroJugadorActualizadoAux,_,RestoTableroJugadorActualizadoAux),
  nth1(4,TableroJugadorActualizado,SueloActualizado,RestoTableroJugadorActualizadoAux).

pasarASuelo(Cont, PosLibres, Ficha, NumFichasSuelo, SueloAux, SueloActualizado):-                           %Coloca en suelo las fichas que no se han podido colocar en reserva
  NumFichasSuelo > 0,
  Cont < NumFichasSuelo,
  Cont < PosLibres,!,
  append(SueloAux, [Ficha], SueloAux2),
  ContAux is Cont + 1,
  pasarASuelo(ContAux, PosLibres, Ficha, NumFichasSuelo, SueloAux2, SueloActualizado).

pasarASuelo(_,_,_,_, SueloActualizado, SueloActualizado).

pasarACaja(Mesa,NumFichasCaja,Ficha,MesaActualizada):-                                                      %Coloca en la caja las fichas que no se han podido colocar en suelo
  NumFichasCaja > 0,!,
  nth1(5,Mesa,Caja,RestoMesa),
  nth1(Ficha,Caja,FichasCaja,RestoCaja),
  FichasCajaActualizadas is FichasCaja + NumFichasCaja,
  nth1(Ficha,CajaActualizada,FichasCajaActualizadas,RestoCaja),
  nth1(5,MesaActualizada,CajaActualizada,RestoMesa).

pasarACaja(MesaActualizada,_,_,MesaActualizada).


%-------------------------------------------------------------------------------
%-----------Comprueba si se puede colocar un tipo de ficha en una fila----------
%-------------------------------------------------------------------------------


comprobarFilas(Tablero,Ficha, FilaElegida):-                                                                %Comprueba si se puede colocar una ficha en una fila
  nth1(3, Tablero, Mosaico),                                                                                %Se podrá colocar si está vacía o contiene mas fichas del mismo tipo
  write('En que fila de la reserva desea colocar las fichas?:'),
  read(Fila),
  nth1(Fila, Mosaico, FilaMosaico),
  count(Ficha, FilaMosaico,_, ListaRes),
  ListaRes == [],
  nth1(2, Tablero, FilasTab),
  nth1(Fila, FilasTab, FilaTab),
  comprobarFilas2(Ficha, FilaTab),!,
  FilaElegida is Fila.

comprobarFilas(Tablero,Ficha, FilaElegida):-
  writeln('No se puede colocar en la fila indicada.'),
comprobarFilasBuc(Tablero,Ficha, FilaElegida).

comprobarFilasBuc(Tablero,Ficha, FilaElegida):-
comprobarFilas(Tablero,Ficha, FilaElegida).

comprobarFilas2(_, FilaC):-
filaIsEmpty(FilaC),!.

comprobarFilas2(Ficha, Fila):-
count(Ficha, Fila,_, ListaC),
nth1(1, ListaC, FichaFila),
Ficha = FichaFila.


%-------------------------------------------------------------------------------
%---------------------------FUNCIONES AUXILIARES--------------------------------
%-------------------------------------------------------------------------------

count(_, [], 0, []).                                                            %Devuelve las veces que se repite una ficha en una lista. También devuelve una lista que contenga
count(X, [X | T], N, L) :-                                                      %el las fichas.
  !, count(X, T, N1, L1),
  N is N1 + 1,
  append(L1,[X],L).
count(X, [_ | T], N, L) :-
  count(X, T, N, L).

contar(_, [], 0).                                                               %Hace lo mismo que count pero no devuelve la lista
contar(X, [X | T], N) :-
  !, contar(X, T, N1),
  N is N1 + 1.
contar(X, [_| T], N) :-
  contar(X, T, N).


filaIsEmpty([Head|Resto]):-                                                     %Comprueba si una fila no tiene fichas
  !,
  Head = '   ',
  filaIsEmpty(Resto).

filaIsEmpty(_).

%----------------------------------------------------------------------------------------------
%---------------------------Funcion de comprobacion final de ronda-----------------------------
%----------------------------------------------------------------------------------------------

resetInicial(Jugadores,Tableros,TablerosFinal,Sig):-
  write(Jugadores),write('q'),                                                                                   %Devuelve la ficha de jugador inicial al centro y estblece
  nth1(Jugadores,Tableros,Tablero,TablerosAActualizar),                                                          %el nuevo jugador inicial de la sig ronda
  nth1(1,Tablero,Ini,TableroAActualizar),
  Ini==1, !,
  Sig=Jugadores,
  nth1(1,TableroActualizado,0,TableroAActualizar),
  nth1(Jugadores,TablerosFinal,TableroActualizado,TablerosAActualizar).

resetInicial(Jugadores,Tableros,TablerosFinal,Sig):-                                                             %Si el jugador no es inicial sigue comprobando
  SigJugador is Jugadores-1,
  resetInicial(SigJugador,Tableros,TablerosFinal,Sig).

finRonda(Jugadores,Mesa,Tableros,SigA,MesaFinal,TablerosFinal,SigF):-
  nth1(3,Mesa,Centro),                                                                                           %Comprueba si el centro y factorias estan vacios (fin ronda),
  Centro == [],                                                                                                  %actualiza la mesa y tableros e indica el nuevo primer jugador
  nth1(2,Mesa,Factorias,MesaAActualizar),
  finRondaFactorias(Jugadores,Mesa,Tableros,Factorias,MesaActualizada0,TablerosActualizados),!,
  insercionEnTableros(Jugadores,TablerosActualizados,[],TablerosPreFinal,Resto),
  nth1(5,MesaActualizada0,CajaPrevia,MesaSinCaja),
  append(CajaPrevia,Resto,CajaActual),
  nth1(5,MesaActualizada,CajaActual,MesaSinCaja),
  resetInicial(Jugadores,TablerosPreFinal,TablerosFinal,SigFF),
  SigF is SigFF-1,
  nth1(1,MesaActualizada,_,MesaSinFichaInicial),
  nth1(1,MesaFinal,1,MesaSinFichaInicial).

finRonda(Jugadores,Mesa,Tableros,SigA,MesaFinal,TablerosFinal,SigF):-                                            %En caso de que no sea el final de Ronda no hace nada
  SigF=SigA,
  MesaFinal = Mesa,
  TablerosFinal = Tableros.

llenarBolsa(Mesa,NumFactorias,MesaActualizada):-                                                                 %En caso de que en la bolsa no haya fichas suficientes
  nth1(4,Mesa,Bolsa,MesaSinBolsa),                                                                               %se rellena con la caja
  nth1(4,MesaSinBolsa,Caja,MesaSinBolsaSinCaja),
  nth1(1,Bolsa,Fichas1),
  nth1(2,Bolsa,Fichas2),
  nth1(3,Bolsa,Fichas3),
  nth1(4,Bolsa,Fichas4),
  Total is Fichas1+Fichas2+Fichas3+Fichas4,
  Limite is NumFactorias*4,
  Total<Limite,!,
  pasarCajaABolsa(Bolsa,Caja,BolsaNueva,CajaNueva),
  nth1(4,MesaSinBolsaConCaja,CajaNueva,MesaSinBolsaSinCaja),
  nth1(4,MesaActualizada,BolsaNueva,MesaSinBolsaConCaja).

llenarBolsa(Mesa,NumFactorias,MesaActualizada):-                                                                 %En caso de que en la bolsa haya fichas suficientes no se rellena
  MesaActualizada = Mesa.

sueloACaja(Tableros,Mesa,TablerosActualizados,MesaActualizada):-                                                 %Fichas del suelo de tableros pasan a cajas
  length(Tableros,NumTableros),
  sueloACajaAux(NumTableros,Tableros,Mesa,TablerosActualizados,MesaActualizada).

sueloACajaAux(0,Tableros,Mesa,TablerosActualizados,MesaActualizada):-                                            %Fichas del suelo de tableros pasan a cajas fin AUX
  TablerosActualizados=Tableros,
  MesaActualizada=Mesa,!.

sueloACajaAux(NumTableros,Tableros,Mesa,TablerosActualizados,MesaActualizada):-                                  %Fichas del suelo de tableros pasan a cajas AUX
  nth1(NumTableros,Tableros,TableroJugador,TablerosAActualizar),
  nth1(4,TableroJugador,Suelo,TableroJugadorAActualizar),
  nth1(4,TableroJugadorActualizado,[],TableroJugadorAActualizar),
  nth1(NumTableros,TablerosActualizados0,TableroJugadorActualizado,TablerosAActualizar),
  nth1(5,Mesa,Caja,MesaAActualizar),
  append(Caja,Suelo,CajaActualizada),
  nth1(5,MesaActualizada0,CajaActualizada,MesaAActualizar),
  SigTablero is NumTableros-1,
  sueloACajaAux(SigTablero,TablerosActualizados0,MesaActualizada0,TablerosActualizados,MesaActualizada).

finRondaFactorias(Jugadores,Mesa,Tableros,Factorias,MesaFinal,TablerosActualizados):-                            %Comprueba que todas las factorias esten vacias, y actualiza la mesa y
  length(Factorias,NumFactorias),                                                                                %suelos, añade si es necesario la caja a la bolsay genera las nuevas factorias
  fincheckFactorias(NumFactorias,Factorias),
  sueloACaja(Tableros,Mesa,TablerosActualizados,MesaCajaAnadida),
  llenarBolsa(MesaCajaAnadida,NumFactorias,MesaActualizada),
  nth1(4,MesaActualizada,Bolsa,MesaSinBolsa),
  nth1(2,MesaSinBolsa,_,MesaSinBolsaSinFactorias),
  generarFactorias(Jugadores,Bolsa,FactoriasGeneradas,BolsaRestante),
  nth1(2,MesaSinBolsaConFactorias,FactoriasGeneradas,MesaSinBolsaSinFactorias),
  nth1(4,MesaFinal,BolsaRestante,MesaSinBolsaConFactorias).

fincheckFactorias(0,Factorias):- !, true.                                                                        %Comprueba que las factorias esten vacias fin
fincheckFactorias(NumFactorias,Factorias):-                                                                      %Comprueba que las factorias esten vacias
 nth1(NumFactorias,Factorias,Factoria),
 Factoria == [],
 SigFactoria is NumFactorias-1,
 fincheckFactorias(SigFactoria,Factorias).

insercionEnTableros(0,Tableros,RestoI,TablerosFinal,RestoF):-                                                                  %Pasa lineas de reservas adecuadas a los mosaicos fin
  TablerosFinal = Tableros,
  RestoF=RestoI,!.

insercionEnTableros(Jugadores,Tableros,RestoI,TablerosFinal,RestoF):-                                                          %Pasa reservas adecuadas a los mosaicos
  añadirReservaAMosaico(Jugadores,Tableros,TablerosActualizado,RestoJugador),
  JugadorSiguiente is Jugadores-1,
  append(RestoI,RestoJugador,RestoActualizado),
  insercionEnTableros(JugadorSiguiente,TablerosActualizado,RestoActualizado,TablerosFinal,RestoF).

pasarCajaABolsa(Caja, Bolsa, CajaResultante, BolsaResultante):-                                                  %Introduce el contenido de la caja en la bolsa
  pasarCajaABolsaAux(Caja,Bolsa,1,CajaResultante,BolsaResultante).

pasarCajaABolsaAux(Caja, Bolsa, 6, CajaResultante, BolsaResultante):-                                            %Introduce el contenido de la caja en la bolsa fin AUX
  BolsaResultante=Bolsa,
  CajaResultante=[].

pasarCajaABolsaAux(Caja, Bolsa, Cont, CajaResultante, BolsaResultante):-                                         %Introduce el contenido de la caja en la bolsa AUX
  contar(Cont, Caja, NumFichas),
  append(Bolsa, [NumFichas], NuevaBolsa),
  write(NuevaBolsa),
  Cont1 is Cont+1,
  pasarCajaABolsaAux(Caja, NuevaBolsa, Cont1, CajaResultante, BolsaResultante).
