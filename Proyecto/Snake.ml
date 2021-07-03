 #load "Unix.cma";; (*Esta es para que funcionen esas cosas de unix*)
 #load "Graphics.cma";; (* Esta es como digamos una funcion de unix que es un sistema operativo y es para los graficos *)
 
 open Graphics;; (*Asi haces que se abra una libreria en ocaml*)
 open Printf;;

 exception Lose;; (* Es la excepcion que arroja cuando pierde*)
 exception Win;;  (* Es la excepcion que arroja cuando gana*)
 
 Random.self_init ();; (*Para que la vibora inicie en el lugar que sea*)
 Graphics.open_graph " 640x480";; (* la resolucion del grafico*)
 
 let ancho,largo = 640,480;; (* Resolucion del grafico *)
 let Tamanio_Total = 16;;   (* Es creo que el tamaño de cuadros o sea son 16 cuadros *)
 let Tamanio_cuadro = [|640/Tamanio_Total;480/Tamanio_Total|];; (* cuanto mide el cuadro de largo, y cuando mide de ancho, porque es un rectangulo*)


 type direction = Top | Right | Down | Left;; (* Las Teclas*)
 
 type cell = Full | Empty;; 
 
 type espacio = { mutable cuadros : cell array array ;
                tamanio : int };;      (* Es el tamaño del mundo de la serpiente en matrices*)
 
 type snake = { mutable pos : (int * int) array ;
              mutable dir : direction };;   (* El tamaño de la serpiente y va aumentando y cambiando la posicion y direccion*)
 
 (**
  Hacer que el programa se pause por n segundos, o sea si ganaste o perdiste 
  *)

 let Esperando n =
   let Empezar = Unix.gettimeofday() in
     let rec delay t =
     try
       ignore (Unix.select [] [] [] t)
       with Unix.Unix_error(Unix.EINTR, _, _) ->
       let Nuevo = Unix.gettimeofday() in
         let Resto = Empezar +. n -. Nuevo in
           if Resto > 0.0 then delay Resto in
             delay n;;
 

 (**
  
  Iniciar el campo vacio y la posicion de la serpiente
  *)
 let Iniciar tamanio =
   { cuadros = Array.make_matrix tamanio tamanio Empty ;
     tamanio = tamanio },
   { pos = [|(tamanio/2,tamanio/2);(tamanio/2,tamanio/2+1)|] ;
       dir = Top };;
 
 (**
   Checa si la serpiente no se comio asi misma
  *)
 let rec bite (x,y) snake i =
   if i = Array.length snake.pos then
     false
   else
     let a,b = snake.pos.(i) in
       if a=x && b=y then
         true
       else
         bite (x,y) (snake) (i+1);;
 
 (**
   Sirve para que crezca la serpiente cuando come una manzana 
  *)
 let addApple snake espacio x y =
   if not (bite (x,y) (snake) (0)) then
     espacio.cuadros.(x).(y) <- Full;;
 
 (**
    Sirve para generar manzanas al azar en el mundito
  *)
 let rec agregarmanzana snake espacio n =
   if n > 0 then
     begin
       addApple snake espacio (Random.int espacio.tamanio) (Random.int espacio.tamanio);
       agregarmanzana snake espacio (n-1);
     end
   else
     ();;
 
 (**
     Para mostrar el rectangulo, pero habia un modo donde podias atravesar la pared y donde no
     entonces me imagino que este es donde se muestra el cuadro donde si chocas se muere
  *)
 let mostrarrect i j couleur =
     Graphics.set_color couleur;
     Graphics.fill_rect (i*Tamanio_cuadro.(0)) (j*Tamanio_cuadro.(1)) Tamanio_cuadro.(0) Tamanio_cuadro.(1);;
 
 (**
    Para mostrar graficamente la manzana
  *)
 let mostrarmanzana i j =
     Graphics.set_color 0xff0000;
     Graphics.fill_ellipse (i*Tamanio_cuadro.(0)+Tamanio_cuadro.(0)/2) (j*Tamanio_cuadro.(1)+Tamanio_cuadro.(1)/2) (Tamanio_cuadro.(0)/2) (Tamanio_cuadro.(1)/2);
     Graphics.set_color 0x00ff00;
   Graphics.fill_arc (i*Tamanio_cuadro.(0)+Tamanio_cuadro.(0)/2) (j*Tamanio_cuadro.(1)+Tamanio_cuadro.(1)/2) (Tamanio_cuadro.(0)/3) (Tamanio_cuadro.(1)/5) 180 210;
   Graphics.fill_arc (i*Tamanio_cuadro.(0)+Tamanio_cuadro.(0)/2) (j*Tamanio_cuadro.(1)+Tamanio_cuadro.(1)/2) (Tamanio_cuadro.(0)/3) (Tamanio_cuadro.(1)/5) 100 150;;
 
 (**
     Mostrar toda la cuadricula (todo el mundo)
  *)
 let mostrarespacio espacio =
   for i=0 to espacio.tamanio-1 do
     for j=0 to espacio.tamanio-1 do
       if espacio.cuadros.(i).(j) = Full then
         begin
           mostrarrect i j 0xffffff;
           mostrarmanzana i j;
         end
       else
         mostrarrect i j 0xffffff
     done
   done;;
 
 (**
    Mostrar la serpiente (en teoria esta funcion se abre cada vez que se come una manzana )
  *)
 let mostrarsnake snake =
   for i=0 to Array.length snake.pos -1 do
     let x,y = snake.pos.(i) in
       if i = Array.length snake.pos -1 then
         mostrarrect x y 0xaaff44
       else
         mostrarrect x y 0x55ff55
   done;;
 
 (**
     Esta funcion digamos, manda a llamar las dos funciones de arriba, y hace que aparezcan tanto el mapa como la serpiente
  *)
 let display snake espacio =
   mostrarespacio espacio;
   mostrarsnake snake;;
 


 (**
      Esa funcion checa las teclas que se presionan, el match que es como un if
      entonces pues son las cuatro y la ultima en caso de que sea otra cosa no haga nada
  *)
  
 let action snake k =
   match k with
   'z' -> snake.dir <- Top
   | 'q' -> snake.dir <- Left
   | 's' -> snake.dir <- Down
   | 'd' -> snake.dir <- Right
   | _ -> ();;
 
 (**
  Checa si se comio una manzana o si no se comio nada
  *)
 let comer espacio (x,y) =
   if espacio.cuadros.(x).(y) = Full then
     begin
       espacio.cuadros.(x).(y) <- Empty;  (* Aqui checa si por ejemplo se movio un pixel y no era una manzana pues sigue vacio*)
       true;
     end
   else
     false;;
 

     
 (**
     Esta funcion mueve todas las partes de la serpiente
  *)
 let moversnake (x,y) dx dy espacio snake =
   let nx,ny = x+dx,y+dy in
     if(nx<0 || nx>espacio.tamanio || ny<0 || ny>espacio.tamanio || bite (nx,ny) (snake) (0)) then
       raise Lose
     else if Array.length snake.pos >= 10 then
       raise Win
     else
       (nx,ny);;
 
 (**
  Mueve la serpiente y ejecuta acciones como moverse para la izquierda o derecha arriba o abajo
  *)
 let run snake espacio =
   match snake.dir with
     Right ->
       let n = moversnake snake.pos.(Array.length snake.pos -1) (1) (0) (espacio) (snake) in
       if comer espacio n then
         Array.append snake.pos [|n|]
       else
         Array.sub (Array.append snake.pos [|n|]) (1) (Array.length snake.pos)
     | Left ->
       let n = moversnake snake.pos.(Array.length snake.pos -1) (-1) (0) (espacio) (snake) in
       if comer espacio n then
         Array.append snake.pos [|n|]
       else
         Array.sub (Array.append snake.pos [|n|]) (1) (Array.length snake.pos)
     | Top ->
       let n = moversnake snake.pos.(Array.length snake.pos -1) (0) (1) (espacio) (snake) in
       if comer espacio n then
         Array.append snake.pos [|n|]
       else
         Array.sub (Array.append snake.pos [|n|]) (1) (Array.length snake.pos)
     | Down ->
       let n = moversnake snake.pos.(Array.length snake.pos -1) (0) (-1) (espacio) (snake) in
       if comer espacio n then
         Array.append snake.pos [|n|]
       else
         Array.sub (Array.append snake.pos [|n|]) (1) (Array.length snake.pos);;
 
 (**
    Empieza un nuevo ciclo de accion (cuando pierdes y eso o cuando ganas )
  *)
 let move snake espacio =
   snake.pos <- run snake espacio;;
 

 (**
  Empezar el juego
  *)
 let program snake espacio fKey =
   while true do
     begin
       Esperando 1.0;
       if Graphics.key_pressed() then
         fKey (snake) (Graphics.read_key());
       move snake espacio;
       display snake espacio;
     end
   done;;
 
 let espacio,snake = Iniciar Tamanio_Total;;  (* Aqui estan inicializados mundo y serpiente*)
 agregarmanzana snake espacio 10;;
 
 display snake espacio;;
 
 program snake espacio action;;
 
 read_line();; (* Aqui pues recibe el comando  la tecla de arriba y se va a la funcion donde checa cual tecla es*)

 