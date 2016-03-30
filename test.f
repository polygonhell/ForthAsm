: t1 1 2 + ;
: t2 t1 3 + ;
: t3 t2 dup + ;
: t4 S" Hello World!!" S" The End!" s" Hello World!!" ;

: main t4 ;
