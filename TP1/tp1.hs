import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{- CONSIDERACIONES -}
{- Todas las funciones que reciban un universo tienen como requiere que el universo sea valido. A continuación, se define:
  Personaje válido:
    - Si el personaje muere, no puede moverse.
  Objeto válido:
    - un objeto destruido no puede ser tomado
    - un objeto no puede ser tomado por un personaje muerto
    - un objeto solo puede ser tomado por un personaje de su universo
  Universo válido:
    - No contiene objetos con nombre repetidos (el string del constructor es único, y no hay dos objetos con esto igual)
    - No contiene personajes con nombres repetidos (misma aclaracion que punto anterior)
    - los personajes y objetos de un universo válido, son válidos
    - la relación entre objetos y personanje cumple lo dicho abajo (en aclaraciones)
  Relacion entre Objetos y Personajes (aclaraciones)
    - Un personaje muerto mantiene sus objetos tomados
    - Un objeto libre que es destruido no puede se le puede aplicar ningún otro constructor
  Thanos
    - Si thanos está muerto, se gana
-}

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o) 

{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> a) -> ( a -> Dirección -> a) -> (a -> a) -> Personaje -> a 
foldPersonaje fPersonaje fMueve fMuere p = case p of
    Personaje pos name -> fPersonaje pos name
    Mueve per dir -> fMueve  (recFold per) dir
    Muere per -> fMuere (recFold per)
  where recFold = foldPersonaje fPersonaje fMueve fMuere

foldObjeto :: (Posición -> String -> a) -> (a -> Personaje -> a) -> (a -> a) -> Objeto  -> a
foldObjeto fObjeto fTomado fEsDestruido obj = case obj of
    Objeto pos name -> fObjeto pos name
    Tomado obj per -> fTomado (recFold obj) per
    EsDestruido obj -> fEsDestruido (recFold obj)
  where recFold = foldObjeto fObjeto fTomado fEsDestruido

{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje const siguiente_posición id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (flip const) const id

{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en = filterMap es_un_objeto objeto_de

personajes_en :: Universo -> [Personaje]
personajes_en = filterMap es_un_personaje personaje_de

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap f g = foldr (\elem rec -> if f elem then g elem : rec else rec) []

{- Para devolver listas de Objeto's o Personaje's, estas funciones
 - van filtrando por elementos que sean de tipo "(Left|Right) *" correspondientemente. Al mismo tiempo, se 
 - les va aplicando la función (personaje|objeto)_de para 'sacarles el tipo Either' 
 - a los elementos del resultado.
 -
 - Usamos filterMap porque ayuda a aumentar la legibilidad.
 -}

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de p u = foldr(\elem rec -> if (en_posesión_de p elem) then elem:rec else rec) [] (objetos_en u)

{-Ejercicio 5-}
-- Asume que hay al menos un objeto
objeto_libre_mas_cercano :: Universo -> Personaje -> Objeto
objeto_libre_mas_cercano u p = fst $ foldl (\(fst_free_obj, distance) obj ->
    let distA = distancia (Left p) (Right obj) in
    if distA < distance then (obj, distA) else (fst_free_obj, distance))
    (fst_free_obj, distance) free_obj
  where
    distance = distancia (Left p) (Right fst_free_obj)
    free_obj = objetos_libres_en u
    fst_free_obj = head (objetos_libres_en u)

{-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = está_el_personaje "Thanos" u && gemas_de_thanos == 6
  where
    gemas_de_thanos = length (filter es_una_gema objetos_de_thanos)
    objetos_de_thanos = objetos_en_posesión_de "Thanos" u

{- "tiene_thanos..." calcula los objetos en posesión de Thanos y obtiene aquellos que sean gemas (que se
 - llamen "Gema de..."). Luego, se devuelve si Thanos tiene exactamente 6 gemas del infinito.
 - Obviamente, todo esto se realiza 'sii' él está, vivo, en el universo.
 -}

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = (not (tiene_thanos_todas_las_gemas u)  &&
                      ((thor && stormBreaker) || (wanda && vision && gemaDeLaMente))) || not (está_el_personaje "Thanos" u)
  where
	thor = está_el_personaje "Thor" u 
	stormBreaker = está_el_objeto "StormBreaker" u && en_posesión_de "Thor" (objeto_de_nombre "StormBreaker" u)
	wanda = está_el_personaje "Wanda" u
	vision = está_el_personaje "Vision" u
	gemaDeLaMente = está_el_objeto "Gema de la Mente" u && en_posesión_de "Vision" (objeto_de_nombre "Gema de la Mente" u)

{-Tests-}
main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

--Personajes
phil = Personaje (0,0) "Phil"
groot = Personaje(1,2) "Groot"
cap = Personaje (2,1) "cap"
iron_man = Personaje (10,22) "iron man"
thanos = Personaje (10,100) "Thanos"
vision = Mueve (Personaje (20,20) "Vision") Norte
wanda = Personaje (0,0) "Wanda"
thor = Personaje (4,4) "Thor"
capitanEmpanada = Personaje (100,100) "Capitan Empanada"
gabi = Personaje (19,19) "gabi"
mario = Personaje (1203,3030) "mario"
personajePrueba = Personaje (0, 0) "personaje1"
joaco = Mueve (Personaje (777,777) "joaco") Norte
pepe_rip = Muere (Personaje (1919293,929393) "pepe")

--Objetos
mark_12 = Tomado (Objeto (100,100) "Mark 12") iron_man
lentes = Tomado (Objeto (3,3) "lentes") iron_man
escudo = Tomado (Objeto (22,2) "escudo") cap
paleta_dhs = Tomado (Objeto (20,20) "paleta dhs") mario
zapas_joma = Tomado (Objeto (10,2) "zapas_joma") mario
mjölnir = Objeto (2,2) "Jonathan"
ojoDeUatu = Objeto(2,2) "Ojo de Uatu"
capaDrStrange = Objeto (8, 4) "Capa del Dr Strange"
tesseract = Objeto (2,18) "Tesseract"
empanda_de_carne = Tomado (Objeto (120,102) "empanada de carne") capitanEmpanada
empanda_de_pollo = Tomado (Objeto (101,101) "empanada de pollo") capitanEmpanada
empanada_de_humita = Tomado (Objeto (101,103) "empanada de humita") capitanEmpanada
microfono = Tomado (Objeto (19,20) "microfono") gabi
stormBreaker = Tomado (Objeto (4,4) "StormBreaker") thor
gema_de_la_empanada = Objeto (1010,2020) "Gema de la Empanada"
gema_de_la_menteVision = Tomado (Objeto (0,0) "Gema de la Mente") vision
gema_de_la_menteThanos = Tomado (Objeto (0,0) "Gema de la Mente") thanos
gema_del_tiempo = Tomado (Objeto (0,0) "Gema del Tiempo") thanos
gema_del_espacio = Tomado (Objeto (0,0) "Gema del Espacio") thanos
gema_del_alma = Tomado (Objeto (0,0) "Gema del Alma") thanos 
gema_de_la_realidad = Tomado (Objeto (0,0) "Gema de la Realidad") thanos
gema_del_poder = Tomado (Objeto (0,0) "Gema del Poder") thanos
mesa_ping_pong = Tomado (Objeto (1000,1000) "mesa de ping pong") mario
pelotita_donic = Tomado (Objeto (10000,10000) "pelotita donic") mario
paleta_sensei = Tomado (Objeto (159,159) "paleta sensei 4 estrellas") mario
mesa_c25 = Tomado (Objeto (100000,100000) "MESA ALMAR C5") mario
hurricane_3 = Tomado (Objeto (1000,10000) "hurricane 3") mario
short = Tomado (Objeto (392,201) "short") joaco
escudo_destruido = EsDestruido escudo
mascara = EsDestruido (Objeto (1919,3493) "mascara")
pepometro = Tomado (Objeto(666,666) "PEPOMETRO") pepe_rip
tiempo = Tomado (Objeto (-1000000,999999) "tiempo") pepe_rip

--Universos
universoPrueba = [Right (Objeto (2, 3) "obj1"), Right (Objeto (1, 2) "obj2"), Right (Objeto (0, 1) "obj3")]
uniPong = universo_con [phil,cap,iron_man,mario,gabi,capitanEmpanada] [mark_12,lentes,escudo,paleta_dhs,zapas_joma,microfono,empanda_de_carne]
universo_obj_libres_unico = universo_con [phil, thor, cap, groot] [tesseract, ojoDeUatu, escudo, capaDrStrange, stormBreaker]
universo_obj_libres_dos = universo_con [groot, thor, cap, groot] [tesseract, ojoDeUatu, escudo, capaDrStrange, stormBreaker, mjölnir]
universo_obj_libres_y_tomados = universo_con [groot] [gema_del_poder, tesseract]
full_ping_pong = universo_con [mario,iron_man,joaco,gabi,thor, capitanEmpanada, thanos] [hurricane_3, paleta_dhs, mesa_c25, pelotita_donic, mesa_ping_pong, paleta_sensei,zapas_joma]
universo_sin_empanadas = universo_con [capitanEmpanada, joaco, mario] [hurricane_3,short,mesa_c25]
universo_cosas_rotas = universo_con [cap,joaco, capitanEmpanada] [mascara, escudo, empanada_de_humita, short,escudo_destruido, empanda_de_carne]
uni_rip_pepe = universo_con [iron_man, pepe_rip, mario, capitanEmpanada] [lentes, pepometro, paleta_dhs, empanada_de_humita, mark_12, tiempo]

--Universos relacionados a Thanos
universo_sin_thanos = universo_con [phil] [mjölnir]
universo_thanos_win = universo_con [thanos, thor] [stormBreaker, gema_de_la_menteThanos, gema_de_la_realidad, gema_del_alma, gema_del_espacio, gema_del_poder, gema_del_tiempo]
universo_thanos_win_dead_thor = universo_con [thanos,(Muere thor), vision] [stormBreaker, gema_de_la_menteVision, gema_de_la_realidad, gema_del_alma, gema_del_espacio, gema_del_poder, gema_del_tiempo]
universo_thanos_lose1 = universo_con [thanos, thor, vision] [stormBreaker, gema_de_la_menteVision, gema_de_la_realidad, gema_del_alma, gema_del_espacio, gema_del_poder, gema_del_tiempo]
universo_thanos_lose2 = universo_con [thanos, wanda, vision] [gema_de_la_menteVision, gema_de_la_realidad, gema_del_alma, gema_del_espacio, gema_del_poder, gema_del_tiempo]
universo_gema_rota = universo_con [thor, phil, vision, thanos] [(Tomado empanada_de_humita thanos),gema_de_la_menteThanos, gema_de_la_realidad, gema_del_alma, gema_del_espacio, gema_del_poder, (EsDestruido gema_del_tiempo), (Tomado mjölnir thanos)]
universo_faltan_gemas = universo_con [thor, phil, vision, thanos] [(Tomado empanada_de_humita thanos),gema_de_la_menteThanos, gema_de_la_realidad, gema_del_alma, gema_del_espacio, gema_del_poder, (Tomado mjölnir thanos)]
universo_thanos_muerto = universo_con [(Muere thanos), thor] [stormBreaker, gema_de_la_menteThanos, gema_de_la_realidad, gema_del_alma, gema_del_espacio, gema_del_poder, gema_del_tiempo]

--Función para los tests del Ej 1
fRecursiva :: Either Personaje Objeto -> Int
fRecursiva x = case x of
	(Left p) -> foldPersonaje fInit fMid fEnd p
	(Right o) -> foldObjeto fInit fMid fEnd o
  where
	fInit = \_ _ -> 0
	fMid = \r _ -> r+2
	fEnd = \r -> r+1
{- Usamos fRecursiva para ver que la recursión tanto en objetos como personajes
 - funcione en instancias distintas.
 -}

testsEj1 = test [ -- Casos de test para el ejercicio 1
  fRecursiva (Left phil) -- personaje a secas
  	~=? 0,
  fRecursiva (Left(Muere phil)) -- personaje muerto
  	~=? 1,
  fRecursiva (Left (Mueve phil Norte)) -- personaje movido
  	~=? 2,
  fRecursiva (Left (Muere (Mueve phil Sur))) -- todos
  	~=? 3,
  fRecursiva (Right mjölnir) -- objeto a secas
  	~=? 0,
  fRecursiva (Right (EsDestruido mjölnir)) -- objeto destruido
  	~=? 1,
  fRecursiva (Right (Tomado mjölnir phil)) -- objeto tomado
  	~=? 2,
  fRecursiva (Right (EsDestruido (Tomado mjölnir phil))) -- todos
  	~=? 3
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil
    ~=? (0,0),
  posición_personaje (Mueve phil Norte)
    ~=? (0,1),
  posición_personaje (Muere phil)
    ~=? (0,0),
  posición_personaje (Muere (Mueve phil Sur))
    ~=? (0,-1),
  nombre_objeto mjölnir
    ~=? "Jonathan",
  nombre_objeto (Tomado mjölnir phil)
    ~=? "Jonathan",
  nombre_objeto (EsDestruido mjölnir)
    ~=? "Jonathan",
  nombre_objeto (EsDestruido (Tomado mjölnir phil))
    ~=? "Jonathan"
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []      
    ~=? [],           
  objetos_en [Left gabi, Left thanos, Left thor, Left iron_man]  ~=? [],
  objetos_en [Right empanada_de_humita, Right empanda_de_carne, Right empanda_de_pollo] ~=? [empanada_de_humita, empanda_de_carne, empanda_de_pollo],
  objetos_en uniPong ~=? [mark_12,lentes,escudo,paleta_dhs,zapas_joma,microfono,empanda_de_carne],

  personajes_en [] ~=? [],
  personajes_en [Left gabi, Left thanos, Left thor, Left iron_man]  ~=? [gabi, thanos, thor, iron_man],
  personajes_en [Right empanada_de_humita, Right empanda_de_carne, Right empanda_de_pollo] ~=? [],
  personajes_en uniPong ~=? [phil,cap,iron_man,mario,gabi,capitanEmpanada]
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "mario" uniPong       -- Caso de test 1 - personaje con objetos en el universo y otros objetos
    ~=? [Tomado (Objeto (20.0,20.0) "paleta dhs") (Personaje (1203.0,3030.0) "mario"),
         Tomado (Objeto (10.0,2.0) "zapas_joma") (Personaje (1203.0,3030.0) "mario")],    
  objetos_en_posesión_de "mario" full_ping_pong --podria ir todos los objetos del universo
    ~=? [Tomado (Objeto (1000.0,10000.0) "hurricane 3") (Personaje (1203.0,3030.0) "mario"),
         Tomado (Objeto (20.0,20.0) "paleta dhs") (Personaje (1203.0,3030.0) "mario"),
         Tomado (Objeto (100000.0,100000.0) "MESA ALMAR C5") (Personaje (1203.0,3030.0) "mario"),
         Tomado (Objeto (10000.0,10000.0) "pelotita donic") (Personaje (1203.0,3030.0) "mario"),
         Tomado (Objeto (1000.0,1000.0) "mesa de ping pong") (Personaje (1203.0,3030.0) "mario"),
         Tomado (Objeto (159.0,159.0) "paleta sensei 4 estrellas") (Personaje (1203.0,3030.0) "mario"),
         Tomado (Objeto (10.0,2.0) "zapas_joma") (Personaje (1203.0,3030.0) "mario")],
  objetos_en_posesión_de "Capitan Empanada" universo_sin_empanadas
    ~=? [],
  objetos_en_posesión_de "cap" universo_cosas_rotas
    ~=? [escudo],
  objetos_en_posesión_de "pepe" uni_rip_pepe
  ~=?  [Tomado (Objeto (666.0,666.0) "PEPOMETRO") (Muere (Personaje (1919293.0,929393.0) "pepe")),
        Tomado (Objeto (-1000000.0,999999.0) "tiempo") (Muere (Personaje (1919293.0,929393.0) "pepe"))] 
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano [Right mjölnir, Left phil] phil     
    ~=? mjölnir,                                    
  objeto_libre_mas_cercano universo_obj_libres_unico groot   
    ~=? ojoDeUatu,					    
  objeto_libre_mas_cercano universo_obj_libres_dos groot      
    ~=? ojoDeUatu,					   
    objeto_libre_mas_cercano universo_obj_libres_y_tomados groot ~=? tesseract
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos
    ~=? False,
  tiene_thanos_todas_las_gemas universo_thanos_muerto
    ~=? False,
  tiene_thanos_todas_las_gemas universo_thanos_win
    ~=? True,
  tiene_thanos_todas_las_gemas universo_gema_rota
    ~=? False,
  tiene_thanos_todas_las_gemas universo_faltan_gemas
    ~=? False
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos
    ~=? True,
  podemos_ganarle_a_thanos universo_thanos_win ~=? False,
  podemos_ganarle_a_thanos universo_thanos_win_dead_thor ~=? False,
  podemos_ganarle_a_thanos universo_thanos_lose1 ~=? True,
  podemos_ganarle_a_thanos universo_thanos_lose2 ~=? True
  ]
