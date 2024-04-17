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
    
-- Habría que preguntar si debe estar libre?? En la consigna sólo dice "el objeto más cercano" (a pesar del nombre de la función).

{-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = está_el_personaje "Thanos" u && gemas_de_thanos == 6
  where
    gemas_de_thanos = length (filter es_una_gema objetos_de_thanos)
    objetos_de_thanos = objetos_en_posesión_de "Thanos" u

{- "tiene_thanos..." calcula los objetos en posesión de Thanos y obtiene aquellos que sean gemas (que se
 - llamen "Gema de..."). Luego, se devuelve si Thanos tiene exactamente 6 gemas del infinito.
 - Obviamente, todo esto se realiza si él está, vivo, en el universo.
 -}

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = not thanosWin  &&
                      ((thor && stormBreaker) || (wanda && vision && gemaDeLaMente))
  where
  thanosWin = está_el_personaje "Thanos" u && tiene_thanos_todas_las_gemas u
  thor = está_el_personaje "Thor" u 
  stormBreaker = está_el_objeto "StormBreaker" u && en_posesión_de "Thor" (objeto_de_nombre "StormBreaker" u)
  wanda = está_el_personaje "Wanda" u
  vision = está_el_personaje "Vision" u
  gemaDeLaMente = está_el_objeto "Gema de la Mente" u && en_posesión_de "Vision" (objeto_de_nombre "Gema de la Mente" u)

{-
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

expectAny :: a -> [a] -> Bool
expectAny actual expected = elem actual expected ~? ("Se esperaba cualquiera de: " ++ show expected ++ "\n pero se recibió: " ++ show actual)
{- expectAny toma el resultado de aplicar la función y una lista de posibles soluciones al caso de test.
 - 
 - expectAny se usa en los casos de test donde puede haber "empates" y la solución puede ser cualquiera de
 - los "expected". 
 - Esta función se la acreditamos a la cátedra de IP.
 -}

-}

phil = Personaje (0,0) "Phil"
cap = Personaje (2,1) "cap"
iron_man = Personaje (10,22) "iron man"
mark_12 = Tomado (Objeto (100,100) "Mark 12") iron_man
lentes = Tomado (Objeto (3,3) "lentes") iron_man
escudo = Tomado (Objeto (22,2) "escudo") cap
paleta_dhs = Tomado (Objeto (20,20) "paleta dhs") mario
mario = Personaje (1203,3030) "mario"
zapas_joma = Tomado (Objeto (10,2) "zapas_joma") mario
mjölnir = Objeto (2,2) "Mjölnir"
gema_de_la_empanada = Objeto (1010,2020) "Gema de la Empanada"
empanda_de_carne = Tomado (Objeto (120,102) "empanada de carne") capitanEmpanada
empanda_de_pollo = Tomado (Objeto (101,101) "empanada de pollo") capitanEmpanada
empanada_de_humita = Tomado (Objeto (101,103) "empanada de humita") capitanEmpanada
capitanEmpanada = Personaje (100,100) "Capitan Empanada"
gabi = Personaje (19,19) "gabi"
microfono = Tomado (Objeto (19,20) "microfono") gabi
universo_sin_thanos = universo_con [phil] [mjölnir]
uniPong = universo_con [phil,cap,iron_man,mario,gabi,capitanEmpanada] [mark_12,lentes,escudo,paleta_dhs,zapas_joma,microfono,empanda_de_carne]

{-

Mini test Ej5
universoPrueba = [Right (Objeto (2, 3) "obj1"), Right (Objeto (1, 2) "obj2"), Right (Objeto (0, 1) "obj3")]
personajePrueba = Personaje (0, 0) "personaje1"

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
    ~=? []                             -- Caso de test 1 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ]
  -}
