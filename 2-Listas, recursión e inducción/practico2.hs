{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NPlusKPatterns #-}
import Distribution.Simple.Utils (xargs)
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

-----------------------------------------------------------------
----   RECURSIÓN  -----------------------------------------------
-----------------------------------------------------------------

-----------------------------------------
-- Funciones recursivas típicas         |
-----------------------------------------

factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Integral p => p -> p
factorial' 0 = 1
factorial' (n+1) = (n + 1) * factorial n

sumatoria :: (Eq p, Num p) => p -> p
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)

potencia :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
potencia _ 0 = 1
potencia base 1 = base
potencia base expon = base * potencia base (expon-1)

----------------------------------
-- Funciones de tipo FILTER      |
----------------------------------

--a) soloPares : [Int] → [Int], que dada una lista de enteros xs devuelve una lista sólo con los números
--   pares contenidos en xs, en el mismo orden y con las mismas repeticiones (si las hubiera).
--   Por ejemplo: soloPares.[3, 0, −2, 12] = [0, −2, 12]

soloPares:: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs)         
    | even x = x: soloPares xs
--  | x `mod` 2 == 0 = x: soloPares xs
    | otherwise = soloPares xs

-- b) mayoresQue10 : [Int] → [Int], que dada una lista de enteros xs devuelve una lista sólo con los números
--    mayores que 10 contenidos en xs,
--    Por ejemplo: mayoresQue10.[3, 0, −2, 12] = [12]

mayoresQue10:: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x:xs)
    | x > 10 = x : mayoresQue10 xs
    | otherwise = mayoresQue10 xs

--c) mayoresQue : Int → [Int] → [Int], que dado un entero n y una lista de enteros xs devuelve una lista
--   sólo con los números mayores que n contenidos en xs,
--   Por ejemplo: mayoresQue,2.[3, 0, −2, 12] = [3, 12]

mayoresQue :: Ord a => a -> [a] -> [a]
mayoresQue _ [] = []
mayoresQue n (x:xs)
    | x > n = x : mayoresQue n xs
    | otherwise = mayoresQue n xs

-- d) ¿Se te ocurren otros ejemplos de funciones filter?

-- Sí :

-- Ejemplo 1: Función que dada una lista de tuplas [(nombre de producto, precio), (nombre de producto, precio)] devuelva solo aquellas que cuesten
--            n pesos o más
listaProductos :: [([Char], Integer)]
listaProductos = [("monitor", 12000), ("mouse", 3000), ("teclado", 11000), ("auriculares", 13000), ("mousepad", 5500)]

desdePesos :: Ord t => t -> [(a, t)] -> [(a, t)]
desdePesos _ [] = []
desdePesos n ((producto, precio):xs)
    | precio >= n = (producto, precio) : desdePesos n xs
    | otherwise = desdePesos n xs

-- Ejemplo 2: Funcion que dada una lista de tuplas [(pelicula, categoria)] y una categoria devuelva una lista con las peliculas que sean de
--            dicha categoria
listaPeliculas :: [([Char], [Char])]
listaPeliculas = [("Red", "infantil"), ("Interstellar", "Ciencia Ficcion"), ("Animales Fantasticos", "fantasia"), ("El Origen", "Ciencia Ficcion"), ("Son como ninios", "comedia"), ("Click", "comedia"), ("Harry Potter I", "fantasia"), ("Shrek", "infantil")]

deCategoria :: Eq t => t -> [(a, t)] -> [a]
deCategoria _ [] = []
deCategoria categ ((pelicula, categoria): xs)
    | categ == categoria = pelicula : deCategoria categ xs
    | otherwise = deCategoria categ xs

----------------------------------
-- Funciones de tipo map         |
----------------------------------

-- a ) sumar1 : [Int] → [Int], que dada una lista de enteros le suma uno a cada uno de sus elementos.
--   Por ejemplo: sumar1.[3, 0, −2] = [4, 1, −1]

sumar1 :: Num a => [a] -> [a]
sumar1 [] = []
sumar1 (x:xs) = x + 1 : sumar1 xs

-- b) duplica : [Int] → [Int], que dada una lista de enteros duplica cada uno de sus elementos.
--    Por ejemplo: duplica.[3, 0, −2] = [6, 0, −4]

duplica :: Num a => [a] -> [a]
duplica [] = []
duplica (x:xs) = x*2 : duplica xs

-- c) multiplica : Int → [Int] → [Int], que dado un número n y una lista, multiplica cada uno de los
--    elementos por n.
--    Por ejemplo: multiplica.3.[3, 0, −2] = [9, 0, −6]

multiplicaPor :: Num a => a -> [a] -> [a]
multiplicaPor _ [] = []
multiplicaPor n (x:xs) = x * n : multiplicaPor n xs

-- d) Se te ocurre otra funcion de este tipo? 

-- Si : 

absoluto :: Num a => [a] -> [a]
absoluto [] = []
absoluto (x:xs) = abs x : absoluto xs

----------------------------------
-- Funciones de tipo fold        |
----------------------------------

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Lista vacía, no tiene máximo"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise   = maxTail
    where maxTail = maximum' xs

-- a) todosMenores10 : [Int] → Bool, que dada una lista devuelve True si ésta consiste sólo de números
--    menores que 10.

todosMenores10 :: (Ord a, Num a) => [a] -> Bool
todosMenores10 [x] = x < 10
todosMenores10 (x:xs)
    | x < 10 = todosMenores10 xs
    | x >= 10 = False

-- b) hay0 : [Int] → Bool, que dada una lista decide si existe algún 0 en ella.
hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs)
    | x == 0 = True
    | otherwise = hay0 xs

-- c) sum : [Int] → Int, que dada una lista devuelve la suma de todos sus elementos.
sumaNumeros :: Num p => [p] -> p
sumaNumeros [] = 0
sumaNumeros (x:xs) = x + sumaNumeros xs

-- d) ¿Se te ocurre algún otro ejemplo de una función de este tipo?

--Toma una lista de tuplas y devuelve la sumatoria del maximo de cada tupla
sumaMax :: (Ord a, Num a) => [(a, a)] -> a
sumaMax [] = error "No tiene sentido calcularlo sobre una lista vacía"
sumaMax [(a,b)] = max a b
sumaMax ((a,b):xss) = max a b + sumaMax xss

----------------------------------
-- Funciones de tipo zip         |
----------------------------------

-- repartir : [String] → [String] → [(String, String)] donde los elementos de la
-- primera lista son nombres de personas y los de la segunda lista son cartas españolas es una función que
-- devuelve una lista de pares que le asigna a cada persona una carta.

repartir :: [String] -> [String] -> [(String, String)]
repartir [][] = []
repartir (x:xs)(y:ys) = (x,y) : repartir xs ys

----------------------------------
-- Funciones de tipo unzip       |
----------------------------------

-- lista de ternas donde el primer elemento representa
-- el nombre de un alumno, el segundo el apellido, y el tercero la edad, la función que devuelve la lista
-- de todos los apellidos de los alumnos en una de tipo unzip.
listaConNomApeEda :: [([Char], [Char], Integer)]
listaConNomApeEda  = [("Juan", "Dominguez", 22), ("Maria", "Gutierrez", 19), ("Damian", "Rojas", 18)]

apellidos :: [(a, b, c)] -> [b] --Pendiente como restringir el tipo de a b y c en este caso...
apellidos [] = []
apellidos ((_,apellido,_):xs) = apellido : apellidos xs
--apellidos ((nombre,apellido,edad):xs) = apellido : apellidos xs

-------------------------------------------------------
--          OPERADORES BÁSICOS SOBRE LISTAS           |
-------------------------------------------------------

-- # : Cardinal 

cardinal:: [a] -> Int 
cardinal [] = 0
cardinal (x:xs) = 1 + cardinal xs

-- ! : Índice - !! en Haskell

indice:: [a] -> Int -> a
indice [] n = error "Fuera de rango. \n"
indice (x:xs) 0 = x
--indice (x:xs) (n+1) = indice xs n
indice (x:xs) n = indice xs (n-1)

-- ⬆ : Tomar - take en haskell
tomar:: [a] -> Int -> [a]
tomar xs 0 = []
tomar [] n = []
tomar (x:xs) n = x : tomar xs (n-1)

-- ⬇ : Tirar - drop en haskell
tirar:: [a] -> Int -> [a]
tirar [] n = []
tirar xs 0 = xs
tirar (x:xs) n = tirar xs (n-1)

-- ++ : Concatenar
concatenar:: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys

-- ◀ : Snoc (pegar a la derecha)
snoc:: [a] -> a -> [a]
snoc xs x = xs ++ [x]


-------------------------------------------------------
--          MÁS FUNCIONES RECURSIVAS                  |
-------------------------------------------------------

-- a) maximo : [Int] → Int, que calcula el máximo elemento de una lista de enteros.
-- Por ejemplo: maximo.[2, 5, 1, 7, 3] = 7

--tipo fold
maximo:: [Int] -> Int 
maximo [] = error "No hay maximo del vacio \n"
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

-- b) sumaPares : [(Num, Num)] → Num, que dada una lista de pares de números, devuelve la sumatoria
-- de todos los números de todos los pares.
-- Por ejemplo: sumaPares.[(1, 2)(7, 8)(11, 0)] = 29

--Tipo fold
sumaPares :: Num p => [(p, p)] -> p
sumaPares [] = 0
sumaPares ((x,y):xs) = x + y + sumaPares xs

-- c) todos0y1 : [Int] → Bool, que dada una lista devuelve True si ésta consiste sólo de 0s y 1s.
-- Por ejemplo: todos0y1.[1, 0, 1, 2, 0, 1] = False, todos0y1.[1, 0, 1, 0, 0, 1] = True

--Tipo fold
todos0y1:: [Int] -> Bool 
todos0y1 [] = error "Esta lista esta vacia, no tiene sentido el calculo \n"
todos0y1 [x] = x == 0 || x == 1
todos0y1 (x:xs)
    | x == 1 || x == 0 = todos0y1 xs
    | otherwise = False

-- d) quitar0s : [Int] → [Int] que dada una lista de enteros devuelve la lista pero quitando todos los ceros.
-- Por ejemplo quitar0s.[2, 0, 3, 4] = [2, 3, 4]

--Tipo filter
quitar0s:: [Int] -> [Int]
quitar0s [] = []
quitar0s (x:xs) 
    | x == 0 = quitar0s xs
    | x /= 0 = x : quitar0s xs

-- e) ultimo : [A] → A, que devuelve el último elemento de una lista.
-- Por ejemplo: ultimo.[10, 5, 3, 1] = 1

--Tipo filter
ultimo:: [a] -> a
ultimo [] = error "Una lista vacia no tiene ultimo elemento...\n"
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- f) repetir : Num → Num → [Num], que dado un número n mayor o igual a 0 y un número k arbitrario
-- construye una lista donde k aparece repetido n veces.
-- Por ejemplo: repetir,3,6 = [6, 6, 6]

-- //! Tipo ______?????? tipo constructor ?¿ xd
repetir ::  Float -> Int -> [Float]
repetir _ 0 = []
repetir n k = n : repetir n (k-1)

-- g) concat : [[A]] → [A] que toma una lista de listas y devuelve la concatenación de todas ellas.
-- Por ejemplo: concat.[[1, 4], [], [2]] = [1, 4, 2]

--Tipo unzip
concatListaDeListas:: [[a]] -> [a]
concatListaDeListas [] = []
concatListaDeListas (xs:xss) = xs ++ concatListaDeListas xss

-- h) rev : [A] → [A] que toma una lista y devuelve una lista con los mismos elementos pero en orden
-- inverso.
-- Por ejemplo: rev.[1, 2, 3] = [3, 2, 1]

--Tipo map
rev:: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

--------------Extra---------------------------------------

-- a) listasIguales : [A] → [A] → Bool, que determina si dos listas son iguales, es decir, contienen los
-- mismos elementos en las mismas posiciones respectivamente.
-- Por ejemplo: listasIguales.[1, 2, 3].[1, 2, 3] = True, listasIguales.[1, 2, 3, 4].[1, 3, 2, 4] = False

--Tipo fold
listasIguales :: Eq a => [a] -> [a] -> Bool
listasIguales [][] = True
listasIguales [x] [y] = x == y 
listasIguales (x:xs)(y:ys)
    | length (x:xs) /= length (y:ys) = False 
    | x == y = listasIguales xs ys
    | x /= y = False

-- b) mejorNota : [(String,Int,Int,Int)] → [(String,Int)], que selecciona la nota más alta de cada alumno.
-- Por ejemplo: mejorNota.[(“Matias”,7,7,8),(“Juan”,10,6,9),(“Lucas”,2,10,10)] =
-- [(“Matias”,8),(“Juan”,10),(“Lucas”,10)].

--Tipo unzip
mejorNota:: [(String,Int,Int,Int)] -> [(String,Int)]
mejorNota [] = []
mejorNota ((alumno,nota1,nota2,nota3):xs) = (alumno, max nota1 (max nota2 nota3)) : mejorNota xs


-- c) incPrim : [(Int,Int)] → [(Int,Int)], que dada una lista de pares de enteros, le suma 1 al primer
-- número de cada par.
-- Por ejemplo: incPrim.[(20, 5),(50, 9)] = [(21, 5),(51, 9)], incPrim.[(4, 11),(3, 0)] = [(5, 11),(4, 0)].

--Tipo map
incPrim:: [(Int,Int)]->[(Int,Int)]
incPrim [] = []
incPrim ((a,b):xs) = (succ a, b) : incPrim xs

-- d) expandir : String → String, pone espacios entre cada letra de una palabra.
-- Por ejemplo: expandir."hola" = "h o l a" (¡sin espacio al final!).

--Tipo map
expandir:: String -> String
expandir "" = ""
expandir [x] = [x]
expandir (x:xs) = (x : " ") ++ expandir xs

--------------------------------------------

sumaListas:: [[Int]] -> [Int]
sumaListas [[]] = [0]
sumaListas (xs: xss) = sum xs : sumaListas xss

ordena :: Ord b => [(b, b)] -> [(b, b)]
ordena [] = []
ordena ((x,y) : xs) 
    | x >= y = (y,x) : ordena xs
    | x < y = (x,y) : ordena xs

cambiarVporB :: [Char] -> [Char]
cambiarVporB "" = ""
cambiarVporB (x:xs)
    | x == 'v' = 'b' : cambiarVporB xs
    | x /= 'v' = x : cambiarVporB xs
