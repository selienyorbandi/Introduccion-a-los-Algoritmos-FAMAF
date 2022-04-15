{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NPlusKPatterns #-}
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


--------------Extra--------

sumaListas:: [[Int]] -> [Int]
sumaListas [[]] = [0]
sumaListas (xs: xss) = sum xs : sumaListas xss



