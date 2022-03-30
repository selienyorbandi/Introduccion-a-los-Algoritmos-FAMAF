{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

---------------------------------------------------------------------------------------
-- Funciones definidas por partes                                                     
--------------------------------------------------------------------------------------- 

signo :: (Ord a, Num a) => a -> Char
signo x 
    | x > 0 = '+'
    | x == 0 = '0'
    | x < 0 = '-'
    | otherwise = 'n'

---------------------------------------------------------------------------------------
-- (a) entre0y9 :  Int → Bool, que dado un entero devuelve True si el entero se encuentra entre 0 y 9.

entre0y9:: Int -> Bool
entre0y9 m
    | m >= 0 && m <= 9 = True
    | otherwise = False

---------------------------------------------------------------------------------------
-- (b) rangoPrecio : Int → String, que dado un número que representa el precio de una computadora,
--     retorne “muy barato” si el precio es menor a 2000, “demasiado caro” si el precio es mayor que 5000,
--     “hay que verlo bien” si el precio está entre 2000 y 5000, y “esto no puede ser!” si el precio es negativo.

rangoPrecio:: Int -> String
rangoPrecio precio
    | precio < 0 = "Esto no puede ser!"
    | precio < 2000 = "Muy barato"
    | precio >= 2000 && precio <= 5000 = "Hay que verlo bien"
    | precio > 5000 = "Demasiado caro"

---------------------------------------------------------------------------------------
-- (c) absoluto : Int → Int, que dado un entero retorne su valor absoluto.

absoluto:: Int -> Int
absoluto x
    | x < 0 = -x
    | x >= 0 = x

---------------------------------------------------------------------------------------
-- (d) esMultiplo2 : Int → Bool, que dado un entero n devuelve True si n es múltiplo de 2.
-- Ayuda: usar mod, el operador que devuelve el resto de la división.

esMultiplo2:: Int -> Bool
esMultiplo2 a
--  | a `mod` 2 == 0 = True
    | even a = True               -- even x ya es la funcion nativa de Haskell para determinarlo
    | otherwise = False

---------------------------------------------------------------------------------------
-- Se pueden combinar funciones, algunas utiles: mod a b (resto de a / b); div a v (division entera); max y min
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
-- 19. Definir la función esMultiploDe : Num→ Num→ Bool , que devuelve True si el segundo es múltiplo del
--     primero. Ejemplo: esMultiploDe 3 12 = True.

-- esMultiploDe:: Num -> Num -> Bool
esMultiploDe :: Integral a => a -> a -> Bool
esMultiploDe k n
    | mod n k == 0 = True
    | otherwise = False

---------------------------------------------------------------------------------------
-- 20. Definir la función esBisiesto: Num→ Bool , que indica si un año es bisiesto. Un año es bisiesto si es
--     divisible por 400 o es divisible por 4 pero no es divisible por 100.

--esBisiesto:: Num -> Bool
esBisiesto :: Integral a => a -> Bool
esBisiesto anio 
    | anio `mod` 400 == 0 = True
    | anio `mod` 4 == 0 && anio `mod` 100 /= 0 = True
    | otherwise = False

---------------------------------------------------------------------------------------
-- 21. Definir la función dispersion : Num→ Num→ Num→ Num, que toma los tres valores y devuelve la
--     diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las
--     versiones de dos elementos. De esa forma se puede definir dispersión sin escribir ninguna guarda (las
--     guardas están en max y min, que estamos usando).

--dispersion:: Num -> Num -> Num -> Num

dispersion :: (Num a, Ord a) => a -> a -> a -> a
dispersion num1 num2 num3 = max num1 (max num2 num3) - min num1 (min num2 num3)


---------------------------------------------------------------------------------------
-- 22. Definir la función celsiusToFahr : Num→ Num, pasa una temperatura en grados Celsius a grados
--     Fahrenheit. Para realizar la conversión hay que multiplicar por 1.8 y sumar 32.

-- celsiusToFahr:: Num -> Num
celsiusToFahr:: Float -> Float
celsiusToFahr grados = grados * 1.8 + 32

---------------------------------------------------------------------------------------
-- 23. Definir la función fahrToCelsius : Num→ Num, la inversa de la anterior. Para realizar la conversión hay
--     que primero restar 32 y después dividir por 1.8.

fahrToCelsius:: Float -> Float
fahrToCelsius grados = (grados - 32) / 1.8

---------------------------------------------------------------------------------------
-- 24. Definir la función haceFrioF : Num→ Bool , indica si una temperatura expresada en grados Fahrenheit es
--     frı́a. Decimos que hace frı́o si la temperatura es menor a 8 grados Celsius.

haceFrioF:: Float -> Bool
haceFrioF gradosFahr = fahrToCelsius gradosFahr < 8

---------------------------------------------------------------------------------------
-- 25 TUPLAS
---------------------------------------------------------------------------------------

-- (a) segundo3 : (Num, Num, Num) → Num, que dada una terna de enteros devuelve su segundo elemento.

--segundo3:: (Num, Num, Num) -> Num
--segundo3 :: (a, b, c) -> b
segundo3 :: Num a => (a, a, a) -> a
segundo3 (num1,num2,num3) = num2

---------------------------------------------------------------------------------------
-- (b) ordena : (Num, Num) → (Num, Num), que dados dos enteros los ordena de menor a mayor.
--ordena:: (Num, Num) -> (Num, Num)
ordena :: (Ord b,Num b) => (b, b) -> (b, b)
ordena (a,b) = (min a b, max a b)

---------------------------------------------------------------------------------------
--(c) rangoPrecioParametrizado : Num → (Num, Num) → String que dado un número x, que representa
--    el precio de un producto, y un par (menor, mayor) que represente el rango de precios que uno espera
--    encontrar, retorne “muy barato” si x está por debajo del rango, “demasiado caro” si está por arriba
--    del rango, “hay que verlo bien” si el precio está en el rango, y “esto no puede ser!” si x es negativo.

rangoPrecioParametrizado :: (Ord a, Num a) => a -> (a, a) -> [Char]
rangoPrecioParametrizado precio (menor, mayor)
    | precio < 0 || menor < 0 || mayor < 0 = "Esto no puede ser"
    | precio < menor = "Muy barato"
    | precio >= menor && precio <= mayor = "Hay que verlo bien"
    | precio > mayor = "Demasiado caro"

---------------------------------------------------------------------------------------
-- (d) mayor3 : (Num, Num, Num) → (Bool , Bool , Bool ), que dada una una terna de enteros devuelve una
--     terna de valores booleanos que indica si cada uno de los enteros es mayor que 3.
--     Por ejemplo: mayor3.(1, 4, 3) = (False, True, False) y mayor3.(5, 1984, 6) = (True, True, True)

mayor3:: (Num a, Ord a) => (a,a,a) -> (Bool, Bool, Bool)
mayor3 (entero1, entero2, entero3) = (entero1 > 3, entero2 > 3, entero3 > 3)

---------------------------------------------------------------------------------------
--(e) todosIguales : (Num, Num, Num) → Bool que dada una terna de enteros devuelva True si todos sus
--    elementos son iguales y False en caso contrario.
--    Por ejemplo: todosIguales.(1, 4, 3) = False y todosIguales.(1, 1, 1) = True

todosIguales :: (Eq a, Num a) => (a, a, a) -> Bool
todosIguales (entero1, entero2, entero3) = (entero1 == entero2) && (entero2 == entero3)
