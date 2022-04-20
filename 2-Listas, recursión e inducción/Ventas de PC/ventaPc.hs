{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
vendedores :: [[Char]]
vendedores = ["Martin", "Diego", "Claudio", "Jose"]

ventas :: [((Integer, Integer, Integer), [Char], [[Char]])]
ventas = [((1,2,2006), "Martin", ["Monitor GPRS 3000", "Motherboard ASUS 1500"]),((1,2,2006), "Diego", ["Monitor ASC 543", "Motherboard Pindorcho"]),((10,2,2006), "Martin", ["Monitor ASC 543", "Motherboard ASUS 1200"]),((12,2,2006), "Diego", ["Monitor GPRS 3000", "Motherboard ASUS 1200"]),((4,3,2006), "Diego", ["Monitor GPRS 3000", "Motherboard ASUS 1500"])]

precios :: [([Char], Integer)]
precios = [("Monitor GPRS 3000", 200), ("Motherboard ASUS 1500", 120), ("Monitor ASC 543", 250),("Motherboard ASUS 1200", 100), ("Motherboard Pindorcho", 30)]

-- a) precioMaquina, recibe una lista de componentes, devuelve el precio de la máquina que se puede armar
-- con esos componentes, que es la suma de los precios de cada componente incluido.
-- Por ejemplo: precioMaquina [“Monitor GPRS 3000”, “Motherboard ASUS 1500”] = 320
-- ($200 del monitor mas $120 del motherboard)

precio:: [([Char], Integer)] -> String -> Integer
precio [] item = 0
precio ((nombre,valor):xs) item
    | nombre == item = valor
    | otherwise = precio xs item

precioItm :: String -> Integer
precioItm item = precio precios item 

precioMaquina :: [String] -> Integer
precioMaquina [] = 0
precioMaquina (x:xs) = precioItm x + precioMaquina xs

-- b) cantVentasComponente, recibe un componente y devuelve la cantidad de veces que fue vendido, o
-- sea que formó parte de una máquina que se vendió.
-- Por ejemplo: cantVentasComponente “Monitor ASC 543” = 2
-- La lista de ventas no se pasa por parámetro, se asume que está identificada por ventas.

-- c) vendedorDelMes, se le pasa un par que representa (mes,ano) y devuelve el nombre del vendedor que
-- mas vendio en plata en el mes. O sea no cantidad de ventas, sino importe total de las ventas. El
-- importe de una venta es el que indica la funcion precioMaquina.
-- Por ejemplo: vendedorDelMes (2,2006) = “Martin”
-- (Vendio por $670, una maquina de $320 y otra de $350)

-- d) Obtener las ventas de un mes, de forma que:
-- ventasMes (2,2006) = 1050

-- e) Obtener las ventas totales realizadas por un vendedor sin lımite de fecha, de forma que:
-- ventasVendedor “Diego” = 900

-- f) huboVentas que indica si hubo ventas en un mes y ano determinados.
-- Por ejemplo: huboVentas (1, 2006) = False
