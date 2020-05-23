import Text.Show.Functions()
import Data.Char

main :: IO ()
main = return ()

type Habilidad = String
type Objeto = Barbaro -> Barbaro
type Aventura = Barbaro -> Bool

data Barbaro = CrearBarbaro { 
                        nombre :: String,
                        fuerza :: Int,
                        habilidades :: [Habilidad],
                        objetos :: [Objeto]
                } deriving (Show)

dave :: Barbaro
dave = CrearBarbaro "Dave" 100 ["tejer","escribirPoesia", "tejer", "tejer"] [ardilla,megafono {--,libroPedKing--}]

jorge :: Barbaro
jorge = CrearBarbaro "Jorge" 80 ["Aooo","Eooo","Iooo","Oooo"] [ardilla {--,libroPedKing--}]

faffy :: Barbaro
faffy = CrearBarbaro "Faffy" 100 ["robar"] [ardilla {--,libroPedKing--}]

modificarFuerza :: Int -> Objeto
modificarFuerza unaFuerza unBarbaro = unBarbaro { fuerza = fuerza unBarbaro + unaFuerza } 

espada :: Int -> Objeto
espada unPeso unBarbaro = modificarFuerza (unPeso*2) unBarbaro

agregarHabilidad :: Habilidad -> Objeto
agregarHabilidad unaHabilidad unBarbaro = unBarbaro { habilidades = unaHabilidad : habilidades unBarbaro }

amuletosMisticos :: String -> Habilidad -> Objeto
amuletosMisticos unAmuleto unaHabilidad unBarbaro 
    | unAmuleto == "puerco-marranos" = agregarHabilidad unaHabilidad unBarbaro
    | otherwise = unBarbaro

borrarObjetos :: Objeto
borrarObjetos unBarbaro = unBarbaro { objetos = [] }

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = ((agregarHabilidad "magia").borrarObjetos) unBarbaro

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

-- es mas expresivo Barbaro -> Barbaro que -> Objeto en este caso
cuerda :: Objeto -> Objeto -> Barbaro -> Barbaro
cuerda unaFuncion otraFuncion unBarbaro = (unaFuncion.otraFuncion) unBarbaro
 
concatenarHabilidades :: Barbaro -> String
concatenarHabilidades unBarbaro = (concat.habilidades) unBarbaro

pasasHabilidadesAMayuscula :: Barbaro -> Barbaro
pasasHabilidadesAMayuscula unBarbaro = unBarbaro { habilidades = [((map toUpper).concatenarHabilidades) unBarbaro] }

megafono :: Objeto
megafono unBarbaro = pasasHabilidadesAMayuscula unBarbaro

-- es mas expresivo Barbaro -> Barbaro que -> Objeto en este caso
megafonoBarbarico :: Objeto
megafonoBarbarico unBarbaro = cuerda ardilla megafono unBarbaro

--es mas expresivo Barbaro -> Bool que -> Aventura en este caso
tieneHabilidad :: Habilidad -> Barbaro -> Bool
tieneHabilidad unaHabilidad unBarbaro = (any (==unaHabilidad).habilidades) unBarbaro

invasionDeSuciosDuendes :: Aventura
invasionDeSuciosDuendes unBarbaro = tieneHabilidad "Escribir Poesia Atroz" unBarbaro

cremalleraDelTiempo :: Aventura
cremalleraDelTiempo unBarbaro = (tienePulgares.nombre) unBarbaro

tienePulgares :: String -> Bool
tienePulgares "Faffy" = True
tienePulgares "Astro" = True
tienePulgares _ = False

saqueo :: Aventura
saqueo unBarbaro =  (tieneHabilidad "robar" unBarbaro) && (80 < fuerza unBarbaro)

cantidadDeObjetos :: Barbaro -> Int
cantidadDeObjetos unBarbaro = (length.objetos) unBarbaro

obtenerGritoDeGuerra :: Barbaro -> Int 
obtenerGritoDeGuerra unBarbaro = ((4*).cantidadDeObjetos) unBarbaro

cantidadDeLetrasDeHabilidades :: Barbaro -> Int
cantidadDeLetrasDeHabilidades unBarbaro = (length.concatenarHabilidades) unBarbaro

gritoDeGuerra :: Aventura
gritoDeGuerra unBarbaro = obtenerGritoDeGuerra unBarbaro >= cantidadDeLetrasDeHabilidades unBarbaro

cantidadHabilidadesConMasDe3Vocales :: Aventura
cantidadHabilidadesConMasDe3Vocales unBarbaro = (all contieneMasDe3Vocales.habilidades) unBarbaro

contieneMasDe3Vocales :: Habilidad -> Bool
contieneMasDe3Vocales unaHabilidad = (length.filter esVocal) unaHabilidad > 3

--como "Escribir Poesia Atroz" comienza con mayuscula, agregamos los casos de habilidades que comienzen con mayuscula vocal. 
esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aeiouAEIOU" 

habilidadComienzaConMaysucula :: Aventura
habilidadComienzaConMaysucula unBarbaro = (length.(filter esMayuscula.habilidades)) unBarbaro == (length.habilidades) unBarbaro

esMayuscula :: Habilidad -> Bool
esMayuscula unaHabilidad = (isUpper.head) unaHabilidad

caligrafia :: Aventura
caligrafia unBarbaro = cantidadHabilidadesConMasDe3Vocales unBarbaro && habilidadComienzaConMaysucula unBarbaro

ritualDeFechorias :: Aventura
ritualDeFechorias unBarbaro = caligrafia unBarbaro || gritoDeGuerra unBarbaro || saqueo unBarbaro

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes listaBarbaros unaAventura = filter unaAventura listaBarbaros

-- es mas expresivo Barbaro -> Barbaro que -> Objeto en este caso
listaSinRepetirHabilidades :: Barbaro -> Barbaro
listaSinRepetirHabilidades unBarbaro = unBarbaro { habilidades = (eliminarHabilidadesRepetidas.habilidades) unBarbaro}

eliminarHabilidadesRepetidas :: [Habilidad] -> [Habilidad]
eliminarHabilidadesRepetidas [] = []
eliminarHabilidadesRepetidas (cabeza : cola) = cabeza : eliminarHabilidadesRepetidas (filter ((/=)cabeza) cola)  

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = ((utilizarObjetos (objetos unBarbaro)).nuevoDescendiente) unBarbaro : descendientes (nuevoDescendiente unBarbaro)

nuevoDescendiente :: Barbaro -> Barbaro
nuevoDescendiente unBarbaro = unBarbaro {nombre = nombre unBarbaro ++ "*", fuerza = fuerza unBarbaro, habilidades = (eliminarHabilidadesRepetidas.habilidades) unBarbaro, objetos = objetos unBarbaro}

utilizarObjetos :: [Objeto] -> Objeto
utilizarObjetos listaObjetos = foldl1 (.) listaObjetos 

--Punto C 
-- No se podria aplicar eliminarHabilidadesRepetidas en objetos, dado que toma una lista de Strings y de vuelve otra lista de Strings
--, y los objetos son de tipo Barbaro -> Barbaro. Pero si se podria sobre el nombre de un barbaro dado que es del mismo tipo que las 
-- habilidades.  

