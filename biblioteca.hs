import Data.List


data Libro = Libro {    
    titulo :: String,
    autor :: String,
    paginas :: Int
}deriving(Eq, Show)
type Biblioteca = [Libro]

elVisitante, shingeki1,shingeki3,shingeki127,fundacion,sandman5,sandman10,sandman12,eragon,eldest,brisignr,legado::Libro

elVisitante = Libro "El visitante" "Stephen King" 592
shingeki1 = Libro "Shingeki no Kyojin cap 1" "Hajime Isayama" 40
shingeki3 = Libro "Shingeki no Kyojin cap 3" "Hajime Isayama" 40
shingeki127 = Libro "Shingeki no Kyojin cap 127" "Hajime Isayama" 40
fundacion = Libro "Fundacion" "Isaac Asimov" 230
sandman5 = Libro "Sandman" "Neil Gaiman" 35
sandman10 = Libro "Sandman" "Neil Gaiman" 35
sandman12 = Libro "Sandman" "Neil Gaiman" 35
eragon = Libro "Eragon " "Christopher Paolini" 544
eldest = Libro "Eldest " "Christopher Paolini" 704
brisignr = Libro "Brisignr " "Christopher Paolini" 700
legado = Libro "Legado " "Christopher Paolini" 811

biblioteca :: Biblioteca
biblioteca = [elVisitante,shingeki1,shingeki3,shingeki127,fundacion,sandman10,sandman12,sandman5,eragon,eldest,brisignr,legado]

-- 1 promedio paginas 
promedioDePaginas:: Biblioteca -> Int
promedioDePaginas biblioteca = sumatoriaPaginas biblioteca `div` numeroDeLibros biblioteca

numeroDeLibros :: Biblioteca -> Int
numeroDeLibros  = genericLength 

sumatoriaPaginas :: Biblioteca -> Int
sumatoriaPaginas  = foldl (\acumulador libro -> acumulador + paginas libro) 0 

-- 2 lecturaObligatoria

sagaEragon ::[Libro]
sagaEragon = [eragon,eldest,brisignr,legado]
esDeLaSagaEragon:: Libro-> Bool
esDeLaSagaEragon libro = elem libro sagaEragon
esFundacionConPaginas:: Libro-> Bool
esFundacionConPaginas libro = (titulo libro == "Fundacion" && paginas libro == 230)

lecturasObligatorias::Biblioteca -> [Libro]
lecturasObligatorias biblioteca = filter (\libro-> (autor libro == "Stephen King")|| (esDeLaSagaEragon libro) || (esFundacionConPaginas libro) ) biblioteca

-- 3 es fantasiosa?

esFantasiosa:: Biblioteca -> Bool
esFantasiosa biblioteca = any (\libro -> autor libro == "Christopher Paolini" || autor libro == "Neil Gaiman")  biblioteca

-- 4 nombre de la biblioteca
listaDeTitulos::Biblioteca->[String]
listaDeTitulos = map (\libro-> titulo libro) 
sacarVocalesYEspacios:: String->String
sacarVocalesYEspacios  = filter (\caracter-> caracter/= 'a' && caracter /= 'e' && caracter /= 'i' && caracter /= 'o' && caracter /= 'u' && caracter/= 'A' && caracter /= 'E' && caracter /= 'I' && caracter /= 'O' && caracter /= 'U' && caracter /= ' ') 
nombreDeLaBiblioteca::Biblioteca->String
nombreDeLaBiblioteca biblioteca = foldl (\acumulador titulo-> acumulador ++ (sacarVocalesYEspacios titulo) ) "" (listaDeTitulos biblioteca)

-- 5 es biblioteca ligera
esBibliotecaLigera::Biblioteca->Bool
esBibliotecaLigera biblioteca = all (\libro-> paginas libro <= 40) biblioteca

-- 6 genero del libro
esJapones:: String->Bool
esJapones autor = elem autor ["Hajime Isayama"]
genero:: Libro->String
genero libro 
    |autor libro == "Stephen King" = "Terror"
    |esJapones (autor libro) = "Manga"
    |paginas libro < 40 = "Comics"
    | otherwise = "No clasificado"