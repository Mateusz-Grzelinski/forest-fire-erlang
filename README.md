# Erlang - model of forest fire

<https://en.wikipedia.org/wiki/Forest-fire_model>

Projekt AGH, semestr 5, PWiR

## Instrukcja obsługi

Aby skompilować:

```bash
erl
> c(forest_fire).
```

Program można uruchomić na 2 sposoby:

Podając 4 parametry: rozmiar świata X, rozmiar świata Y, prawdopodobieństwo wyrośnięcia drzewa na pustym obszarze P, prawdopodobieństwo zapalenia drzewa F. Typowo P jest dużo większe od F.

```bash
> forest_fire:main(10, 10, 0.1, 0.01).
```

Poprzez podanie nazwy pliku. Ten plik musi być specjalnie przygotowany

```bash
> forest_fire:main("forest_fire.txt").
```

## Przykład pliku „forest_fire.txt”:

-   X - rozmiar świata X,
-   Y - rozmiar świata Y,
-   P - prawdopodobieństwo wyrośnięcia drzewa na pustym obszarze P, w zakresie od 0 do 1
-   F - prawdopodobieństwo zapalenia drzewa F, , w zakresie od 0 do 1
-   Świat
    -   musi mieć X wierszy
    -   musi mieć Y kolumn
    -   f - ogień
    -   t - drzewo
    -   o - puste pole

```
P = 0.1
F = 0.01
X = 3
Y = 3
ffo
fto
ooo
```

## Świat

Świat (las) może znajdować się w 3ch stanach: drzewo, pali się lub puste. W programie wypisywane są jako symbole, odpowiednio: T, F, spacja.

Wewnętrznie świat przechowywany jest jako lista zawierająca indeks X, Y oraz pole, np:

```
[{1,1, on_fire}, {1,2, empty}, {2,1,tree}, ...]
```

## Procesy

Proces główny spawnuje X*Y procesów, po jeden na każde pole. Kiedy obliczana jest kolejna iteracja, procesy otrzymują:

- przetwarzany element
- świat
- P - prawdopodobieństwo wyrośnięcia drzewa na pustym obszarze P, w zakresie od 0 do 1
- F - prawdopodobieństwo zapalenia drzewa F, , w zakresie od 0 do 1

Następnie:

- wyliczają sąsiedztwo 3×3
- wykonują reguły
- zwracają nową wartość pola

Proces główny oczekuje na wszystkie wyniki i wypisuje kolejną iterację

