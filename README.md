# simple RAM to C compiler

Překladač upraveného RAM (random acces machine) do jazyku C.



## Přiřazení

```
Cell <- Expression
```
Přiřadí do buňky `Cell` hodnotu výrazu `Expression`

Buňky můžou jsou adresovány nezápornýmy čísly. A přistupuje se do nich `[Number]`. Dále lze použít pomocné buňky
indexované velkýmy písmeny `A-Z`. Možná je i nepřímá adresace a to do nekonečné hloubky, např. `[[[1]]]`, `[[1]]`.

Povolené výrazy se skládají z operátorů `+ - * /`(dělení je pouze celočíselné), závorek a buněk.

Příklady přiřazení:
```
A <- (1+A) / ([[1]] - 2)
[1] <-C + 42* [1]
[[0]] <- [[0]] + 1
```

-------------------------------------------------------------------------------------

## Vstup a výstup

```
Input: Cell
```
Načte do buňky `Cell` číslo ze `stdin`.

```
Output: Expr
```
Vytiskne hodnotu výrazu `Expr` na `stdout`.



-------------------------------------------------------------------------------------


## Podmínky

```
If Cond do Instruction
```
Zkontroluje podmínku `Cond` a pokud uspěje provede příkaz `Instruction`. Možné podmínky jsou porovnávání výrazů
operátory `==, !=, <, <=, >, >=` a porovnávání booleovských hodnot pomocí pomocí `AND, OR`. Příklady If statementů:
```
If A > B do A <- A-1
If A == B OR A == C do A <- 42
If (A == B AND A == C) OR A == [42] do A <- 1
```

-------------------------------------------------------------------------------------

## Skoky

```
Point: Instrukce
```
Nastaví na řádek místo `Point`, na které jde skočit pomocí `Goto`


```
Goto -> Point
```
Skočí na řádek s nastaveným místem `Point`.

Příklady použití:

```
here: Output: 1
Goto -> here
```
Kód rozeznámá velká/malá písmena, ale nekouká na mezery. 

