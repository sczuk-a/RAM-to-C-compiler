# simple RAM to C compiler


#Použití

Program bere 2 argumenty. První udává adresu souboru s kódem. Druhý Udává adresu, kam se má kód přeložit. Pokud je
program zavolán bez druhého argumentu, použije se stejná adresa jako u prvního. 

Také je možné pustit s `--help` pro zobrazení informací.




# Model

Překladač upraveného modelu RAM (https://en.wikipedia.org/wiki/Random-access_machine) do jazyku C.
Program se skládá z posloupnosti instrukcí, které vykonává od začátku do konce. Výtočet končí, pokud program narazí na
instrukci `halt` nebo dojde na konec. Program pracuje s celými čísly a nekonečnou páskou buněk, indexovanou nezápornýmy
čísly, do které si může čísla ukládat. K buňce se přistoupí jako `[Int]`, kde `Int` je její adresa. Je možná i nepřímá
adresace do nekonečné hloubky `[[Int]]`. Dále je k dispozici 26 pomocných buněk indexovaných velkými písmeny `A-Z`.
ukládat.


Paměťová složitost je závislá na nejvyšší použité adrese buňky, ne na počtu použitých buňek !!





#### Přiřazení

```
Cell <- Expression
```
Přiřadí do buňky `Cell` hodnotu výrazu `Expression`

```

-------------------------------------------------------------------------------------

#### Vstup a výstup

```
Input: Cell
```
Načte do buňky `Cell` číslo ze `stdin`.

```
Output: Expr
```
Vytiskne hodnotu výrazu `Expr` na `stdout`.



-------------------------------------------------------------------------------------


#### Podmínky

```
If Cond do Instruction
```
Zkontroluje podmínku `Cond` a pokud uspěje provede příkaz `Instruction`. Možné podmínky jsou porovnávání výrazů
operátory `==, !=, <, <=, >, >=` a porovnávání booleovských hodnot pomocí pomocí `AND, OR`. Příklady If statementů:

-------------------------------------------------------------------------------------

#### Skoky

```
Point: Instrukce
```
Nastaví na řádek místo `Point`, na které jde skočit pomocí `Goto`


```
Goto -> Point
```
Skočí na řádek s nastaveným místem `Point`.

```
Halt
```
Ukončí běh programu.

Příklady použití:

```
here: Output: 1
Goto -> here
```

-------------------------------------------------------------------------------------

Kód rozeznámá velká/malá písmena, ale nekouká na mezery. 
Překladač ignoruje cokoliv za `#` až do konce řádky.

Příklady jednoduchých programů jsou ve složcd `examples`.






