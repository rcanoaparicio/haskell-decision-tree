DECISION TREES
==============
This project is a **decision tree** programmed using **Haskell**. It uses a [Mushroom Dataset](https://archive.ics.uci.edu/ml/datasets/Mushroom) to generate the tree and then classifies new mushrooms introduced by the user as poisonous or edible.

Requisites
-----------
In order to execute the program, you must be able to execute the program `ghc` in your system.

Usage
-----
The usage of the program is very simple:
1. `Compilation`: using using a command line console, you'll be able to compile the program by executing 
```
ghc dts.hs
```
2. `Execution`: once the executable is generated, you'll be able to execute the program
```
./dts
./dts.exe               (on Windows)
```
3. `Running`: now the program execution will start, loading the data from the file and creating the **decision tree**. From here you can interact with it by typing the properties of the mushroom you like to test.

References
-------
* [Decision Trees](https://gebakx.github.io/hs-dts/)
* Gerard Escudero, 2020. [Machine Learning](https://gebakx.github.io/ml/#1).
* Jeff Schlimmer, 1981. [Mushroom Data Set](https://archive.ics.uci.edu/ml/datasets/Mushroom). [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/index.php).
