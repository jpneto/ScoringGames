ScoringGames
============

The <b>Scoring Games Calculator</b> is a Haskell set of modules to research the Universe of Guaranteed Scoring Combinatorial Games. The classical [CGT Conway Universe](http://en.wikipedia.org/wiki/Surreal_number) is order-embedded in it.

The main module is **Scoring** with all the data structures and functions to perform operations over scoring game values.

Given a ruleset $R$, any valid position $p \in R$ is represented abstractly by module **Position**. This module can be instanciated by many concrete rulesets (eg, Dots'n'Boxes) where each one of these rulesets is codified on their respective modules. The project already includes several ruleset implementations, namely **Dots'n'Boxes**, **Kobber** and **Diskonnect**.

As an eg, the next picture shows the value of the given Kobber position:

<center><img src="scoringEg.png" style="width:316 px;height:164px" alt="Kobber position" ></center>

## Installation

1. Download an Haskell system, eg, the [Haskell Platform](https://www.haskell.org/platform/) and install it

2. Clone this [project](https://github.com/jpneto/ScoringGames) or just [download as a ZIP](https://github.com/jpneto/ScoringGames/archive/master.zip) from GitHub and unzip it

3. Inside the Haskell interpreter (eg, in Windows select Winghci), load the appropriate module and start using it

Download the [user guide](https://github.com/jpneto/ScoringGames/blob/master/userGuide/userGuide.pdf?raw=true) for a more detailed introduction/tutorial.
