  $ circ ccs.sem
  step (CM | CS) \ coin \ coffee
    res ~pub: (CM | ~coin . coffee . CS) \ coin \ coffee
  step CM | CS
    par1 coin: ~coffee . CM | CS
    par2 ~pub: CM | ~coin . coffee . CS
  step (CM | ~coin . coffee . CS) \ coin \ coffee
    res tau: (~coffee . CM | coffee . CS) \ coin \ coffee
  run (CM | CS) \ coin \ coffee
    no final configuration (every path loops)
