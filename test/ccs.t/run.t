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
  step a . (~c . 0 + b . 0) | ((c . 0 + ~a . 0) | ~b . 0)
    par1 a: (~c . 0 + b . 0) | ((c . 0 + ~a . 0) | ~b . 0)
    par2 c: a . (~c . 0 + b . 0) | (0 | ~b . 0)
    par2 ~a: a . (~c . 0 + b . 0) | (0 | ~b . 0)
    par2 ~b: a . (~c . 0 + b . 0) | ((c . 0 + ~a . 0) | 0)
    com1 tau: (~c . 0 + b . 0) | (0 | ~b . 0)
