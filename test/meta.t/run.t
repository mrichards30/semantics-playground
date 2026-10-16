  $ circ meta.sem
  show 1 + 2 * 3
    7
  show 7 / 2
    3
  show -(4 - 6)
    2
  show 1 < 2 < 3
    true
  show 1 < 3 < 2
    false
  show 2 in {1, 2, 3}
    true
  show 5 in int
    true
  show 7 / 0
    arb
  show 1 + true
    arb
  show (1 / 0) = (1 / 0)
    false
  show (1 / 0) < 1 || (1 / 0) >= 1
    false
  show arb in int
    false
  show if 1 / 0 then 1 else 2
    arb
  show total where total = inner + border and inner = 3 * 4 and border = 2 * (3 + 4)
    26
  show 1 where unused = 1 / 0
    1
  show x where x = x + 1
    arb
  show `1 + 2` in e
    true
  show `1 + true` in e
    true
  show `skip` in v
    true
  show `1 + 2` = `1 + 2`
    true
  show `1 + 2` = `2 + 1`
    false
  show dom {`x` |-> 1, `y` |-> 2}
    {x, y}
  show {`x` |-> 1}(`x`)
    1
  show {`x` |-> 1}(`y`)
    arb
  show {`x` |-> 1}[`x` |-> 5][`z` |-> 0]
    {x |-> 5, z |-> 0}
  show {3, 1, 2, 1}
    {1, 2, 3}
