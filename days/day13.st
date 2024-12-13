Object subclass: Machine [
    | vecA vecB prize matrix |
    parse: line
    [ line scanf: 'Button A: X+%d, Y+%d Button B: X+%d, Y+%d Prize: X=%d, Y=%d'
           with: [ :aX :aY :bX :bY :pX :pY |
                   vecA := Posn x: aX y: aY.
                   vecB := Posn x: bX y: bY.
                   prize := Posn x: pX y: pY.
                   matrix := self computeMatrix ] ]

    printOn: st
    [ st << '{A: ' << vecA << '; B: ' << vecB << '; P: ' << prize << '}' ]

    computeMatrix
    [ ^(Mat2 col: vecA col: vecB) inverse
           ifNil: [self error: 'Linearly dependent'] ]

    solve: offV
    [ | vec | vec := matrix *> (prize + offV).
      (vec x isInteger and: [vec y isInteger])
          ifTrue: [^vec] ifFalse: [^nil] ]

    tokenCost: soln [ soln ifNil: [^0]. ^(soln x * 3) + soln y ]

    solveCost: offV [ ^self tokenCost: (self solve: offV) ]
]

AOC input: [ stdin chain contents; tokenize: (String with: $<10> with: $<10>);
                   collect: [:it | Machine new parse: it] ];
    part1: 0; part2: 10000000000000;
    result: [ :machines :off | | offV |
              offV := Posn x: off y: off.
              machines inject: 0 into: [:a :it | a + (it solveCost: offV)] ];
    finish.
