Object subclass: Machine [
    | vecA vecB prize |
    parse: line
    [ line scanf: 'Button A: X+%d, Y+%d Button B: X+%d, Y+%d Prize: X=%d, Y=%d'
           with: [ :aX :aY :bX :bY :pX :pY |
                   vecA := Posn x: aX y: aY.
                   vecB := Posn x: bX y: bY.
                   prize := Posn x: pX y: pY ] ]

    printOn: st
    [ st << '{A: ' << vecA << '; B: ' << vecB << '; P: ' << prize << '}' ]

    priceA [^3] priceB [^1]

    solve: offV
    [ ^(Mat2 col: vecA col: vecB) inverse
           ifNotNil: [
               :inv | | vec |
               vec := inv *> (prize + offV).
               (vec x isInteger and: [vec y isInteger])
                   ifTrue: [^vec]
                   ifFalse: [^nil] ]
           ifNil: [
               "linearly dependent -- not actually reached"
               | slvA slvB |
               'Linearly dependent!' displayNl.
               slvA := self solveLinDep: vecA.
               slvB := self solveLinDep: vecB.
               slvA ifNil: [
                   slvB ifNil: [^nil]
                        ifNotNil: [^Posn x: 0 y: slvB]].
               slvB ifNil: [^Posn x: slvA y: 0].
               ((slvA * self priceA) <= (slvB * self priceB))
                   ifTrue: [^Posn x: slvA y: 0]
                   ifFalse: [^Posn x: 0 y: slvB]] ]

    tokenCost: soln
    [ soln ifNil: [^0].
      ^(soln x * self priceA) + (soln y * self priceB) ]

    solveCost: offV [ ^self tokenCost: (self solve: offV) ]

    solveLinDep: vec
    [ | sf |
      sf := prize x / vec x.
      (prize y / vec y) = sf ifFalse: [^nil].
      sf isInteger ifFalse: [^nil].
      ^sf ]
]

AOC input: [ stdin chain contents; tokenize: (String with: $<10> with: $<10>);
                   collect: [:it | Machine new parse: it] ];
    part1: 0; part2: 10000000000000;
    result: [ :machines :off | | offV |
              offV := Posn x: off y: off.
              machines inject: 0 into: [:a :it | a + (it solveCost: offV)] ];
    finish.
