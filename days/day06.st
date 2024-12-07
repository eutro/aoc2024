Grid extend [
    | guard dir origGuard visitedPosns |

    visitedPosns: n [visitedPosns:=n]
    visitedPosns [^visitedPosns]
    dir [^dir] guard [^guard]

    initGuard
    [ self allPosnsDo: [
        :pos |
        ((self at: pos) = $^) ifTrue: [
            origGuard := pos.
            self at: pos put: $..
            self resetGuard.
            ^guard.
        ]]]

    resetGuard
    [ origGuard ifNil: [self initGuard].
      guard := origGuard.
      dir := Posn up. ]

    isSolid: p [ ^(self at: p) = $# ]
    isEmpty: p [ ^(self at: p) = $. ]

    copy [ ^Grid new rows: rows; restore: self state ]

    stepGuard
    [ [self isSolid: guard + dir] whileTrue:
          [ dir := dir rotateCcw. ].
      guard := guard + dir. ]

    state [^{guard . dir}]
    restore: st [guard := st at: 1. dir := st at: 2]

    guardOob [^(self at: guard) isNil]

    walkDo: block
    [ [self guardOob] whileFalse: [block value. self stepGuard] ]

    stepToTurn
    [ guard := guard copy.
      [self isSolid: guard + dir] whileTrue: [dir := dir rotateCcw].
      [self isEmpty: guard] whileTrue: [guard += dir].
      (self isSolid: guard) ifTrue: [guard -= dir] ]

    hasCycleFrom: state
    [ | seen st |
      seen := Set new.
      self restore: state.
      [self guardOob] whileFalse:
          [self stepToTurn. st := self state.
           (seen includes: st) ifTrue: [^true].
           seen add: st].
      ^false ]
]

AOC input: [ Grid new rows: stdin toLines asArray ];
    part1: [ :grid | | posns prev |
             posns := Dictionary new. prev := nil.
             grid walkDo: [
                 posns at: grid guard ifAbsentPut: [prev].
                 prev := grid state].
             grid visitedPosns: posns.
             posns size ];
    part2: [ :grid | | i | i := 1.
             grid visitedPosns removeKey: grid guard.
             grid visitedPosns associations count:
                 [ :ass |
                   grid at: ass key put: $#.
                   [grid hasCycleFrom: ass value]
                       ensure: [grid at: ass key put: $.] ] ];
    result: [ :grid :part | part value: grid resetGuard ];
    finish.
