Grid extend [
    | guard dir origGuard visitedPosns |

    visitedPosns: n [visitedPosns:=n]
    visitedPosns [^visitedPosns]
    dir [^dir]
    guard
    [ guard ifNotNil: [:it|^it] ifNil:
          [ self allPosnsDo:
                [ :pos |
                  (self at: pos) = $^ ifTrue: [
                      origGuard := pos.
                      self at: pos put: $..
                      self resetGuard.
                      ^guard.
                  ]]].
      ^nil ]

    resetGuard [ guard := origGuard. dir := Posn up. ]

    isSolid: p [ ^(self at: p) = $# ]

    copy [ ^Grid new rows: rows; restore: self state ]

    stepGuard
    [ self guard.
      [self isSolid: (guard + dir)] whileTrue:
          [ dir := dir rotateCcw. ].
      guard := guard + dir. ]

    state [^{self guard . self dir}]
    restore: st [guard := st at: 1. dir := st at: 2]

    guardOob [^(self at: self guard) isNil]

    walkDo: block
    [ [self guardOob] whileFalse: [block value. self stepGuard] ]

    hasCycle
    [ | fast | fast := self resetGuard copy.
      [fast guardOob] whileFalse:
          [self stepGuard. fast stepGuard stepGuard.
           self state = fast state ifTrue: [^true]].
      ^false ]
]

AOC input: [ Grid new rows: stdin toLines asArray ];
    part1: [ :grid | | posns |
             posns := Set new.
             grid walkDo: [posns add: grid guard].
             grid visitedPosns: posns.
             posns ];
    part2: [ :grid | | i | i := 1.
             grid visitedPosns select:
                 [ :p | grid at: p put: $#. i printNl. i := i + 1.
                   [grid hasCycle] ensure: [grid at: p put: $.] ] ];
    result: [ :grid :part | (part value: grid resetGuard) size ];
    finish.
