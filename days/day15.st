Object subclass: Transaction [
    | updates |
    initialize [ updates := LookupTable new. ]
    updates [^updates]

    putAll: o
    [ o updates keysAndValuesDo: [:pos :new | self at: pos put: new]]

    movedFrom: pos [ self at: pos put: $. ]
    at: pos put: new
    [ | old |
      old := updates at: pos ifAbsent: [$.].
      old = $. ifTrue: [^updates at: pos put: new].
      (new = $. or: [old = new]) ifTrue: [^nil].
      self error: ('Contradictory transaction %1 <- %2: already %3'
                       % {pos . new . old}) ]

    hasMovedFrom: pos [ ^(updates at: pos ifAbsent: [nil]) notNil ]

    performOn: grid
    [ updates keysAndValuesDo: [:pos :new | grid at: pos put: new] ]
]

Object subclass: Warehouse [
    | grid insns robot insnMap |

    initialize
    [ insnMap := LookupTable new.
      insnMap at: $< put: Posn left;
              at: $> put: Posn right;
              at: $v put: Posn down;
              at: $^ put: Posn up. ]

    grid: g [grid := g. self findRobot]
    insns: i [insns := i]

    printOn: st [ st << grid. ]

    findRobot
    [ grid allPosnsDo: [:it | (grid at: it) = $@ ifTrue: [^robot := it]].
      self error: 'No robot' ]

    tryMoveIn: trans from: src by: off
    [ | blk |
      "(trans hasMovedFrom: src) ifTrue: [^true]. -- just makes it slower"
      blk := grid at: src.
      blk = $. ifTrue: [^true].
      blk = $# ifTrue: [^false].

      off x = 0 ifTrue: [
          blk = $[ ifTrue: [^self tryMoveIn0: trans from: src and: src + Posn right by: off].
          blk = $] ifTrue: [^self tryMoveIn0: trans from: src and: src + Posn left by: off].
      ].
      ^self tryMoveIn0: trans from: src by: off ]

    tryMoveIn0: trans from: src1 and: src2 by: off
    [ ^(self tryMoveIn0: trans from: src1 by: off) and:
           [self tryMoveIn0: trans from: src2 by: off] ]

    tryMoveIn0: trans from: src by: off
    [ | dst blk |
      dst := src + off.
      blk := grid at: src.
      (self tryMoveIn: trans from: dst by: off) ifTrue: [
          trans movedFrom: src; at: dst put: blk.
          ^true ]. ^false ]

    followInsn: insn
    [ | dir trans |
      dir := insnMap at: insn ifAbsent: [^false].
      trans := Transaction new.
      (self tryMoveIn: trans from: robot by: dir) ifTrue: [
          trans performOn: grid. robot := robot + dir] ]

    runInsns [ insns do: [:it | self followInsn: it] ]

    sumGps
    [ | total | total := 0.
      grid allPosnsDo: [
          :it | | tile | tile := grid at: it.
          (tile = $O or: [tile = $[]) ifTrue: [
              total := total + it gpsCoord ] ].
      ^total ]

    copyExpand: expand
    [ ^expand ifFalse: [ ^self copy ]
              ifTrue: [ ^self copyExpand ] ]

    postCopy [ grid := grid copy ]

    copyExpand
    [ | expandMap rows |
      expandMap := LookupTable new.
      expandMap at: $# put: '##';
                at: $O put: '[]';
                at: $@ put: '@.';
                at: $. put: '..'.
      rows := grid rows collect: [
          :row | (row toList collect: [:c | expandMap at: c]) join ].
      self grid: (Grid new rows: rows) ]
]

Posn extend [
    gpsCoord [ ^self y * 100 + x - 101 ]
]

AOC input: [ stdin contents splitDoubleNl letArrayInBlock: [
               :rows :insns |
               Warehouse new grid: (Grid new rows: rows lines);
                     insns: insns ] ];
    part1: false; part2: true;
    result: [ :input :part | (input copyExpand: part) runInsns sumGps ];
    finish
