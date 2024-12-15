Object subclass: Transaction [
    | updates |
    initialize [ updates := LookupTable new. ]
    updates [^updates]

    putAll: o
    [ o updates keysAndValuesDo: [
        :pos :new | self at: pos put: new ]]

    at: pos put: new
    [ | old |
      old := updates at: pos ifAbsent: [nil].
      (old isNil or: [old = $.]) ifTrue: [^updates at: pos put: new].
      (new = $. or: [old = new]) ifTrue: [^nil].
      self error: ('Contradictory transaction %1 <- %2: already %3'
                       % {pos . new . old}) ]

    performOn: grid
    [ updates keysAndValuesDo: [
        :pos :new | grid at: pos put: new ] ]
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
    [ grid allPosnsDo: [
        :it | (grid at: it) = $@ ifTrue: [
            ^robot := it. ]].
      self error: 'No robot' ]

    tryMove: src by: off
    [ | dst blk |
      blk := grid at: src.
      blk = $. ifTrue: [^true].
      blk = $# ifTrue: [^false].
      dst := src + off.
      (self tryMove: dst by: off)
          ifFalse: [^false]
          ifTrue: [
              grid at: src put: $..
              grid at: dst put: blk.
              ^true ] ]

    tryMoveTrans: src by: off
    [ | dst blk |
      blk := grid at: src.
      blk = $. ifTrue: [^Transaction new].
      blk = $# ifTrue: [^nil].

      off x = 0 ifTrue: [
          blk = $[ ifTrue: [^self tryMoveTrans0: src and: src + Posn right by: off].
          blk = $] ifTrue: [^self tryMoveTrans0: src and: src + Posn left by: off].
      ].
      ^self tryMoveTrans0: src by: off ]

    tryMoveTrans0: src1 and: src2 by: off
    [ | dst1 dst2 blk1 blk2 |
      dst1 := src1 + off. dst2 := src2 + off.
      blk1 := grid at: src1. blk2 := grid at: src2.
      ^(self tryMoveTrans: dst1 by: off) ifNotNil: [
          :th | (self tryMoveTrans: dst2 by: off) ifNotNil: [
              :th2 |
              th putAll: th2.
              th at: src1 put: $..
              th at: src2 put: $..
              th at: dst1 put: blk1.
              th at: dst2 put: blk2.
              th ]]]

    tryMoveTrans0: src by: off
    [ | dst blk |
      dst := src + off.
      blk := grid at: src.
      ^(self tryMoveTrans: dst by: off) ifNotNil: [
          :th |
          th at: src put: $..
          th at: dst put: blk.
          th ]]

    followInsn: insn
    [ | dir |
      dir := insnMap at: insn ifAbsent: [^false].
      (self tryMoveTrans: robot by: dir) ifNotNil: [
          :th | th performOn: grid. robot := robot + dir] ]

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
