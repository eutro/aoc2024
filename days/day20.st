Object subclass: Racetrack [
    | grid distToEnd start end trackLen allCheats |
    rows: r
    [ grid := Grid new rows: r.
      self initTrack ]

    initTrack
    [ | pos dir step |
      trackLen := 1.
      grid allPosnsDo: [
        :p | | c | c := grid at: p.
        (c = $S or: [c = $E]) ifTrue: [
            grid at: p put: $..
            c = $S ifTrue: [start := p].
            c = $E ifTrue: [end := p].
        ].
        c = $. ifTrue: [ trackLen := trackLen + 1 ]].
      distToEnd := Grid width: grid width
                        height: grid height
                        initWith: [:p | nil].

      pos := start.
      dir := {Posn up. Posn down. Posn left. Posn right} findFirstElt: [
          :dir | self isEmpty: start + dir ].
      step := 0.
      [ pos = end ] whileFalse: [
          distToEnd at: pos put: trackLen - step.
          dir := {dir. dir rotateCw. dir rotateCcw} findFirstElt: [
              :dir | self isEmpty: pos + dir ].
          pos := pos + dir.
          step := step + 1.
      ].
      distToEnd at: end put: 0.
    ]

    isEmpty: pos [ ^(grid at: pos) = $. ]

    countCheatsWithTime: time
    [ | total |
      total := 0.
      "allCheats := Bag new."
      grid allPosnsDo: [
          :pos | total := total + (self countCheatsFrom: pos withTime: time) ].
      "allCheats printNl."
      ^total ]

    countCheatsFrom: pos withTime: time
    [ | dist |
      dist := distToEnd at: pos.
      dist ifNil: [^0].
      "dist < 100 ifTrue: [^0]."
      ^self rangeCount: [
          :dpos :ddist |
          self validCheatFrom: pos dist: dist
               to: dpos dist: ddist
               withTime: time
      ]
            inManhDistance: time
            from: pos
            maxDist: dist - 100. ]

    validCheatFrom: pos dist: dist
    to: dpos dist: ddist
    withTime: time
    [ | ctime saved |
      ctime := (dpos x - pos x) abs +
               (dpos y - pos y) abs.
      ctime > time ifTrue: [^false].
      saved := dist - ddist - ctime.
      "saved > 0 ifTrue: [allCheats add: saved]."
      ^saved >= 100 ]

    rangeCount: block
    inManhDistance: distmh
    from: pos
    maxDist: mscore
    [ | ddist dpos count px py doit |
      count := 0.
      dpos := Posn new.
      px := pos x. py := pos y.
      distmh negated to: distmh do: [
          :dx | distmh negated to: distmh do: [
              :dy | 
              (dx abs + dy abs) <= distmh ifTrue: [
                  dpos x: px + dx; y: py + dy.
                  ddist := distToEnd at: dpos.
                  ddist ifNotNil: [
                      (block value: dpos value: ddist)
                          ifTrue: [ count := count + 1 ]]]]].
      ^count ]
]

AOC input: [ Racetrack new rows: stdin toLines asArray ];
    part1: 2; part2: 20;
    result: [ :track :time | track countCheatsWithTime: time ];
    finish.
