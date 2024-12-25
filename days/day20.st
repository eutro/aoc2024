Object subclass: Racetrack [
    | grid distToEnd start end trackLen allCheats tree |
    rows: r
    [ grid := Grid new rows: r.
      self initTrack ]

    initTrack
    [ | pos dir step posnList |
      "Find the start, end, and count the path tiles..."
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

      "Trace the path, obtaining the distances to the end..."
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

      "Build a k-d tree to answer range queries
       over the {distToEnd . x . y} dimensions."
      posnList := OrderedCollection new.
      distToEnd allPosnsDo: [
          :p | (distToEnd at: p) ifNotNil: [
              :dist | posnList add: {dist. p x. p y}]].
      tree := KDTree fromPoints: posnList. ]

    isEmpty: pos [ ^(grid at: pos) = $. ]

    countCheatsWithTime: time
    [ | total |
      total := 0.
      grid allPosnsDo: [
          :pos | total := total + (self countCheatsFrom: pos withTime: time) ].
      ^total ]

    countCheatsFrom: pos withTime: time
    [ | dist count |
      dist := distToEnd at: pos.
      dist ifNil: [^0].
      dist < 102 ifTrue: [^0].
      count := 0.
      self rangeDo: [
          :dpos :ddist |
          (self validCheatFrom: pos dist: dist
                to: dpos dist: ddist
                withTime: time) ifTrue: [
              count := count + 1 ]]
           inManhDistance: time
           from: pos
           maxDist: dist - 102.
      ^count ]

    "Answers whether POS -> DPOS is a cheat that indeed only takes
     TIME ps, and saves more than 100 ps."
    validCheatFrom: pos dist: dist
    to: dpos dist: ddist
    withTime: time
    [ | ctime saved |
      ctime := (dpos x - pos x) abs +
               (dpos y - pos y) abs.
      ctime > time ifTrue: [^false].
      saved := dist - ddist - ctime.
      ^saved >= 100 ]

    "Call BLOCK with DPOS and DDIST for each position within
     DISTMH of POS, which is at most MSCORE away from the end.
     BLOCK may be called with other positions."
    OLDrangeDo: block
    inManhDistance: distmh
    from: pos
    maxDist: mscore
    [ | ddist dpos px py |
      dpos := Posn new.
      px := pos x. py := pos y.
      distmh negated to: distmh do: [
          :dx | distmh negated to: distmh do: [
              :dy | 
              (dx abs + dy abs) <= distmh ifTrue: [
                  dpos x: px + dx; y: py + dy.
                  ddist := distToEnd at: dpos.
                  ddist ifNotNil: [
                      block value: dpos value: ddist ]]]] ]

    "Optimisation of the above that uses the k-d tree to answer range queries."
    rangeDo: block
    inManhDistance: distmh
    from: pos
    maxDist: mscore
    [ | min max dpos |
      min := {0. pos x - distmh. pos y - distmh}.
      max := {mscore. pos x + distmh. pos y + distmh}.
      dpos := Posn new.
      tree from: min to: max do: [
          :ddist :dstx :dsty |
          dpos x: dstx y: dsty.
          block value: dpos value: ddist ] asSpreader ]
]

AOC input: [ Racetrack new rows: stdin toLines asArray ];
    part1: 2; part2: 20;
    result: [ :track :time | track countCheatsWithTime: time ];
    finish.
