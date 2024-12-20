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

Object subclass: KDTree [
    from: min to: max do: block
    [ self subclassResponsibility ]
]
KDTree class extend [
    fromPoints: points
    [ ^self fromShuffledPoints: (Random new shuffle: points asArray) ]

    fromShuffledPoints: points
    [ ^self fromShuffledPoints: points axis: 1 ]

    leafMaxSize [ ^5 ]
    fromShuffledPoints: points axis: ax
    [ | med splits dims nextAx value |
      points size <= self leafMaxSize ifTrue: [^KDLeaf new points: points asArray].
      med := self guessMedian: points axis: ax.
      dims := med size.
      nextAx := ax \\ dims + 1.
      value := med at: ax.
      splits := self splitPoints: points axis: ax on: value.
      ^KDInner new
           left: (self fromShuffledPoints: (splits at: 1) axis: nextAx)
           right: (self fromShuffledPoints: (splits at: 2) axis: nextAx)
           axis: ax
           value: value ]

    splitPoints: points axis: ax on: val
    [ | l r fairLeft |
      l := OrderedCollection new.
      r := OrderedCollection new.
      fairLeft := true.
      points do: [
          :point | | v onLeft |
          v := point at: ax.
          onLeft := (v < val) or: [
              v = val and: [fairLeft := fairLeft not. fairLeft]].
          (onLeft ifTrue: [l] ifFalse: [r]) add: point ].
      ^{l . r} ]

    medianGuessSize [ ^5 ]
    guessMedian: points axis: ax
    [ | arr |
      arr := 1 to: (points size min: self medianGuessSize)
               collect: [:i | points at: i].
      arr sort: [:l :r | (l at: ax) <= (r at: ax)].
      ^arr at: arr size // 2 + 1 ]
]

KDTree subclass: KDLeaf [
    | points |
    points: n [points := n. "self class leafSize: points size"]
    from: min to: max do: block [ points do: block ]

    printOn: st [ st << 'lf' << points ]
]
KDLeaf class extend [
    | leafSizes |
    leafSizes [ ^leafSizes ifNil: [leafSizes := Bag new] ]
    leafSize: sz [ self leafSizes add: sz ]
]

KDTree subclass: KDInner [
    | left right axis value |
    left: l right: r axis: ax value: v
    [left:=l. right:=r. axis:=ax. value:=v]
    from: min to: max do: block
    [ (min at: axis) <= value ifTrue: [ left from: min to: max do: block ].
      (max at: axis) >= value ifTrue: [ right from: min to: max do: block ] ]

    printOn: st [ st << '{' << left << ' <= [' << axis << '] '
                  << value << ' <= ' << right << '}' ]
]

AOC input: [ Racetrack new rows: stdin toLines asArray ];
    part1: 2; part2: 20;
    result: [ :track :time | track countCheatsWithTime: time ];
    finish.
