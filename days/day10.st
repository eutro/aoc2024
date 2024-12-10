Grid subclass: HeightMap [
    | invHeights |
    rows: r [super rows: r. self initInvHeights]

    initInvHeights
    [invHeights := (0 to: 9) collect: [:h | (super allPosnsOf: h) asArray]]

    allPosnsOf: h [^invHeights at: h + 1]
]

Object subclass: TrailMap [
    | heightMap scores allDirs heightsRev |
    scores [^scores]

    initFor: pos [ ^Set new add: pos; yourself ]
    initScore [ ^Set new ]
    combineScore: score with: other [ score addAll: other. ^score ]
    numberFromScore: score [ ^score size ]

    initialize [ allDirs := {Posn up. Posn down. Posn left. Posn right}. ]
    heights: hm [ heightMap := hm. self initScores. ]
    initScores
    [ scores :=
          Grid width: heightMap width height: heightMap height
              initWith: [
                  :pos | ((heightMap at: pos) = 9)
                       ifTrue: [self initFor: pos]
                       ifFalse: [nil]] ]

    computeScores
    [ (8 to: 0 by: -1) do: [:i | self computeScoresForHeight: i]. ]

    computeScoresForHeight: h
    [ | h1 | h1 := h + 1.
      (heightMap allPosnsOf: h) do: [
          :pos | | res | res := self initScore.
          allDirs collect: [
              :off | | offp | offp := pos + off.
              ((heightMap at: offp) = h1) ifTrue: [
                  res := self combineScore: res with: (scores at: offp) ]].
          scores at: pos put: res ]]

    trailheadScores
    [ ^(heightMap allPosnsOf: 0) collect: [
          :p | self numberFromScore: (scores at: p)] ]
]

TrailMap subclass: TrailMapP2 [
    initFor: pos [^1]
    initScore [^0]
    combineScore: score with: other [^score + other]
    numberFromScore: score [^score]
]

AOC input: [ | rows |
             rows := stdin toLines asArray collect: [
                 :line | line asArray collect: [:it | it digitValue]].
             HeightMap new rows: rows ];
    part1: TrailMap; part2: TrailMapP2;
    result: [ :heights :Class | | map |
              map := Class new heights: heights.
              map computeScores trailheadScores sum ];
    finish.
