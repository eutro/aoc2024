Object subclass: TrailMap [
    | heightMap scores allDirs |
    scores [^scores]

    initFor: pos [ ^Set new add: pos; yourself ]
    initScore [ ^Set new ]
    combineScore: score with: other [ score addAll: other. ^score ]
    numberFromScore: score [ ^score size ]

    initialize [ allDirs := {Posn up. Posn down. Posn left. Posn right}. ]
    heights: hm [ heightMap := hm. self initScores. ]
    initScores
    [ | rows |
      rows := (1 to: heightMap height) collect: [
          :y | (1 to: heightMap width) collect: [
              :x | | pos | pos := Posn x: x y: y.
              ((heightMap at: pos) = 9)
                  ifTrue: [self initFor: pos]
                  ifFalse: [nil]]].
      scores := Grid new rows: rows ]

    computeScores
    [ (8 to: 0 by: -1) do: [:i | self computeScoresForHeight: i]. ]

    computeScoresForHeight: h
    [ | h1 | h1 := h + 1.
      heightMap allPosnsDo: [
          :pos |
          (heightMap at: pos) = h ifTrue: [
              | res | res := self initScore.
              allDirs collect: [
                  :off | | offp | offp := pos + off.
                  ((heightMap at: offp) = h1) ifTrue:
                      [ res := self combineScore: res with: (scores at: offp) ]].
              scores at: pos put: res ]]]

    trailheadScores
    [ ^(heightMap allPosnsOf: 0) collect: [:p | self numberFromScore: (scores at: p)] ]
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
             Grid new rows: rows ];
    part1: TrailMap; part2: TrailMapP2;
    result: [ :heights :Class | | map |
              map := Class new heights: heights.
              map computeScores trailheadScores sum ];
    finish.
