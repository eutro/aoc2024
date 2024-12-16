Object subclass: Maze [
    | grid start end bestScore
      seen queue |

    rows: r [ grid := Grid new rows: r. self findLandmarks ]

    findLandmarks
    [ grid allPosnsDo: [
        :pos | | tile | tile := grid at: pos.
        (tile = $S or: [tile = $E]) ifTrue: [
            tile = $S ifTrue: [start := pos]
                      ifFalse: [end := pos].
            grid at: pos put: $.. ]] ]

    bestPath
    [ "seen[pos][dir] -> {score . Set(previous-states ...)}"
      seen := grid collect: [:i | LookupTable new].
      "queue: score -> {pos . facing}"
      queue := SortedCollection new.

      self enqueuePos: start dir: Posn right withScore: 0 fromState: nil.
      self enqueuePos: start dir: Posn up withScore: 1000 fromState: nil.

      [ queue isEmpty ] whileFalse: [
          | entry state |
          entry := queue removeFirst.
          entry value letArrayInBlock: [
              :pos :dir |
              self stepPathAt: pos dir: dir score: entry key ]].

      bestScore := FloatD infinity.
      (seen at: end) do: [:it | bestScore := bestScore min: (it at: 1)].
      ^bestScore ]

    stepPathAt: oldPos dir: dir score: oldScore
    [ | rots pos nextPos score |
      rots := { dir rotateCw . dir rotateCcw }.
      pos := oldPos.
      score := oldScore.

      [ nextPos := pos + dir.
        (self isOpen: nextPos) ifFalse: [^nil].
        score := score + 1.
        self addPos: nextPos dir: dir withScore: score
             fromState: { pos . dir }.
        pos := nextPos.

        rots do: [
            :rot | | rotPos |
            rotPos := pos + rot.
            (self isOpen: rotPos) ifTrue: [
                self enqueuePos: rotPos dir: rot
                     withScore: score + 1001
                     fromState: { pos . dir }.
            ]]
      ] repeat ]

    addPos: pos dir: dir withScore: score fromState: oldState
    [ | seenState oldScore cameFrom seenDirState |
      seenDirState := seen at: pos.
      seenState := seenDirState at: dir ifAbsent: [
          seenDirState at: dir put: {FloatD infinity . Set new}
      ].
      oldScore := seenState at: 1.
      cameFrom := seenState at: 2.

      score > oldScore ifTrue: [^false].
      score < oldScore ifTrue: [
          seenState at: 1 put: score.
          cameFrom empty ].
      oldState ifNotNil: [ cameFrom add: oldState ].
      ^score ~= oldScore ]

    enqueuePos: pos dir: dir withScore: score fromState: oldState
    [ | ok |
      ok := self addPos: pos dir: dir withScore: score fromState: oldState.
      ok ifTrue: [queue add: score -> {pos . dir}].
      ^ok ]

    isOpen: pos [ ^(grid at: pos) = $. ]
    printOn: st [ st << grid ]

    bestPathCount
    [ | bestPathSet |
      bestPathSet := Set new.
      (seen at: end) keysAndValuesDo: [
          :dir :i | (i at: 1) = bestScore ifTrue: [
              self traceBackFrom: end dir: dir into: bestPathSet]].
      ^bestPathSet size ]

    traceBackFrom: pos dir: dir into: set
    [ set add: pos.
      grid at: pos put: $O.
      (((seen at: pos) at: dir ifAbsent: [^nil]) at: 2) do: [
          :nPos :nDir | self traceBackFrom: nPos dir: nDir into: set
      ] asSpreader ]
]

AOC input: [ Maze new rows: stdin toLines asArray ];
    part1: [ :maze | maze bestPath ];
    part2: [ :maze | maze bestPathCount ];
    result: [ :maze :part | part value: maze ];
    finish.
