Object subclass: MemorySpace [
    | corruption origin dirs end
      grid steps size
      queue seen |

    initialize
    [ origin := Posn x: 1 y: 1.
      dirs := {Posn up. Posn down. Posn left. Posn right} ]

    corruption: n [corruption := n collect: [:it | it + origin]]
    corruption [^corruption]

    grid [^grid]

    plotAtTime: time
    [ | g | g := Grid width: size height: size initWith: [:i | $.].
      g rows: (g rows collect: [:it | it asString]).
      1 to: time do: [:i | g at: (corruption at: i) put: $#].
      g printNl ]

    findExitWithInitial: initial
    [ | stepsInitial |
      size := 71.
      grid := Grid width: size height: size initWith: [
                       :i | {"blocked at: "FloatD infinity.
                             "last reachable before: "nil}].
      corruption keysAndValuesDo: [
          :time :pos | (grid at: pos) at: 1 put: time ].

      queue := OrderedCollection new.
      self enqueue: origin withLastTime: ((grid at: origin) at: 1).

      steps := 0.
      end := Posn x: size y: size.
      [ queue isEmpty ] whileFalse: [
          self stepOnce.
          steps := steps + 1.
          (stepsInitial isNil and: [
              ((grid at: end) at: 2)
                  ifNil: [false]
                  ifNotNil: [:it | it > initial]]) ifTrue: [
              stepsInitial := steps.
          ].
      ].
      ^stepsInitial ]

    posnThatBlocksEnd [ ^(corruption at: ((grid at: end) at: 2)) - origin ]

    stepOnce
    [ | oldQueue |
      oldQueue := queue.
      queue := OrderedCollection new.
      oldQueue do: [
          :posn :time | dirs do: [
              :dir |
              self enqueue: (posn + dir)
                   withLastTime: time ]]
                       asSpreader ]

    enqueue: posn withLastTime: lastTime
    [ | entry bestTime |
      entry := grid at: posn.
      entry ifNil: [^false].
      bestTime := (entry at: 1)
                      ifNil: [lastTime]
                      ifNotNil: [:e | lastTime min: e].
      (entry at: 2) ifNotNil: [
          :prevLastReachable |
          bestTime <= prevLastReachable ifTrue: [^false]].
      entry at: 2 put: bestTime.
      queue add: {posn . bestTime}.
      ^true ]
]

AOC input: [ | posns |
             posns := stdin toLines asArray collect: [
                 :it | it scanf: '%d,%d' with: [:x :y | Posn x: x y: y]].
             MemorySpace new corruption: posns ];
    part1: [ :space | space findExitWithInitial: 1024 ];
    part2: [ :space | | p | p := space posnThatBlocksEnd.
             '%1,%2' % {p x . p y} ];
    result: [ :space :part | part value: space ];
    finish
