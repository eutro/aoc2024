Object subclass: Robot [
    | pos vel |
    parse: line
    [ line scanf: 'p=%d,%d v=%d,%d' with: [
              :px :py :vx :vy |
              pos := Posn x: px y: py.
              vel := Posn x: vx y: vy ]]

    printOn: st [ st << '{p=' << pos << ' v=' << vel << '}' ]

    posAt: time in: br
    [ ^Posn
           x: (pos x + (vel x * (time \\ br w))) \\ br w
           y: (pos y + (vel y * (time \\ br h))) \\ br h ]
]

Array extend [
    incAt: pos [ self at: pos put: 1 + (self at: pos) ]
]

Posn subclass: Bathroom [
    | midx midy |
    w [^x] h [^y]

    x: n [super x: n. midx := x // 2]
    y: n [super y: n. midy := y // 2]

    countQuadrants: posns
    [ | qs |
      qs := Array new: 4 withAll: 0.
      posns do: [ :p | self putPosn: p toQuad: qs ].
      ^qs inject: 1 into: [:l :r | l * r] ]

    putPosn: p toQuad: qs
    [ | top bot |
      top := p y < midy.
      bot := p y > midy.
      p x < midx ifTrue:
          [ top ifTrue: [^qs incAt: 1].
            bot ifTrue: [^qs incAt: 2].
            ^nil ].
      p x > midx ifTrue:
          [ top ifTrue: [^qs incAt: 3].
            bot ifTrue: [^qs incAt: 4].
            ^nil ] ]

    findMaxOf: robots inPeriod: p withTrue: filter
    [ | best bestTime |
      best := -1.
      1 to: p do: [
          :time | | cnt |
          cnt := robots count: [:it | filter value: (it posAt: time in: self)].
          cnt > best ifTrue: [best := cnt. bestTime := time] ].
      ^bestTime ]

    plot: robots at: time
    [ | grid orig |
      grid := 1 to: self h collect: [:i | String new: self w withAll: $.].
      grid := Grid new rows: grid.
      orig := Posn x: 1 y: 1.
      robots do: [:r | grid at: orig + (r posAt: time in: self) put: $#].
      stdout << 'At ' << time << ':'; nl; << grid; nl; nl. ]

    betweenPercentile: min and: max by: getKey
    [ | mine minV maxV |
      mine := getKey value: self.
      minV := mine * min.
      maxV := mine * max.
      ^[ :it | | its | its := getKey value: it.
         minV <= its and: [its <= maxV] ] ]

    inMiddleBy: getKey
    [ ^self betweenPercentile: 0.4 and: 0.6 by: getKey ]

    findMaxRobots: robots inMiddleBy: getKey
    [ ^self findMaxOf: robots
            inPeriod: (getKey value: self)
            withTrue: (self inMiddleBy: getKey) ]

    findTreeTimeOf: robots
    [ | bestx besty |
      "The tree is a little pictogram in the center of the image -- we
      find the offset with which the robots condense here on both the
      X and Y axes, then use CRT to compute where these coincide."
      bestx := self findMaxRobots: robots inMiddleBy: [:it | it x].
      besty := self findMaxRobots: robots inMiddleBy: [:it | it y].

      (self w gcd: self h) = 1 ifFalse: [self error: 'Not coprime'].
      ^(self w egcd: self h) letArrayInBlock:
           [ :cx :cy |
             (bestx * cy * self h)
             + (besty * cx * self w)
             \\ (self w * self h) ] ]
]

Bathroom class extend [
    width: w height: h [^self x: w y: h]
]

theBathroom := Bathroom width: 101 height: 103.

AOC input: [ stdin toLines asArray collect: [ :it | Robot new parse: it ] ];
    part1: [ :robots | | posns |
             posns := robots collect: [:it | it posAt: 100 in: theBathroom].
             theBathroom countQuadrants: posns ];
    part2: [ :robots | theBathroom findTreeTimeOf: robots ];
    result: [ :robots :part | part value: robots. ];
    finish.
