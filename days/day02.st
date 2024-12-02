Collection extend [
    checkSafeStep: a to: b
    [ |delta| (a < 1) | (b > self size) ifTrue: [^true].
      delta := (self at: b) - (self at: a).
      ^(1 <= delta) & (delta <= 3) ]

    safetyFwd
    [ | dp | dp := Array new: self size + 1.
      (self size to: 0 by: -1) do: [
          :i | | v |
          v := i + 1 to: self size + 1.
          v := v select: [ :j | self checkSafeStep: i to: j ].
          v := v collect: [ :j | (dp at: (j + 1) ifAbsent: [0]) + (j - i - 1) ].
          v := v fold: [ :l :r | l min: r ].
          dp at: (i + 1) put: v ].
      ^dp at: 1 ]
    safety [ ^self safetyFwd min: self reverse safetyFwd ]
]

AOC input: [ stdin toLines collect:
                 [ :ln | ((ln tokenize: ' ')
                              collect: [ :lvl | lvl scanf: '%d' with: [:e|e] ])
                             safety ] ];
    part1: 0; part2: 1;
    result: [ :reports :damp | reports count: [ :it | it <= damp ] ];
    finish