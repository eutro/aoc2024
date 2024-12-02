Collection extend [
    checkSafeStep: a to: b
    [ (a < 1) | (b > self size) ifTrue: [^true].
      ^(self at: b) - (self at: a) between: 1 and: 3 ]

    safetyFwd
    [ | dp | dp := Array new: self size + 1.
      (self size to: 0 by: -1) do: [
          :i | i chain + 1;
             to: i + 2; "or `self size + 1`, but only jumps of 1 are possible"
             select: [ :j | self checkSafeStep: i to: j ];
             collect: [ :j | (dp at: (j + 1) ifAbsent: [0]) + (j - i - 1) ];
             minimum; >* [ :v | dp at: (i + 1) put: v ] ].
      ^dp at: 1 ]
    safety [ ^self safetyFwd min: self reverse safetyFwd ]
]

AOC input: [ stdin toLines collect: [
               :ln | ln chain tokenize: ' ';
                   collect: [ :lvl | lvl asNumber ]; safety ] ];
    part1: 0; part2: 1;
    result: [ :reports :damp | reports count: [ :it | it <= damp ] ];
    finish
