Object subclass: TowelChars [
    | table |
    initialize
    [ table := LookupTable new.
      table at: $w put: 1;
            at: $u put: 2;
            at: $b put: 3;
            at: $r put: 4;
            at: $g put: 5 ]

    digitFor: c [ ^table at: c ifAbsent: [self error: 'Bad character'] ]

    charsToDigits: cs [ ^cs asArray collect: [:c | self digitFor: c] ]

    countWays: design with: trie
    [ | dp | dp := Array new: design size + 1 withAll: 0.
      dp at: 1 put: 1.
      1 to: design size do: [
          :i | | subtrie j c cnt |
          cnt := dp at: i.
          cnt > 0 ifTrue: [
              subtrie := trie.
              j := i.
              [ j <= design size and: [subtrie notNil] ] whileTrue: [
                  c := design at: j.
                  j := j + 1.
                  subtrie := subtrie at: c.
                  subtrie ifNotNil: [
                      subtrie isPresent ifTrue:
                          [dp at: j put: cnt + (dp at: j)]].
              ]]].
      ^dp at: dp size. ]
]

Object subclass: Trie [
    | isRoot children |
    initialize [ isRoot := false. children := Array new: 5. ]

    isPresent [ ^isRoot ]

    at: c [ ^children at: c ifAbsent: [^nil] ]
    getOrCreateAt: c
    [ | t | t := children at: c.
      t ifNil: [t := children at: c put: Trie new].
      ^t ]

    add: arr [ ^self add: arr startingAt: 1 ]
    add: arr startingAt: i
    [ | c | c := arr at: i ifAbsent: [isRoot := true. ^self].
      (self getOrCreateAt: c) add: arr startingAt: i + 1. ]

    printOn: st [ st << (isRoot ifTrue: ['X'] ifFalse: ['O']) << children ]
]

AOC input: [ stdin contents splitDoubleNl letArrayInBlock: [
               :patterns :designs |
               | chars trie ways | chars := TowelChars new.
               trie := Trie new.
               patterns chain tokenize: ', ';
                        collect: [:it | chars charsToDigits: it];
                        do: [:it | trie add: it].
               ways := designs lines chain
                           collect: [:it | chars charsToDigits: it];
                           collect: [:it | chars countWays: it with: trie].
               ways ] ];
    part1: [ :ways | ways count: [:it | it > 0] ];
    part2: [ :ways | ways sum ];
    result: [ :ways :part | part value: ways ];
    finish
    
