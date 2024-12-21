Object subclass: KeyseqSet [
    | seqs size length |
    size [^size]
    length [^length]

    seqs: s
    [ s do: [
          :e | | len sz |
          len := self lengthOf: e.
          sz := self sizeOf: e.
          length
              ifNil: [
              length := len. size := sz.
              seqs := OrderedCollection with: e.
          ]
              ifNotNil: [
                  :l |
                  len = l ifTrue: [
                      size := size + 1.
                      seqs add: e.
                  ] ifFalse: [
                      len < l ifTrue: [
                          length := len. size := sz.
                          seqs := OrderedCollection with: e.
                      ]
                  ]]
      ].
      seqs := seqs asArray.
    ]

    sizeOf: e [e isString ifTrue: [^1] ifFalse: [^e size]]
    lengthOf: e [e isString ifTrue: [^e size] ifFalse: [^e length]]
    allOf: e [e isString ifTrue: [^{e}] ifFalse: [^e allOf]]

    allOf
    [ | set |
      set := Set new.
      seqs collect: [:e | set addAll: (self allOf: e)].
      ^set ]

    printOn: st [ st << self class << '['
                  << 's=' << size << ',l=' << length
                  << ']' <<  seqs ]
]

KeyseqSet subclass: KeyseqCat [
    seqs: s
    [ seqs := s asArray.
      size := seqs inject: 1 into: [:a :r | a * (self sizeOf: r)].
      length := seqs inject: 0 into: [:a :r | a + (self lengthOf: r)] ]

    allOf
    [ | set |
      set := Set new.
      self allOfInto: set from: 1 pfx: ''.
      ^set ]

    allOfInto: set from: i pfx: pfx
    [ | e | i > seqs size ifTrue: [^set add: pfx].
      e := seqs at: i.
      (self allOf: e) do: [
          :str | self allOfInto: set from: i + 1 pfx: pfx,str]]
]

Object subclass: Keypad [
    | grid paths optimal |
    grid: g
    [ grid := g.
      paths := LookupTable new.
      grid allPosnsDo: [:pos | self pathsFrom: pos] ]
    paths [^paths]

    pathsFrom: pos
    [ (grid at: pos) = $. ifTrue: [^self].
      grid allPosnsDo: [:opos | self pathsFrom: pos to: opos] ]

    pathsFrom: pos to: opos
    [ | dx dy c1 c2 seqs |
      c2 := grid at: opos.
      c2 = $. ifTrue: [^self].
      c1 := grid at: pos.

      dx := opos x - pos x.
      dy := opos y - pos y.
      seqs := self allPathsFrom: pos by: dx and: dy prefix: ''.
      paths at: (String with: c1 with: c2) put: seqs ]

    allPathsFrom: pos by: dx and: dy prefix: pfx
    [ | set sym oset |
      (grid at: pos) = $. ifTrue: [^#()].
      (dx = 0 and: [dy = 0]) ifTrue: [^{pfx,'A'}].
      set := OrderedCollection new.
      dx = 0 ifFalse: [
          sym := dx < 0 ifTrue: ['<'] ifFalse: ['>'].
          oset := self allPathsFrom: pos + (Posn x: dx sign y: 0)
                       by: dx - dx sign and: dy prefix: pfx,sym.
          set addAll: oset.
      ].
      dy = 0 ifFalse: [
          sym := dy < 0 ifTrue: ['^'] ifFalse: ['v'].
          oset := self allPathsFrom: pos + (Posn x: 0 y: dy sign)
                       by: dx and: dy - dy sign prefix: pfx,sym.
          set addAll: oset.
      ].
      ^set ]

    pressPath: key to: okey [ ^paths at: (String with: key with: okey) ]

    printOn: st [ st << grid ]

    "Replace the path set for each key pair with a singleton set of the
     best path, in terms of keypresses required by a sufficiently long chain."
    optimisePaths
    [ | robot |
      optimal ifNotNil: [^self].
      optimal := true.
      robot := Robot robotChain: 4. "seems to be a fixed point"
      paths keysAndValuesDo: [
          :key :seqs | | nseqs |
          nseqs := seqs collect: [:it | {(robot keyseqFor: it) length . it}].
          nseqs sort: [:l :r | (l at: 1) <= (r at: 1)].
          paths at: key put: {(nseqs at: 1) at: 2}]]
]
Keypad class extend [
    | numpad dirpad |
    parse: rows [ ^Keypad new grid: (Grid new rows: rows) ]
    numpad [^numpad ifNil: [
        numpad := Keypad parse:
         #('789'
           '456'
           '123'
           '.0A')]]
    dirpad [^dirpad ifNil: [
        dirpad := Keypad parse:
         #('.^A'
           '<v>')]]
]

Object subclass: Robot [
    | keypad controlledBy seqCache depth |
    keypad: kp [keypad := kp. depth := 0]
    keypad: kp controlledBy: robo
    [keypad := kp. controlledBy := robo.
     depth := robo ifNil: [0] ifNotNil: [:it | it depth + 1]]

    depth [^depth]
    initialize [seqCache := LookupTable new]

    keyseqFor: str
    [ ^seqCache at: str ifAbsent: [
               ^seqCache at: str put: (self computeKeyseqFor: str)] ]

    computeKeyseqFor: str
    [ | steps |
      steps := 0 to: str size - 1 collect: [
        :i | | paths |
        paths := keypad pressPath: (str at: i ifAbsent: [$A])
                        to: (str at: i + 1).
        controlledBy ifNotNil: [
            paths := paths collect: [:it | controlledBy keyseqFor: it]].
        KeyseqSet new seqs: paths
      ].
      ^KeyseqCat new seqs: steps ]
]

Robot class extend [
    robotChain: len
    [ len = 0 ifTrue: [^nil].
      ^self new keypad: Keypad dirpad controlledBy:
                (self robotChain: len - 1) ]

    fullChain: keypad length: len
    [ ^self new keypad: keypad controlledBy: (self robotChain: len) ]
]

AOC input: [ Keypad dirpad optimisePaths.
             "Keypad numpad optimisePaths. -- it's not even worth it"
             stdin toLines asArray ];
    part1: 2; part2: 25; partN: 100 ":D";
    result: [ :codes :chainLen | | robot |
              robot := Robot fullChain: Keypad numpad length: chainLen.
              (codes collect: [
                  :code | (robot keyseqFor: code) length * code asNumber])
                  sum ];
    finish.
