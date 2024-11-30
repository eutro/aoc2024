Object subclass: CharPattern [
    | pattern assoc next |
    pattern [ ^pattern ]
    assoc [ ^assoc ]
    init: patIn
    [ pattern := patIn.
      assoc := patIn tokenize: '\|'.
      ^self ]

    next: nextPat
    [ nextPat ifNotNil: [
        next := nextPat.
        pattern := pattern,'|',nextPat pattern ] ]

    findIn: line
    [ ^(line searchRegex: pattern) match ]

    interpWord: word
    [ ^(assoc indexOf: word ifAbsent: [nil])
          ifNil: [ next interpWord: word ]
          ifNotNil: [:it | it - 1] ]

    findAndInterp: line
    [ ^self interpWord: (self findIn: line) ]

    reversed
    [ ^(CharPattern
            new: ((assoc collect: [:it | it reverse])
                      join: '|'))
           next: next ]
]

CharPattern class extend [
    new: pattern
    [ | r | r := super new. r init: pattern. ^r ]
]

String extend [
    calibration: pat rpat: rpat
    [ | d1 d2 |
      d1 := pat findAndInterp: self.
      d2 := rpat findAndInterp: self reverse.
      ^(d1 * 10) + d2 ]
]

OrderedCollection extend [
    collectPart: pat
    [ | rpat |
      rpat := pat reversed.
      ^self collect: [
          :line |
          [ line calibration: pat rpat: rpat ]
          ifError: [ :exn | exn ]
      ]
    ]
]

pat1 := CharPattern new: '0|1|2|3|4|5|6|7|8|9'.
pat2 := CharPattern new: 'zero|one|two|three|four|five|six|seven|eight|nine'.
pat2 next: pat1.

AOC part1: pat1;
    part2: pat2;
    arg: [ FileStream stdin toLines ];
    map: [ :lines :pat | (lines collectPart: pat) sum ];
    finish.
