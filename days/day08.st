Grid subclass: AntennaGrid [
    | antennae |
    rows: r [ super rows: r. self indexAntennae ]

    indexAntennae
    [ antennae := Dictionary new.
      self allPosnsDo: [
        :pos | | c | c := self at: pos.
        c = $. ifFalse: [ antennae at: c inA: Set add: pos ]
    ] ]

    antinodesWithOffsets: offs
    [ | set | set := Set new.
      antennae do:
          [ :posns | self putAntinodesBetween: posns into: set with: offs ].
      ^set ]

    putAntinodesBetween: posns into: set with: offs
    [ posns do: [
        :p1 | posns do: [
            :p2 | p1 = p2 ifFalse: [
                self antinodeFrom: p1 to: p2 into: set with: offs.
                self antinodeFrom: p2 to: p1 into: set with: offs.
            ]]]]

    antinodeFrom: src to: dst into: set with: offs
    [ | delta pos | delta := dst - src.
      offs do: [
          :off | pos := dst + (delta * off).
          (self isInBounds: pos) ifFalse: [^self].
          set add: pos ]]
]

AOC input: [ AntennaGrid new rows: stdin toLines asArray ];
    part1: #(1);
    part2: (0 to: 9999);
    result: [ :grid :part | (grid antinodesWithOffsets: part) size ];
    finish.
