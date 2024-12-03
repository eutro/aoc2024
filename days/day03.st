Object subclass: VM [
    | enabled total |
    initialize [ enabled := true. total := 0. ]
    total [ ^total ]

    interpret: insns
    [ insns do: [
        :it |
        (it scanf: 'mul(%d, %d)'
            with: [
                 :l :r |
                 enabled ifTrue: [
                     total := total + (l * r)
                 ]
             ])
        ifNil: [ enabled := (it = 'do()') ]. ].
      ^total ]
]

AOC input: [ stdin contents ];
    part1: 'mul\([0-9]{1,3},[0-9]{1,3}\)';
    part2: 'mul\([0-9]{1,3},[0-9]{1,3}\)|do\(\)|don''t\(\)';
    result: [ :inp :re | VM new interpret: (inp chain allOccurrencesOfRegex: re) ];
    finish
