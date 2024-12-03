Object subclass: VM [
    | enabled total mulOnly |
    initialize [ enabled := true. total := 0. ]
    mulOnly: it [ mulOnly := it ]

    do: doOrDont [ mulOnly ifFalse: [ enabled := doOrDont ] ]
    do [ self do: true ] dont [ self do: false ]

    perform: insn
    [ insn = 'do()' ifTrue: [ ^self do ].
      insn = 'don''t()' ifTrue: [ ^self dont ].
      enabled ifTrue: [
          insn scanf: 'mul(%d,%d)'
               with: [ :l :r | total := total + (l * r) ] ] ]

    interpret: insns [ insns do: [ :insn | self perform: insn ]. ^total ]
]

AOC input: [ stdin contents allOccurrencesOfRegex:
                 'mul\([0-9]{1,3},[0-9]{1,3}\)|do\(\)|don''t\(\)' ];
    part1: true; part2: false;
    result: [ :insns :part | VM new mulOnly: part; interpret: insns ];
    finish
