AOC input: [ (stdin toLines collect: [ :ln | ln scanf: '%d %d' ])
                 transposed collect: [ :it | it asSortedCollection ] ];
    part1: [ :left :right | left with: right collect: [ :l :r | (l - r) abs ] ];
    part2: [ :left :right | | counts | counts := left toBag.
             right collect: [ :elt | elt * (counts occurrencesOf: elt) ] ];
    result: [ :input :part | (part valueWithArguments: input) sum ];
    finish.
