AOC part1: [ :lines | | sorted |
             sorted := (lines collect: [ :it | it asSortedCollection readStream ]).
             ((sorted at: 1) with: (sorted at: 2)) collect:
                 [ :arr | ((arr at: 1) - (arr at: 2)) abs ] ];
    part2: [ :lines | | counts | counts := Dictionary new.
             (lines at: 2) do: [
                 :elt | counts at: elt
                               put: 1 + (counts at: elt ifAbsent: [0]) ].
             (lines at: 1) collect: [ :elt | elt * (counts at: elt ifAbsent: [0]) ] ];
    arg: [ | inp l r | inp := FileStream stdin toLines collect: [ :ln | ln scanf: '%d %d' ].
           l := inp collect: [ :e | e at: 1 ].
           r := inp collect: [ :e | e at: 2 ].
           {l . r} ];
    map: [ :lines :part | (part value: lines) sum ];
    finish.
