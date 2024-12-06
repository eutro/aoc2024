Iterable extend [
    orPats [ ^self fold: [:l :r | l or: r] ]
]

Object subclass: Pattern [
    | orig symAndPosns | orig: n [orig:=n] symAndPosns: n [symAndPosns:=n]
    occursAt: pos in: grid
    [ ^symAndPosns allSatisfy: [
          :sym :rel |
          "stdout << (pos + rel) << ' = ' << sym << ' ' << ((grid at: pos + rel) = sym); nl."
          (grid at: pos + rel) = sym ] asSpreader ]
    countIn: grid
    [ ^(grid allPosnsOf: orig) count: [ :pos | self occursAt: pos in: grid ] ]
    or: oPat [ ^UnionPattern new patterns: {self. oPat} ]
    mapPosns: block
    [ ^Pattern new orig: orig;
               symAndPosns: (
                   symAndPosns collect: [
                   :sym :rel | {sym . block value: rel}
               ] asSpreader) ]
]
Object subclass: UnionPattern [
    | patterns | patterns: p [patterns:=p]
    countIn: grid
    [ ^patterns inject: 0 into: [ :acc :p | acc + (p countIn: grid) ] ]
    or: oPat [ ^UnionPattern new patterns: patterns, {oPat} ]
]

AOC input: [ Grid new rows: (stdin toLines) ];
    part1: (#((0 1) (0 -1) (1 0) (-1 0) (1 1) (-1 -1) (1 -1) (-1 1))
                asPosns chain collect:
                [ :pos | Pattern new orig: $X;
                       symAndPosns: (
                           #($M $A $S)
                               with: (1 to: 3)
                               collect: [:c :n | {c. pos * n}])];
             orPats);
    part2: ((#((1 1) (-1 -1)) asPosns collect:
                 [ :l | #((-1 1) (1 -1)) asPosns collect:
                      [ :r | Pattern new orig: $A;
                           symAndPosns: {{$M.l*-1}.{$S.l}.{$M.r*-1}.{$S.r}} ]])
                join orPats);
    result: [ :grid :pat | pat countIn: grid ];
    finish.
