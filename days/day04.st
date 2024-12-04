Object subclass: Posn [
    | x y | x [^x] y [^y] x: n [x:=n] y: n [y:=n]
    + o [ ^Posn x: x + o x y: y + o y ]
    * n [ ^Posn x: x * n y: y * n ]
    rotateCw [ ^Posn x: y y: x negated ]
    rotateCcw [ ^Posn x: y negated y: x ]
    rotate180 [ ^Posn x: x negated y: y negated ]

    printOn: s [ s << '<' << x << ', ' << y << '>'. ]
]
Posn class extend [
    x: x y: y [ ^Posn new x: x; y: y ]
    up: n [ ^Posn x: 0 y: n negated ]
    down: n [ ^Posn x: 0 y: n ]
    left: n [ ^Posn x: n negated y: 0 ]
    right: n [ ^Posn x: n y: 0 ]
    up [^Posn up: 1]
    down [^Posn down: 1]
    left [^Posn left: 1]
    right [^Posn right: 1]
]

Array extend [ asPosn [ ^Posn x: (self at: 1) y: (self at: 2) ] ]
Iterable extend [
    orPats [ ^self fold: [:l :r | l or: r] ]
    asPosns [ ^self collect: [:it | it asPosn] ]
]

Object subclass: Grid [
    | rows | rows: n [rows:=n]
    at: pos [ ^(rows at: pos y ifAbsent: [^nil]) at: pos x ifAbsent: [nil] ]
    height [^rows size] width [^(rows at: 1) size]
    allPosnsOf: elt
    [ | oc p | oc := OrderedCollection new.
      (1 to: self height) do:
          [ :y | (1 to: self width) do:
               [ :x | p := Posn x: x y: y.
                 elt = (self at: p) ifTrue: [ oc add: p ] ] ].
      ^oc ]

    printOn: st [ rows do: [ :r | st << r; nl ] ]
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
