Object subclass: AddOp
[ tryDo: n target: tgt [ ^tgt - n ] ]
Object subclass: MulOp
[ tryDo: n target: tgt
  [ (tgt rem: n) = 0 ifFalse: [^nil].
    ^tgt quo: n ] ]
MulOp subclass: CatOp
[ tryDo: n target: tgt
  [ | p10 |
    p10 := (n + 1) ceilingLog: 10.
    p10 := 10 raisedTo: p10.
    ^super tryDo: p10 target: tgt - n ] ]

Object subclass: Equation [
    | target numbers |
    target: n [target:=n] numbers: n [numbers:=n]

    testValue [^target]

    printOn: dst [dst << target << ': ' << numbers]

    "Backtracking search right-to-left over the number sequence."
    canSolveFrom: idx target: tgt with: ops
    [ | n n1 ok | n := numbers at: idx.
      idx = 1 ifTrue: [^n = tgt].
      tgt < n ifTrue: [^false].
      ops do: [
          :op | n1 := op tryDo: n target: tgt.
          n1 ifNotNil: [
              ok := self canSolveFrom: idx - 1 target: n1 with: ops.
              ok ifTrue: [^true] ]].
      ^false ]
    canSolveWith: ops [^self canSolveFrom: numbers size target: target with: ops]
]

Equation class extend [
    parse: line
    [^(line tokenize: ': ') letArrayInBlock: [
         :target :numbers |
         Equation new target: target asNumber;
                 numbers: ((numbers tokenize: ' ')
                               collect: [:it|it asNumber])]]
]

AOC input: [ stdin toLines collect: [:line | Equation parse: line] ];
    part1: {MulOp new. AddOp new};
    part2: {CatOp new. MulOp new. AddOp new};
    result: [ :eqns :part |
              eqns chain select: [:eqn | eqn canSolveWith: part];
                    collect: [:it | it testValue];
                    sum ];
    finish.
