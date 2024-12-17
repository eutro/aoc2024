Object subclass: VM [
    | A B C ip optable code |

    copyFrom: vm [ code := vm code. ]

    A:n[A:=n] B:n[B:=n] C:n[C:=n]
    code: n [code := n]
    code [^code]

    parse: str
    [ str scanf: ' Register A: %d Register B: %d Register C: %d Program: %s'
          with: [
              :a :b :c :prog |
              A := a. B := b. C := c.
              code := (prog tokenize: ',') collect: [:it | it asNumber].
          ] ]

    printOn: st
    [ st << ('A: %1, B: %2, C: %3, ip: %4' % {A. B. C. ip}) ]

    initialize
    [ ip := 1.
      optable :=
          {[:x|self adv: (self combo: x)].
           [:x|self bxl: x].
           [:x|self bst: (self combo: x)].
           [:x|self jnz: x + 1].
           [:x|self bxc: x].
           [:x|self out: (self combo: x)].
           [:x|self bdv: (self combo: x)].
           [:x|self cdv: (self combo: x)]}. ]

    combo: v
    [ v <= 3 ifTrue: [^v].
      v <= 5 ifTrue: [v = 4 ifTrue: [^A] ifFalse: [^B]]
             ifFalse: [v = 6 ifTrue: [^C] ifFalse: [self error: 'Reserved']] ]

    step
    [ | insn arg |
      insn := code at: ip.
      arg := code at: ip + 1.
      ip := ip + 2.
      ^(optable at: insn + 1) value: arg ]

    run
    [ ip := 1. [ ip < code size ] whileTrue: [ self step ] ]

    "adv bxl bst jnz bxc out bdv cdv"
]

VM subclass: DisassVM [
    | os |
    initialize
    [ super initialize.
      A := 'A'. B := 'B'. C := 'C'.
      os := stdout ]
    output [nil]
    adv:x [os<<('A <- A >> %1'%{x});nl]
    bxl:x [os<<('B <- B ^ %1'%{x});nl]
    bst:x [os<<('B <- %1 & 7'%{x});nl]
    jnz:x [os<<('A != 0 ? goto %1'%{x});nl]
    bxc:x [os<<('B <- B ^ C');nl]
    out:x [os<<('output %1 & 7'%{x});nl]
    bdv:x [os<<('B <- A >> %1'%{x});nl]
    cdv:x [os<<('C <- A >> %1'%{x});nl]
]

VM subclass: InterpVM [
    | out |
    initialize [super initialize. out := OrderedCollection new.]
    output [^out]
    adv:x [A := A bitShift: x negated]
    bxl:x [B := B bitXor: x]
    bst:x [B := x bitAnd: 7]
    jnz:x [A ~= 0 ifTrue: [ip := x]]
    bxc:x [B := B bitXor: C]
    out:x [out add: (x bitAnd: 7)]
    bdv:x [B := A bitShift: x negated]
    cdv:x [C := A bitShift: x negated]
]

InterpVM subclass: ForcingVM [
    | output |

    out: x [ output notNil ifTrue: [self error: 'Double output'].
             output := x bitAnd: 7. ]
    jnz: x [ ip > code size & x = 1 ifFalse: [self error: 'Bad jump'] ]

    runWithA: a
    [ A := a. output := nil. self run ]

    forceOutputAt: idx withA: a
    [ | requiredOut |
      "The code looks something like this:

      B <- A & 7
      ... mixing B with A and C
      A <- A >> 3
      output B & 7
      A != 0 ? goto 1

      So in each iteration, A is shifted by 3 bits, and one value is output.
      Thus we simply go backwards starting with 0 trying each bit triple."

      idx = 0 ifTrue: [^a].
      requiredOut := code at: idx.
      0 to: 7 do: [
          :x | | testA |
          testA := (a bitShift: 3) + x.
          self runWithA: testA.
          A ~= a ifTrue: [self error: 'Bad program'].
          (output = requiredOut) ifTrue: [
              (self forceOutputAt: idx - 1 withA: testA) ifNotNil:
                  [:res | ^res]]].
      ^nil ]
]

AOC input: [ InterpVM new parse: stdin contents ];
    part1: [ :vm | vm run output chain collect: [:it | it displayString]; join: ',' ];
    part2: [ :vm | ForcingVM new copyFrom: vm; forceOutputAt: vm code size withA: 0 ];
    result: [ :vm :part | part value: vm ];
    finish.
