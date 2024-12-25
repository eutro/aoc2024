Object subclass: KeyOrLock [
    | isLock height value |
    value [^value]
    isLock [^isLock]

    rows: r
    [ | cols |
      cols := r lines transposed.
      isLock := cols allSatisfy: [:it | (it at: 1) = $#].
      height := (cols at: 1) size.
      value := cols collect: [
          :it | | first | first := it at: 1.
          it count: [:c | c = first]] ]
]

AOC input: [ stdin contents splitDoubleNl collect:
                 [:it | KeyOrLock new rows: it] ];
    part1: [ :input | | locks keys minPos total |
             locks := OrderedCollection new.
             keys := OrderedCollection new.
             input do: [:it | it isLock
                            ifTrue: [locks add: it value]
                            ifFalse: [keys add: it value]].
             locks := KDTree fromPoints: locks.

             total := 0.
             minPos := Array new: (keys at: 1) size withAll: 0.
             keys do: [
                 :key | locks from: minPos to: key do: [
                     :lock |
                     ((key with: lock) allSatisfy: [
                         :k :l | k >= l ] asSpreader)
                     ifTrue: [total := total + 1]]].
             total ];
    result: [ :input :part | part value: input ];
    finish.
