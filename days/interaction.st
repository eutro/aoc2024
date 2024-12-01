Object subclass: ExplorerStream [
    | stream objects | on: aStream
    [ stream := aStream.
      objects := OrderedCollection new. ]

    title: title [ stream << '# ' << title ; nl. ]
    subtitle: block [ stream << '**'. block value. stream << '**'; nl; nl. ]
    text: text [ text displayOn: stream ]
    nl [ stream nl ]

    linkTextTo: obj name: text
    [ |r| r := '[%1][%2]' % {text . objects size}.
      objects add: obj. ^r ]

    linkTo: obj name: text [ self text: (self linkTextTo: obj name: text). ]

    section: title do: block [ self text: '## '; text: title; nl. block value ]
    ssection: title do: block [ self text: '### '; text: title; nl. block value ]
]
ExplorerStream class extend [ on: stream [ ^super new on: stream ] ]

Object extend [
    emacsExploreOn: exs
    [ self basicEmacsExploreOn: exs;
           emacsExploreMembersOn: exs]

    basicEmacsExploreOn: exs
    [ exs title: self;
          subtitle: [
          exs text: 'of class: ';
              linkTo: self class name: self class ] ]

    emacsExploreMembersOn: exs []
]

Class extend [
    emacsExploreOn: exs
    [ self basicEmacsExploreOn: exs.
      self superclass ifNotNil: [
          :sc | exs subtitle: [
              exs text: 'Superclass: ';
                  linkTo: sc name: sc.
          ] ].
      self category ifNotNil: [
          :cat | exs subtitle: [ exs text: 'Category: '; text: cat ] ].
      self comment
          ifNotNil: [ :cmnt | exs text: cmnt; nl; nl ]
          ifNil: [ exs text: 'No comment.'; nl; nl ].

      self subclasses size > 0 ifTrue: [
          exs subtitle: [
          exs text: 'Subclasses: '.
          self subclasses
              do: [ :sc | exs linkTo: sc name: sc ]
              separatedBy: [ exs text: ', ' ]. ] ].

      exs section: 'Own members' do:
          [ self selectors do: [
              :selector | | method |
              exs ssection: selector asString do:
                  [ (self sourceCodeAt: selector ifAbsent: [nil])
                        ifNotNil: [ :source | exs text: source ]
                        ifNil: [ exs text: 'No source available.' ] ]; nl; nl.
          ] ]
    ]
]
