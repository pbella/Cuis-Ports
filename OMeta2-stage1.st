'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 1:17:33 pm'!
Exception subclass: #OM2Fail
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
Object subclass: #OM2Failer
	instanceVariableNames: 'used'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
Object subclass: #OM2Stream
	instanceVariableNames: 'head tail memo'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OM2Stream subclass: #OM2StreamProxy
	instanceVariableNames: 'target'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OM2Stream subclass: #OM2StreamDebugger
	instanceVariableNames: 'om2stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OM2Stream subclass: #OM2LazyStream
	instanceVariableNames: 'stream pos'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OM2Stream subclass: #OM2EndOfStream
	instanceVariableNames: 'stream pos'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
Compiler subclass: #OMeta2Compiler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
Object subclass: #OMeta2Base
	instanceVariableNames: 'input om2streams haltingPoint'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OMeta2Base subclass: #OMeta2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OMeta2 subclass: #OMeta2RuleParser
	instanceVariableNames: 'temps'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OMeta2 subclass: #OMeta2RuleTranslator
	instanceVariableNames: 'grammarClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OMeta2 subclass: #OMeta2Optimizer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OMeta2 subclass: #OMeta2Flattener
	instanceVariableNames: 'ws'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!

Smalltalk at: #OMeta2Fail put: OM2Fail new!

!OM2Fail methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:03'!
defaultAction

	self error: 'match failed'! !


!OM2Failer methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 14:23'!
initialize

	used := false! !

!OM2Failer methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 13:44'!
used

	^ used! !

!OM2Failer methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 13:42'!
value

	used := true.
	OMeta2Fail signal! !



!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:04'!
basicTail

	^ tail! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:38'!
forgetEverything

	memo := IdentityDictionary new! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 11:35'!
head

	^ head! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 13:38'!
initHead: h tail: t

	head := h.
	tail := t! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 11:34'!
initialize

	memo := IdentityDictionary new! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/19/2009 23:13'!
inputSpecies

	^ Array! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 14:13'!
memo

	^ memo! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:51'!
pos

	^ -1! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:05'!
printOn: aStream

	| inputIsString curr |
	inputIsString := (self inputSpecies inheritsFrom: String) and: [(self inputSpecies inheritsFrom: Symbol) not].
	curr := self.
	aStream
		nextPutAll: 'an ';
		nextPutAll: self class name;
		nextPut: $(.
	[curr notNil] whileTrue: [
		(curr isKindOf: OM2EndOfStream) ifTrue: [
			aStream nextPut: $).
			^ self
		].
		inputIsString
			ifTrue: [aStream nextPut: curr head]
			ifFalse: [
				curr head printOn: aStream.
				aStream space
			].
		curr := curr basicTail.
	].
	aStream nextPutAll: '...)'! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 11:35'!
tail

	^ tail! !

!OM2Stream methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:40'!
transitiveForgetEverything

	| curr |
	curr := self.
	[curr notNil] whileTrue: [
		curr forgetEverything.
		curr := curr basicTail
	]! !

!OM2StreamProxy methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:05'!
basicTail

	^ target basicTail! !

!OM2StreamProxy methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 12:08'!
head

	head ifNil: [head := target head].
	^ head
! !

!OM2StreamProxy methodsFor: 'as yet unclassified' stamp: 'aw 2/19/2009 23:45'!
initTarget: anOM2Stream

	target := anOM2Stream! !

!OM2StreamProxy methodsFor: 'as yet unclassified' stamp: 'aw 2/19/2009 23:13'!
inputSpecies

	^ target inputSpecies! !

!OM2StreamProxy methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:51'!
pos

	^ target pos! !

!OM2StreamProxy methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 12:07'!
tail

	tail ifNil: [tail := OM2StreamProxy for: target tail].
	^ tail! !

!OM2StreamProxy methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 14:10'!
target

	^ target! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

OM2StreamProxy class
	instanceVariableNames: ''!

!OM2StreamProxy class methodsFor: 'as yet unclassified' stamp: 'aw 2/19/2009 23:44'!
for: anOM2Stream

	^ self new initTarget: anOM2Stream! !


!OM2StreamDebugger methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 13:28'!
forgetEverything

	om2stream forgetEverything! !

!OM2StreamDebugger methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 13:40'!
head

	^ om2stream head! !

!OM2StreamDebugger methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 15:26'!
initOm2stream: anOM2Stream

	om2stream := anOM2Stream! !

!OM2StreamDebugger methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 13:27'!
memo

	^ om2stream memo! !

!OM2StreamDebugger methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 15:57'!
printOn: aStream

	aStream nextPutAll: 'an OM2StreamDebugger('.
	om2stream printOn: aStream.
	aStream nextPut: $)! !

!OM2StreamDebugger methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 13:40'!
tail

	^ om2stream tail! !

!OM2StreamDebugger methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 13:29'!
transitiveForgetEverything

	om2stream transitiveForgetEverything! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

OM2StreamDebugger class
	instanceVariableNames: ''!

!OM2StreamDebugger class methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 15:28'!
for: anOM2Stream

	^ self new initOm2stream: anOM2Stream! !

!OM2LazyStream methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:50'!
initHead: h stream: s pos: p

	head := h.
	stream := s.
	pos := p! !

!OM2LazyStream methodsFor: 'as yet unclassified' stamp: 'aw 2/19/2009 23:15'!
inputSpecies

	^ stream originalContents species! !

!OM2LazyStream methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:43'!
pos

	^ pos! !

!OM2LazyStream methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:42'!
tail

	tail ifNil: [tail := OM2LazyStream for: stream withPos: pos + 1].
	^ tail! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

OM2LazyStream class
	instanceVariableNames: ''!

!OM2LazyStream class methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:44'!
for: aReadStream withPos: pos

	^ aReadStream atEnd
		ifTrue: [OM2EndOfStream new initStream: aReadStream pos: pos]
		ifFalse: [self new initHead: aReadStream next stream: aReadStream pos: pos]! !

!OM2EndOfStream methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:29'!
head

	OMeta2Fail signal! !

!OM2EndOfStream methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:50'!
initStream: s pos: p

	stream := s.
	pos := p! !

!OM2EndOfStream methodsFor: 'as yet unclassified' stamp: 'aw 2/19/2009 23:14'!
inputSpecies

	^ stream originalContents species! !

!OM2EndOfStream methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:49'!
pos

	^ pos! !

!OM2EndOfStream methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:29'!
tail

	OMeta2Fail signal! !


!OMeta2Compiler methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:30'!
compile: origCode in: cls classified: aCategory notifying: notify ifFail: failBlock

	^ self compile: origCode in: cls notifying: notify ifFail: failBlock! !

!OMeta2Compiler methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:57'!
compile: origCode in: cls notifying: notify ifFail: failBlock

	| origCodeStream parseTree structuredCode translatedCode |
	origCodeStream := origCode asString readStream.
	self from: origCodeStream class: cls context: nil notifying: notify.
	[
		parseTree := OMeta2RuleParser matchStream: origCodeStream with: #rule withArgs: #() withPlaybackDebugging: false.
		parseTree := OMeta2Optimizer match: parseTree with: #optimizeRule.
		structuredCode := OMeta2RuleTranslator match: parseTree with: #translate withArgs: {cls}.
		translatedCode := OMeta2Flattener match: structuredCode with: #flatten
	] on: OM2Fail do: [
		self notify: '<-- parse error around here -->' at: origCodeStream position.
		^ failBlock value
	].
	^ Compiler new
		compile: translatedCode readStream
		in: cls
		notifying: notify
		ifFail: failBlock! !

!OMeta2Compiler methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 13:10'!
parserClass

	^ self class! !

!OMeta2Compiler methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 13:11'!
parse: aStream class: aClass noPattern: noPattern context: ctxt notifying: req ifFail: aBlock

	^ [
		| parseTree structuredCode code |
		parseTree := OMeta2RuleParser matchAll: aStream contents with: #rule.
		structuredCode := OMeta2RuleTranslator match: parseTree with: #translate withArgs: {aClass}.
		code := OMeta2Flattener match: structuredCode with: #flatten.
		Parser new parse: code readStream class: aClass noPattern: noPattern context: ctxt notifying: req ifFail: aBlock
	] on: OM2Fail do: [aBlock value]! !



!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 15:13'!
anything

	| ans |
	ans := input head.
	input := input tail.
	^ ans! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 14:01'!
apply

	| aRule |
	aRule := self apply: #anything.
	^ self apply: aRule! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 14:09'!
apply: aRule

	" A memoRec is an association whose key is the answer,
	  and whose value is the next input. Failers pretend to
     be memoRecs, but throw a fail in response to #value "
	| memo memoRec |
	input == haltingPoint ifTrue: [self halt].
	memo := input memo.
	memoRec := memo at: aRule ifAbsent: [nil].
	memoRec ifNil: [
		| origInput failer ans |
		origInput := input.
		failer := OM2Failer new.
		memo at: aRule put: failer.
		ans := self perform: aRule.
		memoRec := ans -> input.
		memo at: aRule put: memoRec.
		failer used ifTrue: [
			" left recursion detected "
			| sentinel keepGoing |
			sentinel := input.
			keepGoing := true.
			[keepGoing] whileTrue: [
				[
					input := origInput.
					ans := self perform: aRule.
					input == sentinel ifTrue: [OMeta2Fail signal].
					memoRec key: ans value: input.
				] on: OM2Fail do: [keepGoing := false]
			]
		]		
	].
	input := memoRec value.
	^ memoRec key! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 13:39'!
apply: aRule withArgs: args

	args reverseDo: [:a | input := OM2Stream new initHead: a tail: input].
	^ self perform: aRule! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/19/2009 23:17'!
consumedBy: aBlock

	| origInput i ws |
	origInput := input.
	aBlock value.
	ws := WriteStream on: origInput inputSpecies new.
	i := origInput.
	[i == input] whileFalse: [
		ws nextPut: i head.
		i := i tail
	].
	^ ws contents! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 14:11'!
foreign

	| aGrammar aRule g ans |
	aGrammar := self apply: #anything.
	aRule := self apply: #anything.
	g := aGrammar new initInput: (OM2StreamProxy for: input).
	ans := g apply: aRule.
	input := g input target.
	^ ans! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 13:17'!
forgetEverything

	input transitiveForgetEverything.
	om2streams valuesDo: [:s | s transitiveForgetEverything]! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:33'!
form: aBlock

	| v origInput |
	v := self apply: #anything.
	self pred: (v isCollection and: [v isSequenceable and: [v isSymbol not]]).
	origInput := input.
	input := om2streams at: v ifAbsentPut: [OM2LazyStream for: v readStream withPos: 1].
	aBlock value.
	self apply: #end.
	input := origInput.
	^ v! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 15:19'!
genericMany: aBlock into: anOrderedCollection

	[
		| origInput |
		origInput := input.
		[anOrderedCollection addLast: aBlock value] on: OM2Fail do: [
			input := origInput.
			^ anOrderedCollection
		].
		true
	] whileTrue! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 11:25'!
haltingPoint: anOM2Stream

	haltingPoint := anOM2Stream! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:57'!
indexConsumedBy: aBlock

	| from to |
	from := self pos.
	aBlock value.
	to := self pos.
	^ from -> to! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 14:06'!
initInput: i

	input := i! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:16'!
initialize

	super initialize.
	om2streams := IdentityDictionary new! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 14:10'!
input

	^ input! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 13:48'!
lookahead: aBlock

	| origInput ans |
	origInput := input.
	ans := aBlock value.
	input := origInput.
	^ ans! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 13:56'!
many1: aBlock

	^ self genericMany: aBlock into: (OrderedCollection with: aBlock value)! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 13:56'!
many: aBlock

	^ self genericMany: aBlock into: OrderedCollection new! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:28'!
not: aBlock

	| origInput |
	origInput := input.
	[aBlock value] on: OM2Fail do: [
		input := origInput.
		^ true
	].
	OMeta2Fail signal! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:28'!
ometaOr: choices

	| origInput |
	origInput := input.
	choices do: [:choice |
		input := origInput.
		[^ choice value] on: OM2Fail do: []
	].
	OMeta2Fail signal! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:33'!
opt: aBlock

	^ self ometaOr: {
		[aBlock value].
		[nil]
	}! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:48'!
pos

	^ input pos! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:28'!
pred: aBooleanValue

	" may want to have the compiler inline this automatically, for performance "
	aBooleanValue ifTrue: [^ true].
	OMeta2Fail signal! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 16:16'!
seq

	| xs |
	xs := self apply: #anything.
	xs do: [:x |
		" may want to inline #apply:withArgs: below as an
		  optimization, since this rule gets used a lot "
		self apply: #exactly withArgs: {x}
	].
	^ xs
	! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 15:03'!
super: superclass apply: aRule withArgs: args

	args reverseDo: [:a | input := OM2Stream new initHead: a tail: input].
	^ self perform: aRule withArguments: #() inSuperclass: superclass! !

!OMeta2Base methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:05'!
? aBoolean

	^ self pred: aBoolean! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

OMeta2Base class
	instanceVariableNames: ''!

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 12:35'!
debugMatchAll: aSequenceableCollection with: aRule

	^ self debugMatchAll: aSequenceableCollection with: aRule withArgs: #()! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 12:53'!
debugMatchAll: aSequenceableCollection with: aRule withArgs: args

	^ self matchStream: aSequenceableCollection readStream with: aRule withArgs: args withPlaybackDebugging: true! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 12:52'!
debugMatch: anObject with: aRule

	^ self debugMatch: anObject with: aRule withArgs: #()! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 12:58'!
debugMatch: anObject with: aRule withArgs: args

	^ self debugMatchAll: {anObject} readStream with: aRule withArgs: args! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:39'!
matchAll: aSequenceableCollection with: aRule

	^ self matchAll: aSequenceableCollection with: aRule withArgs: #()! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 12:57'!
matchAll: aSequenceableCollection with: aRule withArgs: args

	^ self matchStream: aSequenceableCollection readStream with: aRule withArgs: args withPlaybackDebugging: false! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'tak 4/7/2009 14:58'!
matcherOn: aReadStream

	| input matcher |
	input := OM2LazyStream for: aReadStream withPos: 1.
	matcher := self new initInput: input.
	^ matcher! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 14:14'!
matchStream: aReadStream with: aRule withArgs: args withPlaybackDebugging: debugging

	| input matcher ans|
	input := OM2LazyStream for: aReadStream withPos: 1.
	matcher := self new initInput: input.
	[
		matcher apply: #empty withArgs: args.
		ans := matcher apply: aRule.
		matcher apply: #end.
		^ ans
	] on: OM2Fail do: [:e |
		| curr prev prevPrev |
		debugging ifFalse: [e signal].
		curr := input.
		prev := nil.
		prevPrev := nil.
		[curr notNil] whileTrue: [
			prevPrev := prev.
			prev := curr.
			curr := curr basicTail
		].
		curr := prevPrev ifNotNil: [prevPrev] ifNil: [prev].
		self inform: 'will halt each time matcher reaches ', curr printString.
		matcher
			initInput: input;
			forgetEverything.
		curr ifNil: [self error: 'you''ve found a bug -- please tell Alex'].
		curr become: (OM2StreamDebugger for: curr copy).
		matcher haltingPoint: curr.
		matcher apply: #empty withArgs: args.
		ans := matcher apply: aRule.
		matcher apply: #end.
		^ ans
	]! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:53'!
match: anObject with: aRule

	^ self match: anObject with: aRule withArgs: #()! !

!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 12:55'!
match: anObject with: aRule withArgs: args

	^ self matchAll: {anObject} readStream with: aRule withArgs: args! !





























OMeta2 subclass: #OMeta2Examples
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OMeta2 subclass: #O2SqueakRecognizer
	instanceVariableNames: ''
	classVariableNames: 'TypeTable'
	poolDictionaries: ''
	category: 'OMeta2'!
OMeta2 subclass: #OMeta2NullOpt
	instanceVariableNames: 'didSomething'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
OMeta2NullOpt subclass: #OMeta2AndOrOpt
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!

!OMeta2 methodsFor: 'as yet unclassified'!
char
	| t1 |
	t1 := self apply: #anything.
	self pred: (t1 isKindOf: Character).
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
digit
	| t1 |
	t1 := self apply: #char.
	self pred: t1 isDigit.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
empty
	^ true! !

!OMeta2 methodsFor: 'as yet unclassified'!
end
	^ self
		not: [self apply: #anything]! !

!OMeta2 methodsFor: 'as yet unclassified'!
exactly
	| t1 t2 |
	t1 := self apply: #anything.
	t2 := self apply: #anything.
	self pred: t1 = t2.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
firstAndRest
	| t1 t2 |
	t1 := self apply: #anything.
	t2 := self apply: #anything.
	^ self
		genericMany: [self apply: t2]
		into: (OrderedCollection
				with: (self apply: t1))! !

!OMeta2 methodsFor: 'as yet unclassified'!
fromTo
	| t1 t2 |
	t1 := self apply: #string.
	t2 := self apply: #string.
	self apply: #seq withArgs: {t1}.
	self
		many: [self
				not: [self apply: #seq withArgs: {t2}].
			self apply: #char].
	self apply: #seq withArgs: {t2}! !

!OMeta2 methodsFor: 'as yet unclassified'!
letter
	| t1 |
	t1 := self apply: #char.
	self pred: t1 isLetter.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
letterOrDigit
	| t1 |
	t1 := self apply: #char.
	self pred: t1 isAlphaNumeric.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
listOf
	| t1 t2 t3 |
	t1 := self apply: #anything.
	t2 := self apply: #anything.
	^ self ometaOr: {[t3 := self apply: t1.
		self
			genericMany: [self apply: #token withArgs: {t2}.
				self apply: t1]
			into: (OrderedCollection with: t3)]. [#()]}! !

!OMeta2 methodsFor: 'as yet unclassified'!
lower
	| t1 |
	t1 := self apply: #char.
	self pred: t1 isLowercase.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
notLast
	| t1 t2 |
	t1 := self apply: #anything.
	t2 := self apply: t1.
	self
		lookahead: [self apply: t1].
	^ t2! !

!OMeta2 methodsFor: 'as yet unclassified'!
number
	| t1 |
	t1 := self apply: #anything.
	self pred: t1 isNumber.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
range
	| t1 t2 t3 |
	t1 := self apply: #anything.
	t2 := self apply: #anything.
	t3 := self apply: #anything.
	self pred: t1 <= t3 & t3 <= t2.
	^ t3! !

!OMeta2 methodsFor: 'as yet unclassified'!
space
	| t1 |
	t1 := self apply: #char.
	self pred: t1 asciiValue <= 32.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
spaces
	^ self
		many: [self apply: #space]! !

!OMeta2 methodsFor: 'as yet unclassified'!
string
	| t1 |
	t1 := self apply: #anything.
	self pred: t1 isString.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
symbol
	| t1 |
	t1 := self apply: #anything.
	self pred: t1 isSymbol.
	^ t1! !

!OMeta2 methodsFor: 'as yet unclassified'!
token
	| t1 |
	t1 := self apply: #anything.
	self apply: #spaces.
	^ self apply: #seq withArgs: {t1}! !

!OMeta2 methodsFor: 'as yet unclassified'!
upper
	| t1 |
	t1 := self apply: #char.
	self pred: t1 isUppercase.
	^ t1! !


!OMeta2Examples methodsFor: 'as yet unclassified'!
anotherSqueakMethod
	self inform: 'good-bye world'! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
aSqueakMethod
	self inform: 'hello world'! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
digit
	| t1 |
	^ true
		ifTrue: [t1 := self
						super: OMeta2
						apply: #digit
						withArgs: {}.
			t1 digitValue]! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
fact
	| t1 t2 |
	^ self ometaOr: {[true
			ifTrue: [self apply: #exactly withArgs: {0}.
				1]]. [true
			ifTrue: [t1 := self apply: #anything.
				t2 := self apply: #fact withArgs: {t1 - 1}.
				t1 * t2]]}! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
formTest
	^ self ometaOr: {[self
			form: [true
					ifTrue: [self inform: input hash printString.
						self apply: #exactly withArgs: {#foo}.
						self apply: #exactly withArgs: {#bar}]]]. [self
			form: [true
					ifTrue: [self inform: input hash printString.
						self apply: #exactly withArgs: {#bar}]]]}! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
greeting
	^ self
		consumedBy: [true
				ifTrue: [self apply: #seq withArgs: {'howdy'}.
					self
						opt: [self apply: #seq withArgs: {'-ho'}]]]! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
identifier
	^ true
		ifTrue: [self apply: #spaces.
			self
				consumedBy: [true
						ifTrue: [self apply: #letter.
							self
								many: [self ometaOr: {[self apply: #letter]. [self apply: #digit]}]]]]! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
identifierIdx
	^ true
		ifTrue: [self apply: #spaces.
			self
				indexConsumedBy: [true
						ifTrue: [self apply: #letter.
							self
								many: [self ometaOr: {[self apply: #letter]. [self apply: #digit]}]]]]! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
identifiers
	^ self
		many: [true
				ifTrue: [self inform: self pos printString.
					self apply: #identifier]]! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
number
	| t1 t2 |
	^ self ometaOr: {[true
			ifTrue: [t1 := self apply: #number.
				t2 := self apply: #digit.
				t1 * 10 + t2]]. [self apply: #digit]}! !

!OMeta2Examples methodsFor: 'as yet unclassified'!
structure
	^ true
		ifTrue: [self apply: #exactly withArgs: {$a}.
			self apply: #exactly withArgs: {$b}.
			self apply: #exactly withArgs: {$c}.
			self apply: #exactly withArgs: {1}.
			self apply: #exactly withArgs: {2}.
			self apply: #exactly withArgs: {3}.
			self
				form: [self
						many: [self apply: #anything]]]! !



!OMeta2RuleParser methodsFor: 'as yet unclassified'!
application
	| t1 t2 t3 |
	self ometaOr: {[self apply: #token withArgs: {'^'}.
		t1 := #SuperApp]. [t1 := #App]}.
	t2 := self apply: #name.
	t3 := self apply: #args.
	^ (OrderedCollection with: t1) add: t2;
		 addAll: t3;
		 yourself! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
args
	| t1 |
	^ self ometaOr: {[self apply: #exactly withArgs: {$(}.
		t1 := self apply: #listOf withArgs: {#squeakExpression. '.'}.
		self apply: #token withArgs: {')'}.
		t1]. [self
			not: [self apply: #exactly withArgs: {$(}].
		#()]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
characterLiteral
	| t1 |
	self apply: #spaces.
	self apply: #exactly withArgs: {$$}.
	t1 := self apply: #char.
	^ {#App. #exactly. t1 storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
characters
	| t1 |
	self apply: #token withArgs: {'``'}.
	t1 := self
				many: [self
						not: [self apply: #exactly withArgs: {$'}.
							self apply: #exactly withArgs: {$'}].
					self apply: #char].
	self apply: #exactly withArgs: {$'}.
	self apply: #exactly withArgs: {$'}.
	^ {#App. #seq. (String withAll: t1) storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
expr
	| t1 |
	t1 := self apply: #listOf withArgs: {#expr4. '|'}.
	^ (OrderedCollection with: #Or) addAll: t1;
		 yourself! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
expr1
	| t1 t2 |
	^ self ometaOr: {[t1 := self ometaOr: {[self apply: #keyword withArgs: {'true'}]. [self apply: #keyword withArgs: {'false'}]. [self apply: #keyword withArgs: {'nil'}]}.
		{#App. #exactly. t1}]. [self apply: #application]. [self apply: #semanticAction]. [self apply: #semanticPredicate]. [self apply: #characters]. [self apply: #tokenSugar]. [self apply: #stringLiteral]. [self apply: #symbolLiteral]. [self apply: #numberLiteral]. [self apply: #characterLiteral]. [self apply: #token withArgs: {'{'}.
		t2 := self apply: #expr.
		self apply: #token withArgs: {'}'}.
		{#Form. t2}]. [self apply: #token withArgs: {'<'}.
		t2 := self apply: #expr.
		self apply: #token withArgs: {'>'}.
		{#ConsBy. t2}]. [self apply: #token withArgs: {'@<'}.
		t2 := self apply: #expr.
		self apply: #token withArgs: {'>'}.
		{#IdxConsBy. t2}]. [self apply: #token withArgs: {'('}.
		t2 := self apply: #expr.
		self apply: #token withArgs: {')'}.
		t2]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
expr2
	| t1 |
	^ self ometaOr: {[self apply: #token withArgs: {'~'}.
		t1 := self apply: #expr2.
		{#Not. t1}]. [self apply: #token withArgs: {'&'}.
		t1 := self apply: #expr2.
		{#Lookahead. t1}]. [self apply: #expr1]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
expr3
	| t1 t2 |
	^ self ometaOr: {[t1 := self apply: #expr2.
		t1 := self apply: #optIter withArgs: {t1}.
		self ometaOr: {[self apply: #exactly withArgs: {$:}.
			t2 := self apply: #nsName.
			temps add: t2.
			{#Set. t2. t1}]. [t1]}]. [self apply: #token withArgs: {':'}.
		t2 := self apply: #nsName.
		temps add: t2.
		{#Set. t2. {#App. #anything}}]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
expr4
	| t1 |
	t1 := self
				many: [self apply: #expr3].
	^ (OrderedCollection with: #And) addAll: t1;
		 yourself! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
initialize
	super initialize.
	temps := IdentitySet new! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
keyword
	| t1 |
	t1 := self apply: #anything.
	self apply: #token withArgs: {t1}.
	self
		not: [self apply: #letterOrDigit].
	^ t1! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
name
	self apply: #spaces.
	^ self apply: #nsName! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
nameFirst
	^ self apply: #letter! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
nameRest
	^ self ometaOr: {[self apply: #nameFirst]. [self apply: #digit]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
nsName
	| t1 |
	^ self ometaOr: {[t1 := self apply: #firstAndRest withArgs: {#nameFirst. #nameRest}.
		(String withAll: t1) asSymbol]. [self apply: #exactly withArgs: {$_}.
		^ #anything]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
numberLiteral
	| t1 t2 |
	self apply: #spaces.
	t1 := self ometaOr: {[self apply: #exactly withArgs: {$-}.
				self apply: #spaces.
				-1]. [1]}.
	t2 := self
				many1: [self apply: #digit].
	^ {#App. #exactly. (t1 * (String withAll: t2) asNumber) storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
optIter
	| t1 |
	t1 := self apply: #anything.
	^ self ometaOr: {[self apply: #token withArgs: {'*'}.
		{#Many. t1}]. [self apply: #token withArgs: {'+'}.
		{#Many1. t1}]. [self apply: #token withArgs: {'?'}.
		self
			not: [self apply: #exactly withArgs: {$[}].
		{#Opt. t1}]. [t1]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
rule
	| t1 t2 t3 t4 |
	^ self ometaOr: {[self
			not: [self
					many: [self
							super: OMeta2
							apply: #space
							withArgs: #()].
				self apply: #nsName.
				self apply: #expr4.
				self apply: #token withArgs: {'='}].
		t4 := self
					consumedBy: [self
							many: [self apply: #char]].
		{#Squeak. t4}]. [t1 := self
					lookahead: [self
							many: [self
									super: OMeta2
									apply: #space
									withArgs: #()].
						self apply: #nsName].
		t2 := self apply: #rulePart withArgs: {t1}.
		t3 := self
					many: [self apply: #token withArgs: {','}.
						self apply: #rulePart withArgs: {t1}].
		self apply: #spaces.
		self apply: #end.
		{#Rule. t1. temps asArray. (OrderedCollection with: #Or with: t2) addAll: t3;
			 yourself}]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
rulePart
	| t1 t2 t3 |
	t1 := self apply: #anything.
	self pred: (self apply: #name)
			= t1.
	t2 := self apply: #expr4.
	^ self ometaOr: {[self apply: #token withArgs: {'='}.
		t3 := self apply: #expr.
		{#And. t2. t3}]. [t2]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
semanticAction
	| t1 |
	self ometaOr: {[self apply: #token withArgs: {'->'}]. []}.
	self apply: #token withArgs: {'['}.
	t1 := self apply: #squeakExpression.
	self apply: #exactly withArgs: {$]}.
	^ {#Act. t1}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
semanticPredicate
	| t1 |
	self apply: #token withArgs: {'?['}.
	t1 := self apply: #squeakExpression.
	self apply: #exactly withArgs: {$]}.
	^ {#Pred. t1}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
space
	^ self ometaOr: {[self
			super: OMeta2
			apply: #space
			withArgs: #()]. [self apply: #fromTo withArgs: {'/*'. '*/'}]. [self apply: #fromTo withArgs: {'//'. Character lf asString}]. [self apply: #seq withArgs: {'//'}.
		self
			many: [self
					super: OMeta2
					apply: #space
					withArgs: #()]]}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
squeakExpression
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #foreign withArgs: {O2SqueakRecognizer. #squeakExpr}.
			self apply: #spaces.
			t1]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
stringLiteral
	| t1 |
	self apply: #token withArgs: {''''}.
	t1 := self
				many: [self ometaOr: {[self apply: #exactly withArgs: {$'}.
						self apply: #exactly withArgs: {$'}.
						$']. [self
							not: [self apply: #exactly withArgs: {$'}].
						self apply: #char]}].
	self apply: #exactly withArgs: {$'}.
	^ {#App. #exactly. (String withAll: t1) storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
symbolLiteral
	| t1 |
	self apply: #token withArgs: {'#'}.
	t1 := self apply: #nsName.
	^ {#App. #exactly. t1 storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified'!
tokenSugar
	| t1 |
	self apply: #token withArgs: {'"'}.
	t1 := self
				many: [self
						not: [self apply: #exactly withArgs: {$"}].
					self apply: #char].
	self apply: #exactly withArgs: {$"}.
	^ {#App. #token. (String withAll: t1) storeString}! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

OMeta2RuleParser class
	instanceVariableNames: ''!

!OMeta2RuleParser class methodsFor: 'as yet unclassified'!
isOMeta2Rule: t1 
	^ [(self matchAll: t1 with: #rule) first ~= #Squeak]
		on: OM2Fail
		do: [false]! !





!OMeta2Optimizer methodsFor: 'as yet unclassified'!
optimizeRule
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #anything.
			self
				many: [t1 := self apply: #foreign withArgs: {OMeta2AndOrOpt. #optimize. t1}].
			t1]! !


!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
arrayConstr
	^ self ometaOr: {[true
			ifTrue: [self apply: #token withArgs: {'{'}.
				self apply: #expr.
				self
					many: [true
							ifTrue: [self apply: #token withArgs: {'.'}.
								self apply: #expr]].
				self ometaOr: {[self apply: #token withArgs: {'.'}]. [self apply: #empty]}.
				self apply: #token withArgs: {'}'}]]. [true
			ifTrue: [self apply: #token withArgs: {'{'}.
				self apply: #token withArgs: {'}'}]]}! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
arrayLit
	^ true
		ifTrue: [self apply: #token withArgs: {'#'}.
			self apply: #token withArgs: {'('}.
			self
				many: [self ometaOr: {[self apply: #literal]. [self apply: #arrayLit]. [true
							ifTrue: [self apply: #spaces.
								self apply: #tsArraySymbol]]}].
			self apply: #token withArgs: {')'}]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
binary
	^ true
		ifTrue: [self apply: #spaces.
			self apply: #tsBinary]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
binaryExpr
	^ self ometaOr: {[true
			ifTrue: [self apply: #binaryExpr.
				self apply: #binaryMsg]]. [self apply: #unaryExpr]}! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
binaryMsg
	^ true
		ifTrue: [self apply: #binary.
			self apply: #unaryExpr]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
block
	^ true
		ifTrue: [self apply: #token withArgs: {'['}.
			self ometaOr: {[true
					ifTrue: [self
							many1: [true
									ifTrue: [self apply: #token withArgs: {':'}.
										self apply: #identifier]].
						self apply: #token withArgs: {'|'}]]. [self apply: #empty]}.
			self ometaOr: {[true
					ifTrue: [self apply: #token withArgs: {'|'}.
						self
							many: [self apply: #identifier].
						self apply: #token withArgs: {'|'}]]. [self apply: #empty]}.
			self ometaOr: {[true
					ifTrue: [self apply: #expr.
						self
							many: [true
									ifTrue: [self apply: #token withArgs: {'.'}.
										self apply: #expr]].
						self ometaOr: {[true
								ifTrue: [self apply: #token withArgs: {'.'}.
									self apply: #token withArgs: {'^'}.
									self apply: #expr]]. [self apply: #empty]}]]. [true
					ifTrue: [self apply: #token withArgs: {'^'}.
						self apply: #expr]]. [self apply: #empty]}.
			self ometaOr: {[self apply: #token withArgs: {'.'}]. [self apply: #empty]}.
			self apply: #token withArgs: {']'}]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
cascade
	^ self ometaOr: {[self apply: #identifier]. [self apply: #binaryMsg]. [self apply: #keywordMsg]}! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
expr
	^ self ometaOr: {[true
			ifTrue: [self apply: #identifier.
				self ometaOr: {[self apply: #token withArgs: {':='}]. [self apply: #token withArgs: {'_'}]}.
				self apply: #expr]]. [self apply: #msgExpr]}! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
identifier
	^ true
		ifTrue: [self apply: #spaces.
			self apply: #tsIdentifier.
			self
				not: [self apply: #exactly withArgs: {$:}]]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
keyword
	^ true
		ifTrue: [self apply: #spaces.
			self apply: #tsKeyword]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
keywordExpr
	^ true
		ifTrue: [self apply: #binaryExpr.
			self apply: #keywordMsg]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
keywordMsg
	^ self ometaOr: {[true
			ifTrue: [self apply: #keywordMsg.
				self apply: #keywordMsgPart]]. [self apply: #keywordMsgPart]}! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
keywordMsgPart
	^ true
		ifTrue: [self apply: #keyword.
			self apply: #binaryExpr]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
literal
	^ true
		ifTrue: [self apply: #spaces.
			self ometaOr: {[self apply: #tsNumber]. [self apply: #tsCharacter]. [self apply: #tsString]. [self apply: #tsSymbol]}]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
msgExpr
	^ true
		ifTrue: [self ometaOr: {[self apply: #keywordExpr]. [self apply: #binaryExpr]}.
			self
				many: [true
						ifTrue: [self apply: #token withArgs: {';'}.
							self apply: #cascade]]]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
squeakExpr
	^ self
		consumedBy: [self apply: #expr]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
symbol
	^ true
		ifTrue: [self apply: #token withArgs: {'#'}.
			self apply: #spaces.
			self ometaOr: {[self apply: #tsString]. [true
					ifTrue: [self apply: #tsKeyword.
						self ometaOr: {[self apply: #tsIdentifier]. [self apply: #empty]}]]}]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tcBinaryChar
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #char.
			self pred: (TypeTable at: t1 asciiValue)
					== #xBinary]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsArraySymbol
	^ self ometaOr: {[true
			ifTrue: [self
					many1: [self apply: #tsKeyword].
				self ometaOr: {[self apply: #tsIdentifier]. [self apply: #empty]}]]. [self apply: #tsIdentifier]}! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsBinary
	^ true
		ifTrue: [self ometaOr: {[self apply: #exactly withArgs: {$|}]. [self apply: #tcBinaryChar]}.
			self
				many: [self apply: #tcBinaryChar]]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsCharacter
	^ true
		ifTrue: [self apply: #exactly withArgs: {$$}.
			self apply: #char]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsIdentifier
	^ true
		ifTrue: [self apply: #letter.
			self
				many: [self ometaOr: {[self apply: #letter]. [self apply: #digit]}]]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsKeyword
	^ true
		ifTrue: [self apply: #tsIdentifier.
			self apply: #exactly withArgs: {$:}]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsNatural
	^ self
		many1: [self apply: #digit]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsNumber
	^ true
		ifTrue: [self ometaOr: {[self apply: #exactly withArgs: {$+}]. [self apply: #exactly withArgs: {$-}]. [self apply: #empty]}.
			self apply: #tsNatural]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsString
	^ true
		ifTrue: [self apply: #exactly withArgs: {$'}.
			self
				many: [self ometaOr: {[true
							ifTrue: [self apply: #exactly withArgs: {$'}.
								self apply: #exactly withArgs: {$'}]]. [true
							ifTrue: [self
									not: [self apply: #exactly withArgs: {$'}].
								self apply: #char]]}].
			self apply: #exactly withArgs: {$'}]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
tsSymbol
	^ true
		ifTrue: [self apply: #exactly withArgs: {$#}.
			self apply: #spaces.
			self ometaOr: {[self apply: #tsString]. [self apply: #tsArraySymbol]}]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
unaryExpr
	^ true
		ifTrue: [self apply: #unit.
			self
				many: [self apply: #identifier]]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified'!
unit
	^ self ometaOr: {[self apply: #literal]. [self apply: #identifier]. [self apply: #arrayLit]. [self apply: #arrayConstr]. [self apply: #block]. [true
			ifTrue: [self apply: #token withArgs: {'('}.
				self apply: #expr.
				self apply: #token withArgs: {')'}]]}! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

O2SqueakRecognizer class
	instanceVariableNames: ''!

!O2SqueakRecognizer class methodsFor: 'as yet unclassified'!
initialize
	TypeTable := #(#xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xDelimiter #xDelimiter #xBinary #xDelimiter #xDelimiter #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #doIt #xBinary #xDelimiter #xBinary #xDoubleQuote #xLitQuote #xDollar #xBinary #xBinary #xSingleQuote #leftParenthesis #rightParenthesis #xBinary #xBinary #xBinary #xBinary #period #xBinary #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xColon #semicolon #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #leftBracket #xBinary #rightBracket #upArrow #leftArrow #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #leftBrace #verticalBar #rightBrace #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary )! !


O2SqueakRecognizer initialize!


!OMeta2NullOpt methodsFor: 'as yet unclassified'!
and
	| t1 |
	^ true
		ifTrue: [t1 := self
						many: [self apply: #trans].
			t1 addFirst: #And;
				 yourself]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
consby
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			{#ConsBy. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
form
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			{#Form. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
helped
	^ self pred: didSomething! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
idxconsby
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			{#IdxConsBy. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
initialize
	super initialize.
	didSomething := false! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
lookahead
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			{#Lookahead. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
many
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			{#Many. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
many1
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			{#Many1. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
not
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			{#Not. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
opt
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			{#Opt. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
optimize
	| t1 |
	^ true
		ifTrue: [t1 := self apply: #trans.
			self apply: #helped.
			t1]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
or
	| t1 |
	^ true
		ifTrue: [t1 := self
						many: [self apply: #trans].
			t1 addFirst: #Or;
				 yourself]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
rule
	| t1 t2 t3 |
	^ true
		ifTrue: [t2 := self apply: #anything.
			t3 := self apply: #anything.
			t1 := self apply: #trans.
			{#Rule. t2. t3. t1}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
set
	| t1 t2 |
	^ true
		ifTrue: [t1 := self apply: #anything.
			t2 := self apply: #trans.
			{#Set. t1. t2}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
setHelped
	^ didSomething := true! !

!OMeta2NullOpt methodsFor: 'as yet unclassified'!
trans
	| t1 t2 |
	^ self ometaOr: {[true
			ifTrue: [self
					form: [true
							ifTrue: [t1 := self apply: #anything.
								t1 := t1 asLowercase asSymbol.
								self
									pred: (self class canUnderstand: t1).
								t2 := self apply: #apply withArgs: {t1}]].
				t2]]. [self apply: #anything]}! !


!OMeta2AndOrOpt methodsFor: 'as yet unclassified'!
and
	| t1 t2 |
	^ self ometaOr: {[true
			ifTrue: [t2 := self apply: #trans.
				self apply: #end.
				self apply: #setHelped.
				t2]]. [true
			ifTrue: [t1 := self apply: #transInside withArgs: {#And}.
				t1 addFirst: #And;
					 yourself]]}! !

!OMeta2AndOrOpt methodsFor: 'as yet unclassified'!
or
	| t1 t2 |
	^ self ometaOr: {[true
			ifTrue: [t2 := self apply: #trans.
				self apply: #end.
				self apply: #setHelped.
				t2]]. [true
			ifTrue: [t1 := self apply: #transInside withArgs: {#Or}.
				t1 addFirst: #Or;
					 yourself]]}! !

!OMeta2AndOrOpt methodsFor: 'as yet unclassified'!
transInside
	| t1 t2 t3 t4 |
	^ true
		ifTrue: [t2 := self apply: #anything.
			self ometaOr: {[true
					ifTrue: [self
							form: [true
									ifTrue: [self apply: #exactly withArgs: {t2}.
										t3 := self apply: #transInside withArgs: {t2}]].
						t1 := self apply: #transInside withArgs: {t2}.
						self apply: #setHelped.
						t3 , t1]]. [true
					ifTrue: [t4 := self apply: #trans.
						t3 := self apply: #transInside withArgs: {t2}.
						t3 addFirst: t4;
							 yourself]]. [true
					ifTrue: [self apply: #empty.
						OrderedCollection new]]}]! !


!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
act
	| t1 |
	t1 := self apply: #string.
	^ {'('. t1. ')'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
and
	| t1 |
	t1 := self
				many: [self apply: #trans].
	^ {'(true ifTrue: ['. self delim: t1 with: '. '. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
app
	| t1 t2 |
	t1 := self apply: #symbol.
	^ self ometaOr: {[t2 := self
					many1: [self apply: #anything].
		t2 := self delim: t2 with: '. '.
		{'(self apply: '. t1 storeString. ' withArgs: {'. t2. '})'}]. [{'(self apply: '. t1 storeString. ')'}]}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
consby
	| t1 |
	t1 := self apply: #trans.
	^ {'(self consumedBy: ['. t1. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
delim: t1 with: t2 
	| t3 t4 |
	t3 := true.
	t4 := OrderedCollection new.
	t1
		do: [:t5 | 
			t3
				ifTrue: [t3 := false]
				ifFalse: [t4 add: t2].
			t4 add: t5].
	^ t4! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
form
	| t1 |
	t1 := self apply: #trans.
	^ {'(self form: ['. t1. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
idxconsby
	| t1 |
	t1 := self apply: #trans.
	^ {'(self indexConsumedBy: ['. t1. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
lookahead
	| t1 |
	t1 := self apply: #trans.
	^ {'(self lookahead: ['. t1. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
many
	| t1 |
	t1 := self apply: #trans.
	^ {'(self many: ['. t1. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
many1
	| t1 |
	t1 := self apply: #trans.
	^ {'(self many1: ['. t1. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
not
	| t1 |
	t1 := self apply: #trans.
	^ {'(self not: ['. t1. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
opt
	| t1 |
	t1 := self apply: #trans.
	^ {'(self opt: ['. t1. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
or
	| t1 t2 |
	t1 := self
				many: [t2 := self apply: #trans.
					{'['. t2. ']'}].
	^ {'(self ometaOr: {'. self delim: t1 with: '. '. '})'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
pred
	| t1 |
	t1 := self apply: #string.
	^ {'(self pred: ('. t1. '))'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
rule
	| t1 t2 t3 |
	t1 := self apply: #symbol.
	t2 := self apply: #anything.
	t2 := t2
				select: [:t4 | (grammarClass instVarNames includes: t4) not].
	t2 := t2
				collect: [:t4 | t4 asString].
	t3 := self apply: #trans.
	^ {t1. ' |'. self delim: t2 with: ' '. ' | ^ '. t3}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
set
	| t1 t2 |
	t1 := self apply: #symbol.
	t2 := self apply: #trans.
	^ {'('. t1 asString. ' := '. t2. ')'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
squeak
	^ self apply: #string! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
superapp
	| t1 t2 |
	t1 := self apply: #symbol.
	t2 := self
				many: [self apply: #anything].
	t2 := self delim: t2 with: '. '.
	^ {'(self super: '. grammarClass superclass name. ' apply: '. t1 storeString. ' withArgs: {'. t2. '})'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
trans
	| t1 t2 |
	self
		form: [t1 := self apply: #symbol.
			t2 := self apply: t1 asLowercase asSymbol].
	^ t2! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified'!
translate
	grammarClass := self apply: #anything.
	^ self apply: #trans! !


!OMeta2Flattener methodsFor: 'as yet unclassified'!
flatten
	| t1 |
	t1 := self apply: #anything.
	self apply: #iFlatten withArgs: {t1}.
	^ ws contents! !

!OMeta2Flattener methodsFor: 'as yet unclassified'!
iFlatten
	| t1 |
	^ self ometaOr: {[t1 := self apply: #string.
		ws nextPutAll: t1]. [self
			form: [self
					many: [self apply: #iFlatten]]]}! !

!OMeta2Flattener methodsFor: 'as yet unclassified'!
initialize
	super initialize.
	ws := (String new: 64) writeStream! !







!OMeta2Base class methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:52'!
compilerClass

        ^ OMeta2Compiler! !



"-- END OF STAGE 1 BOOTSTRAP --"!