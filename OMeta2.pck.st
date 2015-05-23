'From Cuis 4.2 of 25 July 2013 [latest update: #2332] on 22 May 2015 at 7:13:26.485179 pm'!
'Description '!
!provides: 'OMeta2' 1 1!
!classDefinition: #OMeta2 category: #OMeta2!
OMeta2Base subclass: #OMeta2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'OMeta2 class' category: #OMeta2!
OMeta2 class
	instanceVariableNames: ''!

!classDefinition: #O2SqueakRecognizer category: #OMeta2!
OMeta2 subclass: #O2SqueakRecognizer
	instanceVariableNames: ''
	classVariableNames: 'TypeTable'
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'O2SqueakRecognizer class' category: #OMeta2!
O2SqueakRecognizer class
	instanceVariableNames: ''!

!classDefinition: #OMeta2Examples category: #OMeta2!
OMeta2 subclass: #OMeta2Examples
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'OMeta2Examples class' category: #OMeta2!
OMeta2Examples class
	instanceVariableNames: ''!

!classDefinition: #OMeta2Flattener category: #OMeta2!
OMeta2 subclass: #OMeta2Flattener
	instanceVariableNames: 'ws'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'OMeta2Flattener class' category: #OMeta2!
OMeta2Flattener class
	instanceVariableNames: ''!

!classDefinition: #OMeta2NullOpt category: #OMeta2!
OMeta2 subclass: #OMeta2NullOpt
	instanceVariableNames: 'didSomething'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'OMeta2NullOpt class' category: #OMeta2!
OMeta2NullOpt class
	instanceVariableNames: ''!

!classDefinition: #OMeta2AndOrOpt category: #OMeta2!
OMeta2NullOpt subclass: #OMeta2AndOrOpt
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'OMeta2AndOrOpt class' category: #OMeta2!
OMeta2AndOrOpt class
	instanceVariableNames: ''!

!classDefinition: #OMeta2Optimizer category: #OMeta2!
OMeta2 subclass: #OMeta2Optimizer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'OMeta2Optimizer class' category: #OMeta2!
OMeta2Optimizer class
	instanceVariableNames: ''!

!classDefinition: #OMeta2RuleParser category: #OMeta2!
OMeta2 subclass: #OMeta2RuleParser
	instanceVariableNames: 'temps'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'OMeta2RuleParser class' category: #OMeta2!
OMeta2RuleParser class
	instanceVariableNames: ''!

!classDefinition: #OMeta2RuleTranslator category: #OMeta2!
OMeta2 subclass: #OMeta2RuleTranslator
	instanceVariableNames: 'grammarClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!classDefinition: 'OMeta2RuleTranslator class' category: #OMeta2!
OMeta2RuleTranslator class
	instanceVariableNames: ''!


!OMeta2 commentStamp: '<historical>' prior: 0!
TODOS:

* implement OMeta -> Squeak translator
* implement Squeak parser
* implement OMeta/Squeak "compiler", make it OMeta2's compilerClass

* rewrite #empty and #firstAndRest in OMeta syntax
* consider implementing position-related functionality (see http://www.tinlizzie.org/ometa-js/ometa-base.js)
* consider the optimization suggestions in the comments of OMeta2Lib's methods!

!OMeta2Examples commentStamp: '<historical>' prior: 0!
Here's how to run these examples:

	OMeta2Examples match: 5 with: #fact.
	OMeta2Examples matchAll: '1234' with: #number.
	OMeta2Examples matchAll: 'abc123' with: #identifier.
	OMeta2Examples matchAll: #($a $b $c 1 2 3 #(4 5)) with: #structure.
	OMeta2Examples matchAll: 'howdy' with: #greeting.!

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:23'!
char =

	anything:x ?[x isKindOf: Character] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:24'!
digit =

	char:x ?[x isDigit] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 16:14'!
empty

	^ true! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:25'!
end =

	~anything! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:25'!
exactly :wanted =

	anything:got ?[wanted = got] -> [wanted]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 13:52'!
firstAndRest

	| first rest |
	first := self apply: #anything.
	rest := self apply: #anything.
	^ self genericMany: [self apply: rest] into: (OrderedCollection with: (self apply: first))! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:27'!
fromTo :x :y =

	seq(x) (~seq(y) char)* seq(y)! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:28'!
letter =

	char:x ?[x isLetter] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:28'!
letterOrDigit =

	char:x ?[x isAlphaNumeric] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:29'!
listOf :rule :delim =

		apply(rule):x (token(delim) apply(rule))*:xs -> [xs addFirst: x; yourself]
	|	empty -> [#()]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:30'!
lower =

	char:x ?[x isLowercase] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:31'!
notLast :rule =

	apply(rule):ans &apply(rule) -> [ans]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:31'!
number =

	anything:x ?[x isNumber] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:32'!
range :a :b =

	anything:x ?[a <= x & (x <= b)] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:32'!
space =

	char:x ?[x asciiValue <= 32] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:33'!
spaces =

	space*! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:34'!
string =

	anything:x ?[x isString] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:34'!
symbol =

	anything:x ?[x isSymbol] -> [x]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:34'!
token :t =

	spaces seq(t)! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:35'!
upper =

	char:x ?[x isUppercase] -> [x]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:43'!
arrayConstr =

	"{" expr ("." expr)* ("." | empty) "}"
|	"{" "}"! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:57'!
arrayLit =

	"#" "(" (literal | arrayLit | spaces tsArraySymbol)* ")"! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:57'!
binary =

	spaces tsBinary! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:58'!
binaryExpr =

	binaryExpr binaryMsg
|	unaryExpr! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:58'!
binaryMsg =

	binary unaryExpr! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:08'!
block =

	"["
		(	(":" identifier)+ "|"
		|	empty
		)

		(	"|" identifier* "|"
		|	empty
		)

		(	expr ("." expr)* ("." "^" expr | empty)
		|	"^" expr
		|	empty
		)

		(	"."
		|	empty
		)

	"]"! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:58'!
cascade =

	identifier
|	binaryMsg
|	keywordMsg! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:59'!
expr =

	identifier (":=" | "_") expr
|	msgExpr! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:18'!
identifier =

	spaces tsIdentifier ~$:! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:19'!
keyword =

	spaces tsKeyword! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:19'!
keywordExpr =

	binaryExpr keywordMsg! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:19'!
keywordMsg =

	keywordMsg keywordMsgPart
|	keywordMsgPart! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:20'!
keywordMsgPart =

	keyword binaryExpr! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:20'!
literal =

	spaces	(	tsNumber
			|	tsCharacter
			|	tsString
			|	tsSymbol
			)
! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:21'!
msgExpr =

	(keywordExpr | binaryExpr) (";" cascade)*! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:30'!
squeakExpr =

	<expr>! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:23'!
symbol =

	"#" spaces
		(	tsString
		|	tsKeyword (tsIdentifier | empty)
		)! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:23'!
tcBinaryChar =

	char:x ?[(TypeTable at: x asciiValue) == #xBinary]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:25'!
tsArraySymbol =

	tsKeyword+ (tsIdentifier | empty)
|	tsIdentifier! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:25'!
tsBinary =

	($| | tcBinaryChar) tcBinaryChar*! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:25'!
tsCharacter =

	$$ char! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:26'!
tsIdentifier =

	letter (letter | digit)*! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:31'!
tsKeyword =

	tsIdentifier $:! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:26'!
tsNatural =

	digit+! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:27'!
tsNumber =

	($+ | $- | empty) tsNatural! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:28'!
tsString =

	$' ($' $' | ~$' char)* $'! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:28'!
tsSymbol =

	$# spaces (tsString | tsArraySymbol)! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:28'!
unaryExpr =

	unit identifier*! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:29'!
unit =

	literal
|	identifier
|	arrayLit
|	arrayConstr
|	block
|	"(" expr ")"
! !

!O2SqueakRecognizer class methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 02:04'!
initialize

	TypeTable := #(#xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xDelimiter #xDelimiter #xBinary #xDelimiter #xDelimiter #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #doIt #xBinary #xDelimiter #xBinary #xDoubleQuote #xLitQuote #xDollar #xBinary #xBinary #xSingleQuote #leftParenthesis #rightParenthesis #xBinary #xBinary #xBinary #xBinary #period #xBinary #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xColon #semicolon #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #leftBracket #xBinary #rightBracket #upArrow #leftArrow #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #leftBrace #verticalBar #rightBrace #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary)! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 02:35'!
aSqueakMethod

	self inform: 'hello world'! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 02:36'!
anotherSqueakMethod

	self inform: 'good-bye world'! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:58'!
digit =

	// this example shows the new syntax for super rule applications
	// think of the ^ as an arrow that's pointing up to the parent grammar

	^digit:d -> [d digitValue]! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:58'!
fact 0 = [1],
fact _:n = fact(n - 1):m -> [n * m]

// this example shows that rules can have multiple definitions (they are tried in order)
! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 12:04'!
formTest =

	{[self inform: input hash printString] #foo #bar}
|	{[self inform: input hash printString] #bar}! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:36'!
greeting = <``howdy'' ``-ho''?>! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:45'!
identifier =

	// this is an example of the new <> operator

	spaces <letter (letter | digit)*>! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:48'!
identifierIdx =

	// this is an example of the new @<> operator

	spaces @<letter (letter | digit)*>! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:54'!
identifiers =

	([self inform: self pos printString] identifier)*! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:46'!
number =

	// this is an example of left recursion support

	number:n digit:d -> [n * 10 + d]
|	digit! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:46'!
structure =

	// this example shows how the {}s are used for matching sequenceable collections

	$a $b $c 1 2 3 {anything*}! !

!OMeta2Flattener methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:40'!
flatten :tree =

	iFlatten(tree) -> [ws contents]! !

!OMeta2Flattener methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:41'!
iFlatten =

	string:s 	[ws nextPutAll: s]
|	{iFlatten*}! !

!OMeta2Flattener methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:05'!
initialize

	super initialize.
	ws := (String new: 64) writeStream! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
and = trans*:xs -> [xs addFirst: #And; yourself]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
consby = trans:x -> [{#ConsBy. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
form = trans:x -> [{#Form. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:01'!
helped = ?[didSomething]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
idxconsby = trans:x -> [{#IdxConsBy. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:30'!
initialize

	super initialize.
	didSomething := false! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
lookahead = trans:x -> [{#Lookahead. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
many = trans:x -> [{#Many. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
many1 = trans:x -> [{#Many1. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
not = trans:x -> [{#Not. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
opt = trans:x -> [{#Opt. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:02'!
optimize = trans:x helped -> [x]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
or = trans*:xs -> [xs addFirst: #Or; yourself]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
rule = _:name _:temps trans:body -> [{#Rule. name. temps. body}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
set = _:name trans:val -> [{#Set. name. val}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:00'!
setHelped = [didSomething := true]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:42'!
trans =

	{:t [t asLowercase asSymbol]:t ?[self class canUnderstand: t] apply(t):ans} -> [ans]
|	_! !

!OMeta2AndOrOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:46'!
and =

	trans:x end setHelped -> [x]
|	transInside(#And):xs -> [xs addFirst: #And; yourself]! !

!OMeta2AndOrOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:46'!
or =

	trans:x end setHelped -> [x]
|	transInside(#Or):xs -> [xs addFirst: #Or; yourself]! !

!OMeta2AndOrOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:36'!
transInside :t =

	{exactly(t) transInside(t):xs} transInside(t):ys setHelped -> [xs, ys]
|	trans:x transInside(t):xs -> [xs addFirst: x; yourself]
|	empty -> [OrderedCollection new]! !

!OMeta2Optimizer methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:53'!
optimizeRule = _:ans (foreign(OMeta2AndOrOpt. #optimize. ans):ans)* -> [ans]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:59'!
application =

	"^" name:rule args:as				-> [{#SuperApp. rule}, as]
|	name:grm $. nsName:rule args:as	-> [{#App. #foreign. grm. ('#', rule) asSymbol}, as]
|	name:rule args:as						-> [{#App. rule}, as]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:00'!
args =

	$( listOf(#squeakExpression. '.'):ans ")" -> [ans]
|	~$( -> [#()]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:00'!
characterLiteral =

	spaces $$ char:c -> [{#App. #exactly. c storeString}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:01'!
characters =

	"``" (~($' $') char)*:xs $' $' -> [{#App. #seq. (String withAll: xs) storeString}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:01'!
expr =

	listOf(#expr4. '|'):xs -> [(OrderedCollection with: #Or) addAll: xs; yourself]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:01'!
expr1 =

	(keyword('true') | keyword('false') | keyword('nil')):lit -> [{#App. #exactly. lit}]
|	application
|	semanticAction
|	semanticPredicate
|	characters
|	tokenSugar
|	stringLiteral
|	symbolLiteral
|	numberLiteral
|	characterLiteral
|	"{"  expr:e "}" -> [{#Form. e}]
|	"<"  expr:e ">" -> [{#ConsBy. e}]
|	"@<" expr:e ">" -> [{#IdxConsBy. e}]
|	"("  expr:e ")" -> [e]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:01'!
expr2 =

	"~" expr2:x -> [{#Not. x}]
|	"&" expr2:x -> [{#Lookahead. x}]
|	expr1! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:02'!
expr3 =

	expr2:x optIter(x):x
		(	$: nsName:n [temps add: n]	-> [{#Set. n. x}]
		|	empty								-> [x]
		)
|	":" nsName:n [temps add: n]			-> [{#Set. n. {#App. #anything}}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:02'!
expr4 =

	expr3*:xs -> [(OrderedCollection with: #And) addAll: xs; yourself]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:04'!
initialize

	super initialize.
	temps := IdentitySet new! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:02'!
keyword :xs =

	token(xs) ~letterOrDigit -> [xs]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:02'!
name =

	spaces nsName! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:03'!
nameFirst =

	letter! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:03'!
nameRest =

	nameFirst | digit! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:03'!
nsName =

	firstAndRest(#nameFirst. #nameRest):xs -> [(String withAll: xs) asSymbol]
|	$_ -> [#anything]
! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:04'!
numberLiteral =

	spaces ($- spaces -> [-1] | empty -> [1]):sign digit+:ds
		-> [{#App. #exactly. (sign * (String withAll: ds) asNumber) storeString}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:04'!
optIter :x =

	"*"		-> [{#Many.  x}]
|	"+"		-> [{#Many1. x}]
|	"?" ~$[	-> [{#Opt. x}]
|	empty		-> [x]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:04'!
rule =

	~(^space* nsName expr4 "=") <char*>:squeakCode -> [{#Squeak. squeakCode}]
|	&(^space* nsName):n rulePart(n):x ("," rulePart(n))*:xs spaces end
		-> [{#Rule. n. temps asSortedCollection. (OrderedCollection with: #Or with: x) addAll: xs; yourself}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:05'!
rulePart :ruleName =

	name:n ?[n = ruleName] expr4:b1
		(	"=" expr:b2	-> [{#And. b1. b2}]
		|	empty			-> [b1]
		)! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:05'!
semanticAction =

	"->"? "[" squeakExpression:e $] -> [{#Act. e}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:05'!
semanticPredicate =

	"?[" squeakExpression:e $] -> [{#Pred. e}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 18:59'!
space =

	^space | fromTo('/*'. '*/') | fromTo('//'. Character lf asString)! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:08'!
squeakExpression =

	foreign(O2SqueakRecognizer. #squeakExpr):ans spaces -> [ans]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:06'!
stringLiteral =

	"'" ($' $' -> [$'] | ~$' char)*:xs $' -> [{#App. #exactly. (String withAll: xs) storeString}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:06'!
symbolLiteral =

	// TODO: make this accept, e.g., #foo:bar:
	"#" nsName:s -> [{#App. #exactly. s storeString}]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:07'!
tokenSugar =

	token('"') (~$" char)*:xs $" -> [{#App. #token. (String withAll: xs) storeString}]! !

!OMeta2RuleParser class methodsFor: 'as yet unclassified' stamp: 'aw 2/23/2009 10:07'!
isOMeta2Rule: aString

	^ [(self matchAll: aString with: #rule) first ~= #Squeak] on: OM2Fail do: [false]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:07'!
act = string:x -> [{'('. x. ')'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:08'!
and = trans*:xs -> [{'(true ifTrue: ['. self delim: xs with: '. '. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:08'!
app =

	symbol:rule
	(	anything+:args [self delim: args with: '. ']:args -> [{'(self apply: '. rule storeString. ' withArgs: {'. args. '})'}]
	|	[{'(self apply: '. rule storeString. ')'}]
	)! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:08'!
consby =

	trans:x -> [{'(self consumedBy: ['. x. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 14:43'!
delim: aSequenceableCollection  with: anObject

	| first ans |
	
	first := true.
	ans := OrderedCollection new.
	aSequenceableCollection do: [:x |
		first ifTrue: [first := false] ifFalse: [ans add: anObject].
		ans add: x
	].
	^ ans! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:09'!
form =

	trans:x -> [{'(self form: ['. x. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:09'!
idxconsby =

	trans:x -> [{'(self indexConsumedBy: ['. x. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:09'!
lookahead =

	trans:x -> [{'(self lookahead: ['. x. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:10'!
many =

	trans:x -> [{'(self many: ['. x. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:10'!
many1 =

	trans:x -> [{'(self many1: ['. x. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:10'!
not =

	trans:x -> [{'(self not: ['. x. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:10'!
opt =

	trans:x -> [{'(self opt: ['. x. '])'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:11'!
or =

	(trans:x -> [{'['. x. ']'}])*:xs -> [{'(self ometaOr: {'. self delim: xs with: '. '. '})'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:11'!
pred =

	string:x -> [{'(self pred: ('. x. '))'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:11'!
rule =

	symbol:name
	anything:temps [temps select: [:t | (grammarClass instVarNames includes: t) not]]:temps
	trans:body
	-> [{name. ' |'. self delim: temps asSortedCollection with: ' '. ' | ^ '. body}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:12'!
set =

	symbol:n trans:v -> [{'('. n asString. ' := '. v. ')'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:12'!
squeak = string! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:12'!
superapp =

	symbol:rule anything*:args
	[self delim: args with: '. ']:args
	-> [{'(self super: '. grammarClass superclass name. ' apply: '. rule storeString. ' withArgs: {'. args. '})'}]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:13'!
trans = {symbol:type apply(type asLowercase asSymbol):ans} -> [ans]! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 19:13'!
translate :grammarClass = trans! !
O2SqueakRecognizer initialize!
