'From Cuis 4.2 of 25 July 2013 [latest update: #2332] on 23 May 2015 at 10:08:45.189392 pm'!
'Description Example parsers for OMeta2'!
!provides: 'OMeta2Examples' 1 1!
!requires: 'OMeta2' 1 0 nil!
!classDefinition: #OMeta2Examples category: #OMeta2Examples!
OMeta2 subclass: #OMeta2Examples
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Examples'!
!classDefinition: 'OMeta2Examples class' category: #OMeta2Examples!
OMeta2Examples class
	instanceVariableNames: ''!

!classDefinition: #OMeta2LambdaCalculusParserExample category: #OMeta2Examples!
OMeta2 subclass: #OMeta2LambdaCalculusParserExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Examples'!
!classDefinition: 'OMeta2LambdaCalculusParserExample class' category: #OMeta2Examples!
OMeta2LambdaCalculusParserExample class
	instanceVariableNames: ''!

!classDefinition: #OMeta2TreeWalkerExample category: #OMeta2Examples!
OMeta2 subclass: #OMeta2TreeWalkerExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Examples'!
!classDefinition: 'OMeta2TreeWalkerExample class' category: #OMeta2Examples!
OMeta2TreeWalkerExample class
	instanceVariableNames: ''!

!classDefinition: #OMeta2TreeExample category: #OMeta2Examples!
Object subclass: #OMeta2TreeExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Examples'!
!classDefinition: 'OMeta2TreeExample class' category: #OMeta2Examples!
OMeta2TreeExample class
	instanceVariableNames: ''!

!classDefinition: #OMeta2TreeEmptyExample category: #OMeta2Examples!
OMeta2TreeExample subclass: #OMeta2TreeEmptyExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Examples'!
!classDefinition: 'OMeta2TreeEmptyExample class' category: #OMeta2Examples!
OMeta2TreeEmptyExample class
	instanceVariableNames: ''!

!classDefinition: #OMeta2TreeLeafExample category: #OMeta2Examples!
OMeta2TreeExample subclass: #OMeta2TreeLeafExample
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Examples'!
!classDefinition: 'OMeta2TreeLeafExample class' category: #OMeta2Examples!
OMeta2TreeLeafExample class
	instanceVariableNames: ''!

!classDefinition: #OMeta2TreeNodeExample category: #OMeta2Examples!
OMeta2TreeExample subclass: #OMeta2TreeNodeExample
	instanceVariableNames: 'leftTree rightTree'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Examples'!
!classDefinition: 'OMeta2TreeNodeExample class' category: #OMeta2Examples!
OMeta2TreeNodeExample class
	instanceVariableNames: ''!


!OMeta2Examples commentStamp: '<historical>' prior: 0!
Here's how to run these examples:

	OMeta2Examples match: 5 with: #fact.
	OMeta2Examples matchAll: '1234' with: #number.
	OMeta2Examples matchAll: 'abc123' with: #identifier.
	OMeta2Examples matchAll: #($a $b $c 1 2 3 #(4 5)) with: #structure.
	OMeta2Examples matchAll: 'howdy' with: #greeting.!

!OMeta2LambdaCalculusParserExample commentStamp: '<historical>' prior: 0!
A port of the Lambda Calculus parser from: http://www.tinlizzie.org/ometa-js/#Lambda_Calculus

There are several other OMeta examples at this site.  Unfortunately for Smalltalk users, they're all in OMeta/JS which has a slightly different syntax.  Hopefully having this example to compare to will help you to translate the syntax.

To run this example:

OMeta2LambdaCalculus demo!

!OMeta2TreeWalkerExample commentStamp: '<historical>' prior: 0!
See OMeta2TreeExample!

!OMeta2TreeExample commentStamp: '<historical>' prior: 0!
Based on Frank Shearar's post http://www.lshift.net/blog/2011/05/15/algebraic-data-types-and-ometa2/

This example demonstrates using OMeta to parse a tree structure.  The parser is in OMeta2TreeWalkerExample.

To run this example:

OMeta2TreeExample demo
!

!OMeta2TreeEmptyExample commentStamp: '<historical>' prior: 0!
See OMeta2TreeExample!

!OMeta2TreeLeafExample commentStamp: '<historical>' prior: 0!
See OMeta2TreeExample!

!OMeta2TreeNodeExample commentStamp: '<historical>' prior: 0!
See OMeta2TreeExample!

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

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 18:06'!
addExpr  = addExpr:x "+" mulExpr:y  -> [{'add'. x. y}]
         | addExpr:x "-" mulExpr:y             -> [{'sub'. x. y}]         
         | mulExpr! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 17:17'!
digit    = ^digit:d                 -> [d digitValue]! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 05:07'!
expr = addExpr:e -> [{e}]! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 17:57'!
iName        = firstAndRest(#nameFirst. #nameRest):r -> [(String withAll: r) asSymbol]! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 18:06'!
logExpr  = logExpr:x "and" primExpr:y -> [{'and'. x. y}]
         | logExpr:x "or" primExpr:y           -> [{'or'. x. y}]
         | logExpr:x "xor" primExpr:y          -> [{'xor'. x. y}]
         | "not" logExpr:x                           -> [{'not'. x}]
         | primExpr! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 18:07'!
mulExpr  = mulExpr:x "*" powExpr:y                  -> [{'mul'. x. y}]
         | mulExpr:x "/" powExpr:y                           -> [{'div'. x. y}]
         | powExpr! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 05:06'!
nameFirst    = letter! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 05:06'!
nameRest     = nameFirst | digit! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 05:01'!
number   = number:n digit:d         -> [n * 10 + d]
         | digit! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 18:07'!
powExpr  = powExpr:x "^" relExpr:y  -> [{'pow'. x. y}]
         | relExpr! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 22:08'!
primExpr = "(" expr:x ")"               -> [x]
         | spaces iName:nn spaces   -> [{'name'. nn}]
         | spaces number:n spaces   -> [{'num'. n}]! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 18:07'!
relExpr  = relExpr:x ">" logExpr:y   -> [{'gt'. x. y}]
         | relExpr:x "<" logExpr:y          -> [{'lt'. x. y}]
         | relExpr:x ">=" logExpr:y       -> [{'gte'. x. y}]
         | relExpr:x "<=" logExpr:y       -> [{'lte'. x. y}]
         | relExpr:x "=" logExpr:y          -> [{'eq'. x. y}]
         | relExpr:x "!!=" logExpr:y         -> [{'neq'. x. y}]
         | logExpr! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 19:09'!
root = root:r  ";" expr:ef -> [r, ef]
         | expr:e                  -> [e]! !

!OMeta2LambdaCalculusParserExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 19:24'!
rootHang = root:r  ';' expr:ef -> [{r. ef}]
	// FIXME - Added an error above.. why do we go into an infinite loop attempting when calling via debugMatchAll:?
         | expr:e                  -> [e]! !

!OMeta2LambdaCalculusParserExample class methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 19:22'!
demo
	^ self matchAll: 'x and y;x*y' with: #root.! !

!OMeta2TreeWalkerExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 14:49'!
depth =
        {#Empty} -> [0]
        | {#Leaf anything} -> [1]
        | {#Node depth:l depth:r} -> [(l max: r) + 1]
! !

!OMeta2TreeWalkerExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 14:38'!
sum =
        {#Empty} -> [0]
        | {#Leaf anything:v} -> [v]
        | {#Node sum:l sum:r} -> [l + r]! !

!OMeta2TreeExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 16:18'!
depth
	^ OMeta2TreeWalkerExample match: self with: #depth! !

!OMeta2TreeExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 14:55'!
isCollection
	^ true! !

!OMeta2TreeExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 14:55'!
isSequenceable
	^ true! !

!OMeta2TreeExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 16:18'!
readStream
	^ self unapply readStream! !

!OMeta2TreeExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 16:18'!
sum
	^ OMeta2TreeWalkerExample match: self with: #sum! !

!OMeta2TreeExample class methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 17:01'!
demo
	| tree |
	tree := self demoTree.
	^ Dictionary new
		
			at: #depth
			put: tree depth;
		
			at: #sum:
			put: tree sum;
		yourself.! !

!OMeta2TreeExample class methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 16:12'!
demoTree
	^ OMeta2TreeNodeExample new
		left:
			(OMeta2TreeNodeExample new
				left:
					(OMeta2TreeLeafExample new
						value: 3;
						yourself);
				right:
					(OMeta2TreeNodeExample new
						left: OMeta2TreeLeafExample new;
						right:
							(OMeta2TreeNodeExample new
								left: OMeta2TreeLeafExample new;
								right: OMeta2TreeLeafExample new;
								yourself);
						yourself);
				yourself);
		right:
			(OMeta2TreeNodeExample new
				left:
					(OMeta2TreeLeafExample new
						value: 5;
						yourself);
				right: OMeta2TreeEmptyExample new yourself;
				yourself);
		yourself.! !

!OMeta2TreeEmptyExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 16:25'!
unapply
	^ #(#Empty ).! !

!OMeta2TreeLeafExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 15:44'!
initialize
	value := 1! !

!OMeta2TreeLeafExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 16:24'!
unapply
	^ {#Leaf. value.}! !

!OMeta2TreeLeafExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 15:31'!
value: aNumber
	value := aNumber! !

!OMeta2TreeNodeExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 15:25'!
left: anObject
	leftTree := anObject! !

!OMeta2TreeNodeExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 15:25'!
right: anObject
	rightTree := anObject! !

!OMeta2TreeNodeExample methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 16:24'!
unapply
	^ {#Node. leftTree unapply. rightTree unapply.}! !
