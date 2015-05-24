'From Cuis 4.2 of 25 July 2013 [latest update: #2332] on 23 May 2015 at 10:01:08.461436 pm'!
'Description Tests for OMeta2'!
!provides: 'OMeta2Tests' 1 1!
!requires: 'OMeta2' 1 0 nil!
!requires: 'OMeta2Examples' 1 0 nil!
!classDefinition: #OMeta2ExampleTests category: #OMeta2Tests!
TestCase subclass: #OMeta2ExampleTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Tests'!
!classDefinition: 'OMeta2ExampleTests class' category: #OMeta2Tests!
OMeta2ExampleTests class
	instanceVariableNames: ''!


!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 16:52'!
testFactorial
	self should: (OMeta2Examples match: 5 with: #fact) = 120.
! !

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 16:54'!
testGreeting
	self should: (OMeta2Examples matchAll: 'howdy' with: #greeting) = 'howdy'! !

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 16:56'!
testIdentifierIndex
	self should: (OMeta2Examples matchAll: 'abc123sd' with: #identifierIdx) = (Association key: 1 value: 9)! !

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 22:01'!
testLambdaCalculus
	self should: (OMeta2LambdaCalculusParserExample matchAll: '4' with: #digit) = 4.
	self should: (OMeta2LambdaCalculusParserExample matchAll: 'x and y' with: #root) =   #(#('and' #('name' #x) #('name' #y))).
	self should: (OMeta2LambdaCalculusParserExample matchAll: 'x and y;x*y' with: #root) =   #(#('and' #('name' #x) #('name' #y)) #('mul' #('name' #x) #('name' #y)))! !

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 16:52'!
testNumber
	self should: (OMeta2Examples matchAll: '1234' with: #number) = 1234.
! !

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 16:53'!
testObjectStructure
	self should: (OMeta2Examples matchAll: #($a $b $c 1 2 3 #(4 5)) with: #structure) = #(4 5)
! !

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/22/2015 16:52'!
testStringMatch
	self should: (OMeta2Examples matchAll: 'abc123' with: #identifier) = 'abc123'
! !

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/23/2015 16:22'!
testTree
	| tree |
	tree := OMeta2TreeExample demoTree.
	self should:  tree unapply = #(#Node #(#Node #(#Leaf 3) #(#Node #(#Leaf 1) #(#Node #(#Leaf 1) #(#Leaf 1)))) #(#Node #(#Leaf 5) #(#Empty))).
	self should: tree depth = 5.
	self should: tree sum = 11.! !
