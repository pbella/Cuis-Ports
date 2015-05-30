'From Cuis 4.2 of 25 July 2013 [latest update: #2337] on 29 May 2015 at 9:18:14.805648 pm'!
'Description Tests for OMeta2'!
!provides: 'OMeta2Tests' 1 2!
!requires: 'OMeta2Examples' 1 0 nil!
!requires: 'OMeta2' 1 0 nil!
!classDefinition: #OMeta2ExampleTests category: #OMeta2Tests!
TestCase subclass: #OMeta2ExampleTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2Tests'!
!classDefinition: 'OMeta2ExampleTests class' category: #OMeta2Tests!
OMeta2ExampleTests class
	instanceVariableNames: ''!


!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/26/2015 03:43'!
testFactorial
	self should: (OMeta2Examples match: 0 with: #fact) = 1.
	self should: (OMeta2Examples match: 5 with: #fact) = 120.
	self should: (OMeta2Examples match: 10 with: #fact) = 3628800.! !

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

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/29/2015 21:16'!
testLisp
	| lispClass |
	lispClass := OMeta2LispExample.

	"*** ev rule tests..."

	"string"
	self should: (lispClass eval: #t) = true.
	self should: (lispClass eval: #z) = nil.

	"lambda... under app rule tests"

	"quote"
	self should: (lispClass eval: #(quote 1)) = 1.
	self should: (lispClass eval: #(quote (1 2 3))) = #(1 2 3).

	"cond"
	self should: (lispClass eval: #(cond nil)) = nil.
	self should: (lispClass eval:
		#(cond
			((eq (quote a) (quote b)) (quote optiona))
			((eq (quote b) (quote b)) (quote optionb))
			(t (quote invalid)))) = #optionb.
	self should: (lispClass eval:
		#(cond
			((eq (quote a) (quote b)) (quote optiona))
			((eq (quote b) (quote 3)) (quote optionb))
			(t (quote invalid)))) = #invalid.

	"*** app rule tests..."

	"car"
	self should: (lispClass eval: #(car (quote (1 2 3)))) = 1.

	"cdr"
	self flag: #fixme.
	"Should this be an OC?"
	"self should: (lispClass eval: #(cdr (quote (1 2 3)))) = #(2 3)."
	self should: (lispClass eval: #(cdr (quote (1 2 3)))) = (OrderedCollection new add: 2; add: 3; yourself).

	"cons"
	self should: (lispClass eval: #(cons (quote a) (quote b))) = #(a b).

	"atom"
	self should: (lispClass eval: #(atom (quote a))) = true.
	self should: (lispClass eval: #(atom (cons (quote a) (quote b)))) = false.

	"eq"
	"a and b are both nil so eq should always return true (in a real Lisp environment, this would result in an error)"
	self should: (lispClass eval: #(eq a a)) = true.
	self should: (lispClass eval: #(eq a b)) = true.
	"when quoted, you get the results you were probably expecting"
	self should: (lispClass eval: #(eq (quote a) (quote a))) = true.
	self should: (lispClass eval: #(eq (quote a) (quote b))) = false.

	"lambda"
	self should: (lispClass eval: #((lambda (x) (cons x x)) (quote boo))) = #(boo boo).

	"*** more complex and multiple expressions"
	self should: (lispClass evalAll:
		#((eq (quote a) (quote b))
		(cons (quote x) (quote y)))) = #(x y).
	self should: (lispClass evalAll:
		#((cons
			(eq (quote a) (quote b))
			(cons (quote x) (quote y))
			))) = #(false (x y)).
	self should: (lispClass evalAll:
		#(((lambda (x)
			(cons x x)) (quote boo))
			(eq a b))) = true.
	self should: (lispClass evalAll:
		#((cons
			((lambda (x)
				(cons x x))
				(quote boo))
			(eq x (quote boo))))) = #((boo boo) false).
	self should: (lispClass eval:
		#((lambda (x)
			(cond
				((eq (quote a) x) (quote optiona))
				((eq (quote b) x) (quote optionb))
				(t (quote invalid))))
			(quote a))) = #optiona.
	self should: (lispClass eval:
		#((lambda (x)
			(cond
				((eq (quote a) x) (quote optiona))
				((eq (quote b) x) (quote optionb))
				(t (quote invalid))))
			(quote b))) = #optionb.
	self should: (lispClass eval:
		#((lambda (x)
			(cond
				((eq (quote a) x) (quote optiona))
				((eq (quote b) x) (quote optionb))
				(t (quote invalid))))
			(quote c))) = #invalid.! !

!OMeta2ExampleTests methodsFor: 'as yet unclassified' stamp: 'pb 5/26/2015 03:34'!
testNumber
	self should: (OMeta2Examples matchAll: '1234' with: #number) = 1234.
	self should: [OMeta2Examples match: '1234' with: #number] raise: OM2Fail.
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
