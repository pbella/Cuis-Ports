'From Cuis 4.2 of 25 July 2013 [latest update: #2968] on 4 November 2016 at 2:02:00 am'!
'Description Please enter a description for this package'!
!provides: 'BTreeTests' 1 0!
!requires: 'BTree' 1 0 nil!
!classDefinition: #BTreeAdd category: #BTreeTests!
TestCase subclass: #BTreeAdd
	instanceVariableNames: 'theTree'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BTreeTests'!
!classDefinition: 'BTreeAdd class' category: #BTreeTests!
BTreeAdd class
	instanceVariableNames: ''!


!BTreeAdd methodsFor: 'test' stamp: 'JK 12/14/2015 11:52'!
setUp
	! !

!BTreeAdd methodsFor: 'test' stamp: 'jk 1/26/2016 13:30'!
testAdd_1
	theTree := BTree new: 1.
	self assert: ((theTree at: 1 ifAbsent: [ ^ 'not found' ]) = 'not found').

	theTree add: 1->1.
	self assert: (theTree printString = '1..').
	self assert: ((theTree at: 1 ifAbsent: [ ^ 'not found' ]) = '1').
	theTree add: 2->2.
	self assert: (theTree printString = '1 2.').
	self assert: (theTree depth = 1).
	self assert: ((theTree at: 1 ifAbsent: [ ^ 'not found' ]) = '1').
	self assert: ((theTree at: 2 ifAbsent: [ ^ 'not found' ]) = '2').
	theTree add: 3->3.
	self assert: (theTree printString = '[1..(2)3..]').
	self assert: (theTree depth = 2).
	self assert: ((theTree at: 1 ifAbsent: [ ^ 'not found' ]) = '1').
	self assert: ((theTree at: 2 ifAbsent: [ ^ 'not found' ]) = '2').
	self assert: ((theTree at: 3 ifAbsent: [ ^ 'not found' ]) = '3').
! !

!BTreeAdd methodsFor: 'test' stamp: 'jk 1/26/2016 13:32'!
testAdd_1_b
	theTree := BTree new: 1.
	theTree add: 9->nil.
	theTree add: 6->nil.
	theTree add: 12->nil.
	theTree add: 18->nil.
	theTree add: 21->nil.
	theTree add: 5->nil.
	theTree add: 7->nil.
	theTree add: 2->nil.
	theTree add: 3->nil.
	theTree add: 1->nil.
	theTree add: 4->nil.
	theTree add: 8->nil.
	theTree add: 15->nil.
	theTree add: 16->nil.
	theTree add: 20->nil.
	theTree add: 22->nil.
	theTree add: 19->nil.
	theTree add: 13->nil.
	theTree add: 17->nil.
	theTree add: 11->'11'.
	theTree add: 10->nil.
	theTree add: 14->'14'.
	theTree add: 24->nil.
	theTree add: 25->nil.
	theTree add: 26->'26'.
	theTree add: 23->nil.
	self assert: (theTree depth = 3).
	theTree add: 30->nil.
	self assert: (theTree depth = 4).
	self assert: ((theTree at: 11 ifAbsent: [ ^ 'not found' ]) = '11').
	self assert: ((theTree at: 14 ifAbsent: [ ^ 'not found' ]) = '14').
	self assert: ((theTree at: 26 ifAbsent: [ ^ 'not found' ]) = '26').
	self assert: ((theTree at: 25 ifAbsent: [ ^ 'not found' ]) = nil).
	self assert: ((theTree at: 31 ifAbsent: [ ^ 'not found' ]) = 'not found').
	
! !

!BTreeAdd methodsFor: 'test' stamp: 'JK 12/27/2015 11:00'!
testAdd_2
	theTree := BTree new: 2.
	theTree add: 1->nil.
	self assert: (theTree printString = '1....').
	theTree add: 2->nil.
	self assert: (theTree printString = '1 2...').
	self assert: (theTree depth = 1).
	theTree add: 3->nil.
	self assert: (theTree printString = '1 2 3..').
	self assert: (theTree depth = 1).
	theTree add: 4->nil.
	self assert: (theTree printString = '1 2 3 4.').
	self assert: (theTree depth = 1).
	theTree add: 5->nil.
	self assert: (theTree printString = '[1 2...(3)4 5...]').
	self assert: (theTree depth = 2).
! !

!BTreeAdd methodsFor: 'test' stamp: 'JK 12/14/2015 11:52'!
testEmpty_1
	theTree := BTree new: 1.
	self assert: (theTree printString = '...').
	self assert: (theTree depth = 1).
! !

!BTreeAdd methodsFor: 'test' stamp: 'JK 12/14/2015 11:51'!
testEmpty_2
	theTree := BTree new: 2.
	self assert: (theTree printString = '.....').
	self assert: (theTree depth = 1).
! !

!BTreeAdd methodsFor: 'test' stamp: 'jk 1/26/2016 13:32'!
testMerge
	theTree := BTree new: 2.
	theTree add: 5->nil.
	theTree add: 2->nil.
	theTree add: 6->nil.
	theTree add: 1->nil.
	theTree add: 7->nil.
	theTree add: 9->nil.
	theTree add: 12->nil.
	theTree add: 18->nil.
	theTree add: 21->nil.
	theTree add: 3->nil.
	theTree add: 4->nil.
	theTree add: 8->nil.
	theTree add: 15->nil.
	theTree add: 16->nil.
	theTree add: 20->nil.
	theTree add: 22->nil.
	theTree add: 19->nil.
	
"	theTree inspect."
	self assert: (theTree printString = '[1 2 3 4.(5)6 7 8..(9)12 15...(16)18 19...(20)21 22...]').
	"this will merge left"
	theTree removeKey: 19.
	self assert: (theTree printString = '[1 2 3 4.(5)6 7 8..(9)12 15...(16)18 20 21 22.]').
	theTree removeKey: 5.
	self assert: (theTree printString = '[1 2 3 4.(6)7 8...(9)12 15...(16)18 20 21 22.]').
	theTree removeKey: 6.
	self assert: (theTree printString = '[1 2 3..(4)7 8...(9)12 15...(16)18 20 21 22.]').
	theTree removeKey: 4.
	self assert: (theTree printString = '[1 2...(3)7 8...(9)12 15...(16)18 20 21 22.]').
	theTree removeKey: 3.
	self assert: (theTree printString = '[1 2...(7)8 9 12 15.(16)18 20 21 22.]').
	theTree removeKey: 18.
	self assert: (theTree printString = '[1 2...(7)8 9 12 15.(16)20 21 22..]').
	theTree removeKey: 20.
	self assert: (theTree printString = '[1 2...(7)8 9 12 15.(16)21 22...]').
	theTree removeKey: 21.
	self assert: (theTree printString = '[1 2...(7)8 9 12..(15)16 22...]').
	theTree removeKey: 7.
	self assert: (theTree printString = '[1 2...(8)9 12...(15)16 22...]').
	theTree removeKey: 16.
	self assert: (theTree printString = '[1 2...(8)9 12 15 22.]').
	theTree removeKey: 1.
	self assert: (theTree printString = '[2 8...(9)12 15 22..]').
	theTree removeKey: 15.
	self assert: (theTree printString = '[2 8...(9)12 22...]').
	theTree removeKey: 9.
	self assert: (theTree printString = '2 8 12 22.').
! !

!BTreeAdd methodsFor: 'test' stamp: 'jk 1/26/2016 13:38'!
testRotateL
	theTree := BTree new: 1.
	theTree at: 5 put: '5'.
	theTree at: 2 put: '2'.
	theTree at: 6 put: '6'.
	theTree at: 7 put: '7'.
	self assert: (theTree printString = '[2..(5)6 7.]').
	self assert: ((theTree at: 2 ifAbsent: [ ^ 'not found' ]) = '2').
	theTree removeKey: 2.
	self assert: ((theTree at: 2 ifAbsent: [ ^ 'not found' ]) = 'not found').
	self assert: (theTree printString = '[5..(6)7..]').! !

!BTreeAdd methodsFor: 'test' stamp: 'jk 1/26/2016 13:41'!
testRotateR
	theTree := BTree new: 1.
	theTree at: 5 put: '5'.
	theTree at: 2 put: '2'.
	theTree at: 6 put: '6'.
	theTree at: 1 put: '1'.
	self assert: (theTree printString = '[1 2.(5)6..]').
	self assert: ((theTree at: 6 ifAbsent: [ ^ 'not found' ]) = '6').
	theTree removeKey: 6.
	self assert: ((theTree at: 6 ifAbsent: [ ^ 'not found' ]) = 'not found').
	self assert: ((theTree removeKey: 6 ifAbsent: [ ^ 'not found' ]) = 'not found').
	self assert: (theTree printString = '[1..(2)5..]').
	self assert: ((theTree removeKey: 5 ifAbsent: [ ^ 'found' ]) = 'nil').
! !

!BTreeAdd methodsFor: 'test' stamp: 'jk 1/26/2016 13:37'!
testRotateR1
	theTree := BTree new: 1.
	theTree add: 5->'5'.
	theTree add: 2->'2'.
	theTree add: 6->'6'.
	theTree add: 1->'1'.
	self assert: (theTree printString = '[1 2.(5)6..]').
	self assert: ((theTree at: 6 ifAbsent: [ ^ 'not found' ]) = '6').

	theTree removeKey: 6.
	self assert: ((theTree at: 6 ifAbsent: [ ^ 'not found' ]) = 'not found').
	self assert: (theTree printString = '[1..(2)5..]').
	
	theTree add: 7->'7'.
	theTree add: 9->'9'.
	theTree add: 12->'12'.
	theTree add: 18->'18'.
	theTree add: 21->'21'.
	theTree add: 22->'22'.
	self assert: (theTree printString = '[[1..(2)5..](7)[9..(12)18..(21)22..]]').

	self assert: ((theTree at: 1 ifAbsent: [ ^ 'not found' ]) = '1').
	theTree removeKey: 1.
	self assert: (theTree printString = '[[2 5.(7)9..](12)[18..(21)22..]]').
	self assert: ((theTree at: 1 ifAbsent: [ ^ 'not found' ]) = 'not found').
	
"	theTree inspect."
	theTree removeKey: 22.
	self assert: (theTree printString = '[2 5.(7)9..(12)18 21.]').
"	theTree inspect."! !
