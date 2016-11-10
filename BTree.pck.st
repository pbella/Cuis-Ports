'From Cuis 4.2 of 25 July 2013 [latest update: #2968] on 6 November 2016 at 4:26:22 pm'!
'Description Jonathan Kelly''s BTree implementation.

Still need to implement a number of methods to provide complete functionality. However, enough functionality is in place to allow for basic usage and testing such as a Dictionary drop-in replacement via #at: and #at:put:

Dictionary is currently faster on both small and large collections so performance tuning is needed.'!
!provides: 'BTree' 1 0!
!classDefinition: #BTree category: #BTree!
Object subclass: #BTree
	instanceVariableNames: 'order trunk'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BTree'!
!classDefinition: 'BTree class' category: #BTree!
BTree class
	instanceVariableNames: 'DefaultOrder'!

!classDefinition: #BTreePage category: #BTree!
Object variableSubclass: #BTreePage
	instanceVariableNames: 'order maxItems items'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BTree'!
!classDefinition: 'BTreePage class' category: #BTree!
BTreePage class
	instanceVariableNames: ''!

!classDefinition: #BTreePageInner category: #BTree!
BTreePage variableSubclass: #BTreePageInner
	instanceVariableNames: 'children'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BTree'!
!classDefinition: 'BTreePageInner class' category: #BTree!
BTreePageInner class
	instanceVariableNames: ''!


!BTree commentStamp: '<historical>' prior: 0!
I represent the BTree datastructure, and (will eventually) answer the same protocols as a Dictionary.!

!BTreePage commentStamp: '<historical>' prior: 0!
I am a BTree Page.
I am created by my BTree, and (should) only respond to messages from my BTree.!

!BTreePageInner commentStamp: '<historical>' prior: 0!
I am a BTree Inner Page.
I am created by my BTree, and (should) only respond to messages from my BTree.!

!BTree methodsFor: 'private' stamp: 'JK 12/29/2015 09:59'!
add: anAssociation
	"add anAssociation to the BTree, replacing the existing entry if it exists. Answer the Association."
	
	| popped newTrunk |
	popped := trunk add: anAssociation .
	popped isNil ifFalse: [
		newTrunk := BTreePageInner new: order.
		newTrunk at: 1 put: (popped at: 1).
		newTrunk inc.
		newTrunk lesserAt: 1 put: trunk.
		newTrunk greaterAt: 1 put: (popped at: 2).
		trunk := newTrunk.
	].
	^ anAssociation.! !

!BTree methodsFor: 'accessing' stamp: 'jk 1/4/2016 17:12'!
associations
	| aCollection |
	aCollection := OrderedCollection new: self maxItems.
	"aCollection inspect."
	trunk do: [ :anAssociation |
		aCollection add: anAssociation.
	].
	^ aCollection.! !

!BTree methodsFor: 'accessing' stamp: 'JK 1/2/2016 15:57'!
at:  aKey
	^ self at: aKey ifAbsent: [self errorKeyNotFound].! !

!BTree methodsFor: 'accessing' stamp: 'JK 1/2/2016 15:59'!
at:  aKey ifAbsent: aBlock
	[ ^ trunk find: aKey. ]
	on: Error
	do: [ :exc|
		exc messageText = 'key not found'
			ifTrue: [ ^ aBlock value]
			ifFalse: [exc pass ].
		].! !

!BTree methodsFor: 'accessing' stamp: 'JK 1/2/2016 16:15'!
at:  aKey ifAbsentPut: aBlock
	[ ^ trunk find: aKey. ]
	on: Error
	do: [ :exc|
		exc messageText = 'key not found'
			ifTrue: [ ^ trunk at: aKey put: aBlock value]
			ifFalse: [exc pass ].
		].! !

!BTree methodsFor: 'adding' stamp: 'JK 12/29/2015 09:58'!
at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."
	||
	self add: key->anObject.
	^ anObject.! !

!BTree methodsFor: 'testing' stamp: 'JK 12/14/2015 11:49'!
depth
	^ trunk depth.! !

!BTree methodsFor: 'private' stamp: 'JK 1/2/2016 15:32'!
errorKeyNotFound

	self error: 'key not found'! !

!BTree methodsFor: 'private' stamp: 'JK 12/10/2015 22:32'!
init: anOrder
	order := anOrder.
	trunk := BTreePage new: anOrder.! !

!BTree methodsFor: 'accessing' stamp: 'jk 1/4/2016 17:04'!
keys
	| aCollection |
	aCollection := OrderedCollection new: self maxItems.
	"aCollection inspect."
	trunk do: [ :anAssociation |
		aCollection add: anAssociation key.
	].
	^ aCollection.! !

!BTree methodsFor: 'private' stamp: 'pb 11/4/2016 02:26:58'!
maxItems
	| total pageSize |
	total := 0.
	pageSize := 2 * order.
	0
		to: self depth - 1
		do: [ :lv |
			total := total + (pageSize * (pageSize + 1 raisedTo: lv)).
			"
			Transcript
				show: lv;
				space;
				show: (pageSize + 1 raisedTo: lv);
				space;
				show: total;
				newLine"
				self ].
	^ total.! !

!BTree methodsFor: 'printing' stamp: 'JK 12/13/2015 12:38'!
printOn: aStream
	trunk printOn: aStream.! !

!BTree methodsFor: 'removing' stamp: 'JK 1/2/2016 15:48'!
remove: anAssociation
	^ self remove: anAssociation ifAbsent: [
		self errorKeyNotFound.
	].
! !

!BTree methodsFor: 'removing' stamp: 'JK 1/2/2016 15:51'!
remove: anAssociation ifAbsent: aBlock
	[((trunk remove: anAssociation) and: [ trunk items = 0 ])
		ifTrue: [
			trunk := trunk lesserAt: 1.
		].
	]
	on: Error
	do: [ :exc |
		exc messageText = 'key not found'
			ifTrue: [ ^ aBlock value ]
			ifFalse: [ exc pass ].
	].! !

!BTree methodsFor: 'removing' stamp: 'JK 1/2/2016 15:34'!
removeKey: aValue
	^ self remove: aValue->nil ifAbsent: [
		self errorKeyNotFound.
	].
! !

!BTree methodsFor: 'removing' stamp: 'JK 12/29/2015 10:32'!
removeKey: aValue ifAbsent: aBlock
	^ self remove: aValue->nil ifAbsent: aBlock.
! !

!BTree methodsFor: 'accessing' stamp: 'jk 1/4/2016 17:13'!
values
	| aCollection |
	aCollection := OrderedCollection new: self maxItems.
	"aCollection inspect."
	trunk do: [ :anAssociation |
		aCollection add: anAssociation value
	].
	^ aCollection.! !

!BTree class methodsFor: 'as yet unclassified' stamp: 'jk 1/26/2016 13:13'!
initialize
	"set the default order"
     DefaultOrder := 2.! !

!BTree class methodsFor: 'as yet unclassified' stamp: 'JK 12/6/2015 21:44'!
new
	^ self new: DefaultOrder.! !

!BTree class methodsFor: 'as yet unclassified' stamp: 'JK 12/10/2015 22:28'!
new: anOrder
	^ super new init: anOrder.
! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 15:43'!
add: anAssociation
	| low item theKey|
	
	theKey := anAssociation key.
	low := self finder: theKey.
	
	(items > 0 and: [(item := (self at: low)) isNil not] and: [item key = theKey])
		ifTrue: [
			self at: low put: anAssociation.
			^ nil.
		]
		ifFalse: [
			self shiftRightFrom: low.
			self at: low put: anAssociation.
			items := items + 1.
			^ items > maxItems
				ifTrue: [ self split. ]
				ifFalse: nil.
		].
! !

!BTreePage methodsFor: 'private' stamp: 'JK 12/17/2015 15:38'!
dec
	items := items - 1.! !

!BTreePage methodsFor: 'private' stamp: 'JK 12/14/2015 11:49'!
depth
	^ 1.! !

!BTreePage methodsFor: 'private' stamp: 'JK 1/2/2016 15:32'!
errorKeyNotFound

	self error: 'key not found'! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/26/2016 13:29'!
find: aKey
	| low found |
	
	low := self finder: aKey.
	
	^ (low <= (items) and: [(found := self at: low) key = aKey ])
		ifTrue:
			[ found value ]
		ifFalse:
			[ self errorKeyNotFound ].
	! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 15:43'!
finder: aKey
"find the position in the the page this item should be inserted at"
	|low high mid|
	low := 1.
	high := items.
	
	[
		mid _ high + low // 2.
		low > high.
	] whileFalse: [
		((self at: mid) key < aKey)
			ifTrue: [
				low _ mid + 1
			]
			ifFalse: [
				high _ mid - 1
			].
	].
	^ low.! !

!BTreePage methodsFor: 'private' stamp: 'JK 12/20/2015 11:56'!
greaterAt: idx
	^ nil.! !

!BTreePage methodsFor: 'private' stamp: 'JK 12/20/2015 11:19'!
greaterAt: idx put: obj
	^ self.! !

!BTreePage methodsFor: 'private' stamp: 'JK 12/17/2015 15:38'!
inc
	items := items + 1.! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 15:52'!
init: anOrder
	| |
	order := anOrder.
	maxItems := anOrder * 2.
	items := 0.
	^ self.! !

!BTreePage methodsFor: 'private' stamp: 'JK 12/10/2015 21:04'!
items
	^ items.! !

!BTreePage methodsFor: 'private' stamp: 'JK 12/17/2015 15:42'!
items: n
	items := n.! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 16:11'!
least
	^ self at: 1.! !

!BTreePage methodsFor: 'private' stamp: 'JK 12/20/2015 16:03'!
lesserAt: idx
	^ nil.! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 16:07'!
moveTo: aPage from: fromIdx to: toIdx
	aPage at: toIdx put: (self at:fromIdx).
	self at: fromIdx put: nil.
! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 15:44'!
printOn: aStream
	|idx item|
	idx := 1.
	[idx <= self size ] whileTrue: [
		(item := (self at: idx)) isNil
			ifTrue: [
				aStream nextPut: $.
			]
			ifFalse: [
				idx > 1 ifTrue: [
					aStream nextPut: Character space.
				].
				item key printOn: aStream.
			].
		idx := idx + 1.
	].
! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 15:44'!
remove: anAssociation
	| low found theKey |
	
	theKey := anAssociation key.
	low := self finder: theKey.
	
	(low <= items and: [(found := self at: low) key = theKey ])
		ifTrue: [
			anAssociation value: found value.
			self shiftLeftFrom: low.
			items := items - 1.
		]
		ifFalse: [
			self errorKeyNotFound.
		].
	^ items < order.! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 15:44'!
shiftLeftFrom: anIndex
	||
	anIndex to: items do: [ :idx |
		self at: idx put: (self at: (idx + 1)).
	].
! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 15:44'!
shiftRightFrom: anIndex
	||
	items to: anIndex by: -1 do: [ :idx |
		self at: (idx + 1) put: (self at: idx).
	].
! !

!BTreePage methodsFor: 'private' stamp: 'jk 1/7/2016 15:45'!
split
	"this page is over full (2*order + 1) ... so split the current page in two - and return the middle item and the right half"
	
	| newPage middle pair |
	
	newPage := (self class) new: order.
	middle := order + 1.
	pair := Array with:  (self at: middle) with: newPage.
	self at: middle put: nil.
	
	1 to: order do: [
		:toIndex |
		self moveTo: newPage from: (order + 1 + toIndex) to: toIndex.
	].
	self items: order.
	newPage items: order.
	^ pair.
! !

!BTreePage class methodsFor: 'as yet unclassified' stamp: 'jk 1/7/2016 16:06'!
new: anOrder
	^ (super new: (2 * anOrder + 1)) init: anOrder.! !

!BTreePageInner methodsFor: 'private' stamp: 'jk 1/7/2016 15:59'!
add: anAssociation
	| low popped |
	
	low := self finder: anAssociation key.
	
	popped := (children at: low) add: anAssociation.
	popped isNil ifFalse: [
		self shiftRightFrom: low.
		self at: low put: (popped at: 1).
		items := items + 1.
		self greaterAt: low put: (popped at: 2).
	].
	^ items > maxItems
		ifTrue: [self split]
		ifFalse: nil.
! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/14/2015 11:50'!
depth
	^ 1 + (children at: 1) depth.! !

!BTreePageInner methodsFor: 'private' stamp: 'jk 1/26/2016 13:28'!
find: aKey
	| low found |
	
	low := self finder: aKey.
	
	^ (low <= (items) and: [(found := self at: low) key  = aKey ])
		ifTrue:
			[ found value ]
		ifFalse:
			[ (children at: low) find: aKey ].! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/11/2015 15:46'!
greaterAt: idx
	^ children at: (idx + 1).! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/13/2015 11:03'!
greaterAt: idx put: aPage
	children at: (idx + 1) put: aPage.! !

!BTreePageInner methodsFor: 'private' stamp: 'jk 1/7/2016 16:10'!
init: anOrder
	super init: anOrder.
	children := Array new: maxItems + 2.
	^ self.! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/20/2015 20:41'!
least
	^ (children at: 1) least.! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/11/2015 15:46'!
lesserAt: idx
	^ children at: idx.! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/13/2015 11:04'!
lesserAt: idx put: aPage
	children at: idx put: aPage.! !

!BTreePageInner methodsFor: 'private' stamp: 'pb 11/4/2016 02:26:32'!
mergePage: anIndex
	| left right |
	"
	Transcript
		show: 'mergePage: ';
		show: anIndex;
		newLine.
		"
	left := children at: anIndex.
	right := children at: anIndex + 1.
	left inc.
	left
		at: left items
		put: (self at: anIndex).
	self shiftLeftFrom: anIndex.
	self
		lesserAt: anIndex
		put: left.
	self dec.
	1
		to: right items
		do: [ :idx |
			left inc.
			right
				moveTo: left
				from: idx
				to: left items ].
	left
		greaterAt: left items
		put: (right greaterAt: right items).
	^ items < order.! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/16/2015 11:33'!
moveTo: aPage from: fromIdx to: toIdx
	super moveTo: aPage from: fromIdx to: toIdx.
	aPage lesserAt: toIdx put: (self lesserAt: fromIdx).
	self lesserAt: fromIdx put: nil.
! !

!BTreePageInner methodsFor: 'private' stamp: 'jk 1/7/2016 16:00'!
printOn: aStream
	|x|
	x := 1.
	aStream nextPutAll: '['.
	items == 0
		ifTrue: [
			aStream nextPut: $. . 
		]
		ifFalse: [
			[x <= items] whileTrue: [
				(children at: x) printOn: aStream.
				aStream nextPut: $(.
				(self at: x) key printOn: aStream.
				aStream nextPutAll: ')'.
				x := x + 1.
			].
		 	(children at: x) printOn: aStream.
		].
	aStream nextPutAll: ']'.
! !

!BTreePageInner methodsFor: 'private' stamp: 'jk 1/7/2016 16:00'!
remove: anAssociation
	| theKey low item largerPage largerItem result |
	
	theKey := anAssociation key.
	low := self finder: theKey.
	
	(low <= (items) and: [(item := self at: low) key = theKey ])
		ifTrue: [
			"store the associated object in anAssociation so the caller can return the object"
			anAssociation value: item value.
			largerPage := self greaterAt: low.
			largerItem := largerPage least.
			self at: low put: largerItem.
			low := low + 1.
			result := largerPage remove: largerItem.
		]
		ifFalse: [
			result := ((children at: low) remove: anAssociation)
		].
	result
		ifTrue: [
			low <= items
				ifTrue: [
					(children at: (low + 1)) items > order
						ifTrue: [
							^ self rotateLeft: low.
						]
						ifFalse: [
							(low > 1 and: [(children at: (low - 1)) items > order])
								ifTrue: [
									^ self rotateRight: low.
								]
								ifFalse: [
									self mergePage: low.
								].
							].
				]
				ifFalse: [
					((children at: (low - 1)) items > order)
						ifTrue: [
							^ self rotateRight: low.
						]
						ifFalse: [
							self mergePage: (low - 1).
						].			
				].
		].
	^ items < order.! !

!BTreePageInner methodsFor: 'private' stamp: 'pb 11/4/2016 02:26:21'!
rotateLeft: anIndex
	| left right |
	"
	Transcript
		show: 'rotateLeft: ';
		show: anIndex;
		newLine.
		"
	(left := children at: anIndex)
		at: order
		put: (self at: anIndex).
	left inc.
	self
		at: anIndex
		put: ((right := children at: anIndex + 1) at: 1).
	left
		greaterAt: left items
		put: (right lesserAt: 1).
	right shiftLeftFrom: 1.
	right dec.
	" if we are rotating, it means we found a spare item to balance the tree, so no further balancing required"
	^ false.! !

!BTreePageInner methodsFor: 'private' stamp: 'jk 1/7/2016 16:02'!
rotateRight: anIndex
	| left right |
	
	(right := children at: anIndex) shiftRightFrom: 1.
	right at: 1 put: (self at: (anIndex - 1)).
	right inc.
	self at: (anIndex - 1) put: ((left := children at: (anIndex - 1)) at: left items).
	left at: left items put: nil.
	left dec.

	" if we are rotating, it means we found a spare item to balance the tree, so no further balancing required"
	^ false.! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/20/2015 15:33'!
shiftLeftFrom: anIndex
	||
	super shiftLeftFrom: anIndex.
	anIndex to: (items + 1) do: [ :idx |
		children at: (idx) put: (children at: (idx + 1)).
	].
! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/18/2015 18:12'!
shiftRightFrom: anIndex
	||
	super shiftRightFrom: anIndex.
	(items + 1) to: (anIndex + 1) by: -1 do: [ :idx |
		children at: (idx + 1) put: (children at: idx).
	].
! !

!BTreePageInner methodsFor: 'private' stamp: 'JK 12/27/2015 10:54'!
split
	| pair idx|
	idx := 2 * order + 1.
	pair := super split.
	(pair at:2) greaterAt: order put: (self greaterAt: idx).
	self greaterAt: idx put: nil.
	^ pair.
! !
BTree initialize!
