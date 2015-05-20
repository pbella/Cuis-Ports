!CompiledMethod methodsFor: 'decompiling' stamp: 'rkrk 4/25/2009 01:30'!
methodNode
	"Return the parse tree that represents self.  Note: this is brittle and will need to be updated to reflect any upstream methodNode changes in the future."

	| source |
	"Fix for debugging OMeta"
	((self methodClass inheritsFrom: OMeta2) and: [
		OMeta2RuleParser isOMeta2Rule: (self getSourceFromFile ifNil: [''])
	]) ifTrue: [
		^ self decompile
	].
	^ (source := self getSourceFromFile)
		ifNil: [self decompile]
		ifNotNil: [self parserClass new 
					parse: source 
					class: (self methodClass ifNil: [self sourceClass])]! !

