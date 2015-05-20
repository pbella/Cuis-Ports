!CompiledMethod methodsFor: 'decompiling' stamp: 'pb 5/20/2015 15:28'!
methodNode
	"Return the parse tree that represents self.  Note: this is brittle and will need to be updated to reflect any upstream methodNode changes in the future."
	| aClass source |
	aClass := self methodClass.
	source := self
				getSourceFor: (self selector ifNil: [self defaultSelector])
				in: aClass.
	"Fix for debugging OMeta"
	((self methodClass inheritsFrom: OMeta2) and: [
		OMeta2RuleParser isOMeta2Rule: (self getSourceFromFile ifNil: [''])
	]) ifTrue: [
		^ self decompile
	]. "end OMeta"
	^(Parser new
		encoderClass: (self isBlueBookCompiled
						ifTrue: [EncoderForV3]
						ifFalse: [EncoderForV3PlusClosures]);
		parse: source class: aClass)
			sourceText: source;
			yourself
