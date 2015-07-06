'From Cuis 4.2 of 25 July 2013 [latest update: #2400] on 6 July 2015 at 1:38:12.630297 pm'!
'Description Based on 3DTransform-pbm.19.mcz'!
!provides: '3DTransform' 1 1!
!classDefinition: #CatmullRom category: #'3DTransform-Vectors'!
Object subclass: #CatmullRom
	instanceVariableNames: 'p1 p2 p3 p4'
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'CatmullRom class' category: #'3DTransform-Vectors'!
CatmullRom class
	instanceVariableNames: ''!

!classDefinition: #Vector category: #'3DTransform-Vectors'!
FloatArray variableWordSubclass: #Vector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'Vector class' category: #'3DTransform-Vectors'!
Vector class
	instanceVariableNames: ''!

!classDefinition: #Matrix4x4 category: #'3DTransform-Vectors'!
Vector variableWordSubclass: #Matrix4x4
	instanceVariableNames: ''
	classVariableNames: 'IdentityMatrix ZeroMatrix'
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'Matrix4x4 class' category: #'3DTransform-Vectors'!
Matrix4x4 class
	instanceVariableNames: ''!

!classDefinition: #Quaternion category: #'3DTransform-Vectors'!
Vector variableWordSubclass: #Quaternion
	instanceVariableNames: ''
	classVariableNames: 'QuaternionIdentity'
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'Quaternion class' category: #'3DTransform-Vectors'!
Quaternion class
	instanceVariableNames: ''!

!classDefinition: #Vector2 category: #'3DTransform-Vectors'!
Vector variableWordSubclass: #Vector2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'Vector2 class' category: #'3DTransform-Vectors'!
Vector2 class
	instanceVariableNames: ''!

!classDefinition: #Vector3 category: #'3DTransform-Vectors'!
Vector variableWordSubclass: #Vector3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'Vector3 class' category: #'3DTransform-Vectors'!
Vector3 class
	instanceVariableNames: ''!

!classDefinition: #Vector4 category: #'3DTransform-Vectors'!
Vector variableWordSubclass: #Vector4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'Vector4 class' category: #'3DTransform-Vectors'!
Vector4 class
	instanceVariableNames: ''!

!classDefinition: #VectorArray category: #'3DTransform-Arrays'!
Vector variableWordSubclass: #VectorArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'VectorArray class' category: #'3DTransform-Arrays'!
VectorArray class
	instanceVariableNames: ''!

!classDefinition: #MatrixArray category: #'3DTransform-Arrays'!
VectorArray variableWordSubclass: #MatrixArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'MatrixArray class' category: #'3DTransform-Arrays'!
MatrixArray class
	instanceVariableNames: ''!

!classDefinition: #Matrix4x4Array category: #'3DTransform-Arrays'!
MatrixArray variableWordSubclass: #Matrix4x4Array
	instanceVariableNames: ''
	classVariableNames: 'Zero4x4Matrix'
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'Matrix4x4Array class' category: #'3DTransform-Arrays'!
Matrix4x4Array class
	instanceVariableNames: ''!

!classDefinition: #Texture2Array category: #'3DTransform-Arrays'!
VectorArray variableWordSubclass: #Texture2Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'Texture2Array class' category: #'3DTransform-Arrays'!
Texture2Array class
	instanceVariableNames: ''!

!classDefinition: #Vector2Array category: #'3DTransform-Arrays'!
VectorArray variableWordSubclass: #Vector2Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'Vector2Array class' category: #'3DTransform-Arrays'!
Vector2Array class
	instanceVariableNames: ''!

!classDefinition: #Vector3Array category: #'3DTransform-Arrays'!
VectorArray variableWordSubclass: #Vector3Array
	instanceVariableNames: ''
	classVariableNames: 'ZeroVertex'
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'Vector3Array class' category: #'3DTransform-Arrays'!
Vector3Array class
	instanceVariableNames: ''!

!classDefinition: #VectorColor3Array category: #'3DTransform-Arrays'!
VectorArray variableWordSubclass: #VectorColor3Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'VectorColor3Array class' category: #'3DTransform-Arrays'!
VectorColor3Array class
	instanceVariableNames: ''!

!classDefinition: #VectorColor4Array category: #'3DTransform-Arrays'!
VectorArray variableWordSubclass: #VectorColor4Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'VectorColor4Array class' category: #'3DTransform-Arrays'!
VectorColor4Array class
	instanceVariableNames: ''!

!classDefinition: #VectorRotationArray category: #'3DTransform-Arrays'!
VectorArray variableWordSubclass: #VectorRotationArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Arrays'!
!classDefinition: 'VectorRotationArray class' category: #'3DTransform-Arrays'!
VectorRotationArray class
	instanceVariableNames: ''!

!classDefinition: #VectorColor category: #'3DTransform-Vectors'!
Vector variableWordSubclass: #VectorColor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'VectorColor class' category: #'3DTransform-Vectors'!
VectorColor class
	instanceVariableNames: ''!

!classDefinition: #VectorColor3 category: #'3DTransform-Vectors'!
Vector variableWordSubclass: #VectorColor3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3DTransform-Vectors'!
!classDefinition: 'VectorColor3 class' category: #'3DTransform-Vectors'!
VectorColor3 class
	instanceVariableNames: ''!


!CatmullRom commentStamp: 'das 8/1/2006 16:30' prior: 0!
Simple Catmull-Rom spline object. Simply seed it with four vectors of any dimensionality, then ask for the interpolated value with CatmullRom>>at:t where t is a value between 0.0 and 1.0.!

!Vector commentStamp: '<historical>' prior: 0!
I am the superclass for all floating point vector objects in Croquet.!

!Matrix4x4 commentStamp: '<historical>' prior: 0!
I represent a standard 4x4 transformation matrix used throughout 3D graphics.!

!Quaternion commentStamp: '<historical>' prior: 0!
I represent general 3d rotations by using Unit-Quaternions. Unit-Quaternions are one of the best available representation for rotations in computer graphics because they provide an easy way of doing arithmetic with them and also because they allow us to use spherical linear interpolation (so-called "slerps") of rotations.

Indexed Variables:
	a	<Float>	the real part of the quaternion
	b	<Float>	the first imaginary part of the quaternion
	c	<Float>	the second imaginary part of the quaternion
	d	<Float>	the third imaginary part of the quaternion

!

!Vector2 commentStamp: '<historical>' prior: 0!
I represent simple 2D coordinates in Croquet. I may be used to represent both, 2D points and 2D texture coordinates. !

!Vector3 commentStamp: '<historical>' prior: 0!
I represent simple 3D coordinates, used throughout Croquet.!

!Vector4 commentStamp: '<historical>' prior: 0!
I represent 3D points in homogenous coordinates.!

!VectorArray commentStamp: '<historical>' prior: 0!
I am an array of Vectors.!

!MatrixArray commentStamp: '<historical>' prior: 0!
I am an array of Matrices.!

!Matrix4x4Array commentStamp: '<historical>' prior: 0!
I am an array of Matrix4x4Arrays.!

!Texture2Array commentStamp: '<historical>' prior: 0!
I am an efficient representation of texture coordinates.!

!Vector2Array commentStamp: '<historical>' prior: 0!
I am an efficient representation of Vector2 elements!

!Vector3Array commentStamp: '<historical>' prior: 0!
I am an efficient representation of Vector3 elements.!

!VectorColor4Array commentStamp: 'das 5/22/2005 15:23' prior: 0!
I am an inplace storage area for Color4 items used during lighting and shading.!

!VectorRotationArray commentStamp: '<historical>' prior: 0!
I am an array of Quaternions.!

!VectorColor commentStamp: '<historical>' prior: 0!
I represent an RGBA color value in floating point format. I am used during the lighting and shading computations.!

!SequenceableCollection methodsFor: '*3DTransform' stamp: 'ar 2/7/2006 17:36'!
asVectorColor
	
	^ VectorColor new copyFrom: self.! !

!FloatArray methodsFor: '*3DTransform' stamp: 'ar 2/2/2006 20:15'!
floatAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!FloatArray methodsFor: '*3DTransform' stamp: 'ar 2/2/2006 20:15'!
floatAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !

!Color methodsFor: '*3DTransform' stamp: 'ar 2/7/2006 17:35'!
asVectorColor
	"Convert the receiver into a color"
	^VectorColor new loadFrom: self! !

!Point methodsFor: '*3DTransform' stamp: 'das 5/23/2005 10:42'!
@ aNumber
	^Vector3 x: x y: y z: aNumber! !

!CatmullRom methodsFor: 'compute' stamp: 'das 8/1/2006 16:19'!
at: t

	| t2 t3 |
	t2 := t*t.
	t3 := t2 *t.
	^ p1 + (p2 * t) + (p3 *t2) + (p4 * t3).! !

!CatmullRom methodsFor: 'initialize' stamp: 'das 8/1/2006 16:17'!
v1: v1 v2:v2 v3:v3 v4:v4

	p1 := v2.
	p2 := v3 - v1 * 0.5.
	p3 :=  (2*v1) - (5*v2) + (4*v3) - v4 * 0.5.
	p4 :=  v1 negated + (3*v2) - (3*v3) + v4 * 0.5. ! !

!CatmullRom class methodsFor: 'instance creation' stamp: 'das 8/1/2006 16:51'!
test
	
	| points pen cr |
"This generates a nice shape on the screen. "
	points := {0@0. 0@100. 100@100. 100@200. 200@200. 200@300. 300@300. 300@200. 400@200. 400@100. 500@100. 500@0.}.
	pen := Pen new.
	pen defaultNib:2.
	pen color: Color red.
	pen combinationRule: Form over.
	pen up.
	pen goto:0@100.
	pen down.
	1 to: points size - 3 do:[:i |
		cr := CatmullRom varray: (points copyFrom:i to: i+3) .
		1 to:10 do:[ :j | pen goto: (cr at: j/10.0)].
	].! !

!CatmullRom class methodsFor: 'instance creation' stamp: 'das 8/1/2006 16:31'!
v1: v1 v2: v2 v3: v3 v4:v4

	^ self new v1:v1 v2:v2 v3: v3 v4:v4.! !

!CatmullRom class methodsFor: 'instance creation' stamp: 'das 8/1/2006 16:32'!
varray: varray

	^ self new v1:(varray at:1) v2:(varray at:2) v3: (varray at:3) v4:(varray at:4).! !

!Vector methodsFor: 'as yet unclassified' stamp: 'ar 2/7/2006 16:51'!
copyFrom: array

	| sz |
	sz := self size min: array size.
	1 to: sz do:[:index | self at:index put: (array at: index)].! !

!Vector methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
floatAt: index
	"For subclasses that override #at:"
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!Vector methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
floatAt: index put: value
	"For subclasses that override #at:put:"
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	self basicAt: index put: value asIEEE32BitWord.
	^value! !

!Vector methodsFor: 'initialize'!
loadFrom: srcObject
	self == srcObject ifTrue:[^self].
	self class == srcObject class
		ifTrue:[self replaceFrom: 1 to: self size with: srcObject startingAt: 1]
		ifFalse:[self privateLoadFrom: srcObject]! !

!Vector methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:23'!
numElements
	^self class numElements! !

!Vector methodsFor: 'private' stamp: 'ar 2/1/1999 21:23'!
privateLoadFrom: srcObject
	"Load the receiver from the given source object."
	self error:'Cannot load a ', srcObject class name,' into a ', self class name.! !

!Vector methodsFor: 'private' stamp: 'ar 7/22/2006 19:39'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	| index repOff |
	<primitive: 105>
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self basicAt: index put: (replacement basicAt: repOff + index)]! !

!Vector methodsFor: 'accessing' stamp: 'ar 2/15/1999 22:10'!
wordAt: index
	<primitive: 60>
	^self primitiveFailed! !

!Vector methodsFor: 'accessing' stamp: 'ar 2/15/1999 22:10'!
wordAt: index put: value
	<primitive: 61>
	^self primitiveFailed! !

!Vector class methodsFor: 'island' stamp: 'ar 3/13/2006 17:15'!
howToPassAsArgument
	"We clone all vectors, even the array ones to point out that manipulation of those vectors outside of some context is not a goof idea. We may reassess this later."
	^#passByClone:! !

!Vector class methodsFor: 'class initialization' stamp: 'ar 3/8/2006 23:11'!
initialize
	"Vector initialize"
	DataStream initialize. "register with DataStream"! !

!Vector class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:20'!
new
	^super new: self numElements! !

!Vector class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:21'!
numElements
	^0! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 5/22/2005 14:23'!
* m2
	"Perform a 4x4 matrix multiplication."
	| result |
	result := self class new.
	self privateTransformMatrix: self with: m2 into: result.
	^result! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 5/22/2005 14:24'!
*= m2
	"Perform a 4x4 matrix multiplication with result into self."
	| result |
	result := self * m2.
	self replaceFrom: 1 to: 16 with: result startingAt: 1.
! !

!Matrix4x4 methodsFor: 'arithmetic' stamp: 'das 5/22/2005 14:24'!
+ aMatrix

	| res |
	res := self copy.
	res += aMatrix.
	^ res.! !

!Matrix4x4 methodsFor: 'arithmetic' stamp: 'das 5/22/2005 14:24'!
+= aMatrix
	"Optimized for Matrix/Matrix operations"
	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>
	^super + aMatrix! !

!Matrix4x4 methodsFor: 'arithmetic' stamp: 'das 5/22/2005 14:24'!
- aMatrix

	| res |
	res := self copy.
	res -= aMatrix.
	^ res.! !

!Matrix4x4 methodsFor: 'arithmetic' stamp: 'das 5/22/2005 14:25'!
-= aMatrix
	"Optimized for Matrix/Matrix operations"
	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>
	^super - aMatrix! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a11
	"Return the element a11"
	^self at: 1! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a11: aNumber
	"Store the element a11"
	self at: 1 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a12
	"Return the element a12"
	^self at: 2! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a12: aNumber
	"Store the element a12"
	self at: 2 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a13
	"Return the element a13"
	^self at: 3! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a13: aNumber
	"Store the element a13"
	self at: 3 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a14
	"Return the element a14"
	^self at: 4! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a14: aNumber
	"Store the element a14"
	self at: 4 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a21
	"Return the element a21"
	^self at: 5! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a21: aNumber
	"Store the element a21"
	self at: 5 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a22
	"Return the element a22"
	^self at: 6! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a22: aNumber
	"Store the element a22"
	self at: 6 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a23
	"Return the element a23"
	^self at: 7! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a23: aNumber
	"Store the element a23"
	self at: 7 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a24
	"Return the element a24"
	^self at: 8! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a24: aNumber
	"Store the element a24"
	self at: 8 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a31
	"Return the element a31"
	^self at: 9! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a31: aNumber
	"Store the element a31"
	self at: 9 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a32
	"Return the element a32"
	^self at: 10! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a32: aNumber
	"Store the element a32"
	self at: 10 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a33
	"Return the element a33"
	^self at: 11! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a33: aNumber
	"Store the element a33"
	self at: 11 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a34
	"Return the element a34"
	^self at: 12! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a34: aNumber
	"Store the element a34"
	self at: 12 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a41
	"Return the element a41"
	^self at: 13! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a41: aNumber
	"Store the element a41"
	self at: 13 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a42
	"Return the element a42"
	^self at: 14! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a42: aNumber
	"Store the element a42"
	self at: 14 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a43
	"Return the element a43"
	^self at: 15! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a43: aNumber
	"Store the element a43"
	self at: 15 put: aNumber! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a44
	"Return the element a44"
	^self at: 16! !

!Matrix4x4 methodsFor: 'element-access' stamp: 'das 5/16/2005 12:06'!
a44: aNumber
	"Store the element a44"
	self at: 16 put: aNumber! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/22/2005 15:18'!
addRotationAroundX: anAngle

	| rtrans res |
	rtrans := Matrix4x4 identity.
	rtrans rotationAroundX: anAngle.
	res := Matrix4x4 new.
	self privateTransformMatrix: self with: rtrans into: res.
	self replaceFrom: 1 to: 16 with: res startingAt: 1.
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/22/2005 15:00'!
addRotationAroundY: anAngle

	| rtrans res |
	rtrans := Matrix4x4 identity.
	rtrans rotationAroundY: anAngle.
	res := Matrix4x4 new.
	self privateTransformMatrix: self with: rtrans into: res.
	self replaceFrom: 1 to: 16 with: res startingAt: 1.
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/22/2005 15:00'!
addRotationAroundZ: anAngle

	| rtrans res |
	rtrans := Matrix4x4 identity.
	rtrans rotationAroundZ: anAngle.
	res := Matrix4x4 new.
	self privateTransformMatrix: self with: rtrans into: res.
	self replaceFrom: 1 to: 16 with: res startingAt: 1.
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
alternateRotation
	"Return the angular rotation around each axis of the matrix"

	| cp sp cy sy cr sr vAngles |

	vAngles := Vector3 new.

	((self a13) = 0) ifTrue: [ ((self a33) >= 0)  ifTrue: [ vAngles at: 2 put: 0.
													  cr := (self a11).
													  sr := (self a12).
													  cp := (self a33). ]
											 ifFalse: [ vAngles at: 2 put: (Float pi).
														cr := (self a11) negated.
														sr := (self a12) negated.
														cp := (self a33) negated. ]
							]
					ifFalse: [
								vAngles at: 2 put: (((self a13) negated) arcTan: (self a33)).
								cy := (vAngles at: 3) cos.
								sy := (vAngles at: 3) sin.
								cr := (cy * (self a11)) + (sy * (self a31)).
								sr := (cy* (self a12)) + (sy * (self a32)).
								cp := (cy * (self a33)) - (sy * (self a13)).
							].

	sp := (self a23).
 
	vAngles at: 1 put: (sp arcTan: cp).
	vAngles at: 3 put: (sr arcTan: cr).

	vAngles at: 1 put: ((vAngles at: 1) radiansToDegrees).
	vAngles at: 2 put: ((vAngles at: 2) radiansToDegrees).
	vAngles at: 3 put: ((vAngles at: 3) radiansToDegrees).

	^ vAngles.
! !

!Matrix4x4 methodsFor: 'converting' stamp: 'das 5/16/2005 12:06'!
asMatrix4x4
	^self! !

!Matrix4x4 methodsFor: 'converting' stamp: 'das 5/22/2005 12:08'!
asQuaternion
	"Convert the matrix to a quaternion"

	| x y z a a2 x2 y2 a4 |

	a2 := 0.25 * (1.0 + (self a11) + (self a22) + (self a33)).

	(a2 > 0) ifTrue: [
						a := a2 sqrt.
						a4 := 4.0 * a.
						x := ((self a32) - (self a23)) / a4.
						y := ((self a13) - (self a31)) / a4.
						z := ((self a21) - (self a12)) / a4.
					]
			ifFalse: [
						a := 0.
						x2 := -0.5 * ((self a22) + (self a33)).
						(x2 > 0) ifTrue: [
											x := x2 sqrt.
											x2 := 2 * x.
											y := (self a21) / x2.
											z := (self a31) / x2.
										]
								ifFalse: [
											x := 0.
											y2 := 0.5 * (1.0 - (self a33)).
											(y2 > 0) ifTrue: [
																y := y2 sqrt.
																y2 := 2 * y.
																z := (self a32) / y2.
															]
													ifFalse: [
																y := 0.0.
																z := 1.0.
															]
										]
					].

	^ (Quaternion a: a b: x c: y d: z).
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
at: i at: j
	^ self at: ((i - 1) * 4 + j).
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
at: i at: j put: aValue
	^ self at: ((i - 1) * 4 + j) put: aValue.
! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 9/11/2006 13:32'!
at: a up: u
" dominant at vector - up can change. See #up:at:."

	| side up at |
	side := (a cross: u) normalized negated.
	up := (side cross: a) normalized negated.
	at := a normalized.
	self a11: side x.
	self a21: side y.
	self a31: side z.
	self a12: up x.
	self a22: up y.
	self a32: up z.
	self a13: at x.
	self a23: at y.
	self a33: at z.
	self a44: 1.0.
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
column1
	"Return column 1"

	^ (Vector3 x: (self a11) y: (self a21) z: (self a31)).
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
column1: col
	"Set column 1"
	self a11: col x.
	self a21: col y.
	self a31: col z.
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
column2
	"Return column 2"

	^ (Vector3 x: (self a12) y: (self a22) z: (self a32)).
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
column2: col
	"Set column 2"

	self a12: col x.
	self a22: col y.
	self a32: col z.
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
column3
	"Return column 3"

	^ (Vector3 x: (self a13) y: (self a23) z: (self a33)).
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
column3: col
	"Set column 3"

	self a13: col x.
	self a23: col y.
	self a33: col z.
! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 5/16/2005 12:06'!
composeWith: m2
	"Perform a 4x4 matrix multiplication."
	^self composedWithLocal: m2.! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 5/16/2005 12:06'!
composeWith: m2 times: nTimes

	"Perform a 4x4 matrix exponentiation and multiplication."

	| result |
	result := self.
	nTimes negative ifTrue: [ self halt ].
	nTimes >= 2 ifTrue: [ 
			result := result composeWith: (m2 composedWithLocal: m2) times: nTimes // 2 ].
	(nTimes \\ 2) = 1 ifTrue: [ result := result composedWithLocal: m2].
	^ result
	
! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 5/22/2005 15:15'!
composedWithGlobal: aMatrix4x4
	| result |
	result := self class new.
	self privateTransformMatrix: aMatrix4x4 with: self into: result.
	^result! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 5/22/2005 14:32'!
composedWithLocal: aMatrix4x4
	| result |
	result := self class new.
	self privateTransformMatrix: self with: aMatrix4x4 into: result.
	^result! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 8/31/2006 19:54'!
counterTransposed
	"flip across the other diagonal from normal transpose. Just the 3x3 part. Do not use this unless you really know what you are doing."

	| matrix |
	matrix := self class new.
	matrix 
		a11: self a33; a12: self a23; a13: self a13; 	a14: self a14;
		a21: self a32; a22: self a22; a23: self a12; 	a24: self a24;
		a31: self a31; a32: self a21; a33: self a11; 	a34: self a34;

		a41: self a41; a42: self a42; a43: self a43; 	a44: self a44.
	^matrix! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/22/2005 09:52'!
getFrom: m2

	1 to: 16 do:[:i | self at: i put: (m2 at: i)].! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'bgf 9/12/2006 17:35'!
globalDirToLocal: aPoint
	^ self inverseTransformation localDirToGlobal: aPoint! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'bgf 9/12/2006 17:35'!
globalPointToLocal: aPoint

	"Convenience method for inverse of localPointToGlobal."
	^ self inverseTransformation localPointToGlobal: aPoint! !

!Matrix4x4 methodsFor: 'solving' stamp: 'das 5/16/2005 12:06'!
inplaceDecomposeLU
	"Decompose the receiver in place by using gaussian elimination w/o pivot search"
	| x |
	1 to: 4 do:[:j|
		"i-th equation (row)"
		j+1 to: 4 do:[:i|
			x := (self at: i at: j) / (self at: j at: j).
			j to: 4 do:[:k|
				self at: i at: k put: (self at: i at: k) - ((self at: j at: k) * x)].
			self at: i at: j put: x]].
! !

!Matrix4x4 methodsFor: 'solving' stamp: 'ar 3/26/2006 22:53'!
inplaceHouseHolderInvert
	"Solve the linear equation self * aVector = x by using HouseHolder's transformation.
	Note: This scheme is numerically better than using gaussian elimination even though it takes
	somewhat longer"
	| d x sigma beta sum s|
	<primitive:'primitiveInplaceHouseHolderInvert' module:'CroquetPlugin'>
	x := Matrix4x4 identity.
	d := Matrix4x4 new.
	1 to: 4 do:[:j|
		sigma := 0.0.
		j to: 4 do:[:i| sigma := sigma + ((self at: i at: j) squared)].
		sigma isZero ifTrue:[^nil]. "matrix is singular"
		((self at: j at: j) < 0.0) 
			ifTrue:[ s:= sigma sqrt]
			ifFalse:[ s:= sigma sqrt negated].
		1 to: 4 do:[:r| d at: j at: r put: s].
		beta := 1.0 / ( s * (self at: j at: j) - sigma).
		self at: j at: j put: ((self at: j at: j) - s).
		"update remaining columns"
		j+1 to: 4 do:[:k|
			sum := 0.0.
			j to: 4 do:[:i| sum := sum + ((self at: i at: j) * (self at: i at: k))].
			sum := sum * beta.
			j to: 4 do:[:i| 
				self at: i at: k put: ((self at: i at: k) + ((self at: i at: j) * sum))]].
		"update vector"
		1 to: 4 do:[:r|
			sum := nil.
			j to: 4 do:[:i| 
				sum := sum isNil 
					ifTrue:[(x at: i at: r) * (self at: i at: j)] 
					ifFalse:[sum + ((x at: i at: r) * (self at: i at: j))]].
			sum := sum * beta.
			j to: 4 do:[:i| 
				x at: i at: r put:((x at: i at: r) + (sum * (self at: i at: j)))].
		].
	].
	"Now calculate result"
	1 to: 4 do:[:r|
		4 to: 1 by: -1 do:[:i|
			i+1 to: 4 do:[:j|
				x at: i at: r put: ((x at: i at: r) - ((x at: j at: r) * (self at: i at: j))) ].
			x at: i at: r put: ((x at: i at: r) / (d at: i at: r))].
	].
	self loadFrom: x.
	"Return receiver"! !

!Matrix4x4 methodsFor: 'solving' stamp: 'das 5/16/2005 12:06'!
inplaceHouseHolderTransform: aVector
	"Solve the linear equation self * aVector = x by using HouseHolder's transformation.
	Note: This scheme is numerically better than using gaussian elimination even though it takes
	somewhat longer"
	| d x sigma beta sum s|
	x := Array with: aVector x with: aVector y with: aVector z with: aVector w.
	d := Array new: 4.
	1 to: 4 do:[:j|
		sigma := 0.0.
		j to: 4 do:[:i| sigma := sigma + ((self at: i at: j) squared)].
		sigma isZero ifTrue:[^nil]. "matrix is singular"
		((self at: j at: j) < 0.0) 
			ifTrue:[ s:= d at: j put: (sigma sqrt)]
			ifFalse:[ s:= d at: j put: (sigma sqrt negated)].
		beta := 1.0 / ( s * (self at: j at: j) - sigma).
		self at: j at: j put: ((self at: j at: j) - s).
		"update remaining columns"
		j+1 to: 4 do:[:k|
			sum := 0.0.
			j to: 4 do:[:i| sum := sum + ((self at: i at: j) * (self at: i at: k))].
			sum := sum * beta.
			j to: 4 do:[:i| 
				self at: i at: k put: ((self at: i at: k) + ((self at: i at: j) * sum))]].
		"update vector"
		sum := nil.
		j to: 4 do:[:i| 
			sum := sum isNil 
				ifTrue:[(x at: i) * (self at: i at: j)] 
				ifFalse:[sum + ((x at: i) * (self at: i at: j))]].
		sum := sum * beta.
		j to: 4 do:[:i| 
			x at: i put:((x at: i) + (sum * (self at: i at: j)))].
	].
	"Now calculate result"
	4 to: 1 by: -1 do:[:i|
		i+1 to: 4 do:[:j|
			x at: i put: ((x at: i) - ((x at: j) * (self at: i at: j))) ].
		x at: i put: ((x at: i) / (d at: i))].
	^Vector4 x: (x at: 1) y: (x at: 2) z: (x at: 3) w: (x at: 4)
! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'pb 7/6/2015 13:36'!
inverseTransformation
	"Return the inverse matrix of the receiver."
	^ self shallowCopy inplaceHouseHolderInvert.! !

!Matrix4x4 methodsFor: 'testing' stamp: 'das 5/22/2005 15:16'!
isIdentity
	^self = IdentityMatrix! !

!Matrix4x4 methodsFor: 'testing' stamp: 'das 5/22/2005 15:17'!
isZero
	^self = ZeroMatrix! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
localBoxToGlobal: aTBox

	^TBox min: (self localPointToGlobal: aTBox min) max: (self localPointToGlobal: aTBox max).! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'ar 3/26/2006 22:42'!
localDirToGlobal: aVector
	"Multiply direction vector with the receiver"
	| x y z rx ry rz |
	<primitive: 'primitiveTransformDirection' module: 'CroquetPlugin'>
	x := aVector x.
	y := aVector y.
	z := aVector z.

	rx := (x * self a11) + (y * self a12) + (z * self a13).
	ry := (x * self a21) + (y * self a22) + (z * self a23).
	rz := (x * self a31) + (y * self a32) + (z * self a33).

	^Vector3 x: rx y: ry z: rz! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'ar 3/26/2006 22:43'!
localPointToGlobal: aVector
	"Multiply aVector (temporarily converted to 4D) with the receiver"
	| x y z rx ry rz rw |
	<primitive: 'primitiveTransformVector3' module: 'CroquetPlugin'>

	x := aVector x.
	y := aVector y.
	z := aVector z.

	rx := (x * self a11) + (y * self a12) + (z * self a13) + self a14.
	ry := (x * self a21) + (y * self a22) + (z * self a23) + self a24.
	rz := (x * self a31) + (y * self a32) + (z * self a33) + self a34.
	rw := (x * self a41) + (y * self a42) + (z * self a43) + self a44.

	^Vector3 x:(rx/rw) y: (ry/rw) z: (rz/rw)! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
lookAt
	"Return column 3"

	^ (Vector3 x: (self a13) y: (self a23) z: (self a33)).
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
lookSide
	"Return column 1"

	^ (Vector3 x: (self a11) y: (self a21) z: (self a31)).
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
lookUp
	"Return column 2"

	^ (Vector3 x: (self a12) y: (self a22) z: (self a32)).
! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 5/16/2005 12:06'!
normalize

	self column1: (self column2 cross: self column3) normalized.
	self column2: (self column3 cross: self column1) normalized.
	self column3: (self column1 cross: self column2) normalized.! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'pb 7/6/2015 13:36'!
orientation
	"This is used to return just the orientation part of the matrix.The translation part is 0.0."
	| mat |
	mat := self shallowCopy.
	mat
		translationX: 0.0
		y: 0.0
		z: 0.0.
	^ mat.! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'pb 7/6/2015 13:36'!
orthoNormInverse
	| m x y z rx ry rz |
	<primitive: 'primitiveOrthoNormInverseMatrix' module: 'CroquetPlugin'>
	m := self shallowCopy.
	"transpose upper 3x3 matrix"
	m
		a11: self a11;
		a12: self a21;
		a13: self a31.
	m
		a21: self a12;
		a22: self a22;
		a23: self a32.
	m
		a31: self a13;
		a32: self a23;
		a33: self a33.
	"Compute inverse translation vector"
	x := self a14.
	y := self a24.
	z := self a34.
	rx := x * m a11 + (y * m a12) + (z * m a13).
	ry := x * m a21 + (y * m a22) + (z * m a23).
	rz := x * m a31 + (y * m a32) + (z * m a33).
	m
		a14: 0.0 - rx;
		a24: 0.0 - ry;
		a34: 0.0 - rz.
	^ m" Used to be:
	m := self clone.
	v := m translation.
	m translation: Vector3 zero.
	m := m transposed.
	v := (m localPointToGlobal: v) negated.
	m translation: v.
	^ m.
".! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
pitchYawRoll
	"Assume the receiver describes an orthonormal 3x3 matrix"
	| pitch yaw roll |
	pitch := self a23 negated arcSin.
	yaw := self a13 arcTan: self a33.
	roll := self a21 arcTan: self a22.
	^pitch radiansToDegrees@yaw radiansToDegrees@roll radiansToDegrees! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/22/2005 14:27'!
pitchYawRoll: ypr
	"Assume the receiver describes an orthonormal 3x3 matrix"
	| offset mx my mz |
	offset := self translation.
	mx := self class identity rotationAroundX: ypr x.
	my := self class identity rotationAroundY: ypr y.
	mz := self class identity rotationAroundZ: ypr z.
	self loadFrom: mz * my * mx.
	self translation: offset
! !

!Matrix4x4 methodsFor: 'double dispatching' stamp: 'das 5/16/2005 12:06'!
printOn: aStream
	"Print the receiver on aStream"
	1 to: 4 do:[:r|
		1 to: 4 do:[:c| 
			(self at: r-1*4+c) printOn: aStream.
			aStream nextPut: Character space].
		(r < 4) ifTrue:[aStream nextPut: Character cr]].! !

!Matrix4x4 methodsFor: 'private' stamp: 'ar 3/26/2006 22:42'!
privateTransformMatrix: m1 with: m2 into: m3
	"Perform a 4x4 matrix multiplication
		m2 * m1 = m3
	being equal to first transforming points by m2 and then by m1.
	Note that m1 may be identical to m3.
	NOTE: The primitive implementation does NOT return m3 - and so don't we!!"
	| c1 c2 c3 c4 |
	<primitive: 'primitiveTransformMatrixWithInto' module:'CroquetPlugin'>
	m2 == m3 ifTrue:[^self error:'Argument and result matrix identical'].
	c1 := ((m1 a11 * m2 a11) + (m1 a12 * m2 a21) + 
				(m1 a13 * m2 a31) + (m1 a14 * m2 a41)).
	c2 := ((m1 a11 * m2 a12) + (m1 a12 * m2 a22) + 
				(m1 a13 * m2 a32) + (m1 a14 * m2 a42)).
	c3 := ((m1 a11 * m2 a13) + (m1 a12 * m2 a23) + 
				(m1 a13 * m2 a33) + (m1 a14 * m2 a43)).
	c4 := ((m1 a11 * m2 a14) + (m1 a12 * m2 a24) + 
				(m1 a13 * m2 a34) + (m1 a14 * m2 a44)).

	m3 a11: c1; a12: c2; a13: c3; a14: c4.

	c1 := ((m1 a21 * m2 a11) + (m1 a22 * m2 a21) + 
				(m1 a23 * m2 a31) + (m1 a24 * m2 a41)).
	c2 := ((m1 a21 * m2 a12) + (m1 a22 * m2 a22) + 
				(m1 a23 * m2 a32) + (m1 a24 * m2 a42)).
	c3 := ((m1 a21 * m2 a13) + (m1 a22 * m2 a23) + 
				(m1 a23 * m2 a33) + (m1 a24 * m2 a43)).
	c4 := ((m1 a21 * m2 a14) + (m1 a22 * m2 a24) + 
				(m1 a23 * m2 a34) + (m1 a24 * m2 a44)).

	m3 a21: c1; a22: c2; a23: c3; a24: c4.

	c1 := ((m1 a31 * m2 a11) + (m1 a32 * m2 a21) + 
				(m1 a33 * m2 a31) + (m1 a34 * m2 a41)).
	c2 := ((m1 a31 * m2 a12) + (m1 a32 * m2 a22) + 
				(m1 a33 * m2 a32) + (m1 a34 * m2 a42)).
	c3 := ((m1 a31 * m2 a13) + (m1 a32 * m2 a23) + 
				(m1 a33 * m2 a33) + (m1 a34 * m2 a43)).
	c4 := ((m1 a31 * m2 a14) + (m1 a32 * m2 a24) + 
				(m1 a33 * m2 a34) + (m1 a34 * m2 a44)).

	m3 a31: c1; a32: c2; a33: c3; a34: c4.

	c1 := ((m1 a41 * m2 a11) + (m1 a42 * m2 a21) + 
				(m1 a43 * m2 a31) + (m1 a44 * m2 a41)).
	c2 := ((m1 a41 * m2 a12) + (m1 a42 * m2 a22) + 
				(m1 a43 * m2 a32) + (m1 a44 * m2 a42)).
	c3 := ((m1 a41 * m2 a13) + (m1 a42 * m2 a23) + 
				(m1 a43 * m2 a33) + (m1 a44 * m2 a43)).
	c4 := ((m1 a41 * m2 a14) + (m1 a42 * m2 a24) + 
				(m1 a43 * m2 a34) + (m1 a44 * m2 a44)).

	m3 a41: c1; a42: c2; a43: c3; a44: c4.! !

!Matrix4x4 methodsFor: 'double dispatching' stamp: 'das 5/16/2005 12:06'!
productFromMatrix4x4: matrix
	"Multiply a 4x4 matrix with the receiver."
	| result |
	result := self class new.
	result a11: ((matrix a11 * self a11) + (matrix a12 * self a21) + 
				(matrix a13 * self a31) + (matrix a14 * self a41)).
	result a12: ((matrix a11 * self a12) + (matrix a12 * self a22) + 
				(matrix a13 * self a32) + (matrix a14 * self a42)).
	result a13: ((matrix a11 * self a13) + (matrix a12 * self a23) + 
				(matrix a13 * self a33) + (matrix a14 * self a43)).
	result a14: ((matrix a11 * self a14) + (matrix a12 * self a24) + 
				(matrix a13 * self a34) + (matrix a14 * self a44)).

	result a21: ((matrix a21 * self a11) + (matrix a22 * self a21) + 
				(matrix a23 * self a31) + (matrix a24 * self a41)).
	result a22: ((matrix a21 * self a12) + (matrix a22 * self a22) + 
				(matrix a23 * self a32) + (matrix a24 * self a42)).
	result a23: ((matrix a21 * self a13) + (matrix a22 * self a23) + 
				(matrix a23 * self a33) + (matrix a24 * self a43)).
	result a24: ((matrix a21 * self a14) + (matrix a22 * self a24) + 
				(matrix a23 * self a34) + (matrix a24 * self a44)).

	result a31: ((matrix a31 * self a11) + (matrix a32 * self a21) + 
				(matrix a33 * self a31) + (matrix a34 * self a41)).
	result a32: ((matrix a31 * self a12) + (matrix a32 * self a22) + 
				(matrix a33 * self a32) + (matrix a34 * self a42)).
	result a33: ((matrix a31 * self a13) + (matrix a32 * self a23) + 
				(matrix a33 * self a33) + (matrix a34 * self a43)).
	result a34: ((matrix a31 * self a14) + (matrix a32 * self a24) + 
				(matrix a33 * self a34) + (matrix a34 * self a44)).

	result a41: ((matrix a41 * self a11) + (matrix a42 * self a21) + 
				(matrix a43 * self a31) + (matrix a44 * self a41)).
	result a42: ((matrix a41 * self a12) + (matrix a42 * self a22) + 
				(matrix a43 * self a32) + (matrix a44 * self a42)).
	result a43: ((matrix a41 * self a13) + (matrix a42 * self a23) + 
				(matrix a43 * self a33) + (matrix a44 * self a43)).
	result a44: ((matrix a41 * self a14) + (matrix a42 * self a24) + 
				(matrix a43 * self a34) + (matrix a44 * self a44)).

	^result! !

!Matrix4x4 methodsFor: 'double dispatching' stamp: 'das 5/16/2005 12:06'!
productFromVector3: aVector3
	"Multiply aVector (temporarily converted to 4D) with the receiver"
	| x y z rx ry rz rw |
	x := aVector3 x.
	y := aVector3 y.
	z := aVector3 z.

	rx := (x * self a11) + (y * self a21) + (z * self a31) + self a41.
	ry := (x * self a12) + (y * self a22) + (z * self a32) + self a42.
	rz := (x * self a13) + (y * self a23) + (z * self a33) + self a43.
	rw := (x * self a14) + (y * self a24) + (z * self a34) + self a44.

	^Vector3 x:(rx/rw) y: (ry/rw) z: (rz/rw)! !

!Matrix4x4 methodsFor: 'double dispatching' stamp: 'das 5/16/2005 12:06'!
productFromVector4: aVector4
	"Multiply aVector with the receiver"
	| x y z w rx ry rz rw |
	x := aVector4 x.
	y := aVector4 y.
	z := aVector4 z.
	w := aVector4 w.

	rx := (x * self a11) + (y * self a21) + (z * self a31) + (w * self a41).
	ry := (x * self a12) + (y * self a22) + (z * self a32) + (w * self a42).
	rz := (x * self a13) + (y * self a23) + (z * self a33) + (w * self a43).
	rw := (x * self a14) + (y * self a24) + (z * self a34) + (w * self a44).

	^Vector4 x:rx y: ry z: rz w: rw! !

!Matrix4x4 methodsFor: 'private' stamp: 'das 5/22/2005 09:01'!
ptm: m1 with: m2 into: m3
	"Perform a 4x4 matrix multiplication
		m2 * m1 = m3
	being equal to first transforming points by m2 and then by m1.
	Note that m1 may be identical to m3.
	NOTE: The primitive implementation does NOT return m3 - and so don't we!!"
	| c1 c2 c3 c4 |

	m2 == m3 ifTrue:[^self error:'Argument and result matrix identical'].
	c1 := ((m1 a11 * m2 a11) + (m1 a12 * m2 a21) + 
				(m1 a13 * m2 a31) + (m1 a14 * m2 a41)).
	c2 := ((m1 a11 * m2 a12) + (m1 a12 * m2 a22) + 
				(m1 a13 * m2 a32) + (m1 a14 * m2 a42)).
	c3 := ((m1 a11 * m2 a13) + (m1 a12 * m2 a23) + 
				(m1 a13 * m2 a33) + (m1 a14 * m2 a43)).
	c4 := ((m1 a11 * m2 a14) + (m1 a12 * m2 a24) + 
				(m1 a13 * m2 a34) + (m1 a14 * m2 a44)).

	m3 a11: c1; a12: c2; a13: c3; a14: c4.

	c1 := ((m1 a21 * m2 a11) + (m1 a22 * m2 a21) + 
				(m1 a23 * m2 a31) + (m1 a24 * m2 a41)).
	c2 := ((m1 a21 * m2 a12) + (m1 a22 * m2 a22) + 
				(m1 a23 * m2 a32) + (m1 a24 * m2 a42)).
	c3 := ((m1 a21 * m2 a13) + (m1 a22 * m2 a23) + 
				(m1 a23 * m2 a33) + (m1 a24 * m2 a43)).
	c4 := ((m1 a21 * m2 a14) + (m1 a22 * m2 a24) + 
				(m1 a23 * m2 a34) + (m1 a24 * m2 a44)).

	m3 a21: c1; a22: c2; a23: c3; a24: c4.

	c1 := ((m1 a31 * m2 a11) + (m1 a32 * m2 a21) + 
				(m1 a33 * m2 a31) + (m1 a34 * m2 a41)).
	c2 := ((m1 a31 * m2 a12) + (m1 a32 * m2 a22) + 
				(m1 a33 * m2 a32) + (m1 a34 * m2 a42)).
	c3 := ((m1 a31 * m2 a13) + (m1 a32 * m2 a23) + 
				(m1 a33 * m2 a33) + (m1 a34 * m2 a43)).
	c4 := ((m1 a31 * m2 a14) + (m1 a32 * m2 a24) + 
				(m1 a33 * m2 a34) + (m1 a34 * m2 a44)).

	m3 a31: c1; a32: c2; a33: c3; a34: c4.

	c1 := ((m1 a41 * m2 a11) + (m1 a42 * m2 a21) + 
				(m1 a43 * m2 a31) + (m1 a44 * m2 a41)).
	c2 := ((m1 a41 * m2 a12) + (m1 a42 * m2 a22) + 
				(m1 a43 * m2 a32) + (m1 a44 * m2 a42)).
	c3 := ((m1 a41 * m2 a13) + (m1 a42 * m2 a23) + 
				(m1 a43 * m2 a33) + (m1 a44 * m2 a43)).
	c4 := ((m1 a41 * m2 a14) + (m1 a42 * m2 a24) + 
				(m1 a43 * m2 a34) + (m1 a44 * m2 a44)).

	m3 a41: c1; a42: c2; a43: c3; a44: c4.! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'das 5/16/2005 12:06'!
quickTransformV3ArrayFrom: srcArray to: dstArray
	"Transform the 3 element vertices from srcArray to dstArray.
	ASSUMPTION: a41 = a42 = a43 = 0.0 and a44 = 1.0"
	| a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 x y z index |
	self flag: #b3dPrimitive.
	a11 := self a11.	a12 := self a12.	a13 := self a13.	a14 := self a14.
	a21 := self a21.	a22 := self a22.	a23 := self a23.	a24 := self a24.
	a31 := self a31.	a32 := self a32.	a33 := self a33.	a34 := self a34.
	1 to: srcArray size do:[:i|
		index := i-1*3.
		x := srcArray floatAt: index+1.
		y := srcArray floatAt: index+2.
		z := srcArray floatAt: index+3.
		dstArray floatAt: index+1 put: (a11*x) + (a12*y) + (a13*z) + a14.
		dstArray floatAt: index+2 put: (a21*x) + (a22*y) + (a23*z) + a24.
		dstArray floatAt: index+3 put: (a31*x) + (a32*y) + (a33*z) + a34.
	].
	^dstArray! !

!Matrix4x4 methodsFor: 'private' stamp: 'dsa 3/28/2008 16:13'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	| index repOff |
	<primitive: 105>
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self basicAt: index put: (replacement basicAt: repOff + index)]! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
rotation
	"Return the angular rotation around each axis of the matrix"

	| vRow1 vRow2 vRow3 vScale vShear vAngles vRowCross determinate |

	vRow1 := self row1.
	vRow2 := self row2.
	vRow3 := self row3.

	vScale := Vector3 new.
	vShear := Vector3 new.
	vAngles := Vector3 new.

	vScale at: 1 put: (vRow1 length).
	vRow1 normalize.
	vShear at: 1 put: (vRow1 dot: vRow2).
	vRow2 := vRow2 + (vRow1 * ((vShear at: 1) negated)).

	vScale at: 2 put: (vRow2 length).
	vRow2 normalize.
	vShear at: 1 put: ((vShear at: 1) / (vScale at: 2)).

	vShear at: 2 put: (vRow1 dot: vRow3).
	vRow3 := vRow3 + (vRow1 * ((vShear at: 2) negated)).

	vShear at: 3 put: (vRow2 dot: vRow3).
	vRow3 := vRow3 + (vRow2 * ((vShear at: 3) negated)).

	vScale at: 3 put: (vRow3 length).
	vRow3 normalize.

	vShear at: 2 put: ((vShear at: 2) / (vScale at: 3)).
	vShear at: 3 put: ((vShear at: 3) / (vScale at: 3)).

	vRowCross := vRow2 cross: vRow3.
	determinate := vRow1 dot: vRowCross.

	(determinate < 0.0) ifTrue: [ vRow1 := vRow1 negated.
								vRow2 := vRow2 negated.
								vRow3 := vRow3 negated.
								vScale := vScale negated. ].

	vAngles at: 2 put: ((vRow1 at: 3) negated) arcSin.

	(((vAngles at: 2) cos) ~= 0.0) 
								ifTrue: [ vAngles at: 1 put:
												((vRow2 at: 3) arcTan: (vRow3 at: 3)).
										  vAngles at: 3 put:
												((vRow1 at: 2) arcTan: (vRow1 at: 1)). ]
								ifFalse: [ vAngles at: 1 put:
												((vRow2 at: 1) arcTan: (vRow2 at: 2)).
										  vAngles at: 3 put: 0.0 ].


	vAngles at: 1 put: ((vAngles at: 1) radiansToDegrees).
	vAngles at: 2 put: ((vAngles at: 2) radiansToDegrees).
	vAngles at: 3 put: ((vAngles at: 3) radiansToDegrees).

	^ vAngles.
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
rotation: aVector
	| xRot yRot zRot cosPitch sinPitch cosYaw sinYaw cosRoll sinRoll |

	xRot := (aVector x) degreesToRadians.
	yRot := (aVector y) degreesToRadians.
	zRot := (aVector z) degreesToRadians.

	cosPitch := xRot cos.
	sinPitch := xRot sin.
	cosYaw := yRot cos.
	sinYaw := yRot sin.
	cosRoll := zRot cos.
	sinRoll := zRot sin.

	self a11: (cosRoll*cosYaw).
	self a12: (sinRoll*cosYaw).
	self a13: (sinYaw negated).

	self a21: ((cosRoll*sinYaw*sinPitch) - (sinRoll*cosPitch)).
	self a22: ((cosRoll*cosPitch) + (sinRoll*sinYaw*sinPitch)).
	self a23: (cosYaw*sinPitch).
	self a31: ((cosRoll*sinYaw*cosPitch) + (sinRoll*sinPitch)).
	self a32: ((sinRoll*sinYaw*cosPitch) - (cosRoll*sinPitch)).
	self a33: (cosYaw*cosPitch).

	^ self.
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/22/2005 12:08'!
rotation: anAngle around: aVector3
	"set up a rotation matrix around the direction aVector3"

	self loadFrom: (Quaternion angle: anAngle axis: aVector3) asMatrix4x4! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
rotation: anAngle aroundX: xValue y: yValue z: zValue
	"set up a rotation matrix around the direction x/y/z"
	^self rotation: anAngle around:(Vector3 with: xValue with: yValue with: zValue)! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
rotationAroundX: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a22: c.
	self a23: s negated.
	self a33: c.
	self a32: s.
	^self! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
rotationAroundY: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a11: c.
	self a13: s.
	self a33: c.
	self a31: s negated.
	^self! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
rotationAroundZ: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a11: c.
	self a12: s negated.
	self a22: c.
	self a21: s.
	^self! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
row1
	"Return row 1"

	^ (Vector3 x: (self a11) y: (self a12) z: (self a13)).
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
row1: row
	"Set row 1"
	self a11: row x.
	self a12: row y.
	self a13: row z.
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
row2
	"Return row 2"

	^ (Vector3 x: (self a21) y: (self a22) z: (self a23)).
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
row2: row
	"Set row 2"

	self a21: row x.
	self a22: row y.
	self a23: row z.
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
row3
	"Return row 3"

	^ (Vector3 x: (self a31) y: (self a32) z: (self a33)).
! !

!Matrix4x4 methodsFor: 'row-access' stamp: 'das 5/16/2005 12:06'!
row3: row
	"Set row 3"

	self a31: row x.
	self a32: row y.
	self a33: row z.
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
scaling: aVector

	^self scalingX: aVector x y: aVector y z: aVector z! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
scalingX: xValue y: yValue z: zValue

	self a11: self a11 * xValue.
	self a22: self a22 * yValue.
	self a33: self a33 * zValue.
	^self! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
setBSplineBase
	"Set the receiver to the BSpline base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0 / 6.0;	a12: 3.0 / 6.0;	a13: -3.0 / 6.0;	a14: 1.0 / 6.0;
		a21: 3.0 / 6.0;	a22: -6.0 / 6.0;	a23: 3.0 / 6.0;	a24: 0.0 / 6.0;
		a31: -3.0 / 6.0;	a32: 0.0 / 6.0;	a33: 3.0 / 6.0;	a34: 0.0 / 6.0;
		a41: 1.0 / 6.0;	a42: 4.0 / 6.0;	a43: 1.0 / 6.0;	a44: 0.0 / 6.0
! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
setBetaSplineBaseBias: beta1 tension: beta2
	"Set the receiver to the betaSpline base matrix 
	if beta1=1 and beta2=0 then the bSpline base matrix will be returned"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	| b12 b13 delta |
	b12 := beta1 * beta1.
	b13 := beta1 * b12.
	delta := 1.0 / (beta2 + (2.0 * b13) + 4.0 * (b12 + beta1) +2.0).
	
	self
		a11: delta * -2.0 * b13;
		a12: delta * 2.0 * (beta2 + b13 + b12 + beta1);
		a13: delta * -2.0 * (beta2 + b12 + beta1 + 1.0);
		a14: delta * 2.0;
		a21: delta * 6.0 * b13;
		a22: delta * -3.0 * (beta2 + (2.0 * (b13 + b12)));
		a23: delta * 3.0 * (beta2 + (2.0 * b12));
		a24: 0.0;
		a31: delta * -6.0 * b13;
		a32: delta * 6.0 * (b13 - beta1);
		a33: delta * 6.0 * beta1;
		a34: 0.0;
		a41: delta * 2.0 * b13;
		a42: delta * (beta2 + 4.0 * (b12 + beta1));
		a43: delta * 2.0;
		a44: 0.0
! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
setBezierBase
	"Set the receiver to the bezier base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0;		a12: 3.0;		a13: -3.0;	a14: 1.0;
		a21: 3.0;		a22: -6.0;	a23: 3.0;	a24: 0.0;
		a31: -3.0;	a32: 3.0;	a33: 0.0;	a34: 0.0;
		a41: 1.0;		a42: 0.0;	a43: 0.0;	a44: 0.0! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
setCardinalBase
	"Set the receiver to the cardinal spline base matrix - just catmull * 2"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0;		a12: 3.0;		a13: -3.0;	a14: 1.0;
		a21: 2.0;		a22: -5.0;	a23: 4.0;	a24: -1.0;
		a31: -1.0;	a32: 0.0;	a33: 1.0;		a34: 0.0;
		a41: 0.0;		a42: 2.0;	a43: 0.0;	a44: 0.0
! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
setCatmullBase
	"Set the receiver to the Catmull-Rom base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -0.5;	a12: 1.5;		a13: -1.5;	a14: 0.5;
		a21: 1.0;		a22: -2.5;	a23: 2.0;	a24: -0.5;
		a31: -0.5;	a32: 0.0;	a33: 0.5;	a34: 0.0;
		a41: 0.0;		a42: 1.0;		a43: 0.0;	a44: 0.0
! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/22/2005 15:16'!
setIdentity
	"Set the receiver to the identity matrix"
	self loadFrom: IdentityMatrix! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
setPolylineBase
	"Set the receiver to the polyline base matrix :)"
	self
		a11: 0.0;		a12: 0.0;		a13: 0.0;		a14: 0.0;
		a21: 0.0;		a22: 0.0;	a23: 0.0;	a24: 0.0;
		a31: 0.0;		a32: -1.0;	a33: 1.0;		a34: 0.0;
		a41: 0.0;		a42: 1.0;		a43: 0.0;	a44: 0.0
! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
setScale: aVector
	self 
		a11: aVector x;
		a22: aVector y;
		a33: aVector z! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
setTranslation: aVector
	self 
		a14: aVector x;
		a24: aVector y;
		a34: aVector z! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/22/2005 15:17'!
setZero
	"Set the receiver to the zero matrix"
	self loadFrom: ZeroMatrix! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2005 12:06'!
skew: vector
	"Set the skew-symetric matrix up"
	self a21: vector z.
	self a12: vector z negated.
	self a31: vector y negated.
	self a13: vector y.
	self a32: vector x.
	self a23: vector x negated.
! !

!Matrix4x4 methodsFor: 'solving' stamp: 'pb 7/6/2015 13:37'!
solve3x3: aVector
	"Solve a 3x3 system of linear equations. Assume that all the a[4,x] and a[x,4] are zero.
	NOTE: This is a hack, but it's the fastest way for now."
	| m |
	m := self shallowCopy.
	m a44: 1.
	"need this for inversion"
	m := m inplaceHouseHolderInvert.
	m ifNil: [ ^ nil ].
	^ m localDirToGlobal: aVector.! !

!Matrix4x4 methodsFor: 'solving' stamp: 'pb 7/6/2015 13:37'!
solve: aVector
	^ self shallowCopy inplaceHouseHolderTransform: aVector"or:
	^self clone inplaceDecomposeLU solveLU: aVector
	".! !

!Matrix4x4 methodsFor: 'solving' stamp: 'das 5/16/2005 12:06'!
solveLU: aVector
	"Given a decomposed matrix using gaussian elimination solve the linear equations."
	| x v |
	v := Array with: aVector x with: aVector y with: aVector z with: aVector w.
	"L first"
	1 to: 4 do:[:i| "Top to bottom"
		x := 0.0.
		1 to: i-1 do:[:j|
			"From left to right w/o diagonal element"
			x := x + ((v at: j) * (self at: i at: j))].
		"No need to divide by the diagonal element - this is always 1.0 in L"
		v at: i put: (v at: i) - x].
	"Now U"
	4 to: 1 by: -1 do:[:i| "Bottom to top"
		x := 0.0.
		4 to: i+1 by: -1 do:[:j|
			"From right to left w/o diagonal element"
			x := x + ((v at: j) * (self at: i at: j))].
		"Divide by diagonal element"
		v at: i put: (v at: i) - x / (self at: i at: i)].
	^Vector4 x: (v at: 1) y: (v at: 2) z: (v at: 3) w: (v at: 4)
! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
squaredDistanceFrom: aMatrix
	| sum |
	sum := 0.0.
	1 to: 4 do:[:i|
		1 to: 4 do:[:j|
			sum := sum + ((self at: i at: j) - (aMatrix at: i at: j)) squared]].
	^sum! !

!Matrix4x4 methodsFor: 'comparing' stamp: 'das 5/16/2005 12:06'!
squaredErrorDistanceTo: anotherMatrix
	| result temp |
	result := self - anotherMatrix.
	temp := 0.
	1 to: 4 do: [:i | 1 to: 4 do: [:j| temp := temp + ((result at: i-1*4+j) squared)]].
	^temp sqrt.! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
translation

	^(Vector3 x: self a14 y: self a24 z: self a34)! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
translation: aVector

	^self translationX: aVector x y: aVector y z: aVector z! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 7/18/2006 19:23'!
translationMatrix

	^ Matrix4x4 identity translation: self translation.! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
translationX: xValue y: yValue z: zValue

	self a14: xValue.
	self a24: yValue.
	self a34: zValue.
	^self! !

!Matrix4x4 methodsFor: 'transforming' stamp: 'ar 3/26/2006 22:43'!
transposed
	"Return a transposed copy of the receiver"
	| matrix |
	<primitive: 'primitiveTransposeMatrix' module: 'CroquetPlugin'>
	matrix := self class new.
	matrix 
		a11: self a11; a12: self a21; a13: self a31; a14: self a41;
		a21: self a12; a22: self a22; a23: self a32; a24: self a42;
		a31: self a13; a32: self a23; a33: self a33; a34: self a43;
		a41: self a14; a42: self a24; a43: self a34; a44: self a44.
	^matrix! !

!Matrix4x4 methodsFor: 'accessing' stamp: 'das 5/16/2005 12:06'!
trotation
	"Return the angular rotation around each axis of the matrix"

	| cp sp cy sy cr sr vAngles |

	vAngles := Vector3 new.

	((self a13) = 0) ifTrue: [ ((self a33) >= 0)  ifTrue: [ vAngles at: 2 put: 0.
													  cr := (self a11).
													  sr := (self a12).
													  cp := (self a33). ]
											 ifFalse: [ vAngles at: 2 put: (Float pi).
														cr := (self a11) negated.
														sr := (self a12) negated.
														cp := (self a33) negated. ]
							]
					ifFalse: [
								vAngles at: 2 put: (((self a13) negated) arcTan: (self a33)).
								cy := (vAngles at: 3) cos.
								sy := (vAngles at: 3) sin.
								cr := (cy * (self a11)) + (sy * (self a31)).
								sr := (cy* (self a12)) + (sy * (self a32)).
								cp := (cy * (self a33)) - (sy * (self a13)).
							].

	sp := (self a23).
 
	vAngles at: 1 put: (sp arcTan: cp).
	vAngles at: 3 put: (sr arcTan: cr).

	vAngles at: 1 put: ((vAngles at: 1) radiansToDegrees).
	vAngles at: 2 put: ((vAngles at: 2) radiansToDegrees).
	vAngles at: 3 put: ((vAngles at: 3) radiansToDegrees).

	^ vAngles.
! !

!Matrix4x4 methodsFor: 'initialize' stamp: 'das 9/11/2006 13:38'!
up: u at: a

" dominant up vector - at can change. See #at:up."

	| side up at |
	side := (a cross: u) normalized negated.
	at := (u cross: side) normalized negated.
	up := u normalized.
	self a11: side x.
	self a21: side y.
	self a31: side z.
	self a12: up x.
	self a22: up y.
	self a32: up z.
	self a13: at x.
	self a23: at y.
	self a33: at z.
	self a44: 1.0.
! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 9/11/2006 13:31'!
at: at up: up

" construct and orthonormal matrix from the up and at vectors."
	^ self new at: at up: up.! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2005 12:06'!
identity
	^self new setIdentity! !

!Matrix4x4 class methodsFor: 'class initialization' stamp: 'das 5/22/2005 15:17'!
initialize
	"Matrix4x4 initialize"
	ZeroMatrix := self new.
	IdentityMatrix := self new.
	IdentityMatrix a11: 1.0; a22: 1.0; a33: 1.0; a44: 1.0.! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2005 12:06'!
numElements
	^16! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/22/2005 12:08'!
rotatedBy: angle around: axis centeredAt: origin
	"Create a matrix rotating points around the given origin using the angle/axis pair"
	| xform |
	xform := self withOffset: origin negated.
	xform := xform composedWithGlobal:(Quaternion angle: angle axis: axis) asMatrix4x4.
	xform := xform composedWithGlobal: (self withOffset: origin).
	^xform! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2005 12:06'!
skew: vector

	^ self new skew: vector.! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2005 12:06'!
up: up at: at

" construct and orthonormal matrix from the up and at vectors."
	^ self new up: up at: at.! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2005 12:06'!
withOffset: amount
	^self identity setTranslation: amount! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2005 12:06'!
withRotation: angle around: axis
	^self new rotation: angle around: axis! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2005 12:06'!
withScale: amount
	^self identity setScale: amount! !

!Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2005 12:06'!
zero
	^self new! !

!Quaternion methodsFor: 'arithmetic' stamp: 'das 5/22/2005 12:08'!
* aRotation
	"Multiplying two rotations is the same as concatenating the two rotations."
	| v1 v2 v3 vv |
	v1 := self bcd * aRotation a.
	v2 := aRotation bcd * self a.
	v3 := aRotation bcd cross: self bcd.
	vv := v1 + v2 + v3.
	^Quaternion
		a: (self a * aRotation a) - (self bcd dot: aRotation bcd)
		b: vv x
		c: vv y
		d: vv z! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!
a
	^self at: 1! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!
a: aFloat
	self at: 1 put: aFloat! !

!Quaternion methodsFor: 'initialize' stamp: 'ar 2/1/1999 22:02'!
a: aValue b: bValue c: cValue d: dValue

	self a: aValue.
	self b: bValue.
	self c: cValue.
	self d: dValue.
	(aValue < 0.0) ifTrue:[self *= -1.0].
	self normalize.! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 22:04'!
angle
	^(self a arcCos * 2.0 radiansToDegrees)! !

!Quaternion methodsFor: 'accessing'!
angle: newAngle
	self angle: newAngle axis: self axis! !

!Quaternion methodsFor: 'initialize' stamp: 'das 9/6/2006 19:00'!
angle: anAngle axis: aVector3

	self radiansAngle: anAngle degreesToRadians axis: aVector3
! !

!Quaternion methodsFor: 'converting' stamp: 'das 5/23/2005 10:55'!
asMatrix4x4
	"Given a quaternion q = (a, [ b, c , d]) the rotation matrix can be calculated as
			|	1 -	2(cc+dd),		2(bc-da),		2(db+ca)	|
		m =	|		2(bc+da),	1 - 	2(bb+dd),		2(cd-ba)		|
			|		2(db-ca),		2(cd+ba),	1 -	2(bb+cc)	|
	"
	| a b c d m bb cc dd bc cd db ba ca da |
	a := self a. b := self b. c := self c. d := self d.
	bb := (b * b).	cc := (c * c).	dd := (d * d).
	bc := (b * c).	cd := (c * d).	db := (d * b).
	ba := (b * a).	ca := (c * a).	da := (d * a).
	m := self matrixClass identity.
	m 
		a11: 1.0 - (cc + dd * 2.0);a12: (bc - da * 2.0); 		a13: (db + ca * 2.0);
		a21: (bc + da * 2.0);		a22: 1.0 - (bb + dd * 2.0);a23: (cd - ba * 2.0);
		a31: (db - ca * 2.0);		a32: (cd + ba * 2.0);		a33: 1.0 - (bb + cc * 2.0).
	^m
! !

!Quaternion methodsFor: 'accessing' stamp: 'das 5/16/2005 08:47'!
axis
	| sinAngle |
	sinAngle := self a arcCos sin.
	sinAngle isZero ifTrue:[^Vector3 zero].
	^Vector3 
		x: (self b / sinAngle)
		y: (self c / sinAngle)
		z: (self d / sinAngle)! !

!Quaternion methodsFor: 'accessing'!
axis: newAxis
	self angle: self angle axis: newAxis! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!
b
	^self at: 2! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 22:00'!
b: aFloat
	self at: 2 put: aFloat! !

!Quaternion methodsFor: 'private' stamp: 'das 5/16/2005 08:47'!
bcd
	^Vector3 x: self b y: self c z: self d! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!
c
	^self at: 3! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 22:00'!
c: aFloat
	self at: 3 put: aFloat! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!
d
	^self at: 4! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 2/1/1999 22:00'!
d: aFloat
	self at: 4 put: aFloat! !

!Quaternion methodsFor: 'initialize' stamp: 'das 5/16/2005 08:47'!
from: startVector to: endVector
	"Create a rotation from startVector to endVector.  Vectors should be normalized first.  Note: doesn't work when vectors are 180 degrees to each other."
	| axis cos sin |
	startVector = endVector ifTrue: [^ self setIdentity].

	axis := startVector cross: endVector.
	cos := ((1 + (startVector dot: endVector)) / 2) sqrt.	"half-angle relation"
	sin := cos isZero 
				ifTrue: [
					"180 degree rotation"
					^ self angle: 180 axis: (Vector3 perpendicularTo: startVector)]
				ifFalse: [axis length / 2 / cos].			"double angle relation"
	axis safelyNormalize.
	self a: cos b: axis x * sin c: axis y * sin d: axis z * sin. ! !

!Quaternion methodsFor: 'interpolating' stamp: 'jsp 2/25/1999 15:57'!
interpolateTo: aRotation at: t
	"Spherical linear interpolation (slerp) from the receiver to aQuaternion"
	^self slerpTo: aRotation at: t extraSpins: 0! !

!Quaternion methodsFor: 'private' stamp: 'das 5/16/2005 09:08'!
matrixClass
	^Matrix4x4! !

!Quaternion methodsFor: 'arithmetic' stamp: 'ar 9/17/1999 12:43'!
normalize
	"Normalize the receiver. Note that the actual angle (a) determining the amount of 
	rotation is fixed, since we do not want to modify angles. This leads to:
		a^2 + b^2 + c^2 + d^2 = 1.
		b^2 + c^2 + d^2 = 1 - a^2.
	Note also that the angle (a) can not exceed 1.0 (due its creation by cosine) and
	if it is 1.0 we have exactly the unit quaternion ( 1, [ 0, 0, 0]).
	"
	| oneMinusASquared length |
	oneMinusASquared := 1.0 - (self a squared).
	(oneMinusASquared < 1.0e-10) ifTrue:[^self setIdentity].
	length := ((self b squared + self c squared + self d squared) / oneMinusASquared) sqrt.
	length = 0.0 ifTrue:[^self setIdentity].
	self b: self b / length.
	self c: self c / length.
	self d: self d / length.
! !

!Quaternion methodsFor: 'converting' stamp: 'ar 2/1/1999 22:08'!
normalized
	^self copy normalize! !

!Quaternion methodsFor: 'printing' stamp: 'ar 2/1/1999 22:09'!
printOn: aStream

	aStream 
		nextPutAll: self class name;
		nextPut:$(;
		print: self angle;
		nextPut: Character space;
		print: self axis;
		nextPut:$).! !

!Quaternion methodsFor: 'initialize' stamp: 'das 9/6/2006 19:01'!
radiansAngle: anAngle axis: aVector3

	| angle sin cos |
	angle := anAngle / 2.0.
	cos := angle cos.
	sin := angle sin.
	self a: cos b: aVector3 x * sin c: aVector3 y * sin d: aVector3 z * sin.! !

!Quaternion methodsFor: 'arithmetic' stamp: 'ar 2/8/2006 11:29'!
reversed
	"Negating a quaternion is the same as reversing the angle of rotation"
	^Quaternion
		a: self a negated
		b: self b
		c: self c
		d: self d! !

!Quaternion methodsFor: 'initialize' stamp: 'das 5/22/2005 12:10'!
setIdentity
	^self loadFrom: QuaternionIdentity! !

!Quaternion methodsFor: 'interpolating' stamp: 'ar 2/1/1999 22:08'!
slerpTo: aRotation at: t
	"Spherical linear interpolation (slerp) from the receiver to aQuaternion"
	^self slerpTo: aRotation at: t extraSpins: 0! !

!Quaternion methodsFor: 'interpolating' stamp: 'das 5/22/2005 12:08'!
slerpTo: aRotation at: t extraSpins: spin
	"Sperical Linear Interpolation (slerp).
	Calculate the new quaternion when applying slerp from the receiver (t = 0.0)
	to aRotation (t = 1.0). spin indicates the number of extra rotations to be added.
	The code shown below is from Graphics Gems III"
	| cosT alpha beta flip theta phi sinT |
	alpha := t.
	flip := false.
	"calculate the cosine of the two quaternions on the 4d sphere"
	cosT := self dot: aRotation.
	"if aQuaternion is on the opposite hemisphere reverse the direction
	(note that in quaternion space two points describe the same rotation)"
	cosT < 0.0 ifTrue:[
		flip := true.
		cosT := cosT negated].
	"If the aQuaternion is nearly the same as I am use linear interpolation"
	cosT > 0.99999 ifTrue:[
		"Linear Interpolation"
		beta := 1.0 - alpha
	] ifFalse:[
		"Spherical Interpolation"
		theta := cosT arcCos.
		phi := (spin * Float pi) + theta.
		sinT := theta sin.
		beta := (theta - (alpha * phi)) sin / sinT.
		alpha := (alpha * phi) sin / sinT].

	flip ifTrue:[alpha := alpha negated].
	^Quaternion 
		a: (alpha * aRotation a) + (beta * self a)
		b: (alpha * aRotation b) + (beta * self b)
		c: (alpha * aRotation c) + (beta * self c)
		d: (alpha * aRotation d) + (beta * self d)! !

!Quaternion methodsFor: 'accessing' stamp: 'ar 9/8/2002 16:42'!
turns
	"Answer the number of turns"
	^self angle / 360.0! !

!Quaternion methodsFor: 'initialize' stamp: 'ar 2/1/1999 22:03'!
x: xValue y: yValue z: zValue a: anAngle

	| angle sin cos |
	angle := (anAngle degreesToRadians) / 2.0.
	cos := angle cos.
	sin := angle sin.
	self a: cos b: xValue * sin c: yValue * sin d: zValue * sin! !

!Quaternion class methodsFor: 'instance creation'!
a: aValue b: bValue c: cValue d: dValue
	^self new a: aValue b: bValue c: cValue d: dValue! !

!Quaternion class methodsFor: 'instance creation'!
angle: anAngle axis: aVector3
	^self new angle: anAngle axis: aVector3! !

!Quaternion class methodsFor: 'instance creation'!
axis: aVector3 angle: anAngle
	^self angle: anAngle axis: aVector3! !

!Quaternion class methodsFor: 'instance creation'!
from: startVector to: endVector
	^self new from: startVector to: endVector! !

!Quaternion class methodsFor: 'instance creation'!
identity
	^self new setIdentity! !

!Quaternion class methodsFor: 'class initialization' stamp: 'das 5/22/2005 15:23'!
initialize
	"Quaternion initialize"
	QuaternionIdentity := self new.
	QuaternionIdentity floatAt: 1 put: 1.0.! !

!Quaternion class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:32'!
numElements
	^4! !

!Quaternion class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:32'!
radiansAngle: anAngle axis: aVector3
	^self new radiansAngle: anAngle axis: aVector3! !

!Quaternion class methodsFor: 'instance creation'!
x: xValue y: yValue z: zValue a: anAngle
	^self new x: xValue y: yValue z: zValue a: anAngle! !

!Vector2 methodsFor: 'converting' stamp: 'ar 2/13/1999 20:03'!
asPoint
	^self x @ self y! !

!Vector2 methodsFor: 'accessing' stamp: 'ar 2/6/1999 23:26'!
u
	^self floatAt: 1! !

!Vector2 methodsFor: 'accessing' stamp: 'ar 2/6/1999 23:27'!
u: aFloat
	self floatAt: 1 put: aFloat! !

!Vector2 methodsFor: 'initialize' stamp: 'ar 2/6/1999 23:30'!
u: uValue v: vValue
	self u: uValue.
	self v: vValue.! !

!Vector2 methodsFor: 'accessing' stamp: 'ar 2/6/1999 23:27'!
v
	^self floatAt: 2! !

!Vector2 methodsFor: 'accessing' stamp: 'ar 2/6/1999 23:27'!
v: aFloat
	self floatAt: 2 put: aFloat! !

!Vector2 methodsFor: 'accessing' stamp: 'ar 2/7/1999 02:58'!
x
	^self at: 1! !

!Vector2 methodsFor: 'accessing' stamp: 'ar 5/4/2000 16:00'!
x: aFloat
	self floatAt: 1 put: aFloat! !

!Vector2 methodsFor: 'initialize' stamp: 'ar 5/4/2000 15:50'!
x: uValue y: vValue
	self x: uValue.
	self y: vValue.! !

!Vector2 methodsFor: 'accessing' stamp: 'ar 2/7/1999 02:58'!
y
	^self at: 2! !

!Vector2 methodsFor: 'accessing' stamp: 'ar 5/4/2000 16:00'!
y: aFloat
	self floatAt: 2 put: aFloat! !

!Vector2 class methodsFor: 'instance creation' stamp: 'das 7/5/2006 16:39'!
from: anArray

	^ self x: (anArray at:1) y: (anArray at:2) .! !

!Vector2 class methodsFor: 'instance creation' stamp: 'ar 2/6/1999 23:31'!
numElements
	^2! !

!Vector2 class methodsFor: 'instance creation' stamp: 'ar 2/6/1999 23:31'!
u: uValue v: vValue
	^self new u: uValue v: vValue! !

!Vector2 class methodsFor: 'instance creation' stamp: 'ar 5/4/2000 15:49'!
x: uValue y: vValue
	^self new x: uValue y: vValue! !

!Vector3 methodsFor: 'vector functions' stamp: 'das 12/18/2002 00:03'!
angleTo: aVector 
	"calculate the rotation angle that rotates this vector into aVector. I return the value in terms of radians. In fact, I intend to switch everything to radians at some point. Sign is ignored - if you have a fast way to do this which retains the sign of the angle, please update this."
| s t |
	s := self normalized.
	t := aVector normalized.
	^ (s dot: t) arcCos.
! !

!Vector3 methodsFor: 'converting' stamp: 'ar 10/18/2002 16:30'!
asMoveDirection
	^self! !

!Vector3 methodsFor: 'converting' stamp: 'ar 5/23/2001 00:24'!
asPoint
	^self x @ self y! !

!Vector3 methodsFor: 'converting' stamp: 'ar 10/18/2002 16:30'!
asTurnDirection
	^self! !

!Vector3 methodsFor: 'converting' stamp: 'das 5/16/2005 08:51'!
asVector3
	^self! !

!Vector3 methodsFor: 'converting' stamp: 'das 5/16/2005 08:56'!
asVector4
	^Vector4 x: self x y: self y z: self z w: 1.0! !

!Vector3 methodsFor: 'converting' stamp: 'ar 2/7/2006 17:36'!
asVectorColor

	^ VectorColor r: self x g: self y b: self z a: 1.0.
! !

!Vector3 methodsFor: 'vector functions'!
cross: aVector 
	"calculate the cross product from the receiver with aVector"
	^self species
		x: self y * aVector z - (aVector y * self z)
		y: self z * aVector x - (aVector z * self x)
		z: self x * aVector y - (aVector x * self y)! !

!Vector3 methodsFor: 'vector functions' stamp: 'ar 2/11/2003 18:41'!
distanceTo: aVector
	^(self - aVector) length! !

!Vector3 methodsFor: 'interpolating' stamp: 'das 5/16/2005 08:47'!
interpolateTo: end at: amountDone
	"Interpolates a new vector based on the instance vector, the end state vector, and the amount already done (between 0 and 1)."

	| tX tY tZ |
	tX := self x.
	tY := self y.
	tZ := self z.

	^ (Vector3 x: (tX + (((end x) - tX) * amountDone))
				y: (tY + (((end y) - tY) * amountDone))
				z: (tZ + (((end z) - tZ) * amountDone))).
! !

!Vector3 methodsFor: 'testing' stamp: 'das 5/16/2005 08:47'!
isZero
	^self = Vector3 zero! !

!Vector3 methodsFor: 'vector functions'!
length: newLength
	self safelyNormalize *= newLength! !

!Vector3 methodsFor: 'vector functions' stamp: 'das 5/16/2005 08:47'!
max: aVector
	^Vector3 
		x: (self x max: aVector x)
		y: (self y max: aVector y)
		z: (self z max: aVector z)! !

!Vector3 methodsFor: 'vector functions' stamp: 'das 5/16/2005 08:47'!
min: aVector
	^Vector3 
		x: (self x min: aVector x)
		y: (self y min: aVector y)
		z: (self z min: aVector z)! !

!Vector3 methodsFor: 'vector functions'!
normalize
	self /= self length! !

!Vector3 methodsFor: 'vector functions' stamp: 'das 10/17/2004 08:06'!
normalized

	^self / self length! !

!Vector3 methodsFor: 'private' stamp: 'das 5/16/2005 08:47'!
privateLoadFrom: srcObject
	self x: srcObject x y: srcObject y z: srcObject z.! !

!Vector3 methodsFor: 'vector functions' stamp: 'das 5/23/2005 10:58'!
rotationTo: aVector 
	"calculate the rotation matrix that rotates this vector into aVector. From 'Real-Time Rendering' by Moller and Haines, pgs. 50-52."
| m v e h s t |
	s := self normalized.
	t := aVector normalized.
	v := s cross: t.
	e := s dot: t.
	h := (1-e)/(v dot: v).
	m := Matrix4x4 new.
	m a11: e + (h * v x * v x).
	m a12: (h * v x * v y) - v z.
	m a13: (h * v x * v z) + v y.
	m a21: (h * v x * v y) + v z.
	m a22: e + (h * v y * v y).
	m a23: (h * v y * v z) - v x.
	m a31: (h * v x * v z) - v y.
	m a32: (h * v y * v z) + v x.
	m a33: e + (h * v z * v z).
	m a44: 1.0.
	^ m
! !

!Vector3 methodsFor: 'vector functions' stamp: 'ar 2/7/1999 00:43'!
safelyNormalize
	"Safely normalize the receiver, e.g. check if the length is non-zero"
	| length |
	length := self length.
	length = 1.0 ifTrue:[^self].
	length = 0.0 ifFalse:[self /= length].! !

!Vector3 methodsFor: 'vector functions'!
safelyNormalized
	"Safely normalize the receiver, e.g. check if the length is non-zero"
	^self copy safelyNormalize! !

!Vector3 methodsFor: 'vector functions'!
squaredLength: newLength
	self length: newLength sqrt! !

!Vector3 methodsFor: 'enumerating' stamp: 'ar 5/20/2001 00:05'!
vertexPositionsDo: aBlock
	aBlock value: self! !

!Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
x
	^self at: 1! !

!Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
x: aFloat
	self at: 1 put: aFloat! !

!Vector3 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:24'!
x: x y: y z: z
	self x: x.
	self y: y.
	self z: z.! !

!Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
y
	^self at: 2! !

!Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
y: aFloat
	self at: 2 put: aFloat! !

!Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
z
	^self at: 3! !

!Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
z: aFloat
	self at: 3 put: aFloat! !

!Vector3 methodsFor: 'converting' stamp: 'das 5/16/2005 08:57'!
@ aNumber
	^Vector4 x: self x y: self y z: self z w: aNumber! !

!Vector3 class methodsFor: 'instance creation' stamp: 'das 7/5/2006 16:39'!
from: anArray

	^ self x: (anArray at:1) y: (anArray at:2) z: (anArray at:3).! !

!Vector3 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:23'!
numElements
	^3! !

!Vector3 class methodsFor: 'instance creation' stamp: 'ar 2/15/1999 02:56'!
value: aFloat
	^self x: aFloat y: aFloat z: aFloat! !

!Vector3 class methodsFor: 'instance creation'!
x: x y: y z: z
	^self new x: x y: y z: z! !

!Vector3 class methodsFor: 'instance creation'!
zero
	^self new! !

!Vector4 methodsFor: 'converting' stamp: 'das 5/16/2005 09:12'!
asVector3
	| wValue |
	wValue := self w.
	wValue = 0.0 ifTrue:[^Vector3 zero].
	^Vector3 x: self x / wValue y: self y / wValue z: self z / wValue! !

!Vector4 methodsFor: 'converting' stamp: 'das 5/16/2005 09:12'!
asVector4
	^self! !

!Vector4 methodsFor: 'converting' stamp: 'ar 2/7/2006 17:36'!
asVectorColor

	^ VectorColor r: self x g: self y b: self z a: self w.
! !

!Vector4 methodsFor: 'private' stamp: 'ar 2/6/1999 00:07'!
privateLoadFrom: srcObject
	self x: srcObject x y: srcObject y z: srcObject z w: srcObject w.! !

!Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!
w
	^self at: 4! !

!Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!
w: aFloat
	self at: 4 put: aFloat! !

!Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!
x
	^self at: 1! !

!Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!
x: aFloat
	self at: 1 put: aFloat! !

!Vector4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:17'!
x: x y: y z: z w: w
	self x: x.
	self y: y.
	self z: z.
	self w: w.! !

!Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!
y
	^self at: 2! !

!Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!
y: aFloat
	self at: 2 put: aFloat! !

!Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!
z
	^self at: 3! !

!Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!
z: aFloat
	self at: 3 put: aFloat! !

!Vector4 class methodsFor: 'instance creation' stamp: 'das 7/5/2006 16:40'!
from: anArray

	^ self x: (anArray at:1) y: (anArray at:2) z: (anArray at:3) w:(anArray at:4).! !

!Vector4 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:21'!
numElements
	^4! !

!Vector4 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:21'!
x: x y: y z: z
	^self x: x y: y z: z w: 1.0! !

!Vector4 class methodsFor: 'instance creation'!
x: x y: y z: z w: w
	^self new x: x y: y z: z w: w! !

!Vector4 class methodsFor: 'instance creation'!
zero
	^self new! !

!VectorArray methodsFor: 'accessing' stamp: 'ar 6/23/2002 01:16'!
at: index
	"Return the primitive vertex at the given index"
	| vtx |
	vtx := self contentsClass new.
	vtx replaceFrom: 1 to: vtx size with: self startingAt: index - 1 * self contentsSize + 1.
	^vtx! !

!VectorArray methodsFor: 'accessing' stamp: 'ar 6/23/2002 01:16'!
at: index put: anObject
	"Store the object at the given index in the receiver"
	| idx |
	idx := index - 1 * self contentsSize.
	self privateReplaceFrom: idx+1 to: idx + self contentsSize with: anObject startingAt: 1.
	^anObject! !

!VectorArray methodsFor: 'accessing' stamp: 'ar 2/5/1999 22:48'!
contentsClass
	^self class contentsClass! !

!VectorArray methodsFor: 'accessing' stamp: 'ar 2/5/1999 22:48'!
contentsSize
	^self contentsClass numElements! !

!VectorArray methodsFor: 'copying' stamp: 'ar 2/7/1999 19:48'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1.
	^(self species new: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start! !

!VectorArray methodsFor: 'enumerating' stamp: 'ar 2/6/1999 00:37'!
do: aBlock
	"Overridden to store the (possibly) modified argument back"
	| obj |
	1 to: self size do:[:index|
		obj := self at: index.
		aBlock value: obj.
		self at: index put: obj].! !

!VectorArray methodsFor: 'private' stamp: 'ar 2/6/1999 00:39'!
privateReplaceFrom: start to: stop with: replacement startingAt: repStart 
	<primitive: 105>
	start to: stop do:[:i|
		self basicAt: i put: (replacement at: i - start + repStart).
	].! !

!VectorArray methodsFor: 'enumerating' stamp: 'ar 2/6/1999 00:37'!
readOnlyDo: aBlock
	^super do: aBlock! !

!VectorArray methodsFor: 'private' stamp: 'ar 2/7/1999 19:46'!
replaceFrom: start to: stop with: replacement startingAt: repStart
	| max |
	max := (replacement size - repStart) min: stop-start.
	start to: start+max do:[:i|
		self at: i put: (replacement at: i - start + repStart).
	].! !

!VectorArray methodsFor: 'accessing' stamp: 'ar 2/5/1999 22:49'!
size
	"Return the number of primitive vertices that can be stored in the receiver"
	^self basicSize // self contentsSize! !

!VectorArray class methodsFor: 'instance creation' stamp: 'ar 2/5/1999 22:48'!
contentsClass
	^self subclassResponsibility! !

!VectorArray class methodsFor: 'instance creation' stamp: 'ar 2/5/1999 22:49'!
contentsSize
	^self contentsClass numElements! !

!VectorArray class methodsFor: 'island' stamp: 'ar 3/13/2006 17:16'!
howToPassAsArgument
	"This method is only here to document the fact that (for the time being) we decided to copy vector arrays as well as 'small' vectors. See the superclass' comment"
	^#passByClone:! !

!VectorArray class methodsFor: 'instance creation' stamp: 'ar 2/5/1999 22:49'!
new: n
	^super new: self contentsSize*n! !

!MatrixArray methodsFor: 'as yet unclassified' stamp: 'dsa 3/28/2008 16:06'!
size
	"Return the number of primitive vertices that can be stored in the receiver"
	^self basicSize // self contentsSize! !

!Matrix4x4Array methodsFor: 'as yet unclassified' stamp: 'dsa 3/28/2008 16:02'!
addMatrix: mat
	1 to: self size do:[:i | self at: i put: (self at: i)+mat.].! !

!Matrix4x4Array methodsFor: 'as yet unclassified' stamp: 'dsa 3/28/2008 15:59'!
contentsClass
	^Matrix4x4! !

!Matrix4x4Array methodsFor: 'as yet unclassified' stamp: 'dsa 3/28/2008 15:48'!
contentsSize
	^16! !

!Matrix4x4Array class methodsFor: 'as yet unclassified' stamp: 'dsa 3/28/2008 15:52'!
contentsClass
	^Matrix4x4! !

!Matrix4x4Array class methodsFor: 'as yet unclassified' stamp: 'dsa 3/28/2008 15:52'!
initialize
	"Matrix4x4Array initialize"
	Zero4x4Matrix := Matrix4x4 new.! !

!Texture2Array methodsFor: 'accessing' stamp: 'das 5/16/2005 08:54'!
at: index put: value
	value isPoint
		ifTrue:[super at: index put: (Vector2 u: value x v: value y)]
		ifFalse:[super at: index put: value].
	^value! !

!Texture2Array class methodsFor: 'accessing' stamp: 'das 5/16/2005 08:55'!
contentsClass
	^Vector2! !

!Vector2Array class methodsFor: 'instance creation' stamp: 'das 5/16/2005 08:55'!
contentsClass
	^Vector2! !

!Vector3Array methodsFor: 'math functions' stamp: 'das 9/8/2003 11:02'!
addVector: vec
	1 to: self size do:[:i | self at: i put: (self at: i)+vec.].! !

!Vector3Array methodsFor: 'accessing' stamp: 'pb 7/6/2015 04:27'!
at: index
	"Return the primitive vertex at the given index"
	| vtx |
	vtx := ZeroVertex shallowCopy.
	vtx
		replaceFrom: 1
		to: 3
		with: self
		startingAt: index - 1 * 3 + 1.
	^ vtx.! !

!Vector3Array methodsFor: 'accessing' stamp: 'ar 6/23/2002 16:17'!
at: index put: anObject
	"Store the object at the given index in the receiver"
	| idx |
	idx := index - 1 * 3.
	self privateReplaceFrom: idx+1 to: idx + 3 with: anObject startingAt: 1.
	^anObject! !

!Vector3Array methodsFor: 'accessing' stamp: 'das 5/16/2005 08:48'!
contentsClass
	^Vector3! !

!Vector3Array methodsFor: 'accessing' stamp: 'ar 6/23/2002 16:13'!
contentsSize
	^3! !

!Vector3Array methodsFor: 'math functions' stamp: 'das 9/7/2003 15:25'!
divideByArray: array

	1 to: self size do:[:i | self at: i put: (self at: i)/(array at: i).].! !

!Vector3Array methodsFor: 'math functions' stamp: 'das 9/7/2003 15:22'!
lengthInto: target

	1 to: self size do:[:i | target at: i put: (self at: i) length.].! !

!Vector3Array methodsFor: 'math functions' stamp: 'das 9/7/2003 15:26'!
normalize

	1 to: self size do:[:i | self at: i put: (self at: i) / (self at: i)length.].! !

!Vector3Array methodsFor: 'math functions' stamp: 'das 9/7/2003 15:25'!
scaleByArray: array

	1 to: self size do:[:i | self at: i put: (self at: i)*(array at: i).].! !

!Vector3Array methodsFor: 'math functions' stamp: 'das 9/7/2003 15:24'!
squaredLengthInto: target

	1 to: self size do:[:i | target at: i put: (self at: i) squaredLength.].! !

!Vector3Array methodsFor: 'math functions' stamp: 'das 9/8/2003 11:02'!
subtractVector: vec
	1 to: self size do:[:i | self at: i put: (self at: i)-vec.].! !

!Vector3Array class methodsFor: 'instance creation' stamp: 'das 5/16/2005 08:48'!
contentsClass
	^Vector3! !

!Vector3Array class methodsFor: 'class initialization' stamp: 'das 5/16/2005 08:48'!
initialize
	"Vector3Array initialize"
	ZeroVertex := Vector3 new.! !

!VectorColor3Array methodsFor: 'special ops' stamp: 'das 6/30/2006 07:21'!
+= aColor
	"Add the given color to all the elements in the receiver"
	| r g b |
	r := aColor red.
	g := aColor green.
	b := aColor blue.
	1 to: self basicSize by: 4 do:[:i|
		self floatAt: i put: (self floatAt: i) + r.
		self floatAt: i+1 put: (self floatAt: i+1) + g.
		self floatAt: i+2 put: (self floatAt: i+2) + b.
	].! !

!VectorColor3Array methodsFor: 'special ops' stamp: 'das 6/30/2006 07:21'!
add: aColor4 at: index
	| baseIdx |
	baseIdx := index-1*4.
	self floatAt: baseIdx+1 put: (self floatAt: baseIdx+1) + aColor4 red.
	self floatAt: baseIdx+2 put: (self floatAt: baseIdx+2) + aColor4 green.
	self floatAt: baseIdx+3 put: (self floatAt: baseIdx+3) + aColor4 blue.
! !

!VectorColor3Array methodsFor: 'special ops' stamp: 'das 6/30/2006 07:20'!
clampAllFrom: minValue to: maxValue
	"Clamp all elements in the receiver to be in the range (minValue, maxValue)"
	| value |
	1 to: self basicSize do:[:i|
		value := self floatAt: i.
		value := value min: maxValue.
		value := value max: minValue.
		self floatAt: i put: value.
	].! !

!VectorColor3Array methodsFor: 'special ops' stamp: 'das 6/30/2006 07:20'!
fillWith: anInteger
	<primitive: 145>
	self primitiveFailed! !

!VectorColor3Array class methodsFor: 'instance creation' stamp: 'das 6/30/2006 07:21'!
contentsClass
	^VectorColor3! !

!VectorColor4Array methodsFor: 'special ops' stamp: 'ar 2/4/1999 01:50'!
+= aColor
	"Add the given color to all the elements in the receiver"
	| r g b a |
	r := aColor red.
	g := aColor green.
	b := aColor blue.
	a := aColor alpha.
	1 to: self basicSize by: 4 do:[:i|
		self floatAt: i put: (self floatAt: i) + r.
		self floatAt: i+1 put: (self floatAt: i+1) + g.
		self floatAt: i+2 put: (self floatAt: i+2) + b.
		self floatAt: i+3 put: (self floatAt: i+3) + a.
	].! !

!VectorColor4Array methodsFor: 'special ops' stamp: 'das 5/22/2005 15:23'!
add: aColor4 at: index
	| baseIdx |
	baseIdx := index-1*4.
	self floatAt: baseIdx+1 put: (self floatAt: baseIdx+1) + aColor4 red.
	self floatAt: baseIdx+2 put: (self floatAt: baseIdx+2) + aColor4 green.
	self floatAt: baseIdx+3 put: (self floatAt: baseIdx+3) + aColor4 blue.
	self floatAt: baseIdx+4 put: (self floatAt: baseIdx+4) + aColor4 alpha.
! !

!VectorColor4Array methodsFor: 'special ops'!
clampAllFrom: minValue to: maxValue
	"Clamp all elements in the receiver to be in the range (minValue, maxValue)"
	| value |
	1 to: self basicSize do:[:i|
		value := self floatAt: i.
		value := value min: maxValue.
		value := value max: minValue.
		self floatAt: i put: value.
	].! !

!VectorColor4Array methodsFor: 'special ops'!
fillWith: anInteger
	<primitive: 145>
	self primitiveFailed! !

!VectorColor4Array class methodsFor: 'instance creation' stamp: 'ar 2/7/2006 17:33'!
contentsClass
	^VectorColor! !

!VectorRotationArray class methodsFor: 'instance creation' stamp: 'das 5/22/2005 12:08'!
contentsClass
	^Quaternion! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:14'!
a
	^self floatAt: 4! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:14'!
alpha
	^self floatAt: 4! !

!VectorColor methodsFor: 'accessing'!
alpha: aNumber
	self floatAt: 4 put: aNumber! !

!VectorColor methodsFor: 'converting' stamp: 'ar 5/4/2000 17:59'!
asColor
	^Color r: self red g: self green b: self blue alpha: self alpha! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:14'!
asVector3

	^Vector3 x: self r y: self g z: self b! !

!VectorColor methodsFor: 'accessing' stamp: 'ar 2/7/2006 16:17'!
asVector4

	^Vector3 x: self r y: self g z: self b w: self a.! !

!VectorColor methodsFor: 'converting' stamp: 'ar 2/7/2006 17:36'!
asVectorColor

	^self! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:13'!
b
	^self floatAt: 3! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:13'!
blue
	^self floatAt: 3! !

!VectorColor methodsFor: 'accessing'!
blue: aNumber
	self floatAt: 3 put: aNumber! !

!VectorColor methodsFor: 'converting' stamp: 'das 7/10/2006 09:58'!
copyFrom: array

	| sz |
	sz := self size min: array size.
	1 to: sz do:[:index | self at:index put: (array at: index)].
	array size = 3 ifTrue:[self at:4 put:1.0].! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:13'!
g
	^self floatAt: 2! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:13'!
green
	^self floatAt: 2! !

!VectorColor methodsFor: 'accessing'!
green: aNumber
	self floatAt: 2 put: aNumber! !

!VectorColor methodsFor: 'interpolating' stamp: 'ar 2/7/2006 17:33'!
interpolateTo: end at: amountDone
	"Return the color vector yielded by interpolating from the state of the object to the specified end state at the specified amount done"

	| newColor r g b a |
	r := self red.
	g := self green.
	b := self blue.
	a := self alpha.

	newColor := VectorColor new.
	newColor red: r + (((end red) - r) * amountDone).
	newColor green: g + (((end green) - g) * amountDone).
	newColor blue: b + (((end blue) - b) * amountDone).
	newColor alpha: a + (((end alpha) - a) * amountDone).

	^ newColor.
! !

!VectorColor methodsFor: 'testing' stamp: 'ar 2/15/1999 22:12'!
isZero
	^self alpha isZero! !

!VectorColor methodsFor: 'converting' stamp: 'ar 2/4/1999 20:21'!
pixelValue32
	^self asColor pixelWordForDepth: 32! !

!VectorColor methodsFor: 'private' stamp: 'ar 2/15/1999 22:12'!
privateLoadFrom: srcObject
	| color |
	color := srcObject asColor.
	self red: color red.
	self green: color green.
	self blue: color blue.
	self alpha: color alpha.! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:13'!
r
	^self floatAt: 1! !

!VectorColor methodsFor: 'initialize' stamp: 'ar 2/7/1999 16:21'!
r: rValue g: gValue b: bValue a: aValue
	self red: rValue.
	self green: gValue.
	self blue: bValue.
	self alpha: aValue.! !

!VectorColor methodsFor: 'accessing' stamp: 'das 11/30/2005 15:13'!
red
	^self floatAt: 1! !

!VectorColor methodsFor: 'accessing'!
red: aNumber
	self floatAt: 1 put: aNumber! !

!VectorColor class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:22'!
numElements
	^4! !

!VectorColor class methodsFor: 'instance creation' stamp: 'das 7/6/2006 19:07'!
r: rValue g: gValue b: bValue
	^self new r: rValue g: gValue b: bValue a: 1.0! !

!VectorColor class methodsFor: 'instance creation' stamp: 'ar 2/7/1999 16:21'!
r: rValue g: gValue b: bValue a: aValue
	^self new r: rValue g: gValue b: bValue a: aValue! !

!VectorColor class methodsFor: 'instance creation' stamp: 'ar 2/7/2006 17:33'!
red: r green: g blue: b alpha: a
	"Create an initialize a color vector."

	| newColor |
	newColor := VectorColor new.

	newColor red: r.
	newColor green: g.
	newColor blue: b.
	newColor alpha: a.

	^ newColor.
! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:17'!
a

	^ 1.0.! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:17'!
alpha

	^ 1.0.! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:19'!
alpha: a

 "do nothing"! !

!VectorColor3 methodsFor: 'converting' stamp: 'das 6/30/2006 07:15'!
asColor
	^Color r: self red g: self green b: self blue alpha: self alpha! !

!VectorColor3 methodsFor: 'converting' stamp: 'das 6/30/2006 07:15'!
asVector3

	^Vector3 x: self r y: self g z: self b! !

!VectorColor3 methodsFor: 'converting' stamp: 'das 6/30/2006 07:17'!
asVector4

	^Vector3 x: self r y: self g z: self b w: self a.! !

!VectorColor3 methodsFor: 'converting' stamp: 'das 6/30/2006 07:18'!
asVectorColor

	^VectorColor r: self r g: self g b: self b a:self a.! !

!VectorColor3 methodsFor: 'converting' stamp: 'das 6/30/2006 07:18'!
asVectorColor3

	^self! !

!VectorColor3 methodsFor: 'converting' stamp: 'das 6/30/2006 07:18'!
asVectorColor4

	^VectorColor r: self r g: self g b: self b a:self a.! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
b
	^self floatAt: 3! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
blue
	^self floatAt: 3! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
blue: aNumber
	self floatAt: 3 put: aNumber! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
g
	^self floatAt: 2! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
green
	^self floatAt: 2! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
green: aNumber
	self floatAt: 2 put: aNumber! !

!VectorColor3 methodsFor: 'interpolating' stamp: 'das 6/30/2006 07:15'!
interpolateTo: end at: amountDone
	"Return the color vector yielded by interpolating from the state of the object to the specified end state at the specified amount done"

	| newColor r g b a |
	r := self red.
	g := self green.
	b := self blue.
	a := self alpha.

	newColor := VectorColor new.
	newColor red: r + (((end red) - r) * amountDone).
	newColor green: g + (((end green) - g) * amountDone).
	newColor blue: b + (((end blue) - b) * amountDone).
	newColor alpha: a + (((end alpha) - a) * amountDone).

	^ newColor.
! !

!VectorColor3 methodsFor: 'testing' stamp: 'das 6/30/2006 07:15'!
isZero
	^self alpha isZero! !

!VectorColor3 methodsFor: 'converting' stamp: 'das 6/30/2006 07:15'!
pixelValue32
	^self asColor pixelWordForDepth: 32! !

!VectorColor3 methodsFor: 'private' stamp: 'das 6/30/2006 07:15'!
privateLoadFrom: srcObject
	| color |
	color := srcObject asColor.
	self red: color red.
	self green: color green.
	self blue: color blue.
	self alpha: color alpha.! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
r
	^self floatAt: 1! !

!VectorColor3 methodsFor: 'initialize' stamp: 'das 6/30/2006 07:20'!
r: rValue g: gValue b: bValue
	self red: rValue.
	self green: gValue.
	self blue: bValue.
! !

!VectorColor3 methodsFor: 'initialize' stamp: 'das 6/30/2006 07:15'!
r: rValue g: gValue b: bValue a: aValue
	self red: rValue.
	self green: gValue.
	self blue: bValue.
	self alpha: aValue.! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
red
	^self floatAt: 1! !

!VectorColor3 methodsFor: 'accessing' stamp: 'das 6/30/2006 07:15'!
red: aNumber
	self floatAt: 1 put: aNumber! !

!VectorColor3 class methodsFor: 'instance creation' stamp: 'das 6/30/2006 07:15'!
numElements
	^3! !

!VectorColor3 class methodsFor: 'instance creation' stamp: 'das 6/30/2006 07:16'!
r: rValue g: gValue b: bValue
	^self new r: rValue g: gValue b: bValue! !

!VectorColor3 class methodsFor: 'instance creation' stamp: 'das 6/30/2006 07:15'!
red: r green: g blue: b 
	"Create an initialize a color vector."

	| newColor |
	newColor := VectorColor3 new.

	newColor red: r.
	newColor green: g.
	newColor blue: b.

	^ newColor.
! !
Vector initialize!
Matrix4x4 initialize!
Quaternion initialize!
Matrix4x4Array initialize!
Vector3Array initialize!
