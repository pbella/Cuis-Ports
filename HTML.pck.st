'From Cuis 4.2 of 25 July 2013 [latest update: #2343] on 1 June 2015 at 1:09:13.644142 am'!
'Description Based on HTML-sd.2.mcz found at http://squeaksource.com/@HDNjkoaXwriIV8js/Q0l6qq8Y

- Various fixes to get working under Cuis with cr->lf changes and removed related methods.
- Moved tests to HTMLTest
- Added asXML method on HTML entity to convert a branch of HTML nodes to XML nodes (requires XML package)

Note: this is not a strict HTML parser which makes it handy for processing a lot of the invalid/malformed HTML found on the web.'!
!provides: 'HTML' 1 1!
!classDefinition: #FormInputSet category: #'HTML-Forms'!
ProtoObject subclass: #FormInputSet
	instanceVariableNames: 'inputs browser form'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'FormInputSet class' category: #'HTML-Forms'!
FormInputSet class
	instanceVariableNames: ''!

!classDefinition: #HtmlEntityInspector category: #'HTML-Parser'!
Inspector subclass: #HtmlEntityInspector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser'!
!classDefinition: 'HtmlEntityInspector class' category: #'HTML-Parser'!
HtmlEntityInspector class
	instanceVariableNames: ''!

!classDefinition: #HtmlAttributes category: #'HTML-Parser'!
Dictionary subclass: #HtmlAttributes
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser'!
!classDefinition: 'HtmlAttributes class' category: #'HTML-Parser'!
HtmlAttributes class
	instanceVariableNames: ''!

!classDefinition: #FormInput category: #'HTML-Forms'!
Object subclass: #FormInput
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'FormInput class' category: #'HTML-Forms'!
FormInput class
	instanceVariableNames: ''!

!classDefinition: #HiddenInput category: #'HTML-Forms'!
FormInput subclass: #HiddenInput
	instanceVariableNames: 'name value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'HiddenInput class' category: #'HTML-Forms'!
HiddenInput class
	instanceVariableNames: ''!

!classDefinition: #RadioButtonInput category: #'HTML-Forms'!
FormInput subclass: #RadioButtonInput
	instanceVariableNames: 'inputSet state value button'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'RadioButtonInput class' category: #'HTML-Forms'!
RadioButtonInput class
	instanceVariableNames: ''!

!classDefinition: #RadioButtonSetInput category: #'HTML-Forms'!
FormInput subclass: #RadioButtonSetInput
	instanceVariableNames: 'name buttons defaultButton'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'RadioButtonSetInput class' category: #'HTML-Forms'!
RadioButtonSetInput class
	instanceVariableNames: ''!

!classDefinition: #SelectionInput category: #'HTML-Forms'!
FormInput subclass: #SelectionInput
	instanceVariableNames: 'name defaultValue listMorph values'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'SelectionInput class' category: #'HTML-Forms'!
SelectionInput class
	instanceVariableNames: ''!

!classDefinition: #TextInput category: #'HTML-Forms'!
FormInput subclass: #TextInput
	instanceVariableNames: 'name defaultValue textMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'TextInput class' category: #'HTML-Forms'!
TextInput class
	instanceVariableNames: ''!

!classDefinition: #FileInput category: #'HTML-Forms'!
TextInput subclass: #FileInput
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'FileInput class' category: #'HTML-Forms'!
FileInput class
	instanceVariableNames: ''!

!classDefinition: #ToggleButtonInput category: #'HTML-Forms'!
FormInput subclass: #ToggleButtonInput
	instanceVariableNames: 'button name value state checkedByDefault'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Forms'!
!classDefinition: 'ToggleButtonInput class' category: #'HTML-Forms'!
ToggleButtonInput class
	instanceVariableNames: ''!

!classDefinition: #HtmlEntity category: #'HTML-Parser'!
Object subclass: #HtmlEntity
	instanceVariableNames: 'contents attribs'
	classVariableNames: 'ReverseCharacterEntities'
	poolDictionaries: ''
	category: 'HTML-Parser'!
!classDefinition: 'HtmlEntity class' category: #'HTML-Parser'!
HtmlEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlArea category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlArea
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlArea class' category: #'HTML-Parser Entities'!
HtmlArea class
	instanceVariableNames: ''!

!classDefinition: #HtmlBlockEntity category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlBlockEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlBlockEntity class' category: #'HTML-Parser Entities'!
HtmlBlockEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlBlockQuote category: #'HTML-Parser Entities'!
HtmlBlockEntity subclass: #HtmlBlockQuote
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlBlockQuote class' category: #'HTML-Parser Entities'!
HtmlBlockQuote class
	instanceVariableNames: ''!

!classDefinition: #HtmlDefinitionList category: #'HTML-Parser Entities'!
HtmlBlockEntity subclass: #HtmlDefinitionList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlDefinitionList class' category: #'HTML-Parser Entities'!
HtmlDefinitionList class
	instanceVariableNames: ''!

!classDefinition: #HtmlList category: #'HTML-Parser Entities'!
HtmlBlockEntity subclass: #HtmlList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlList class' category: #'HTML-Parser Entities'!
HtmlList class
	instanceVariableNames: ''!

!classDefinition: #HtmlOrderedList category: #'HTML-Parser Entities'!
HtmlList subclass: #HtmlOrderedList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlOrderedList class' category: #'HTML-Parser Entities'!
HtmlOrderedList class
	instanceVariableNames: ''!

!classDefinition: #HtmlUnorderedList category: #'HTML-Parser Entities'!
HtmlList subclass: #HtmlUnorderedList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlUnorderedList class' category: #'HTML-Parser Entities'!
HtmlUnorderedList class
	instanceVariableNames: ''!

!classDefinition: #HtmlBody category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlBody
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlBody class' category: #'HTML-Parser Entities'!
HtmlBody class
	instanceVariableNames: ''!

!classDefinition: #HtmlDefinitionListElement category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlDefinitionListElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlDefinitionListElement class' category: #'HTML-Parser Entities'!
HtmlDefinitionListElement class
	instanceVariableNames: ''!

!classDefinition: #HtmlDefinitionDefinition category: #'HTML-Parser Entities'!
HtmlDefinitionListElement subclass: #HtmlDefinitionDefinition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlDefinitionDefinition class' category: #'HTML-Parser Entities'!
HtmlDefinitionDefinition class
	instanceVariableNames: ''!

!classDefinition: #HtmlDefinitionTerm category: #'HTML-Parser Entities'!
HtmlDefinitionListElement subclass: #HtmlDefinitionTerm
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlDefinitionTerm class' category: #'HTML-Parser Entities'!
HtmlDefinitionTerm class
	instanceVariableNames: ''!

!classDefinition: #HtmlDocument category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlDocument
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlDocument class' category: #'HTML-Parser Entities'!
HtmlDocument class
	instanceVariableNames: ''!

!classDefinition: #HtmlForm category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlForm
	instanceVariableNames: 'formEntities'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlForm class' category: #'HTML-Parser Entities'!
HtmlForm class
	instanceVariableNames: ''!

!classDefinition: #HtmlFormEntity category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlFormEntity
	instanceVariableNames: 'form'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlFormEntity class' category: #'HTML-Parser Entities'!
HtmlFormEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlButton category: #'HTML-Parser Entities'!
HtmlFormEntity subclass: #HtmlButton
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlButton class' category: #'HTML-Parser Entities'!
HtmlButton class
	instanceVariableNames: ''!

!classDefinition: #HtmlInput category: #'HTML-Parser Entities'!
HtmlFormEntity subclass: #HtmlInput
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlInput class' category: #'HTML-Parser Entities'!
HtmlInput class
	instanceVariableNames: ''!

!classDefinition: #HtmlOption category: #'HTML-Parser Entities'!
HtmlFormEntity subclass: #HtmlOption
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlOption class' category: #'HTML-Parser Entities'!
HtmlOption class
	instanceVariableNames: ''!

!classDefinition: #HtmlOptionGroup category: #'HTML-Parser Entities'!
HtmlFormEntity subclass: #HtmlOptionGroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlOptionGroup class' category: #'HTML-Parser Entities'!
HtmlOptionGroup class
	instanceVariableNames: ''!

!classDefinition: #HtmlSelect category: #'HTML-Parser Entities'!
HtmlFormEntity subclass: #HtmlSelect
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlSelect class' category: #'HTML-Parser Entities'!
HtmlSelect class
	instanceVariableNames: ''!

!classDefinition: #HtmlTextArea category: #'HTML-Parser Entities'!
HtmlFormEntity subclass: #HtmlTextArea
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlTextArea class' category: #'HTML-Parser Entities'!
HtmlTextArea class
	instanceVariableNames: ''!

!classDefinition: #HtmlFrame category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlFrame
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlFrame class' category: #'HTML-Parser Entities'!
HtmlFrame class
	instanceVariableNames: ''!

!classDefinition: #HtmlFrameSet category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlFrameSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlFrameSet class' category: #'HTML-Parser Entities'!
HtmlFrameSet class
	instanceVariableNames: ''!

!classDefinition: #HtmlHead category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlHead
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlHead class' category: #'HTML-Parser Entities'!
HtmlHead class
	instanceVariableNames: ''!

!classDefinition: #HtmlHeadEntity category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlHeadEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlHeadEntity class' category: #'HTML-Parser Entities'!
HtmlHeadEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlMeta category: #'HTML-Parser Entities'!
HtmlHeadEntity subclass: #HtmlMeta
	instanceVariableNames: 'theTag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlMeta class' category: #'HTML-Parser Entities'!
HtmlMeta class
	instanceVariableNames: ''!

!classDefinition: #HtmlStyle category: #'HTML-Parser Entities'!
HtmlHeadEntity subclass: #HtmlStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlStyle class' category: #'HTML-Parser Entities'!
HtmlStyle class
	instanceVariableNames: ''!

!classDefinition: #HtmlTitle category: #'HTML-Parser Entities'!
HtmlHeadEntity subclass: #HtmlTitle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlTitle class' category: #'HTML-Parser Entities'!
HtmlTitle class
	instanceVariableNames: ''!

!classDefinition: #HtmlHeader category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlHeader
	instanceVariableNames: 'level'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlHeader class' category: #'HTML-Parser Entities'!
HtmlHeader class
	instanceVariableNames: ''!

!classDefinition: #HtmlHorizontalRule category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlHorizontalRule
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlHorizontalRule class' category: #'HTML-Parser Entities'!
HtmlHorizontalRule class
	instanceVariableNames: ''!

!classDefinition: #HtmlListItem category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlListItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlListItem class' category: #'HTML-Parser Entities'!
HtmlListItem class
	instanceVariableNames: ''!

!classDefinition: #HtmlMap category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlMap class' category: #'HTML-Parser Entities'!
HtmlMap class
	instanceVariableNames: ''!

!classDefinition: #HtmlParagraph category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlParagraph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlParagraph class' category: #'HTML-Parser Entities'!
HtmlParagraph class
	instanceVariableNames: ''!

!classDefinition: #HtmlPreformattedRegion category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlPreformattedRegion
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlPreformattedRegion class' category: #'HTML-Parser Entities'!
HtmlPreformattedRegion class
	instanceVariableNames: ''!

!classDefinition: #HtmlTable category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlTable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlTable class' category: #'HTML-Parser Entities'!
HtmlTable class
	instanceVariableNames: ''!

!classDefinition: #HtmlTableDataItem category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlTableDataItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlTableDataItem class' category: #'HTML-Parser Entities'!
HtmlTableDataItem class
	instanceVariableNames: ''!

!classDefinition: #HtmlTableHeader category: #'HTML-Parser Entities'!
HtmlTableDataItem subclass: #HtmlTableHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlTableHeader class' category: #'HTML-Parser Entities'!
HtmlTableHeader class
	instanceVariableNames: ''!

!classDefinition: #HtmlTableRow category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlTableRow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlTableRow class' category: #'HTML-Parser Entities'!
HtmlTableRow class
	instanceVariableNames: ''!

!classDefinition: #HtmlTextualEntity category: #'HTML-Parser Entities'!
HtmlEntity subclass: #HtmlTextualEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlTextualEntity class' category: #'HTML-Parser Entities'!
HtmlTextualEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlCommentEntity category: #'HTML-Parser Entities'!
HtmlTextualEntity subclass: #HtmlCommentEntity
	instanceVariableNames: 'commentText'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlCommentEntity class' category: #'HTML-Parser Entities'!
HtmlCommentEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlFontChangeEntity category: #'HTML-Parser Entities'!
HtmlTextualEntity subclass: #HtmlFontChangeEntity
	instanceVariableNames: 'tagName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlFontChangeEntity class' category: #'HTML-Parser Entities'!
HtmlFontChangeEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlBiggerFontEntity category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlBiggerFontEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlBiggerFontEntity class' category: #'HTML-Parser Entities'!
HtmlBiggerFontEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlBoldEntity category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlBoldEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlBoldEntity class' category: #'HTML-Parser Entities'!
HtmlBoldEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlFixedWidthEntity category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlFixedWidthEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlFixedWidthEntity class' category: #'HTML-Parser Entities'!
HtmlFixedWidthEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlFontEntity category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlFontEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlFontEntity class' category: #'HTML-Parser Entities'!
HtmlFontEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlItalicsEntity category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlItalicsEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlItalicsEntity class' category: #'HTML-Parser Entities'!
HtmlItalicsEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlSmallerFontEntity category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlSmallerFontEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlSmallerFontEntity class' category: #'HTML-Parser Entities'!
HtmlSmallerFontEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlStrikeEntity category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlStrikeEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlStrikeEntity class' category: #'HTML-Parser Entities'!
HtmlStrikeEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlSubscript category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlSubscript
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlSubscript class' category: #'HTML-Parser Entities'!
HtmlSubscript class
	instanceVariableNames: ''!

!classDefinition: #HtmlSuperscript category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlSuperscript
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlSuperscript class' category: #'HTML-Parser Entities'!
HtmlSuperscript class
	instanceVariableNames: ''!

!classDefinition: #HtmlUnderlineEntity category: #'HTML-Parser Entities'!
HtmlFontChangeEntity subclass: #HtmlUnderlineEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlUnderlineEntity class' category: #'HTML-Parser Entities'!
HtmlUnderlineEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlSpecialEntity category: #'HTML-Parser Entities'!
HtmlTextualEntity subclass: #HtmlSpecialEntity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlSpecialEntity class' category: #'HTML-Parser Entities'!
HtmlSpecialEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlAnchor category: #'HTML-Parser Entities'!
HtmlSpecialEntity subclass: #HtmlAnchor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlAnchor class' category: #'HTML-Parser Entities'!
HtmlAnchor class
	instanceVariableNames: ''!

!classDefinition: #HtmlBreak category: #'HTML-Parser Entities'!
HtmlSpecialEntity subclass: #HtmlBreak
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlBreak class' category: #'HTML-Parser Entities'!
HtmlBreak class
	instanceVariableNames: ''!

!classDefinition: #HtmlEmbedded category: #'HTML-Parser Entities'!
HtmlSpecialEntity subclass: #HtmlEmbedded
	instanceVariableNames: ''
	classVariableNames: 'ExtensionList'
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlEmbedded class' category: #'HTML-Parser Entities'!
HtmlEmbedded class
	instanceVariableNames: ''!

!classDefinition: #HtmlImage category: #'HTML-Parser Entities'!
HtmlSpecialEntity subclass: #HtmlImage
	instanceVariableNames: 'image'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlImage class' category: #'HTML-Parser Entities'!
HtmlImage class
	instanceVariableNames: ''!

!classDefinition: #HtmlNoEmbed category: #'HTML-Parser Entities'!
HtmlSpecialEntity subclass: #HtmlNoEmbed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlNoEmbed class' category: #'HTML-Parser Entities'!
HtmlNoEmbed class
	instanceVariableNames: ''!

!classDefinition: #HtmlTextEntity category: #'HTML-Parser Entities'!
HtmlTextualEntity subclass: #HtmlTextEntity
	instanceVariableNames: 'text'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser Entities'!
!classDefinition: 'HtmlTextEntity class' category: #'HTML-Parser Entities'!
HtmlTextEntity class
	instanceVariableNames: ''!

!classDefinition: #HtmlFormatter category: #'HTML-Formatter'!
Object subclass: #HtmlFormatter
	instanceVariableNames: 'browser baseUrl formDatas outputStream preformattedLevel indentLevel boldLevel italicsLevel underlineLevel strikeLevel centerLevel urlLink listLengths listTypes precedingSpaces precedingNewlines morphsToEmbed incompleteMorphs anchorLocations imageMaps'
	classVariableNames: 'CSNonSeparators CSSeparators'
	poolDictionaries: ''
	category: 'HTML-Formatter'!
!classDefinition: 'HtmlFormatter class' category: #'HTML-Formatter'!
HtmlFormatter class
	instanceVariableNames: ''!

!classDefinition: #DHtmlFormatter category: #'HTML-Formatter'!
HtmlFormatter subclass: #DHtmlFormatter
	instanceVariableNames: 'fontSpecs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Formatter'!
!classDefinition: 'DHtmlFormatter class' category: #'HTML-Formatter'!
DHtmlFormatter class
	instanceVariableNames: ''!

!classDefinition: #HtmlParser category: #'HTML-Parser'!
Object subclass: #HtmlParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Parser'!
!classDefinition: 'HtmlParser class' category: #'HTML-Parser'!
HtmlParser class
	instanceVariableNames: ''!

!classDefinition: #HtmlToken category: #'HTML-Tokenizer'!
Object subclass: #HtmlToken
	instanceVariableNames: 'source'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Tokenizer'!
!classDefinition: 'HtmlToken class' category: #'HTML-Tokenizer'!
HtmlToken class
	instanceVariableNames: ''!

!classDefinition: #HtmlComment category: #'HTML-Tokenizer'!
HtmlToken subclass: #HtmlComment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Tokenizer'!
!classDefinition: 'HtmlComment class' category: #'HTML-Tokenizer'!
HtmlComment class
	instanceVariableNames: ''!

!classDefinition: #HtmlTag category: #'HTML-Tokenizer'!
HtmlToken subclass: #HtmlTag
	instanceVariableNames: 'isNegated name attribs'
	classVariableNames: 'EntityClasses'
	poolDictionaries: ''
	category: 'HTML-Tokenizer'!
!classDefinition: 'HtmlTag class' category: #'HTML-Tokenizer'!
HtmlTag class
	instanceVariableNames: ''!

!classDefinition: #HtmlText category: #'HTML-Tokenizer'!
HtmlToken subclass: #HtmlText
	instanceVariableNames: 'text'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Tokenizer'!
!classDefinition: 'HtmlText class' category: #'HTML-Tokenizer'!
HtmlText class
	instanceVariableNames: ''!

!classDefinition: #DownloadingImageMorph category: #'HTML-Formatter'!
Morph subclass: #DownloadingImageMorph
	instanceVariableNames: 'url altText defaultExtent image downloadQueue imageMapName formatter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HTML-Formatter'!
!classDefinition: 'DownloadingImageMorph class' category: #'HTML-Formatter'!
DownloadingImageMorph class
	instanceVariableNames: ''!

!classDefinition: #HtmlTokenizer category: #'HTML-Tokenizer'!
Stream subclass: #HtmlTokenizer
	instanceVariableNames: 'inputStream text pos inTextArea textAreaLevel'
	classVariableNames: 'CSAttributeEnders CSNameEnders CSNonSeparators'
	poolDictionaries: ''
	category: 'HTML-Tokenizer'!
!classDefinition: 'HtmlTokenizer class' category: #'HTML-Tokenizer'!
HtmlTokenizer class
	instanceVariableNames: ''!


!FormInputSet commentStamp: '<historical>' prior: 0!
Holds the inputs for an instance of an HTML Form.  It has a link to the browser it will be displayed in, and it has a link to each of the input PluggableTextMorph's that it input will read from.

inputs - maps HtmlInput's into the text morphs which will input their value.!

!FormInput commentStamp: '<historical>' prior: 0!
an input instance for a form.  A form takes its input from a collection of FormInputs; each FormInput has a name and can retrieve a textual value.  WHen a form is submitted, these name-value associations are gathered together and passed to an HTTP server.!

!HiddenInput commentStamp: '<historical>' prior: 0!
a "hidden" input.  It never actually appear on a formatted page, and the user can't change the input value.!

!SelectionInput commentStamp: '<historical>' prior: 0!
allows a user to select from a number of options!

!TextInput commentStamp: '<historical>' prior: 0!
a textual input; it takes text input straight from the user via a PluggableText!

!FileInput commentStamp: '<historical>' prior: 0!
An input field for <INPUT TYPE="file">
Support for uploading files using HTTP/multipart forms
Appearance/behavior as in NS/MS browsers
(i.e., separate filename entry box and browse files button)!

!ToggleButtonInput commentStamp: '<historical>' prior: 0!
an input from a toggle button!

!HtmlEntity commentStamp: '<historical>' prior: 0!
<html>When HtmlParser runs, it generates a tree whose nodes are in HtmlEntity's subclasses.  There is a separate class for most of the available elements in HTML, though some are grouped together under generic classes like HtmlBoldEntity.

Methods of particular interest when modifying or adding subclasses are:
<ul>
<li>initialize:
<li>mayContain:
<li>addToFormatter:
</ul>
!

!HtmlBlockEntity commentStamp: '<historical>' prior: 0!
a moderately high level entitiy.  This includes P, FORM, and UL, among others!

!HtmlDocument commentStamp: '<historical>' prior: 0!
an entire HTML document.  It should have exactly two sub-entities when completed: a HEAD and a BODY!

!HtmlFormEntity commentStamp: '<historical>' prior: 0!
Abstract superclass for elements that only appear in forms!

!HtmlInput commentStamp: '<historical>' prior: 0!
result of an <input> tag!

!HtmlHeadEntity commentStamp: '<historical>' prior: 0!
abstract superclass for entities that may appear in the HEAD section!

!HtmlMeta commentStamp: '<historical>' prior: 0!
some result of a meta tag; unimplemented so far!

!HtmlTableHeader commentStamp: '<historical>' prior: 0!
a TH tag.  Currently treated the same as a TD!

!HtmlTextualEntity commentStamp: '<historical>' prior: 0!
includes raw text, font-changing entities like <b> and <em>, and special entities like <a> and <img>.  All of these are relatively low level regarding formatting; they are superceded by higher level things like <li> and <p>.  When formatted, they flow around like characters.!

!HtmlCommentEntity commentStamp: '<historical>' prior: 0!
a comment from the page!

!HtmlFontChangeEntity commentStamp: '<historical>' prior: 0!
an entity whose effect is to change the font its constituents are displayed in in some way.  Multiple tags might generate almost any of the subclasses, so the tag name is stored explicitly.!

!HtmlBiggerFontEntity commentStamp: '<historical>' prior: 0!
an entity which supposedly increases the font size of its constituents!

!HtmlBoldEntity commentStamp: '<historical>' prior: 0!
an entity which displays its contents in boldface!

!HtmlFixedWidthEntity commentStamp: '<historical>' prior: 0!
An entity that (supposedly) displays its contents in a fixed-width font.  I don't know how to do this, though.  -ls!

!HtmlFontEntity commentStamp: '<historical>' prior: 0!
the <font> tag.  it's here for future expansion....!

!HtmlItalicsEntity commentStamp: '<historical>' prior: 0!
an entity which displays its contents in italics!

!HtmlSmallerFontEntity commentStamp: '<historical>' prior: 0!
supposedly decreases the font size its constituents are displayed in!

!HtmlStrikeEntity commentStamp: '<historical>' prior: 0!
the contents should be displayed struck-through!

!HtmlSubscript commentStamp: '<historical>' prior: 0!
an entity to be displayed as a subscript!

!HtmlSuperscript commentStamp: '<historical>' prior: 0!
an entity whose contents are to be displayed as a superscript!

!HtmlFormatter commentStamp: 'ls 10/28/2003 11:56' prior: 0!
A filter which formats an HtmlDocument into a nice-looking Text suitable for a web browser such as Scamper.  See HtmlFormatter class example1 for usage information.

In principle, this formatter, along with the associated addToFormatter: methods, attempts to format any HTML which is fed to it.
!

!DHtmlFormatter commentStamp: '<historical>' prior: 0!
an attempt to improve HtmlFormatter...
make it a bit more DOMish (eventually)

roadmap
-1-	support for font specs (color, size)
-2-	support for tabless!

!HtmlParser commentStamp: '<historical>' prior: 0!
parses a stream of HtmlToken's into an HtmlDocument.  All token become an entity of some sort in the resulting document; some things are left only as comments, though.!

!HtmlComment commentStamp: '<historical>' prior: 0!
A comment, eg <!!-- this is a comment -->
Normally this is ignored, but it's included so that every byte in the
input gets put into one tag or another.!

!HtmlTag commentStamp: '<historical>' prior: 0!
An HTML tag, eg <a href="foo.html">!

!HtmlText commentStamp: '<historical>' prior: 0!
A sequence of text without any tags in it.  &sp; style characters have been replaced by their actual values. !

!DownloadingImageMorph commentStamp: '<historical>' prior: 0!
a placeholder for an image that is downloading!

!HtmlTokenizer commentStamp: '<historical>' prior: 0!
This class takes a text stream and produces a sequence of HTML tokens.

It requires its source stream to support #peek.!

!String methodsFor: '*HTML' stamp: 'sd 12/4/2010 21:45'!
replaceHtmlCharRefs

        | pos ampIndex scIndex special specialValue outString outPos newOutPos |
        outString := String new: self size.
        outPos := 0.
        pos := 1.
        
        [ pos <= self size ] whileTrue: [ 
                "read up to the next ampersand"
                ampIndex := self indexOf: $& startingAt: pos ifAbsent: [0].
                
                ampIndex = 0 ifTrue: [
                        pos = 1 ifTrue: [ ^self ] ifFalse: [ ampIndex := self size+1 ] ].

                newOutPos := outPos + ampIndex - pos.
                outString
                        replaceFrom: outPos + 1
                        to: newOutPos
                        with: self
                        startingAt: pos.
                outPos := newOutPos.
                pos := ampIndex.

                ampIndex <= self size ifTrue: [
                        "find the $;"
                        scIndex := self indexOf: $; startingAt: ampIndex ifAbsent: [ self size + 1 ].

                        special := self copyFrom: ampIndex+1 to: scIndex-1.       
                        [specialValue := HtmlEntity valueOfHtmlEntity: special]
					ifError: [specialValue := nil].

                        specialValue
                                ifNil: [
                                        "not a recognized entity.  wite it back"
								  scIndex > self size ifTrue: [ scIndex := self size ].

                                        newOutPos := outPos + scIndex - ampIndex + 1.
                                        outString
                                                replaceFrom: outPos+1
                                                to: newOutPos
                                                with: self
                                                startingAt: ampIndex.
                                        outPos := newOutPos.]
                                ifNotNil: [
                                        outPos := outPos + 1.
                                        outString at: outPos put: specialValue "isoToSqueak".].
                      
                        pos := scIndex + 1. ]. ].
        ^outString copyFrom: 1 to: outPos! !

!FormInputSet methodsFor: 'adding inputs' stamp: 'ls 8/5/1998 03:57'!
addInput: anInput
	inputs add: anInput! !

!FormInputSet methodsFor: 'private-initialization' stamp: 'ls 8/5/1998 03:57'!
form: f  browser: b
	inputs := OrderedCollection new.
	form := f.
	browser := b.! !

!FormInputSet methodsFor: 'adding inputs' stamp: 'ls 8/11/1998 03:30'!
inputs
	"return a list of the list of inputs"
	^inputs! !

!FormInputSet methodsFor: 'action' stamp: 'ls 8/5/1998 03:58'!
reset
	"reset all inputs to their default value"
	inputs do: [ :input | input reset ]! !

!FormInputSet methodsFor: 'action' stamp: 'bolot 11/3/1999 03:09'!
submit
	"collect inputs and instruct the browser to do a submission"
	| inputValues |
	inputValues := Dictionary new.

	inputs do: [ :input |
		input active ifTrue: [
			(inputValues includesKey: input name) ifFalse: [
				inputValues at: input name  put: (OrderedCollection new: 1) ].
			(inputValues at: input name)  add: input value ] ].
	browser submitFormWithInputs: inputValues url: form url
		method: form method encoding: form encoding.
	^true! !

!FormInputSet class methodsFor: 'instance creation' stamp: 'ls 7/16/1998 22:01'!
forForm: form  andBrowser: browser
	"create a FormData for the given form and browser"
	^super new form: form  browser: browser! !

!HtmlEntityInspector methodsFor: 'accessing' stamp: 'djp 7/21/1998 17:21'!
fieldList
	^ super fieldList, (Array with: 'asHtml')! !

!HtmlEntityInspector methodsFor: 'accessing' stamp: 'ls 7/24/1998 01:40'!
initialExtent
	"Answer the desired extent for the receiver when it is first opened on the screen.  "

	^ 300 @ 300! !

!HtmlEntityInspector methodsFor: 'selecting' stamp: 'sd 12/4/2010 21:36'!
selection
	^ selectionIndex = self fieldList size 
		ifTrue: [object asHtml]
		ifFalse: [super selection]! !

!HtmlAttributes methodsFor: 'printing' stamp: 'sd 12/4/2010 21:36'!
printHtmlOn: aStream
	self associationsDo: 
		[:element | 
			aStream 
				space;
				nextPutAll: element key asUppercase.
			element value ifNotNil: 
					[aStream nextPut: $=.
					aStream print: element value withoutQuoting]]! !

!FormInput methodsFor: 'input handling' stamp: 'bolot 11/3/1999 20:39'!
active
	"whether this input is currently providing an input"
	^self name isNil not! !

!FormInput methodsFor: 'testing' stamp: 'ls 8/11/1998 20:43'!
isRadioButtonSetInput
	^false! !

!FormInput methodsFor: 'input handling' stamp: 'ls 8/5/1998 06:20'!
name
	"name associated with this input"
	^self subclassResponsibility! !

!FormInput methodsFor: 'input handling' stamp: 'ls 8/5/1998 06:20'!
reset
	"reset to a default value"
	! !

!FormInput methodsFor: 'input handling' stamp: 'ls 8/5/1998 06:20'!
value
	"value associated with this input"
	^self subclassResponsibility! !

!HiddenInput methodsFor: 'input handling' stamp: 'ls 8/5/1998 06:37'!
name
	^name! !

!HiddenInput methodsFor: 'private-initialization' stamp: 'ls 8/5/1998 06:37'!
name: name0  value: value0
	name := name0.	
	value := value0.! !

!HiddenInput methodsFor: 'input handling' stamp: 'ls 8/5/1998 06:37'!
value
	^value! !

!HiddenInput class methodsFor: 'instance creation' stamp: 'ls 8/5/1998 06:38'!
name: name0  value: value
	^self new name: name0  value: value! !

!RadioButtonInput methodsFor: 'private-initialization' stamp: 'bf 11/4/1999 21:45'!
button: aMorph
	button := aMorph! !

!RadioButtonInput methodsFor: 'private-initialization' stamp: 'ls 8/11/1998 20:46'!
inputSet: anInputSet  value: aString
	inputSet := anInputSet.
	value := aString.
	state := false.! !

!RadioButtonInput methodsFor: 'button state' stamp: 'ls 8/11/1998 19:51'!
pressed
	^state! !

!RadioButtonInput methodsFor: 'button state' stamp: 'bf 11/4/1999 21:48'!
pressed: aBoolean
	state := aBoolean.
	self changed: #pressed.
	button ifNotNil: [button step].
	^true! !

!RadioButtonInput methodsFor: 'button state' stamp: 'ls 8/11/1998 20:28'!
toggle
	"my button has been clicked on!!"

	self pressed: self pressed not.
	inputSet  buttonToggled: self.
	^true! !

!RadioButtonInput methodsFor: 'access' stamp: 'ls 8/11/1998 19:52'!
valueIfPressed
	^value! !

!RadioButtonInput class methodsFor: 'instance creation' stamp: 'ls 8/11/1998 20:47'!
inputSet: anInputSet  value: aString
	^self new inputSet: anInputSet  value: aString
! !

!RadioButtonSetInput methodsFor: 'input handling' stamp: 'bolot 11/3/1999 20:40'!
active
	"we are active if and only if one of our buttons is pressed"
	self name isNil
		ifTrue: [^false].
	buttons do: [ :b |
		b pressed ifTrue: [ ^true ] ].
	^false! !

!RadioButtonSetInput methodsFor: 'access' stamp: 'ls 8/11/1998 20:48'!
addInput: buttonInput
	buttons add: buttonInput! !

!RadioButtonSetInput methodsFor: 'input handling' stamp: 'ls 8/11/1998 20:27'!
buttonToggled: aButton
	"a button was toggled; turn all other buttons off"
	buttons do: [ :b |
		b == aButton ifFalse: [
			b pressed: false  ] ].! !

!RadioButtonSetInput methodsFor: 'access' stamp: 'ls 8/11/1998 20:30'!
defaultButton: aButton
	"set which button to toggle on after a reset"
	defaultButton := aButton! !

!RadioButtonSetInput methodsFor: 'testing' stamp: 'ls 8/11/1998 20:43'!
isRadioButtonSetInput
	^true! !

!RadioButtonSetInput methodsFor: 'access' stamp: 'ls 8/11/1998 20:24'!
name
	^name! !

!RadioButtonSetInput methodsFor: 'private-initialization' stamp: 'ls 8/11/1998 20:25'!
name: aString
	name := aString.
	buttons := OrderedCollection new.! !

!RadioButtonSetInput methodsFor: 'input handling' stamp: 'ls 8/11/1998 20:30'!
reset
	buttons do: [ :b |
		b pressed: (b == defaultButton) ].
! !

!RadioButtonSetInput methodsFor: 'input handling' stamp: 'ls 8/11/1998 20:31'!
value
	buttons do: [ :b |
		b pressed ifTrue: [ ^b valueIfPressed ] ].
	self error: 'asked for value when inactive!!'.! !

!RadioButtonSetInput class methodsFor: 'instance creation' stamp: 'ls 8/11/1998 20:29'!
name: aString
	^self new name: aString! !

!SelectionInput methodsFor: 'handling input' stamp: 'bolot 11/3/1999 20:40'!
active
	^self name isNil not and: [listMorph getCurrentSelectionIndex > 0]! !

!SelectionInput methodsFor: 'handling input' stamp: 'ls 8/5/1998 07:15'!
name
	^name! !

!SelectionInput methodsFor: 'private-initialization' stamp: 'ls 8/5/1998 07:55'!
name: name0  defaultValue: defaultValue0  list: list0 values: values0
	name := name0.
	defaultValue := defaultValue0.
	listMorph := list0.
	values := values0.! !

!SelectionInput methodsFor: 'handling input' stamp: 'ls 8/5/1998 07:16'!
reset
	listMorph selection: defaultValue! !

!SelectionInput methodsFor: 'handling input' stamp: 'ls 8/11/1998 21:39'!
value
	^values at: listMorph getCurrentSelectionIndex! !

!SelectionInput class methodsFor: 'instance creation' stamp: 'ls 8/5/1998 07:55'!
name: name0  defaultValue: defaultValue  list: list  values: values
	^self new name: name0  defaultValue: defaultValue  list: list  values: values! !

!TextInput methodsFor: 'input handling' stamp: 'ls 8/5/1998 06:34'!
name
	^name! !

!TextInput methodsFor: 'private-initialization' stamp: 'ls 8/5/1998 04:01'!
name: name0  defaultValue: defaultValue0  textMorph: textMorph0
	name := name0.
	defaultValue := defaultValue0.
	textMorph := textMorph0.! !

!TextInput methodsFor: 'input handling' stamp: 'UO 6/23/2003 16:26'!
reset
	textMorph setText: defaultValue.
	"UO 6/23/2003 - We have to set the model also. setText: is not doing that"
	textMorph model contents: defaultValue! !

!TextInput methodsFor: 'input handling' stamp: 'di 3/10/1999 08:45'!
value
	textMorph hasUnacceptedEdits ifTrue: [ textMorph accept ].
	^textMorph getText asString withInternetLineEndings! !

!TextInput class methodsFor: 'instance creation' stamp: 'ls 8/5/1998 06:23'!
name: name0  defaultValue: defaultValue  textMorph: textMorph
	^self new name: name0  defaultValue: defaultValue  textMorph: textMorph
	! !

!FileInput methodsFor: 'accessing' stamp: 'bolot 11/27/1999 19:37'!
browse
	| file |
	file := (StandardFileMenu oldFileFrom: self directory) ifNil: [^nil].
	file directory isNil ifTrue: [^ nil].

	textMorph setText: (file directory pathName, FileDirectory slash, file name);
		hasUnacceptedEdits: true;
		accept! !

!FileInput methodsFor: 'accessing' stamp: 'bolot 11/27/1999 18:56'!
directory
	^FileDirectory forFileName: self filename! !

!FileInput methodsFor: 'accessing' stamp: 'bolot 11/27/1999 18:43'!
filename
	textMorph hasUnacceptedEdits ifTrue: [ textMorph accept ].
	^textMorph getText asString withInternetLineEndings! !

!FileInput methodsFor: 'accessing' stamp: 'bolot 11/27/1999 18:58'!
localFilename
	^FileDirectory localNameFor: self filename! !

!FileInput methodsFor: 'accessing' stamp: 'gk 2/10/2004 13:26'!
url
	^FileUrl pathParts: ((self directory pathParts) copyWith: self localFilename)! !

!FileInput methodsFor: 'accessing' stamp: 'bolot 11/27/1999 18:55'!
value
	^MIMEDocument contentType: (MIMEDocument guessTypeFromName: self filename)
		content: nil
		url: self url! !

!FileInput class methodsFor: 'instance creation' stamp: 'bolot 11/27/1999 18:36'!
name: aString textMorph: aTextMorph
	^self name: aString defaultValue: '' textMorph: aTextMorph! !

!ToggleButtonInput methodsFor: 'input handling' stamp: 'bolot 11/3/1999 20:40'!
active
	^self name isNil not and: [state]! !

!ToggleButtonInput methodsFor: 'private-initialize' stamp: 'bolot 11/3/1999 20:30'!
button: aButtonMorph
	button := aButtonMorph! !

!ToggleButtonInput methodsFor: 'accessing' stamp: 'bolot 11/3/1999 20:33'!
name
	^name! !

!ToggleButtonInput methodsFor: 'private-initialize' stamp: 'bolot 11/3/1999 20:30'!
name: aName value: aValue checkedByDefault: aFlag
	name := aName.
	value := aValue.
	checkedByDefault := aFlag.
	state := checkedByDefault! !

!ToggleButtonInput methodsFor: 'button state' stamp: 'bolot 11/3/1999 20:25'!
pressed
	^state! !

!ToggleButtonInput methodsFor: 'button state' stamp: 'bf 11/4/1999 21:49'!
pressed: aBoolean
	state := aBoolean.
	self changed: #pressed.
	button ifNotNil: [button step].
	^true! !

!ToggleButtonInput methodsFor: 'button state' stamp: 'bolot 11/3/1999 20:25'!
toggle
	"my button has been clicked on!!"

	self pressed: self pressed not.
	^true! !

!ToggleButtonInput methodsFor: 'accessing' stamp: 'bolot 11/3/1999 20:33'!
value
	^value! !

!ToggleButtonInput class methodsFor: 'instance creation' stamp: 'bolot 11/3/1999 20:30'!
name: aName value: aValue checkedByDefault: aFlag
	^ self new name: aName value: aValue checkedByDefault: aFlag! !

!HtmlEntity methodsFor: 'accessing' stamp: 'djp 7/21/1998 16:21'!
add: anObject
	"add an object to the receiver"
	(anObject isKindOf: String)
		ifTrue: [contents add: (HtmlTextEntity new text: anObject)]
		ifFalse: [contents add: anObject]! !

!HtmlEntity methodsFor: 'contents' stamp: 'ls 6/25/1998 02:03'!
addEntity: anEntity
	"add an entity to the receiver"
	contents add: anEntity! !

!HtmlEntity methodsFor: 'formatting' stamp: 'ls 6/25/1998 15:01'!
addToFormatter: aFormatter
	"by default, just format our childer"
	contents do: [ :e | e addToFormatter: aFormatter ]! !

!HtmlEntity methodsFor: 'enumeration' stamp: 'ls 7/16/1998 23:17'!
allSubentitiesDo: aBlock
	"perform the block recursively on all sub-entities"
	contents do: [ :e | 
		aBlock value: e .
		e allSubentitiesDo: aBlock.
	].
	! !

!HtmlEntity methodsFor: 'converting' stamp: 'djp 7/21/1998 11:58'!
asHtml
	| aStream |
	aStream := WriteStream on: ''.
	self printHtmlOn: aStream.
	^aStream contents.! !

!HtmlEntity methodsFor: 'converting' stamp: 'pb 6/4/2012 16:36'!
asXML
	| xmlNode |
	self flag: #pbfix.
	self subEntities size > 0
		ifTrue: [
			xmlNode := XMLNodeWithElements new.
			self allSubentitiesDo: [ :ea |
				xmlNode ]]
		ifFalse: [ xmlNode := XMLNode new ].
	^ xmlNode.! !

!HtmlEntity methodsFor: 'accessing' stamp: 'djp 7/21/1998 13:51'!
at: key put: anObject
	self attributes ifNil: [self attributes: (HtmlAttributes new)].
	(self attributes) at: key put: anObject! !

!HtmlEntity methodsFor: 'accessing' stamp: 'djp 7/21/1998 16:28'!
attributes
	^attribs ifNil: [attribs := HtmlAttributes new]! !

!HtmlEntity methodsFor: 'attributes' stamp: 'ls 7/26/1998 19:59'!
attributes: newAttributes
	"set all of the attributes at once.  newAttributes should not be modified after passing it in"
	^attribs := newAttributes! !

!HtmlEntity methodsFor: 'contents' stamp: 'ls 6/25/1998 02:02'!
contents
	"return an ordered collection of this entity's contents"
	^contents! !

!HtmlEntity methodsFor: 'attributes' stamp: 'ls 7/31/1998 02:55'!
doesNotUnderstand: aMessage
	"treat the message as an attribute name"
	| selector |
	selector := aMessage selector.

	selector asLowercase = selector ifFalse: [
		"attribute accesses must be in all lowercase.  This should cut down on some false doesNotUnderstand: traps"
		^super doesNotUnderstand: aMessage ].

	selector numArgs == 0 ifTrue: [
		"return the named attribute"
		^self getAttribute: selector asString default: nil ].


	selector numArgs == 1 ifTrue: [
		"set the named attribute"
		self setAttribute: (selector asString copyFrom: 1 to: (selector size-1)) to: aMessage argument.
		^self ].

	^super doesNotUnderstand: aMessage! !

!HtmlEntity methodsFor: 'downloading' stamp: 'ls 7/29/1998 00:52'!
downloadState: baseUrl
	"download any state needed for full rendering.  eg, images need this"
	! !

!HtmlEntity methodsFor: 'attributes' stamp: 'ls 8/5/1998 07:20'!
getAttribute: name
	^self getAttribute: name  default: nil! !

!HtmlEntity methodsFor: 'attributes' stamp: 'ls 7/15/1998 18:52'!
getAttribute: name  default: anObject
	^self getAttribute: name  ifAbsent: [anObject]! !

!HtmlEntity methodsFor: 'attributes' stamp: 'ls 7/15/1998 18:51'!
getAttribute: name  ifAbsent: aBlock
	^attribs at: name ifAbsent: aBlock! !

!HtmlEntity methodsFor: 'private-initialization' stamp: 'ls 7/31/1998 02:57'!
initialize
	contents := OrderedCollection new.
	attribs := HtmlAttributes new.! !

!HtmlEntity methodsFor: 'private-initialization' stamp: 'djp 7/20/1998 20:52'!
initialize: aTag
	self initialize.
	attribs := HtmlAttributes newFrom: aTag attribs.! !

!HtmlEntity methodsFor: 'user interface' stamp: 'pb 6/4/2012 16:37'!
inspect
	"Open an HtmlEntityInspector on the receiver.
	Use basicInspect to get a normal type of inspector."
	"
	HtmlEntityInspector openOn: self withEvalPane: true
	"
	HtmlEntityInspector openOn: self.! !

!HtmlEntity methodsFor: 'user interface' stamp: 'djp 7/21/1998 18:35'!
inspectWithLabel: aLabel
	"Open a HtmlEntityInspector on the receiver. Use basicInspect to get a normal (less useful) type of inspector."

	HtmlEntityInspector openOn: self withEvalPane: true withLabel: aLabel! !

!HtmlEntity methodsFor: 'testing' stamp: 'bolot 11/4/1999 04:42'!
isArea
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 03:02'!
isBlockEntity
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 16:30'!
isComment
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 02:20'!
isDefinitionListElement
	"whether receiver can appear in a DefinitionList"
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/16/1998 23:18'!
isFormEntity
	^false! !

!HtmlEntity methodsFor: 'categorization' stamp: 'ls 7/29/1998 00:26'!
isFrame
	^false! !

!HtmlEntity methodsFor: 'categorization' stamp: 'ls 7/29/1998 00:26'!
isFrameSet
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 02:54'!
isHeadElement
	"whether this can appear in a header"
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 12:55'!
isHeader
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 02:18'!
isListElement
	"is this an HtmlListElement, ie can it appear in a (non-definition) list?"
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 03:12'!
isListItem
	"is this an HtmlListItem, ie can it appear in a (non-definition) list?"
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/21/1998 07:33'!
isOption
	"whether this is an <option> entity"
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 03:03'!
isParagraph
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 12:54'!
isTableDataItem
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 12:53'!
isTableItem
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 12:53'!
isTableRow
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 02:20'!
isTextualEntity
	"is receiver either text, or some low-level text-like entity such as <b> or <em>"
	^false! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/28/1998 20:09'!
lint
	^String streamContents: [ :s | self lint: s ]! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/28/1998 20:37'!
lint: aStream
	"do a lint check, reporting to aStream"
	self lintAttributes: aStream.

	contents do: [ :c |
		(c isComment not   and:  [ (self shouldContain: c) not ]) ifTrue: [ 
			aStream nextPutAll: '<', self tagName, '> should not contain <', c tagName, '>'.
			aStream cr. ] ].

	contents do: [ :c  | c lint: aStream ]! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/28/1998 20:07'!
lintAttributes: aStream
	"check that our attributes are okay.  Print any anomalies to aStream"
	! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 02:03'!
mayContain: anEntity
	"whether we can contain the given entity"
	^self subclassResponsibility! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 02:01'!
mayContainEntity: anEntity
	"whether we can contain the given entity"
	^self subclassResponsibility! !

!HtmlEntity methodsFor: 'attributes' stamp: 'ls 7/17/1998 19:04'!
name
	^self getAttribute: 'name' default: nil! !

!HtmlEntity methodsFor: 'formatting' stamp: 'ls 7/15/1998 18:31'!
parsingFinished
	"some entities need to make a final pass *after* parsing has finished and all the contents of each entity have been established; here is a place to do that"
	contents do: [ :e | e parsingFinished ].! !

!HtmlEntity methodsFor: 'printing' stamp: 'djp 7/20/1998 21:12'!
printHtmlOn: aStream
	^self printHtmlOn: aStream  indent: 0! !

!HtmlEntity methodsFor: 'printing' stamp: 'ls 10/30/1998 10:08'!
printHtmlOn: aStream indent: indent 
	aStream next: indent put: $ ;
	 nextPutAll: '<';
	 nextPutAll: self tagName.
	self attributes associationsDo: [:assoc | aStream space; nextPutAll: assoc key; nextPutAll: '="'; nextPutAll: assoc value; nextPutAll: '"'].
	aStream nextPut: $>;
	 cr.
	contents do: [:entity | entity printHtmlOn: aStream indent: indent + 1].
	aStream nextPutAll: '</'; nextPutAll: self tagName; nextPutAll: '>'.! !

!HtmlEntity methodsFor: 'printing' stamp: 'ls 6/25/1998 03:13'!
printOn: aStream
	^self printOn: aStream  indent: 0! !

!HtmlEntity methodsFor: 'printing' stamp: 'pb 6/4/2012 16:37'!
printOn: aStream indent: indent
	aStream
		
		next: indent
		put: $ ;
		 nextPut: $<;
		 print: self tagName.
	self attributes associationsDo: [ :assoc |
		aStream
			 space;
			 nextPutAll: assoc key;
			 nextPutAll: '=';
			 nextPutAll: assoc value ].
	aStream
		 nextPut: $>;
		 newLine.
	contents do: [ :entity |
		entity
			printOn: aStream
			indent: indent + 1 ].! !

!HtmlEntity methodsFor: 'contents' stamp: 'ls 10/26/1998 17:24'!
removeEntity: anEntity 
	"remove the specified entity"
	contents remove: anEntity! !

!HtmlEntity methodsFor: 'attributes' stamp: 'ls 7/31/1998 03:00'!
setAttribute: name  to: value
	"set the given attribute to the given value"
	attribs at: name asLowercase  put: value! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 7/28/1998 20:35'!
shouldContain: anEntity
	"whether, according to the HTML DTD, this element should actually contain anEntity.  Used for checking the quality of a pages HTML"
	^self mayContain: anEntity! !

!HtmlEntity methodsFor: 'contents' stamp: 'ls 10/26/1998 17:25'!
subEntities
	"return an ordered collection of this entity's contents"
	^ contents! !

!HtmlEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 02:02'!
tagName
	"tag name for ourself"
	^self subclassResponsibility! !

!HtmlEntity methodsFor: 'contents' stamp: 'ls 7/5/1998 00:37'!
textualContents
	"return a string with the concatenated contents of all textual sub-entities"
	^String streamContents: [ :s |
		contents do: [ :e | s nextPutAll: e textualContents ] ]! !

!HtmlEntity class methodsFor: 'character entities' stamp: 'rk 7/7/2004 09:19'!
convertToNumber: aString
	"HtmlEntity convertToNumber: '25'"
	"HtmlEntity convertToNumber: 'xb7'"
	"HtmlEntity convertToNumber: 'o10'"
	| str ch |
	str := ReadStream on: aString asUppercase.
	ch := str peek.
	ch = $X ifTrue: [ str next. ^Number readFrom: str base: 16 ].
	ch = $O ifTrue: [ str next. ^Number readFrom: str base: 8 ].
	ch = $B ifTrue: [ str next. ^Number readFrom: str base: 2 ].
	^Number readFrom: str! !

!HtmlEntity class methodsFor: 'instance creation' stamp: 'ls 7/29/1998 01:27'!
forTag: aTag
	"create a new entity based on the given tag"
	^self new initialize: aTag! !

!HtmlEntity class methodsFor: 'class initialization' stamp: 'sd 12/4/2010 21:33'!
initialize
	"HtmlEntity initialize"

	ReverseCharacterEntities := Dictionary new: 128.
	#('quot' $" 'lt' $< 'amp' $& 'gt' $> 'rsquo' $' 'lsquo' $` 'rdquo' $" 'ldquo' $" ) pairsDo:
		[:s :c | ReverseCharacterEntities at: s put: c asciiValue].
	#('nbsp' 'iexcl' 'cent' 'pound' 'curren' 'yen' 'brvbar' 'sect' 'uml' 'copy' 'ordf' 'laquo' 'not' 'shy' 'reg' 'hibar' 'deg' 'plusmn' 'sup2' 'sup3' 'acute' 'micro' 'para' 'middot' 'cedil' 'sup1' 'ordm' 'raquo' 'frac14' 'frac12' 'frac34' 'iquest' 'Agrave' 'Aacute' 'Acirc' 'Atilde' 'Auml' 'Aring' 'AElig' 'Ccedil' 'Egrave' 'Eacute' 'Ecirc' 'Euml' 'Igrave' 'Iacute' 'Icirc' 'Iuml' 'ETH' 'Ntilde' 'Ograve' 'Oacute' 'Ocirc' 'Otilde' 'Ouml' 'times' 'Oslash' 'Ugrave' 'Uacute' 'Ucirc' 'Uuml' 'Yacute' 'THORN' 'szlig' 'agrave' 'aacute' 'acirc' 'atilde' 'auml' 'aring' 'aelig' 'ccedil' 'egrave' 'eacute' 'ecirc' 'euml' 'igrave' 'iacute' 'icirc' 'iuml' 'eth' 'ntilde' 'ograve' 'oacute' 'ocirc' 'otilde' 'ouml' 'divide' 'oslash' 'ugrave' 'uacute' 'ucirc' 'uuml' 'yacute' 'thorn' 'yuml' ) withIndexDo:
		[:s :i | ReverseCharacterEntities at: s put: i - 1 + 160].! !

!HtmlEntity class methodsFor: 'character entities' stamp: 'sd 12/4/2010 21:33'!
valueOfHtmlEntity: specialEntity
	"Return the character equivalent to the HTML entity."

	| value |
	(specialEntity beginsWith: '#')		"Handle numeric entities"
		ifTrue: [
			"NB: We can display only simple numeric special entities in the"
			"range [9..255] (HTML 3.2).  HTML 4.01 allows the specification of 16 bit"
			"characters, so we do a little fiddling to handle a few special cases"

			value := self convertToNumber: (specialEntity copyFrom: 2 to: specialEntity size).

			"Replace rounded left & right double quotes (HTML 4.01) with simple double quote"
			(value = 8220 or: [value = 8221]) ifTrue: [ value := $" asInteger ].

			"Replace rounded left & right single quotes (HTML 4.01) with simple single quote"
			(value = 8216 or: [value = 8217]) ifTrue: [ value := $' asInteger ].

			"Replace with a space if outside the normal range (arbitrary choice)"
			(value < 9 or: [value > 255]) ifTrue: [ value := 32 ].
			]
		ifFalse: [
			"Otherwise this is most likely a named character entity"
			value := ReverseCharacterEntities at: specialEntity ifAbsent: [^nil].
			].

	 ^Character value: value.! !

!HtmlArea methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:09'!
buildMorph
	"construct a hot-spot morph"
	| coords vertices radiusX radiusY |
	coords := (self coords findTokens: ', ') collect: [:elem | elem asNumber asInteger].
	self shape isEmptyOrNil
		ifTrue: [^nil].

	(self shape asLowercase beginsWith: 'poly')
		ifTrue: [coords size even ifFalse: [^nil].
			vertices := OrderedCollection new.
			coords pairsDo: [:x :y |
				vertices add: x @ y].
			^(PolygonMorph vertices: vertices color: Color transparent
				borderWidth: 1 borderColor: Color transparent) quickFill: false; makeClosed].

	(coords size > 4 or: [coords size < 3])
		ifTrue: [^nil].

	self shape asLowercase = 'circle'
		ifTrue: [radiusX := coords third.
			radiusY := coords last.
			^(EllipseMorph newBounds:
				(((coords first - radiusX) @ (coords second - radiusY))
				extent:
				((2 * radiusX) @ (2 * radiusY)))
			color: Color transparent) borderColor: Color transparent].

	coords size = 4
		ifFalse: [^nil].

	(self shape asLowercase beginsWith: 'rect')
		ifTrue: [^(RectangleMorph newBounds:
				(Rectangle origin: (coords first @ coords second)
				corner: (coords third @ coords last))
			color: Color transparent) borderColor: Color transparent].

	^nil! !

!HtmlArea methodsFor: 'accessing' stamp: 'bolot 11/3/1999 17:48'!
coords
	^self getAttribute: 'coords'! !

!HtmlArea methodsFor: 'accessing' stamp: 'bolot 11/3/1999 17:48'!
href
	^self getAttribute: 'href'! !

!HtmlArea methodsFor: 'testing' stamp: 'bolot 11/4/1999 04:43'!
isArea
	^true! !

!HtmlArea methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:09'!
linkMorphForMap: map andBrowser: browser
	| m |
	(m := self buildMorph) ifNil: [^nil].
	m color: (Color random alpha: 0.1). "hack to ensure the morph is clickable"
	m
		on: #mouseUp
		send: #mouseUpBrowserAndUrl:event:linkMorph:
		to: map
		withValue: {browser. self href}.
	^m! !

!HtmlArea methodsFor: 'testing' stamp: 'bolot 11/3/1999 18:06'!
mayContain: anEntity
	^false! !

!HtmlArea methodsFor: 'accessing' stamp: 'bolot 11/3/1999 17:48'!
shape
	^self getAttribute: 'shape'! !

!HtmlArea methodsFor: 'testing' stamp: 'bolot 11/3/1999 17:47'!
tagName
	^'area'! !

!HtmlBlockEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 03:02'!
isBlockEntity
	^true! !

!HtmlBlockQuote methodsFor: 'formatting' stamp: 'rkris 7/11/2004 13:08'!
addToFormatter: formatter
	formatter ensureNewlines: 2.
	formatter increaseIndent.
	super addToFormatter: formatter.
	formatter decreaseIndent.
	formatter ensureNewlines: 2.! !

!HtmlBlockQuote methodsFor: 'testing' stamp: 'ls 7/17/1998 20:27'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlBlockQuote methodsFor: 'testing' stamp: 'ls 7/17/1998 20:26'!
tagName
	^'blockquote'! !

!HtmlDefinitionList methodsFor: 'formatting' stamp: 'ls 7/4/1998 01:14'!
addToFormatter: formatter
	super addToFormatter: formatter.
	formatter ensureNewlines: 1.! !

!HtmlDefinitionList methodsFor: 'testing' stamp: 'ls 7/4/1998 00:17'!
mayContain: anEntity
	^anEntity isDefinitionListElement! !

!HtmlDefinitionList methodsFor: 'testing' stamp: 'ls 7/4/1998 00:17'!
tagName
	^'dl'! !

!HtmlList methodsFor: 'testing' stamp: 'ls 7/17/1998 19:20'!
mayContain: anElement
	"lists may only contain LI elements"
	"^anElement isListItem"

	"except that people write some sucky HTML out there!!!!  well, let's assume they always put the end tag.  Much safer assumption than that they only put list-items in their lists"
	^true! !

!HtmlList methodsFor: 'testing' stamp: 'ls 7/28/1998 20:40'!
shouldContain: anEntity
	^anEntity isListItem! !

!HtmlOrderedList methodsFor: 'formatting' stamp: 'ls 7/4/1998 19:50'!
addToFormatter: formatter
	formatter startOrderedList.
	super addToFormatter: formatter.
	formatter endOrderedList.! !

!HtmlOrderedList methodsFor: 'testing' stamp: 'ls 6/25/1998 02:07'!
tagName
	^'ol'! !

!HtmlUnorderedList methodsFor: 'formatting' stamp: 'ls 7/4/1998 19:50'!
addToFormatter: formatter
	formatter startUnorderedList.
	super addToFormatter: formatter.
	formatter endUnorderedList.! !

!HtmlUnorderedList methodsFor: 'testing' stamp: 'ls 6/25/1998 02:07'!
tagName
	^'ul'! !

!HtmlBody methodsFor: 'attributes' stamp: 'bolot 12/8/1999 01:40'!
background
	^self getAttribute: 'background' default: nil! !

!HtmlBody methodsFor: 'attributes' stamp: 'bolot 12/8/1999 01:41'!
bgcolor
	^self getAttribute: 'bgcolor' default: 'white'! !

!HtmlBody methodsFor: 'testing' stamp: 'ls 7/4/1998 19:24'!
mayContain: anEntity
	"Body's can contain anything, so that even if we screw up the parsing, all the text will end up actually being included"
	^true! !

!HtmlBody methodsFor: 'lint' stamp: 'ls 7/29/1998 00:24'!
shouldContain: anEntity
	"I don't *think* there are any elements that can be in both the header and the body..."
	^anEntity isHeadElement not! !

!HtmlBody methodsFor: 'testing' stamp: 'ls 6/25/1998 02:04'!
tagName
	^'body'! !

!HtmlDefinitionListElement methodsFor: 'testing' stamp: 'ls 6/25/1998 02:19'!
isDefinitionListElement
	^true! !

!HtmlDefinitionDefinition methodsFor: 'formatting' stamp: 'ls 7/4/1998 01:12'!
addToFormatter: formatter
	formatter ensureNewlines: 1.
	formatter increaseIndent.
	super addToFormatter: formatter.
	formatter decreaseIndent.! !

!HtmlDefinitionDefinition methodsFor: 'testing' stamp: 'ls 7/4/1998 00:20'!
mayContain: anEntity
	^anEntity isBlockEntity or: [ anEntity isTextualEntity ] ! !

!HtmlDefinitionDefinition methodsFor: 'testing' stamp: 'ls 7/4/1998 00:18'!
tagName
	^'dd'! !

!HtmlDefinitionTerm methodsFor: 'formatting' stamp: 'ls 7/4/1998 01:14'!
addToFormatter: formatter
	formatter ensureNewlines: 1.
	super addToFormatter: formatter.! !

!HtmlDefinitionTerm methodsFor: 'testing' stamp: 'ls 7/4/1998 00:19'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlDefinitionTerm methodsFor: 'testing' stamp: 'ls 7/4/1998 00:17'!
tagName
	^'dt'! !

!HtmlDocument methodsFor: 'access' stamp: 'djp 7/21/1998 18:38'!
addToBody: anObject
	"add an object to the Body entity of the receiver"
	self body add: anObject! !

!HtmlDocument methodsFor: 'access' stamp: 'djp 7/21/1998 18:37'!
addToHead: anObject
	"add an object to the head entity of the receiver"
	self head add: anObject! !

!HtmlDocument methodsFor: 'access' stamp: 'djp 7/21/1998 18:28'!
body
	^self contents at: 2! !

!HtmlDocument methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:10'!
formattedText
	"return a version of this document as a formatted Text"
	| formatter |
	formatter := HtmlFormatter preferredFormatterClass new.
	self addToFormatter: formatter.
	^formatter text ! !

!HtmlDocument methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:12'!
formattedTextForBrowser: browser  defaultBaseUrl: defaultBaseUrl
	"return a version of this document as a formatted Text (which includes links and such)"
	| formatter text |

	"set up the formatter"
	formatter := HtmlFormatter preferredFormatterClass new.
	formatter browser: browser.
	formatter baseUrl: defaultBaseUrl.  "should check if the document specifies something else"

	"do the formatting"
	self addToFormatter: formatter.

	"get and return the result"
	text := formatter text.
	^text! !

!HtmlDocument methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:12'!
formattedTextMorph
	"return a version of this document as a formatted TextMorph (which includes links and such)"
	| formatter text textMorph |
	formatter := HtmlFormatter preferredFormatterClass new.
	self addToFormatter: formatter.
	text := formatter text.

	textMorph := TextMorph new initialize.
	textMorph contentsWrapped: text.

	^textMorph! !

!HtmlDocument methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:13'!
formattedTextMorphForBrowser: browser  defaultBaseUrl: defaultBaseUrl
	"return a version of this document as a formatted TextMorph (which includes links and such)"
	| formatter textMorph |

	"set up the formatter"
	formatter := HtmlFormatter preferredFormatterClass new.
	formatter browser: browser.
	formatter baseUrl: defaultBaseUrl.  "should check if the document specifies something else"

	"do the formatting"
	self addToFormatter: formatter.

	"get and return the result"
	textMorph := formatter textMorph .
	^textMorph! !

!HtmlDocument methodsFor: 'access' stamp: 'djp 7/21/1998 18:28'!
head
	^self contents at: 1! !

!HtmlDocument methodsFor: 'testing' stamp: 'ls 6/25/1998 02:57'!
mayContain: anElement
	^true 	"not strictly true, but it makes the parser simpler"! !

!HtmlDocument methodsFor: 'testing' stamp: 'ls 6/25/1998 02:06'!
tagName
	^'html'! !

!HtmlDocument class methodsFor: 'instance creation' stamp: 'ls 7/23/1998 04:26'!
emptyDocument
	"return an empty document"
	^super new add: HtmlHead new; add: HtmlBody new! !

!HtmlForm methodsFor: 'formatting' stamp: 'ls 7/17/1998 00:26'!
addToFormatter: formatter
	formatter startForm: self.
	super addToFormatter: formatter.
	formatter currentFormData reset.
	formatter endForm.! !

!HtmlForm methodsFor: 'attributes' stamp: 'bolot 11/27/1999 19:21'!
encoding
	"encoding for posting"
	^self getAttribute: 'enctype' default: nil.  ! !

!HtmlForm methodsFor: 'access' stamp: 'ls 7/16/1998 23:29'!
formEntities
	^formEntities! !

!HtmlForm methodsFor: 'testing' stamp: 'ls 7/4/1998 19:51'!
isBlockElement
	^true! !

!HtmlForm methodsFor: 'testing' stamp: 'ls 7/15/1998 00:24'!
mayContain: anEntity
	"allow anything.  People probably always put </form> anyway at the end of their forms.  And furthermore, it has no effect on the display, even if this is gotten wrong"
	^true! !

!HtmlForm methodsFor: 'attributes' stamp: 'ls 7/17/1998 00:40'!
method
	"method to submit with"
	^self getAttribute: 'method' default: 'get'! !

!HtmlForm methodsFor: 'parsing' stamp: 'sd 12/4/2010 22:13'!
parsingFinished
	"figure out who our constituents are"

	self allSubentitiesDo: [ :e |
		e isFormEntity ifTrue: [ e form: self ] ].
	super parsingFinished.
	formEntities := OrderedCollection new.
	self allSubentitiesDo: [ :e |
		(e isFormEntity and: [ e form == self ])
			ifTrue: [ formEntities add: e ] ].! !

!HtmlForm methodsFor: 'testing' stamp: 'ls 7/3/1998 23:30'!
tagName
	^'form'! !

!HtmlForm methodsFor: 'attributes' stamp: 'ls 7/17/1998 00:40'!
url
	"url to submit to"
	^self getAttribute: 'action' default: nil.  ! !

!HtmlFormEntity methodsFor: 'access' stamp: 'ls 7/15/1998 00:22'!
form
	"return which form we are in"
	^form! !

!HtmlFormEntity methodsFor: 'access' stamp: 'sd 12/4/2010 22:13'!
form: aForm
	"set which form we are part of"
	form := aForm! !

!HtmlFormEntity methodsFor: 'testing' stamp: 'ls 7/15/1998 00:22'!
isFormEntity
	^true! !

!HtmlFormEntity methodsFor: 'testing' stamp: 'ls 7/29/1998 00:15'!
lint: aStream
	form ifNil: [ aStream nextPutAll: '<', self tagName, '> not within a form'.
		aStream cr. ].
	super lint: aStream.! !

!HtmlButton methodsFor: 'testing' stamp: 'ls 7/21/1998 07:04'!
mayContain: anEntity
	^false! !

!HtmlButton methodsFor: 'testing' stamp: 'ls 7/21/1998 07:04'!
tagName
	^'button'! !

!HtmlInput methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:14'!
addCheckBoxButtonToFormatter: formatter

	| name formData checked button buttonInput |
	"dig up relevant attributes"
	name := self getAttribute: 'name'.
	name ifNil: [ ^self ].
	value := self getAttribute: 'value'.
	value ifNil: [ ^value ].
	formData := formatter currentFormData.
	formData ifNil:  [ ^self ].
	checked := (self getAttribute: 'checked') isNil not.

	"set up the form input"
	buttonInput := ToggleButtonInput name: name value: value checkedByDefault: checked.
	formData addInput: buttonInput.

	"create the actual button"
	button := UpdatingThreePhaseButtonMorph checkBox.
	button target: buttonInput;
		getSelector: #pressed;
		actionSelector: #toggle.
	buttonInput button: button.
	formatter addMorph: button.


! !

!HtmlInput methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:14'!
addFileInputToFormatter: formatter
	"is it a submit button?"
	| inputMorph size fileInput |
	inputMorph := PluggableTextMorph on: StringHolder new text: #contents accept: #acceptContents:.
	size := (self getAttribute: 'size' default: '12') asNumber.
	inputMorph extent: (size*10@20).
	fileInput := FileInput name: self name textMorph: inputMorph.
	formatter addMorph: inputMorph;
		addMorph: ((PluggableButtonMorph on: fileInput getState: nil action: #browse)
				label: 'Browse').
	formatter currentFormData addInput: fileInput! !

!HtmlInput methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:15'!
addImageButtonToFormatter: formatter
	"is it a submit button?"
	| formData imageUrl morph |
	(imageUrl := self getAttribute: 'src') ifNil: [^self].
	formatter baseUrl
		ifNotNil: [imageUrl := imageUrl asUrlRelativeTo: formatter baseUrl].

	morph := DownloadingImageMorph new.
	morph defaultExtent: self imageExtent.
	morph altText: self alt.
	morph url: imageUrl.

	value := self getAttribute: 'name' default: 'Submit'.
	formData := formatter currentFormData.
	morph
		on: #mouseUp
		send: #mouseUpFormData:event:linkMorph:
		to: self
		withValue: formData.
	formatter addIncompleteMorph: morph
! !

!HtmlInput methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:15'!
addRadioButtonToFormatter: formatter
	| name formData checked buttonSet button buttonInput |

	"dig up relevant attributes"
	name := self getAttribute: 'name'.
	name ifNil: [ ^self ].
	value := self getAttribute: 'value'.
	value ifNil: [ ^value ].
	formData := formatter currentFormData.
	formData ifNil:  [ ^self ].
	checked := self getAttribute: 'checked'.


	"find or create the set of buttons with our same name"
	buttonSet := formData inputs detect: [ :i | i isRadioButtonSetInput and: [ i name = name ] ] ifNone: [ nil ].
	buttonSet ifNil: [ 
		"create a new button set"
		buttonSet := RadioButtonSetInput name: name.
		formData addInput: buttonSet. ].

	"set up the form input"
	buttonInput := RadioButtonInput  inputSet: buttonSet value: value.
	buttonSet addInput: buttonInput.
	checked ifNotNil: [
		buttonSet  defaultButton: buttonInput ].

	"create the actual button"
	button := UpdatingThreePhaseButtonMorph radioButton.
	button target: buttonInput;
		getSelector: #pressed;
		actionSelector: #toggle.
	buttonInput button: button.
	formatter addMorph: button.


! !

!HtmlInput methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:15'!
addTextInputToFormatter: formatter
	"is it a submit button?"
	| inputMorph size |
	inputMorph := PluggableTextMorph on: StringHolder new text: #contents accept: #acceptContents:.
	self type = 'password'
		ifTrue: [inputMorph font: (StrikeFont passwordFontSize: 12)].
	size := (self getAttribute: 'size' default: '12') asNumber.
	inputMorph extent: (size*10@20).
	formatter addMorph: inputMorph.
	formatter currentFormData addInput:
		(TextInput name: self name defaultValue: self defaultValue  textMorph: inputMorph).! !

!HtmlInput methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:15'!
addToFormatter: formatter
	formatter currentFormData ifNil: [
		"not in a form.  It's bogus HTML but try to survive"
		^self ].
	"is it a submit button?"
	self type = 'submit' ifTrue: [
		formatter addMorph: ((PluggableButtonMorph on: formatter currentFormData getState: nil action: #submit) label: (self getAttribute: 'value' default: 'Submit')).
		^self ].
	self type = 'image'
		ifTrue: [^self addImageButtonToFormatter: formatter].
	(self type = 'text' or: [self type = 'password'])
		ifTrue: [^self addTextInputToFormatter: formatter].
	self type = 'hidden' ifTrue: [
		formatter currentFormData addInput: (HiddenInput name: self name  value: self defaultValue).
		^self ].
	self type = 'radio' ifTrue: [ 
		^self addRadioButtonToFormatter: formatter ].
	self type = 'checkbox' ifTrue: [ 
		^self addCheckBoxButtonToFormatter: formatter ].
	self type = 'file' ifTrue: [ 
		^self addFileInputToFormatter: formatter ].
	formatter addString: '[form input of type: ', self type, ']'.! !

!HtmlInput methodsFor: 'attributes' stamp: 'ls 9/18/1998 08:22'!
defaultValue
	^(self getAttribute: 'value' default: '') replaceHtmlCharRefs! !

!HtmlInput methodsFor: 'attributes' stamp: 'sd 12/4/2010 22:16'!
imageExtent
	"the image extent, according to the WIDTH and HEIGHT attributes.  returns nil if either WIDTH or HEIGHT is not specified"
	| widthText heightText |
	widthText := self getAttribute: 'width' ifAbsent: [ ^nil ].
	heightText := self getAttribute: 'height' ifAbsent: [ ^nil ].
	^ widthText asNumber @ heightText asNumber! !

!HtmlInput methodsFor: 'testing' stamp: 'ls 7/5/1998 02:15'!
isTextualEntity
	^true! !

!HtmlInput methodsFor: 'testing' stamp: 'ls 7/5/1998 02:14'!
mayContain: anEntity
	^false! !

!HtmlInput methodsFor: 'morphic' stamp: 'ar 3/18/2001 17:27'!
mouseUpEvent: arg1 linkMorph: arg2 formData: arg3
	"Reorder the arguments for existing event handlers"
	(arg3 isMorph and:[arg3 eventHandler notNil]) ifTrue:[arg3 eventHandler fixReversedValueMessages].
	^self mouseUpFormData: arg1 event: arg2 linkMorph: arg3! !

!HtmlInput methodsFor: 'morphic' stamp: 'sd 12/4/2010 22:16'!
mouseUpFormData: formData event: event linkMorph: linkMorph
	| aPoint |
	aPoint := event cursorPoint - linkMorph topLeft.
	formData addInput: (HiddenInput name: (value, '.x') value: aPoint x asInteger asString).
	formData addInput: (HiddenInput name: (value, '.y') value: aPoint y asInteger asString).
	formData submit! !

!HtmlInput methodsFor: 'testing' stamp: 'ls 7/21/1998 06:51'!
suppliesInput
	"whether we actually have input to supply"
	self type = 'text' ifTrue: [ ^true ].
	^false! !

!HtmlInput methodsFor: 'testing' stamp: 'ls 7/5/1998 02:14'!
tagName
	^'input'! !

!HtmlInput methodsFor: 'attributes' stamp: 'ls 7/23/1998 19:09'!
type
	^(self getAttribute: 'type' default: 'text') asLowercase! !

!HtmlOption methodsFor: 'testing' stamp: 'ls 7/21/1998 07:33'!
isOption
	^true! !

!HtmlOption methodsFor: 'attributes' stamp: 'ls 8/5/1998 07:19'!
label
	"label to be displayed for this morph"
	^self getAttribute: 'label' ifAbsent: [self textualContents]! !

!HtmlOption methodsFor: 'testing' stamp: 'ls 7/21/1998 07:32'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlOption methodsFor: 'testing' stamp: 'ls 7/21/1998 07:04'!
tagName
	^'option'! !

!HtmlOption methodsFor: 'attributes' stamp: 'ls 8/5/1998 08:42'!
value
	"value to pass if this option is selected"
	^self getAttribute: 'value' default: '(unspecified)'! !

!HtmlOptionGroup methodsFor: 'testing' stamp: 'ls 7/21/1998 07:33'!
mayContain: anEntity
	^anEntity isOption! !

!HtmlOptionGroup methodsFor: 'testing' stamp: 'ls 7/21/1998 07:05'!
tagName
	^'optgroup'! !

!HtmlSelect methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:16'!
addToFormatter: formatter
	| options defaultOption listMorph names size valueHolder |
	formatter currentFormData ifNil: [
		"not in a form.  It's bogus HTML but try to survive"
		^self ].

	names := OrderedCollection new.
	options := OrderedCollection new.
	defaultOption := nil.
	(self getAttribute: 'multiple') ifNotNil: [
		self flag: #incomplete.
		formatter addString: '[M option list]'.
		^self ].

	contents do: [ :c |  c isOption ifTrue: [
		names add: c value.
		options add: c label withBlanksCondensed.
		(c getAttribute: 'selected') ifNotNil: [ defaultOption := c label ] ] ].

	contents isEmpty ifTrue: [ ^self ].

	defaultOption ifNil: [ defaultOption := options first ].

	size := (self getAttribute: 'size' default: '1') asNumber.
	size = 1
		ifTrue: [listMorph := PopUpChoiceMorph new initialize; contents: defaultOption.
			listMorph items: options; target: nil; getItemsSelector: nil;
				maxExtent: options; border: #useBorder]
		ifFalse: [valueHolder := ValueHolder new contents: (contents indexOf: defaultOption).
			listMorph := PluggableListMorph on: valueHolder list: nil
				selected: #contents  changeSelected: #contents:.
			listMorph list: options.
			listMorph extent: ((listMorph extent x) @ (listMorph scrollDeltaHeight * size))].

	formatter addMorph: listMorph.

	formatter currentFormData addInput:
		(SelectionInput  name: self name  defaultValue: defaultOption
			list: listMorph  values: names asArray)! !

!HtmlSelect methodsFor: 'testing' stamp: 'ls 7/21/1998 07:32'!
isTextualEntity
	^true! !

!HtmlSelect methodsFor: 'testing' stamp: 'ls 7/21/1998 07:32'!
mayContain: anEntity
	^true  "end tag required"! !

!HtmlSelect methodsFor: 'testing' stamp: 'ls 7/21/1998 07:05'!
tagName
	^'select'! !

!HtmlTextArea methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:17'!
addToFormatter: formatter
	| inputMorph |
	formatter currentFormData ifNil: [
		"not in a form.  It's bogus HTML but try to survive"
		^self ].

	formatter ensureNewlines: 1.
	inputMorph := PluggableTextMorph on: StringHolder new text: #contents accept: #acceptContents:.
	inputMorph extent: (self columns * 5) @ (self rows * inputMorph scrollDeltaHeight).
	inputMorph retractable: false.
	formatter addMorph: inputMorph.
	formatter currentFormData addInput: (TextInput name: self name  defaultValue:  self textualContents  textMorph: inputMorph).
	formatter ensureNewlines: 1.! !

!HtmlTextArea methodsFor: 'attributes' stamp: 'sd 12/4/2010 22:17'!
columns
	| a |
	a := self getAttribute: 'cols' default: '20'.
	^(Integer readFrom: (ReadStream on: a)) max: 5.! !

!HtmlTextArea methodsFor: 'attributes' stamp: 'ls 7/17/1998 03:12'!
defaultValue
	^self textualContents  "it would be nice to through the tags in there, too....  Easiest way would probably be to modiy the tokenizer"! !

!HtmlTextArea methodsFor: 'testing' stamp: 'ls 7/5/1998 02:28'!
isBlockEntity
	"not sure...."
	^true! !

!HtmlTextArea methodsFor: 'testing' stamp: 'ls 7/29/1998 00:28'!
mayContain: anEntity
	^true   "really we shouldn't be interpretting tags in here at all, though...."! !

!HtmlTextArea methodsFor: 'attributes' stamp: 'sd 12/4/2010 22:17'!
rows
	| a |
	a := self getAttribute: 'rows' default: '2'.
	^(Integer readFrom: (ReadStream on: a)) max: 1.! !

!HtmlTextArea methodsFor: 'testing' stamp: 'ls 7/17/1998 19:05'!
suppliesInput
	^self name ~= nil! !

!HtmlTextArea methodsFor: 'testing' stamp: 'ls 7/5/1998 02:40'!
tagName
	^'textarea'! !

!HtmlFrame methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:17'!
addToFormatter: formatter
	| src |
	src := self getAttribute: 'src' default: nil.
	formatter ensureNewlines: 1.
	src ifNotNil: [ formatter startLink: src ].
	formatter addString: 'frame '.
	formatter addString: (self name ifNil: ['(unnamed)']).
	src ifNotNil:  [ formatter endLink: src ].
	formatter ensureNewlines: 1.! !

!HtmlFrame methodsFor: 'categorization' stamp: 'ls 7/29/1998 00:25'!
isFrame
	^true! !

!HtmlFrame methodsFor: 'testing' stamp: 'ls 7/26/1998 21:46'!
mayContain: anEntity
	^false! !

!HtmlFrame methodsFor: 'testing' stamp: 'ls 7/26/1998 21:46'!
tagName
	^'frame'! !

!HtmlFrameSet methodsFor: 'categorization' stamp: 'ls 7/29/1998 00:26'!
isFrameSet
	^true! !

!HtmlFrameSet methodsFor: 'testing' stamp: 'ls 7/29/1998 00:26'!
mayContain: anEntity
	^true! !

!HtmlFrameSet methodsFor: 'lint' stamp: 'ls 7/29/1998 00:25'!
shouldContain: anEntity
	^anEntity isFrame or: [ anEntity isFrameSet ]! !

!HtmlFrameSet methodsFor: 'testing' stamp: 'ls 7/26/1998 21:45'!
tagName
	^'frameset'! !

!HtmlHead methodsFor: 'testing' stamp: 'ls 7/28/1998 20:25'!
mayContain: anEntity
	^anEntity isHeadElement or: [ anEntity isComment ]! !

!HtmlHead methodsFor: 'testing' stamp: 'ls 6/25/1998 02:07'!
tagName
	^'head'! !

!HtmlHead methodsFor: 'metainformation' stamp: 'sd 12/4/2010 22:28'!
title
	"return the title, or nil if there isn't one"
	| te |
	te := self titleEntity.
	te ifNil: [ ^nil ].
	^te textualContents! !

!HtmlHead methodsFor: 'metainformation' stamp: 'ls 7/5/1998 01:02'!
titleEntity
	"return the title entity, or nil if there isn't one"
	contents do: [ :e | e tagName = 'title' ifTrue: [ ^e ] ].
	^nil! !

!HtmlHeadEntity methodsFor: 'formatting' stamp: 'ls 6/27/1998 15:38'!
addToFormatter: formatter
	"head elements are normally just meta-information, and thus don't add anything to the formatter"! !

!HtmlHeadEntity methodsFor: 'testing' stamp: 'ls 6/27/1998 15:37'!
isHeadElement
	^true! !

!HtmlMeta methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:28'!
addToFormatter: formatter
	| httpEquiv |
	httpEquiv := self getAttribute: 'http-equiv'.
	httpEquiv ifNil: [ ^self ].
	httpEquiv asLowercase = 'refresh' ifTrue: [
		formatter addString: '{refresh: ', (self getAttribute:  'content' default: ''), '}' ].! !

!HtmlMeta methodsFor: 'initialization' stamp: 'sd 12/4/2010 22:28'!
initialize: aTag
	super initialize: aTag.
	theTag := aTag.! !

!HtmlMeta methodsFor: 'testing' stamp: 'ls 7/5/1998 02:22'!
mayContain: anEntity
	^false! !

!HtmlMeta methodsFor: 'printing' stamp: 'pb 6/4/2012 16:37'!
printOn: aStream indent: indent
	indent timesRepeat: [ aStream space ].
	aStream nextPutAll: 'meta: '.
	theTag printOn: aStream.
	aStream newLine.! !

!HtmlMeta methodsFor: 'testing' stamp: 'ls 7/5/1998 02:22'!
tagName
	^'meta'! !

!HtmlStyle methodsFor: 'testing' stamp: 'ls 7/5/1998 00:15'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlStyle methodsFor: 'testing' stamp: 'ls 7/5/1998 00:15'!
tagName
	^'style'! !

!HtmlTitle methodsFor: 'testing' stamp: 'ls 6/27/1998 15:35'!
isHeadElement
	^true! !

!HtmlTitle methodsFor: 'testing' stamp: 'ls 6/27/1998 15:35'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlTitle methodsFor: 'testing' stamp: 'ls 6/27/1998 15:36'!
tagName
	^'title'! !

!HtmlHeader methodsFor: 'formatting' stamp: 'bolot 5/18/2000 12:03'!
addToFormatter: formatter
	formatter startHeader: level.
	super addToFormatter: formatter.
	formatter endHeader: level! !

!HtmlHeader methodsFor: 'private-initialization' stamp: 'sd 12/4/2010 22:29'!
initialize: aTag
	super initialize: aTag.
	level := aTag name last digitValue.! !

!HtmlHeader methodsFor: 'testing' stamp: 'ls 7/4/1998 12:55'!
isHeader
	^true! !

!HtmlHeader methodsFor: 'testing' stamp: 'ls 6/27/1998 15:58'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlHeader methodsFor: 'testing' stamp: 'ls 7/23/1998 05:42'!
tagName
	^'h', level printString! !

!HtmlHorizontalRule methodsFor: 'formatting' stamp: 'ls 6/27/1998 15:57'!
addToFormatter: formatter
	formatter hr! !

!HtmlHorizontalRule methodsFor: 'testing' stamp: 'ls 6/27/1998 15:52'!
mayContain: anEntity
	^false! !

!HtmlHorizontalRule methodsFor: 'testing' stamp: 'ls 6/25/1998 02:22'!
tagName
	^'hr'! !

!HtmlListItem methodsFor: 'formatting' stamp: 'ls 7/4/1998 19:49'!
addToFormatter: formatter
	formatter startListItem.
	super addToFormatter: formatter.
	formatter endListItem.! !

!HtmlListItem methodsFor: 'testing' stamp: 'ls 6/25/1998 02:10'!
isListItem
	^true! !

!HtmlListItem methodsFor: 'testing' stamp: 'ls 6/25/1998 03:03'!
mayContain: anEntity
	^anEntity isBlockEntity or: [ anEntity isTextualEntity ]! !

!HtmlListItem methodsFor: 'testing' stamp: 'ls 6/25/1998 02:09'!
tagName
	^'li'! !

!HtmlMap methodsFor: 'formatting' stamp: 'bolot 11/4/1999 00:37'!
addToFormatter: formatter
	super addToFormatter: formatter.
	formatter addImageMap: self! !

!HtmlMap methodsFor: 'morphic' stamp: 'sd 12/4/2010 22:30'!
buildImageMapForImage: imageMorph andBrowser: browser
	| areaMorph |
	contents do: [:area |
		(area isArea
		and: [(areaMorph := area linkMorphForMap: self andBrowser: browser) isNil not])
			ifTrue: [imageMorph addMorph: areaMorph]].
	^imageMorph! !

!HtmlMap methodsFor: 'testing' stamp: 'bolot 11/4/1999 04:42'!
mayContain: anEntity
	^anEntity isArea! !

!HtmlMap methodsFor: 'morphic' stamp: 'sd 12/4/2010 22:30'!
mouseUpBrowserAndUrl: browserAndUrl event: event linkMorph: linkMorph
	"this is an image map area, just follow the link"
	| browser url |
	browser := browserAndUrl first.
	url := browserAndUrl second.
	browser jumpToUrl: url! !

!HtmlMap methodsFor: 'morphic' stamp: 'ar 3/18/2001 17:27'!
mouseUpEvent: arg1 linkMorph: arg2 browserAndUrl: arg3
	"Reorder the arguments for existing event handlers"
	(arg3 isMorph and:[arg3 eventHandler notNil]) ifTrue:[arg3 eventHandler fixReversedValueMessages].
	^self mouseUpBrowserAndUrl: arg1 event: arg2 linkMorph: arg3! !

!HtmlMap methodsFor: 'accessing' stamp: 'bolot 11/3/1999 17:55'!
name
	^self getAttribute: 'name'! !

!HtmlMap methodsFor: 'testing' stamp: 'bolot 11/3/1999 17:47'!
tagName
	^'map'! !

!HtmlParagraph methodsFor: 'formatting' stamp: 'ls 7/4/1998 01:16'!
addToFormatter: formatter
	formatter ensureNewlines: 2.
	super addToFormatter: formatter.! !

!HtmlParagraph methodsFor: 'testing' stamp: 'ls 6/25/1998 03:02'!
isBlockEntity
	^true! !

!HtmlParagraph methodsFor: 'testing' stamp: 'ls 6/25/1998 03:03'!
isParagraph
	^true! !

!HtmlParagraph methodsFor: 'testing' stamp: 'ls 6/25/1998 02:12'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlParagraph methodsFor: 'testing' stamp: 'ls 6/25/1998 02:12'!
tagName
	^'p'! !

!HtmlPreformattedRegion methodsFor: 'formatting' stamp: 'ls 7/13/1998 23:15'!
addToFormatter: formatter
	formatter ensureNewlines: 1.
	formatter increasePreformatted.
	super addToFormatter: formatter.
	formatter decreasePreformatted.
	formatter ensureNewlines: 1.! !

!HtmlPreformattedRegion methodsFor: 'testing' stamp: 'ls 7/4/1998 12:06'!
mayContain: anEntity
	^true! !

!HtmlPreformattedRegion methodsFor: 'lint' stamp: 'ls 7/29/1998 00:27'!
shouldContain: anEntity
	^anEntity isTextualEntity! !

!HtmlPreformattedRegion methodsFor: 'testing' stamp: 'ls 7/4/1998 12:06'!
tagName
	^'pre'! !

!HtmlTable methodsFor: 'formatting' stamp: 'ls 9/12/1998 00:52'!
addToFormatter: formatter
	formatter ensureNewlines: 1.
	super addToFormatter: formatter.
	formatter ensureNewlines: 1.! !

!HtmlTable methodsFor: 'accessing' stamp: 'sd 12/4/2010 22:32'!
asArrayOfData
	"Return an Array2D of the table, removing all html.  This in only the text and numbers that the user would see on a web page.  Remove all comments and formatting.  Width is the widest row, and others are padded with empty strings."

	| cc array2D widths |
	cc := contents select: [:ent | ent isTableRow].
	widths := cc contents collect: [:row | 
		row contents count: [:ent | ent isTableDataItem]].
	array2D := Matrix "Pay attention this was Array2D so the code is probably broken since long time" width: (widths max) height: cc size.
	cc withIndexDo: [:row :rowNum |
		array2D atRow: rowNum put: 
			(row asArrayOfData padded: #right to: array2D width with: '')].
	^ array2D! !

!HtmlTable methodsFor: 'testing' stamp: 'ls 7/4/1998 12:53'!
mayContain: anEntity
	^anEntity isTableRow! !

!HtmlTable methodsFor: 'testing' stamp: 'ls 7/4/1998 19:07'!
tagName
	^'table'! !

!HtmlTableDataItem methodsFor: 'formatting' stamp: 'ls 9/5/1998 03:37'!
addToFormatter: formatter
	super addToFormatter: formatter.
	formatter ensureSpaces: 1.! !

!HtmlTableDataItem methodsFor: 'testing' stamp: 'ls 7/4/1998 12:54'!
isTableDataItem
	^true! !

!HtmlTableDataItem methodsFor: 'testing' stamp: 'ls 7/4/1998 12:56'!
mayContain: anEntity
	^anEntity isTextualEntity or: [ anEntity isBlockEntity or: [ anEntity isHeader ] ]! !

!HtmlTableDataItem methodsFor: 'testing' stamp: 'ls 7/4/1998 19:01'!
tagName
	^'td'! !

!HtmlTableHeader methodsFor: 'accessing' stamp: 'ls 9/12/1998 00:51'!
tagName
	^'th'! !

!HtmlTableRow methodsFor: 'formatting' stamp: 'ls 9/12/1998 00:52'!
addToFormatter: formatter
	super addToFormatter: formatter.
	formatter ensureNewlines: 1.! !

!HtmlTableRow methodsFor: 'accessing' stamp: 'sd 12/4/2010 22:32'!
asArrayOfData
	"Return an Array of the table row, removing all html.  This is only the text and numbers that the user would see on a web page.  Remove all comments and formatting."

	| cc |
	cc := contents select: [:ent | ent isTableDataItem].
	^ cc collect: [:ent | ent asHtml asUnHtml withBlanksTrimmed]
		"for now, leave the numbers as strings.  User will know which to convert"! !

!HtmlTableRow methodsFor: 'testing' stamp: 'ls 7/4/1998 12:53'!
isTableRow
	^true! !

!HtmlTableRow methodsFor: 'testing' stamp: 'ls 7/4/1998 12:54'!
mayContain: anEntity
	^anEntity isTableDataItem! !

!HtmlTableRow methodsFor: 'testing' stamp: 'ls 7/4/1998 19:00'!
tagName
	^'tr'! !

!HtmlTextualEntity methodsFor: 'testing' stamp: 'ls 6/25/1998 02:12'!
isTextualEntity
	^true! !

!HtmlCommentEntity methodsFor: 'formatting' stamp: 'ls 7/4/1998 12:27'!
addToFormatter: formatter
	"do nothing"! !

!HtmlCommentEntity methodsFor: 'access' stamp: 'ls 7/4/1998 12:27'!
commentText
	^commentText! !

!HtmlCommentEntity methodsFor: 'private-iniitialization' stamp: 'sd 12/4/2010 22:32'!
initializeWithText: aString
	super initialize.
	commentText := aString.! !

!HtmlCommentEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 16:30'!
isComment
	^true! !

!HtmlCommentEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 12:27'!
mayContain: anEntity
	^false! !

!HtmlCommentEntity methodsFor: 'printing' stamp: 'pb 6/4/2012 16:35'!
printHtmlOn: aStream indent: indent
	indent timesRepeat: [ aStream space ].
	aStream nextPutAll: '<!!-- '.
	aStream nextPutAll: self commentText.
	aStream nextPutAll: ' -->'.
	aStream newLine.! !

!HtmlCommentEntity methodsFor: 'printing' stamp: 'ls 10/26/1998 17:08'!
printOn: aStream indent: indent 
	self printHtmlOn: aStream indent: indent! !

!HtmlCommentEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 12:30'!
tagName	
	"return a bogus tag name"
	^'x-comment'! !

!HtmlFontChangeEntity methodsFor: 'private-initialization' stamp: 'sd 12/4/2010 22:37'!
initialize: aTag
	super initialize: aTag.
	tagName := aTag name! !

!HtmlFontChangeEntity methodsFor: 'testing' stamp: 'ls 7/5/1998 01:58'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlFontChangeEntity methodsFor: 'testing' stamp: 'ls 6/27/1998 12:51'!
tagName
	"must be stored in an i-var, because these classes work for different tags"
	^tagName! !

!HtmlBoldEntity methodsFor: 'formatting' stamp: 'ls 6/27/1998 12:53'!
addToFormatter: formatter
	formatter increaseBold.
	super addToFormatter: formatter.
	formatter decreaseBold.! !

!HtmlFontEntity methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:37'!
addToFormatter: formatter
	| size color textAttribList |
	(formatter respondsTo: #startFont:)
		ifFalse: [^super addToFormatter: formatter].
	size := self getAttribute: 'size'.
	color := self getAttribute: 'color'.
	textAttribList := OrderedCollection new.
	color ifNotNil: [textAttribList add: (TextColor color: (Color fromString: color))].
	(size isEmptyOrNil not and: [size isAllDigits]) 
		ifTrue: [size := (size asNumber - 3) max: 1.
			textAttribList add: (TextFontChange fontNumber: (size min: 4))].
	formatter startFont: textAttribList.
	super addToFormatter: formatter.
	formatter endFont: textAttribList! !

!HtmlItalicsEntity methodsFor: 'formatting' stamp: 'ls 6/27/1998 13:16'!
addToFormatter: formatter
	formatter increaseItalics.
	super addToFormatter: formatter.	
	formatter decreaseItalics.! !

!HtmlStrikeEntity methodsFor: 'formatting' stamp: 'ls 7/5/1998 02:05'!
addToFormatter: formatter
	formatter increaseStrike.
	super addToFormatter: formatter.
	formatter decreaseStrike.! !

!HtmlUnderlineEntity methodsFor: 'formatting' stamp: 'ls 7/5/1998 01:41'!
addToFormatter: formatter
	formatter increaseUnderline.
	super addToFormatter: formatter.	
	formatter decreaseUnderline.! !

!HtmlAnchor methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:39'!
addToFormatter: formatter

	| href name |
	name := self getAttribute: 'name'.
	name ifNotNil: 
		[ formatter noteAnchorStart: name ].
	href := self getAttribute: 'href'.
	href isNil
		ifTrue: [ super addToFormatter: formatter ]
		ifFalse: 
			[ formatter startLink: href.
			super addToFormatter: formatter.
			formatter endLink: href ].
! !

!HtmlAnchor methodsFor: 'testing' stamp: 'sd 12/4/2010 22:38'!
mayContain: anEntity 

	(self attributes includesKey: 'href') ifFalse: [
		"if we have no href, then we can contain nothing"
		^false ].
	^ anEntity isTextualEntity! !

!HtmlAnchor methodsFor: 'testing' stamp: 'sd 12/4/2010 22:38'!
tagName

	^'a'! !

!HtmlBreak methodsFor: 'formatting' stamp: 'ls 7/17/1998 19:21'!
addToFormatter: formatter
	formatter addChar: Character cr.! !

!HtmlBreak methodsFor: 'testing' stamp: 'ls 7/4/1998 16:14'!
mayContain: anEntity
	^false! !

!HtmlBreak methodsFor: 'testing' stamp: 'ls 7/4/1998 16:14'!
tagName
	^'br'! !

!HtmlEmbedded methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:39'!
addToFormatter: formatter
	| url embeddedMorph |
	self src isNil ifTrue: [^self].
	url := self src.
	embeddedMorph := self embeddedMorphFor: url.
	embeddedMorph isNil ifTrue:[^self].
	formatter baseUrl ifNotNil: [url := url asUrlRelativeTo: formatter baseUrl].
	embeddedMorph extent: self extent.
	embeddedMorph sourceUrl: url.
	embeddedMorph setProperty: #embedded toValue: true.
	formatter addIncompleteMorph: embeddedMorph.! !

!HtmlEmbedded methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:40'!
embeddedMorphClassFor: url
	| lastIndex extension className |
	lastIndex := url findLast:[:c| c = $.].
	lastIndex = 0 ifTrue:[^nil].
	extension := url copyFrom: lastIndex+1 to: url size.
	className := ExtensionList at: extension asLowercase ifAbsent:[^nil].
	^Smalltalk globals at: className ifAbsent:[nil]
	! !

!HtmlEmbedded methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:40'!
embeddedMorphFor: url
	| morphClass |
	morphClass := self embeddedMorphClassFor: url.
	^morphClass ifNotNil:[morphClass new]! !

!HtmlEmbedded methodsFor: 'attributes' stamp: 'sd 12/4/2010 22:41'!
extent
	"the image extent, according to the WIDTH and HEIGHT attributes.  returns nil if either WIDTH or HEIGHT is not specified"
	| widthText heightText |
	widthText := self getAttribute: 'width' ifAbsent: [ ^nil ].
	heightText := self getAttribute: 'height' ifAbsent: [ ^nil ].
	^ widthText asNumber @ heightText asNumber! !

!HtmlEmbedded methodsFor: 'testing' stamp: 'ar 11/18/1998 19:07'!
mayContain: anEntity
	^false! !

!HtmlEmbedded methodsFor: 'attributes' stamp: 'ar 11/18/1998 19:07'!
src
	^self getAttribute: 'src' default: nil! !

!HtmlEmbedded methodsFor: 'testing' stamp: 'ar 11/18/1998 19:07'!
tagName
	^'embed'! !

!HtmlEmbedded class methodsFor: 'initialize' stamp: 'ar 11/19/1998 22:46'!
initialize
	"HtmlEmbedded initialize"
	ExtensionList := Dictionary new.
	#(
		('swf'	FlashPlayerMorph)
	) do:[:spec| ExtensionList at: spec first put: spec last].! !

!HtmlImage methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:41'!
addToFormatter: formatter
	| morph url |
	self src isNil ifTrue: [ ^self ].
	url := self src.
	formatter baseUrl ifNotNil: [ 
		url := url asUrlRelativeTo: formatter baseUrl ].
	morph := DownloadingImageMorph new.
	morph defaultExtent: self imageExtent.
	morph altText: self alt.
	morph url: url.
	self imageMapName
		ifNotNil:
			[morph imageMapName: self imageMapName.
			morph formatter: formatter].
	formatter addIncompleteMorph: morph.! !

!HtmlImage methodsFor: 'attributes' stamp: 'ls 8/4/1998 21:58'!
alt
	^(self getAttribute: 'alt') ifNil: ['[image]']! !

!HtmlImage methodsFor: 'downloading' stamp: 'sd 12/4/2010 22:41'!
downloadState: baseUrl 

	|  sourceUrl imageSource |
	image ifNil: [ 
		sourceUrl := self src.
		sourceUrl ifNotNil: [ 
			imageSource := HTTPSocket httpGetDocument: (sourceUrl asUrlRelativeTo: baseUrl asUrl) asString.
			imageSource contentType = 'image/gif'  ifTrue: [
				[image := (GIFReadWriter on: (RWBinaryOrTextStream with: imageSource content) reset binary) nextImage ]
				ifError: [ :a :b |  "could not decode--ignore it"  image := nil ] ].
			 ] ].
! !

!HtmlImage methodsFor: 'attributes' stamp: 'sd 12/4/2010 22:42'!
imageExtent
	"the image extent, according to the WIDTH and HEIGHT attributes.  returns nil if either WIDTH or HEIGHT is not specified"
	| widthText heightText |
	widthText := self getAttribute: 'width' ifAbsent: [ ^nil ].
	heightText := self getAttribute: 'height' ifAbsent: [ ^nil ].
	^ [ widthText asNumber @ heightText asNumber ] ifError: [ :a :b | nil ]! !

!HtmlImage methodsFor: 'attributes' stamp: 'sd 12/4/2010 22:42'!
imageMapName
	| imageMapName |
	(imageMapName := self getAttribute: 'usemap')
		ifNil: [^nil].
	imageMapName first = $#
		ifTrue: [imageMapName := imageMapName copyFrom: 2 to: imageMapName size].
	^imageMapName! !

!HtmlImage methodsFor: 'testing' stamp: 'ls 7/1/1998 02:30'!
mayContain: anEntity
	^false! !

!HtmlImage methodsFor: 'attributes' stamp: 'ls 7/29/1998 00:56'!
src
	^self getAttribute: 'src' default: nil! !

!HtmlImage methodsFor: 'testing' stamp: 'ls 6/25/1998 02:22'!
tagName
	^'img'! !

!HtmlNoEmbed methodsFor: 'formatting' stamp: 'sd 12/4/2010 22:43'!
addToFormatter: aFormatter
	"Check if the last incomplete morph has the property #embedded set.
	If so, assume that the last <EMBED> tag has been handled."
	| morphs |
	morphs := aFormatter incompleteMorphs.
	(morphs isEmpty not and:[(morphs last valueOfProperty: #embedded) == true])
		ifTrue:[^self].
	"If not handled do the usual stuff"
	^super addToFormatter: aFormatter! !

!HtmlNoEmbed methodsFor: 'testing' stamp: 'ar 11/18/1998 21:11'!
mayContain: anEntity
	^anEntity isTextualEntity! !

!HtmlNoEmbed methodsFor: 'testing' stamp: 'ar 11/18/1998 21:02'!
tagName
	^'noembed'! !

!HtmlTextEntity methodsFor: 'formatting' stamp: 'ls 6/25/1998 17:24'!
addToFormatter: aFormatter
	aFormatter addString: text! !

!HtmlTextEntity methodsFor: 'testing' stamp: 'ls 7/28/1998 20:26'!
mayContain: anEntity
	^false! !

!HtmlTextEntity methodsFor: 'printing' stamp: 'ls 10/26/1998 17:09'!
printHtmlOn: aStream indent: indent 
	indent timesRepeat: [aStream space].
	aStream nextPutAll: text.
! !

!HtmlTextEntity methodsFor: 'printing' stamp: 'pb 6/4/2012 16:38'!
printOn: aStream indent: indent
	indent timesRepeat: [ aStream space ].
	aStream nextPutAll: '['.
	aStream nextPutAll: text.
	aStream nextPutAll: ']'.
	aStream newLine.! !

!HtmlTextEntity methodsFor: 'testing' stamp: 'ls 7/4/1998 16:30'!
tagName
	"bogus"
	^'x-text'  ! !

!HtmlTextEntity methodsFor: 'access' stamp: 'ls 6/25/1998 02:58'!
text
	^text! !

!HtmlTextEntity methodsFor: 'access' stamp: 'sd 12/4/2010 22:43'!
text: text0
	text := text0! !

!HtmlTextEntity methodsFor: 'contents' stamp: 'ls 7/5/1998 00:38'!
textualContents
	^text! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 9/12/1998 00:14'!
addChar: c
	"add a single character, updating all the tallies"

	"add the character to the output"
	outputStream nextPut: c.

	"update counters for preceeding spaces and preceding newlines"
	(c = Character space or: [ c = Character tab ]) 
	ifTrue: [ precedingSpaces := precedingSpaces+1.  precedingNewlines := 0 ]
	ifFalse: [
		(c = Character cr) ifTrue: [
			precedingSpaces := 0.
			precedingNewlines := precedingNewlines + 1 ]
		ifFalse: [
			precedingSpaces := precedingNewlines := 0 ] ].! !

!HtmlFormatter methodsFor: 'image maps' stamp: 'bolot 11/4/1999 00:36'!
addImageMap: anImageMap
	imageMaps addLast: anImageMap! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 9/5/1998 18:09'!
addIncompleteMorph: aMorph
	"add a morph, and note that it needs to download some more state before reaching its ultimate state"
	self addMorph: aMorph.
	incompleteMorphs add: aMorph.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/30/1998 02:51'!
addLink: text  url: url
	"add a link with the given url and text"
	| savedAttributes linkAttribute  |

	"set up the link attribute"
	linkAttribute := TextURL new.
	linkAttribute url: url.

	"add the link to the stream"
	savedAttributes := outputStream currentAttributes.
	outputStream currentAttributes: (savedAttributes, linkAttribute).
	outputStream nextPutAll: text.
	outputStream currentAttributes: savedAttributes.

	"reset counters"
	precedingSpaces := precedingNewlines := 0.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ar 12/17/2001 02:18'!
addMorph: aMorph
	"add a morph to the output"
	| savedAttributes |
	self addChar: Character space.

	savedAttributes := outputStream currentAttributes.
	outputStream currentAttributes: (savedAttributes copyWith: (TextAnchor new anchoredMorph: aMorph)).
	self addChar: (Character value: 1).
	outputStream currentAttributes: savedAttributes.

	self addChar: Character space.

	morphsToEmbed add: aMorph.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 8/25/1998 06:10'!
addString: aString
	"adds the text in the given string.  It collapses spaces unless we are in a preformatted region"

	| space compacted lastC i |

	aString isEmpty ifTrue: [ ^self ].

	space := Character space.


	preformattedLevel > 0 ifTrue: [
		"add all the characters as literals"
		outputStream nextPutAll: aString.

		"update the counters"
		lastC := aString last.
		(lastC = space or: [ lastC = Character cr ]) ifTrue: [
			"how many of these are there?"
			i := aString size - 1.
			[ i >= 1 and: [ (aString at: i) = lastC ] ] whileTrue: [ i := i - 1 ].
			i = 0 ifTrue: [
				"the whole string is the same character!!"
				lastC = space ifTrue: [
					precedingSpaces := precedingSpaces + aString size.
					precedingNewlines := 0.
					^self ]
				ifFalse: [
					precedingSpaces := 0.
					precedingNewlines := precedingNewlines + aString size.
					^self ]. ].
			lastC = space ifTrue: [
				precedingSpaces := aString size - i.
				precedingNewlines := 0 ]
			ifFalse: [
				precedingSpaces := 0.
				precedingNewlines := aString size - i ] ] ]
	ifFalse: [
		compacted := aString withSeparatorsCompacted.

		compacted = ' ' ifTrue: [
			"no letters in the string--just white space!!"
			(precedingNewlines = 0 and: [precedingSpaces = 0]) ifTrue: [
				precedingSpaces := 1.
				outputStream nextPut: space. ].
			^self ].

		(compacted first = Character space and: [
			(precedingSpaces > 0) or: [ precedingNewlines > 0] ])
		ifTrue: [ compacted := compacted copyFrom: 2 to: compacted size ].

		outputStream nextPutAll: compacted.

		"update counters"
		precedingNewlines := 0.
		compacted last = space 
			ifTrue: [ precedingSpaces := 1 ]
			ifFalse: [ precedingSpaces := 0 ]. ]! !

!HtmlFormatter methodsFor: 'access' stamp: 'ls 9/10/1998 03:20'!
anchorLocations
	"return a dictionary mapping lowercase-ed anchor names into the integer positions they are located at in the text"
	^anchorLocations! !

!HtmlFormatter methodsFor: 'access' stamp: 'ls 7/16/1998 22:08'!
baseUrl
	"return the base URL for the document we are formatting, if known"
	^baseUrl! !

!HtmlFormatter methodsFor: 'access' stamp: 'ls 7/16/1998 22:09'!
baseUrl: url
	"set the base url.  All relative URLs will be determined relative to it"
	baseUrl := url.! !

!HtmlFormatter methodsFor: 'access' stamp: 'ls 7/16/1998 22:07'!
browser
	"return the browser we are formatting for, or nil if none"
	^browser! !

!HtmlFormatter methodsFor: 'access' stamp: 'ls 7/16/1998 22:08'!
browser: b
	"set what browser we are formatting for"
	browser := b.! !

!HtmlFormatter methodsFor: 'forms' stamp: 'ls 7/16/1998 22:07'!
currentFormData
	"return the current form data, or nil if we aren't inside a form"
	formDatas size > 0 
		ifTrue: [ ^formDatas last ]
		ifFalse: [ ^nil ].! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/27/1998 15:12'!
decreaseBold
	boldLevel := boldLevel - 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/27/1998 15:12'!
decreaseIndent
	indentLevel := indentLevel - 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/27/1998 15:12'!
decreaseItalics
	italicsLevel := italicsLevel - 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/16/1998 22:58'!
decreasePreformatted
	preformattedLevel := preformattedLevel - 1! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/5/1998 02:03'!
decreaseStrike
	strikeLevel := strikeLevel - 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/5/1998 01:39'!
decreaseUnderline
	underlineLevel := underlineLevel - 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'forms' stamp: 'bolot 8/29/1999 18:40'!
endForm
	formDatas size > 0 ifTrue: [ 
		formDatas removeLast. ]
	ifFalse: [ self halt: 'HtmlFormatter: ended more forms that started!!?' ].
	self ensureNewlines: 1.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 12:02'!
endHeader: level
	self decreaseBold.
	self ensureNewlines: 2! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/30/1998 03:17'!
endLink: url
	urlLink := nil.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 8/20/1998 11:34'!
endListItem
	"end a list item"
	self ensureNewlines: 1.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/5/1998 13:17'!
endOrderedList
	"end an ordered list"
	listLengths removeLast.
	listTypes removeLast.
	indentLevel := indentLevel - 1.
	self setAttributes. 

	self ensureNewlines: 1.
	! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/5/1998 13:18'!
endUnorderedList
	"end an unordered list"
	listLengths removeLast.
	listTypes removeLast.
	indentLevel := indentLevel - 1.
	self setAttributes. 
	
	self ensureNewlines: 1.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/25/1998 17:21'!
ensureNewlines: number
	"make sure there are at least number preceding newlines"
	number > precedingNewlines ifTrue: [
		(number - precedingNewlines) timesRepeat: [ self addChar: Character cr ] ].! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 8/16/1998 11:34'!
ensureSpaces: number
	"make sure there are at least number preceding spaces, unless we're at the beginning of a new line"

	precedingNewlines > 0 ifTrue: [ ^ self ].

	number > precedingSpaces ifTrue: [
		(number - precedingSpaces) timesRepeat: [ self addChar: Character space ] ].! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 9/12/1998 00:15'!
hr
	"add an (attempt at a) horizontal rule"
	self ensureNewlines: 1.
	25 timesRepeat: [ self addChar: $- ].
	self ensureNewlines: 1.
	precedingSpaces := 0.
	precedingNewlines := 1000.    "pretend it's the top of a new page"! !

!HtmlFormatter methodsFor: 'image maps' stamp: 'bolot 11/4/1999 00:37'!
imageMapNamed: imageMapName
	^imageMaps detect: [:im | im name asLowercase = imageMapName asLowercase] ifNone: []! !

!HtmlFormatter methodsFor: 'access' stamp: 'ls 9/5/1998 18:08'!
incompleteMorphs
	"list of morphs needing to download some more state"
	^incompleteMorphs! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/27/1998 15:12'!
increaseBold
	boldLevel := boldLevel + 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/27/1998 15:12'!
increaseIndent
	indentLevel := indentLevel + 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/27/1998 15:13'!
increaseItalics
	italicsLevel := italicsLevel + 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/16/1998 22:58'!
increasePreformatted
	preformattedLevel := preformattedLevel + 1! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/5/1998 02:03'!
increaseStrike
	strikeLevel := strikeLevel + 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/5/1998 01:40'!
increaseUnderline
	underlineLevel := underlineLevel + 1.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'private-initialization' stamp: 'rk 7/8/2004 08:48'!
initialize
	outputStream := AttributedTextStream new.
	preformattedLevel := 0.
	indentLevel := boldLevel := italicsLevel := underlineLevel := strikeLevel := centerLevel := 0.
	listLengths := OrderedCollection new.
	listTypes := OrderedCollection new.
	formDatas := OrderedCollection new.
	precedingSpaces := 0.
	precedingNewlines := 1000.   "more than will ever be asked for"
	morphsToEmbed := OrderedCollection new.
	incompleteMorphs := OrderedCollection new.
	anchorLocations := Dictionary new.
	imageMaps := OrderedCollection new.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 9/10/1998 03:26'!
noteAnchorStart: anchorName
	"note that an anchor starts at this point in the output"
	anchorLocations at: anchorName asLowercase put: outputStream size! !

!HtmlFormatter methodsFor: 'private-formatting' stamp: 'ls 8/5/1998 01:41'!
setAttributes
	"set attributes on the output stream"
	| attribs |
	attribs := OrderedCollection new.
	indentLevel > 0 ifTrue: [ attribs add: (TextIndent tabs: indentLevel) ].
	boldLevel > 0 ifTrue: [ attribs add: TextEmphasis bold ].
	italicsLevel >  0 ifTrue: [ attribs add: TextEmphasis italic ].
	underlineLevel > 0 ifTrue: [ attribs add: TextEmphasis underlined ].
	strikeLevel > 0 ifTrue: [ attribs add: TextEmphasis struckOut ].
	urlLink isNil ifFalse: [ attribs add: (TextURL new url: urlLink) ].
	outputStream currentAttributes: attribs! !

!HtmlFormatter methodsFor: 'forms' stamp: 'bolot 8/29/1999 18:40'!
startForm: form
	"a form is beginning"
	self ensureNewlines: 1.
	formDatas addLast: (FormInputSet forForm: form  andBrowser: browser).! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 12:02'!
startHeader: level
	self ensureNewlines: 3.
	self increaseBold! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 6/30/1998 03:17'!
startLink: url
	urlLink := url.
	self setAttributes.! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'bf 11/4/1999 22:23'!
startListItem
	"begin a new list item"
	listTypes size = 0 ifTrue: [ ^self ].
	self ensureNewlines: 1.
	listTypes last = #unordered
		ifTrue: [ self addString: '¥ ' ]
		ifFalse: [ self addString: (listLengths last + 1) printString.
			self addString: '. ' ].
	listLengths at: (listLengths size) put: (listLengths last + 1).! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/4/1998 19:44'!
startOrderedList
	"begin an ordered list"
	listLengths add: 0.
	listTypes add: #ordered.
	indentLevel := indentLevel + 1.
	self setAttributes.
	! !

!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 7/4/1998 19:44'!
startUnorderedList
	"begin an unordered list"
	listLengths add: 0.
	listTypes add: #unordered.
	indentLevel := indentLevel + 1.
	self setAttributes.
	! !

!HtmlFormatter methodsFor: 'formatting' stamp: 'ls 6/30/1998 01:13'!
text
	| text |
	text := outputStream contents.
	^text! !

!HtmlFormatter methodsFor: 'formatting' stamp: 'ls 7/16/1998 21:55'!
textMorph
	| text textMorph |
	text := outputStream contents.
	textMorph := TextMorph new contents: text.
	morphsToEmbed do:[ :m | textMorph addMorph: m ].
	^textMorph! !

!HtmlFormatter class methodsFor: 'examples' stamp: 'sma 2/12/2000 18:53'!
example1
	"(HtmlFormatter example1 asParagraph compositionRectangle: (0@0 extent: 300@500) ) displayAt: 0@0"
	| input |
	input := ReadStream on: 
'<html>
<head>
<title>The Gate of Chaos</title>
</head>

<body>

<h1>Chaos</h1>



<h2>Into the Maelstrom</h2>
Direction is useless in the ever-changing Maelstrom.  However,
if you wander with purpose, you might be able to find....
<ul>
<li><a href="/cgi-bin/w">see who''s logged in</a>
<li><a href="/Telnet/connect.html">log in, if you (oooh) have an account</a>
<li><a href="http://chaos.resnet.gatech.edu:9000/sw">The Chaos Swiki</a>--scribble on chaos
<li>the original <a href="/cgi-bin/guestlog-print">Visitor Sands</a>
<li>my old <a href="rant.html">Rant Page</a>
<li>neverending <a href="/cgi-bin/bread">poll</a>: do you have an opinion on bread?
<li>a <a href="http://www.cc.gatech.edu/~lex/linux.html">Linux page</a>-- free UNIX for PC''s!!
<li><a href="english.au">Hear Linus Himself speak!!!!</a>
<li><a href="/doc/">some docs on all kinds of computer stuff</a>
</ul>

<hr>


<h2>Paths of Retreat</h2>
Several commonly travelled ways have left paths leading 
<em>away</em> from the maelstrom, too:
<p>
<ul>
<li><a href="friends.html">Friends of Chaos</a>
<li><a href="http://www.apache.org/">The <em>Apache</em> home page</a> -- 
        <em>Chaos</em>''s WWW server!!
<li><a href="http://www.ee.umanitoba.ca/~djc/personal/notscape.html">
Notscape</a>

the <a href="http://www.eskimo.com/%7Eirving/anti-ns/">Anti-Netscape
Page</a> -- fight the tyranny!!
</ul>

<hr>
<a href="/analog/usage.html">usage stats</a> for this server

<hr>
<a href="http://www.eff.org/blueribbon.html"><img src="blueribbon.gif" alt="[blue ribbon campaign]"></a>
<a href="http://www.ee.umanitoba.ca/~djc/personal/notscape.html">
<img src="notscape.gif" alt="[NOTscape]">
</a>
<a href="http://www.anybrowser.org/campaign/">
<img src="anybrowser3.gif" alt="[Best Viewed With Any Browser"]></a>
</body>
</html>'.
	^(HtmlParser parse: input) formattedText! !

!HtmlFormatter class methodsFor: 'initialization' stamp: 'ls 8/20/1998 06:05'!
initialize
	"HtmlFormatter initialize"

	CSSeparators := CharacterSet separators.
	CSNonSeparators := CSSeparators complement.! !

!HtmlFormatter class methodsFor: 'instance creation' stamp: 'ls 6/25/1998 17:25'!
new
	^super new initialize! !

!HtmlFormatter class methodsFor: 'instance creation' stamp: 'bolot 5/18/2000 11:36'!
preferredFormatterClass
	^DHtmlFormatter! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 11:55'!
decreaseFontBy: relSize
	self startFont: (TextFontChange fontNumber: ((self lastFontSize - relSize) min: 4))! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 11:23'!
endFont: aFont
	fontSpecs isEmptyOrNil
		ifFalse: [fontSpecs removeLast].
	self setAttributes! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 12:01'!
endHeader: level
	boldLevel := boldLevel - 1. "self decreaseBold"
	self ensureNewlines: 2.
	self endFont: nil.! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 12:00'!
headerFont: level
	^{TextFontChange fontNumber: ((5 - level) max: 1)}! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 11:50'!
increaseFontBy: relSize
	self startFont: (TextFontChange fontNumber: ((self lastFontSize + relSize) min: 4))! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 11:55'!
lastFontSize
	| textAttrib |
	fontSpecs isEmptyOrNil ifTrue: [^1].

	fontSpecs reverseDo: [:specs |
		textAttrib := specs detect: [:attrib | attrib isKindOf: TextFontChange] ifNone: [].
		textAttrib ifNotNil: [^textAttrib fontNumber]].

	^1 "default font size in Squeak (1) corresponds to HTML's default 4"! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 11:57'!
resetFont
	"probably need use document defaults"
	self startFont:
		{TextColor black.
		TextFontChange fontNumber: 1}! !

!DHtmlFormatter methodsFor: 'private-formatting' stamp: 'bolot 5/18/2000 11:26'!
setAttributes
	"set attributes on the output stream"
	| attribs |
	attribs := OrderedCollection new.
	indentLevel > 0 ifTrue: [ attribs add: (TextIndent tabs: indentLevel) ].
	boldLevel > 0 ifTrue: [ attribs add: TextEmphasis bold ].
	italicsLevel >  0 ifTrue: [ attribs add: TextEmphasis italic ].
	underlineLevel > 0 ifTrue: [ attribs add: TextEmphasis underlined ].
	strikeLevel > 0 ifTrue: [ attribs add: TextEmphasis struckOut ].
	urlLink isNil ifFalse: [ attribs add: (TextURL new url: urlLink) ].
	fontSpecs isEmptyOrNil
		ifFalse: [attribs addAll: fontSpecs last]
		ifTrue: [attribs add: (TextFontChange defaultFontChange)].
	outputStream currentAttributes: attribs! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 11:57'!
startFont: aTextAttribList
	"aTextAttribList is a collection of TextAttributes"
	fontSpecs ifNil: [fontSpecs := OrderedCollection new].
	fontSpecs add: aTextAttribList.
	self setAttributes! !

!DHtmlFormatter methodsFor: 'formatting commands' stamp: 'bolot 5/18/2000 12:00'!
startHeader: level
	self ensureNewlines: 3.
	boldLevel := boldLevel + 1. "self increaseBold"
	self startFont: (self headerFont: level).! !

!HtmlParser class methodsFor: 'example' stamp: 'sd 12/4/2010 21:36'!
example1
	"HtmlParser example1"
	| input |
	input := ReadStream on: 
'<html>
<head>
<title>The Gate of Chaos</title>
</head>

<body>

<h1>Chaos</h1>



<h2>Into the Maelstrom</h2>
Direction is useless in the ever-changing Maelstrom.  However,
if you wander with purpose, you might be able to find....
<ul>
<li><a href="/cgi-bin/w">see who''s logged in</a>
<li><a href="/Telnet/connect.html">log in, if you (oooh) have an account</a>
<li><a href="http://chaos.resnet.gatech.edu:9000/sw">The Chaos Swiki</a>--scribble on chaos
<li>the original <a href="/cgi-bin/guestlog-print">Visitor Sands</a>
<li>my old <a href="rant.html">Rant Page</a>
<li>neverending <a href="/cgi-bin/bread">poll</a>: do you have an opinion on bread?
<li>a <a href="http://www.cc.gatech.edu/~lex/linux.html">Linux page</a>-- free UNIX for PC''s!!
<li><a href="english.au">Hear Linus Himself speak!!!!</a>
<li><a href="/doc/">some docs on all kinds of computer stuff</a>
</ul>

<hr>


<h2>Paths of Retreat</h2>
Several commonly travelled ways have left paths leading 
<em>away</em> from the maelstrom, too:
<p>
<ul>
<li><a href="friends.html">Friends of Chaos</a>
<li><a href="http://www.apache.org/">The <em>Apache</em> home page</a> -- 
        <em>Chaos</em>''s WWW server!!
<li><a href="http://www.ee.umanitoba.ca/~djc/personal/notscape.html">
Notscape</a>

the <a href="http://www.eskimo.com/%7Eirving/anti-ns/">Anti-Netscape
Page</a> -- fight the tyranny!!
</ul>

<hr>
<a href="/analog/usage.html">usage stats</a> for this server

<hr>
<a href="http://www.eff.org/blueribbon.html"><img src="blueribbon.gif" alt="[blue ribbon campaign]"></a>
<a href="http://www.ee.umanitoba.ca/~djc/personal/notscape.html">
<img src="notscape.gif" alt="[NOTscape]">
</a>
<a href="http://www.anybrowser.org/campaign/">
<img src="anybrowser3.gif" alt="[Best Viewed With Any Browser]"></a>
</body>
</html>'.

	^HtmlParser parse: input! !

!HtmlParser class methodsFor: 'as yet unclassified' stamp: 'pb 6/1/2015 01:08'!
isAllSeparators: aString
	"Hack to reimplements String isAllSeparators"
	self flag: #fixme.
	aString do: [ :c |
		c isSeparator ifFalse: [ ^ false ]].
	^ true.! !

!HtmlParser class methodsFor: 'parsing' stamp: 'ls 7/28/1998 02:02'!
parse: aStream
	^self parseTokens: (HtmlTokenizer on: aStream)
! !

!HtmlParser class methodsFor: 'parsing' stamp: 'pb 5/31/2015 23:32'!
parseTokens: tokenStream
	| entityStack document head token matchesAnything entity body |
	entityStack := OrderedCollection new.
	"set up initial stack"
	document := HtmlDocument new.
	entityStack add: document.
	head := HtmlHead new.
	document addEntity: head.
	entityStack add: head.
	"go through the tokens, one by one"
	[
	token := tokenStream next.
	token = nil ] whileFalse: [
		(token isTag and: [ token isNegated ])
			ifTrue: [
				"a negated token"
				(token name ~= 'html' and: [ token name ~= 'body' ]) ifTrue: [
					"see if it matches anything in the stack"
					matchesAnything := (entityStack
						detect: [ :e |
							e tagName = token name ]
						ifNone: [ ]) isNil not.
					matchesAnything ifTrue: [
						"pop the stack until we find the right one"
						[ entityStack last tagName ~= token name ] whileTrue: [ entityStack removeLast ].
						entityStack removeLast ]]]
			ifFalse: [
				"not a negated token.  it makes its own entity"
				token isComment ifTrue: [ entity := HtmlCommentEntity new initializeWithText: token source ].
				token isText ifTrue: [
					entity := HtmlTextEntity new text: token text.
					((entityStack last shouldContain: entity) not and: [
						self isAllSeparators: token source]) ifTrue: [ "blank text may never cause the stack to back up"
						entity := HtmlCommentEntity new initializeWithText: token source ]].
				token isTag ifTrue: [
					entity := token entityFor.
					entity = nil ifTrue: [ entity := HtmlCommentEntity new initializeWithText: token source ]].
				token name = 'body' ifTrue: [
					body ifNotNil: [ document removeEntity: body ].
					body := HtmlBody new initialize: token.
					document addEntity: body.
					entityStack add: body ].
				entity = nil ifTrue: [ self error: 'could not deal with this token' ].
				entity isComment
					ifTrue: [ "just stick it anywhere"
						entityStack last addEntity: entity ]
					ifFalse: [
						"only put it in something that is valid"
						[ entityStack last mayContain: entity ] whileFalse: [ entityStack removeLast ].
						"if we have left the head, create a body"
						(entityStack size < 2 and: [ body isNil ]) ifTrue: [
							body := HtmlBody new.
							document addEntity: body.
							entityStack add: body ].
						"add the entity"
						entityStack last addEntity: entity.
						entityStack addLast: entity ]]].
	body == nil ifTrue: [
		"add an empty body"
		body := HtmlBody new.
		document addEntity: body ].
	document parsingFinished.
	^ document.! !

!HtmlToken methodsFor: 'formatting' stamp: 'ls 6/24/1998 17:29'!
addToFormatter: aFormatter
	"add ourself to a formatter"! !

!HtmlToken methodsFor: 'parser support' stamp: 'ls 6/25/1998 02:37'!
entityFor
	^self subclassResponsibility! !

!HtmlToken methodsFor: 'private-initialization' stamp: 'sd 12/4/2010 22:43'!
initialize: s
	"default initialization doesn't do much.  subclasses are free to override"
	source := s
! !

!HtmlToken methodsFor: 'properties' stamp: 'ls 1/25/98 04:47'!
isComment
	"whether this token is an HTML comment"
	^false
! !

!HtmlToken methodsFor: 'properties' stamp: 'ls 1/25/98 04:38'!
isTag
	"is this an HTML tag"
	^false! !

!HtmlToken methodsFor: 'properties' stamp: 'ls 1/25/98 04:39'!
isText
	"return whether it is a string of text"
	^false
! !

!HtmlToken methodsFor: 'printing' stamp: 'ls 1/25/98 05:45'!
printOn: aStream
	aStream nextPutAll: '{';
		nextPutAll: self class name;
		nextPut: $:;
		nextPutAll: self source;
		nextPut: $}.! !

!HtmlToken methodsFor: 'access' stamp: 'ls 1/25/98 04:48'!
source
	"the raw source text that composes this token"
	^source! !

!HtmlToken class methodsFor: 'instance creation' stamp: 'ls 1/25/98 06:01'!
forSource: source  
	^super new initialize: source
! !

!HtmlComment methodsFor: 'parser support' stamp: 'ls 6/25/1998 02:37'!
entityFor
	^self shouldNotImplement! !

!HtmlComment methodsFor: 'properties' stamp: 'ls 1/25/98 04:57'!
isComment
	^true! !

!HtmlComment methodsFor: 'access' stamp: 'ls 1/25/98 04:56'!
text
	"return the text of the comment, the part inside the <!!-- and -->"
	^self notYetImplemented! !

!HtmlTag methodsFor: 'access' stamp: 'ls 1/25/98 04:50'!
attribs
	"return a dictionary mapping attribute names (in lowercase) to their values"
	^attribs
! !

!HtmlTag methodsFor: 'parser support' stamp: 'sd 12/4/2010 22:45'!
entityFor
	"return an empty entity corresponding to this tag"
	| eClass |
	eClass := self class entityClasses at: name ifAbsent: [ ^nil ].
	^eClass forTag: self ! !

!HtmlTag methodsFor: 'access' stamp: 'ls 6/24/1998 18:53'!
isNegated
	^isNegated! !

!HtmlTag methodsFor: 'properties' stamp: 'ls 1/25/98 04:57'!
isTag
	^true! !

!HtmlTag methodsFor: 'access' stamp: 'ls 1/25/98 04:49'!
name
	"return the basic kind of tag, in lowercase"
	^name
! !

!HtmlTag methodsFor: 'private-initialization' stamp: 'sd 12/4/2010 22:47'!
name: name0  negated: isNegated0 attribs: attribs0
	"initialize from the given attributes"
	name := name0.
	isNegated := isNegated0.
	attribs := attribs0 ifNil: [Dictionary new]! !

!HtmlTag class methodsFor: 'parser support' stamp: 'bolot 11/3/1999 17:52'!
entityClasses
	"a Dictionary mapping tag names into the correct entity class"
	"EntityClasses := nil"

	EntityClasses isNil ifFalse: [ ^EntityClasses ].

	EntityClasses := Dictionary new.

	#( 
		frameset	HtmlFrame
		frame	HtmlFrame

		title		HtmlTitle
		style	HtmlStyle
		meta	HtmlMeta

		p		HtmlParagraph
		form	HtmlForm
		blockquote	HtmlBlockQuote

		input	HtmlInput
		textarea	HtmlTextArea
		select	HtmlSelect
		optgroup	HtmlOptionGroup
		option		HtmlOption

		img		HtmlImage
		embed	HtmlEmbedded
		noembed	HtmlNoEmbed
		a		HtmlAnchor
		br		HtmlBreak

		map	HtmlMap
		area	HtmlArea

		li		HtmlListItem
		dd		HtmlDefinitionDefinition
		dt		HtmlDefinitionTerm

		ol		HtmlOrderedList
		ul		HtmlUnorderedList
		dl		HtmlDefinitionList

		h1		HtmlHeader
		h2		HtmlHeader
		h3		HtmlHeader
		h4		HtmlHeader
		h5		HtmlHeader
		h6		HtmlHeader

		hr		HtmlHorizontalRule

		strong	HtmlBoldEntity
		b		HtmlBoldEntity

		em		HtmlItalicsEntity
		i		HtmlItalicsEntity
		dfn 	HtmlItalicsEntity

		u		HtmlUnderlineEntity 

		tt		HtmlFixedWidthEntity
		kbd		HtmlFixedWidthEntity		

		strike	HtmlStrikeEntity

		big		HtmlBiggerFontEntity
		small	HtmlSmallerFontEntity

		sub		HtmlSubscript
		sup		HtmlSuperscript

		font	HtmlFontEntity

		pre		HtmlPreformattedRegion
 
		table	HtmlTable
		tr		HtmlTableRow
		td		HtmlTableDataItem 
		th		HtmlTableHeader
		) pairsDo: [ 
			:tagName :className |
			EntityClasses at: tagName asString put: (Smalltalk at: className) ].

	^EntityClasses ! !

!HtmlTag class methodsFor: 'parser support' stamp: 'ar 11/18/1998 19:27'!
initialize
	"HtmlTag initialize"
	EntityClasses := nil.! !

!HtmlTag class methodsFor: 'instance creation' stamp: 'ls 6/24/1998 18:54'!
source: source0  name: name0  negated: negated0 attribs: attribs0
	^(super forSource: source0) name: name0 negated: negated0 attribs: attribs0! !

!HtmlText methodsFor: 'formatter' stamp: 'ls 6/24/1998 17:29'!
addToFormatter: aFormatter
	"add ourself to a formatter"
	aFormatter addText: source! !

!HtmlText methodsFor: 'private-initialization' stamp: 'BG 11/15/2002 21:40'!
initialize: source0
	super initialize: source0.
	self text: source0 replaceHtmlCharRefs.! !

!HtmlText methodsFor: 'properties' stamp: 'ls 1/25/98 04:57'!
isText
	^true! !

!HtmlText methodsFor: 'access' stamp: 'ls 1/25/98 04:57'!
text
	^text
! !

!HtmlText methodsFor: 'private-initialization' stamp: 'sd 12/4/2010 22:47'!
text: text0
	text := text0.! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'bolot 11/2/1999 14:30'!
altText: aString
	"set the text to be displayed while downloading"
	altText := aString.
	aString ifNotNil: [self setBalloonText: aString].
	self setContents! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'ls 9/9/1998 06:59'!
defaultExtent: aPoint
	"set the size to use when the image hasn't yet downloaded"
	defaultExtent := aPoint! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'ar 11/19/1998 23:08'!
downloadStateIn: aScamper
	"download the image"
	| doc |
	doc := url retrieveContents.
	downloadQueue nextPut: doc.

! !

!DownloadingImageMorph methodsFor: 'accessing' stamp: 'bolot 2/27/2000 23:38'!
formatter
	^formatter! !

!DownloadingImageMorph methodsFor: 'accessing' stamp: 'bolot 2/27/2000 23:38'!
formatter: aFormatter
	formatter := aFormatter! !

!DownloadingImageMorph methodsFor: 'accessing' stamp: 'bolot 2/27/2000 23:34'!
imageMapName
	^imageMapName! !

!DownloadingImageMorph methodsFor: 'accessing' stamp: 'bolot 2/27/2000 23:35'!
imageMapName: aString
	imageMapName := aString! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'ls 9/15/1998 19:21'!
initialize
	super initialize.

	altText := '[image]'.
	self color: Color transparent.
	downloadQueue := SharedQueue new.! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'rk 7/6/2004 17:29'!
setContents
	"set up our morphic contents"
	| imageMorph imageMap |
	self removeAllMorphs.

	image ifNil: [^self setNoImageContents].

	defaultExtent isNil
		ifTrue: [(imageMorph := ImageMorph new) image: image]
		ifFalse: [imageMorph := SketchMorph withForm: image].
	(imageMapName notNil
	and: [formatter notNil
	and: [(imageMap := formatter imageMapNamed: imageMapName) notNil]])
		ifTrue: [imageMap buildImageMapForImage: imageMorph andBrowser: formatter browser].

	imageMorph position: self position.
	self addMorph: imageMorph.
	defaultExtent isNil
		ifFalse: [imageMorph extent: defaultExtent].
	self extent ~= imageMorph extent
		ifTrue: [	self extent: imageMorph extent ]! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'bolot 2/28/2000 00:14'!
setNoImageContents
	"set up our morphic contents in case image download/decoding failed"
	| stringMorph outlineMorph extent |
	altText isEmptyOrNil
		ifTrue: [ self extent: 0@0. "don't display anything..." ^self ].

	stringMorph := StringMorph new.
	stringMorph contents: altText.
	stringMorph position: self position+(2@2).
	self addMorph: stringMorph.

	outlineMorph := RectangleMorph new.
	outlineMorph borderWidth: 1.
	outlineMorph color: Color transparent.
	outlineMorph position: self position.

	"figure out how big to make the box"
	extent := defaultExtent ifNil: [ 0 @ 0 ].
	stringMorph width + 4 > extent x ifTrue: [
		extent := (stringMorph width + 4) @ extent y ].
	stringMorph height + 4 > extent y ifTrue: [
		extent := extent x @ (stringMorph height + 4) ].
	outlineMorph extent: extent.
	self addMorph: outlineMorph.

	self extent: outlineMorph extent
! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'rkris 7/12/2004 14:32'!
step
	| doc |
	downloadQueue size > 0 ifTrue: [
		doc := downloadQueue next.
		(doc notNil and: [doc mainType = 'image'])
		ifTrue: [
			[image := ImageReadWriter  formFromStream: doc contentStream binary]
				ifError: [:err :rcvr | "ignore" image := nil].
			self setContents ] ].! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'ls 9/15/1998 19:19'!
stepTime
	"this check doesn't need to be frequent"
	^500! !

!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'ls 9/5/1998 17:48'!
url: aUrl
	"set the url to download"
	url := aUrl asUrl.! !

!HtmlTokenizer methodsFor: 'stream protocol' stamp: 'ls 8/20/1998 05:15'!
atEnd
	"are there any more tokens?  This is equivalent to whether there is any more input"
	^(pos > text size)! !

!HtmlTokenizer methodsFor: 'private-initialization' stamp: 'pb 6/4/2012 17:27'!
initialize: s
	"text := s withSqueakLineEndings."
	text := s withCuisLineEndings.
	pos := 1.
	textAreaLevel := 0.! !

!HtmlTokenizer methodsFor: 'tokenizing' stamp: 'sd 12/4/2010 22:49'!
next 
	"return the next HtmlToken, or nil if there are no more"
	|token|
	"branch, depending on what the first character is"
	self atEnd ifTrue: [ ^nil ].
	self peekChar = $< 
		ifTrue: [ token := self nextTagOrComment ]
		ifFalse: [ token := self nextText ].


	"return the token, modulo modifications inside of textarea's"
	textAreaLevel > 0 ifTrue: [
		(token isTag and: [ token name = 'textarea' ]) ifTrue: [
			"textarea tag--change textAreaLevel accordingly"

			token isNegated
				ifTrue: [ textAreaLevel := textAreaLevel - 1 ]
				ifFalse: [ textAreaLevel := textAreaLevel -2 ].

			textAreaLevel > 0
				ifTrue: [ 
					"still inside a <textarea>, so convert this tag to text"
					^HtmlText forSource: token source ]
				ifFalse: [ "end of the textarea; return the tag"  ^token ] ].
			"end of the textarea"

		"inside the text area--return the token as text"
		^HtmlText forSource: token source ].

	(token isTag and: [ token isNegated not and: [ token name = 'textarea' ]]) ifTrue: [
		"beginning of a textarea"
		inTextArea := true.
		^token ].
	^token! !

!HtmlTokenizer methodsFor: 'private-tokenizing' stamp: 'sd 12/4/2010 22:50'!
nextAttributeValue
	"return the next sequence of alphanumeric characters; used to read in the value part of a tag's attribute, ie <tagname  attribname=attribvalue>"
	"because of the plethora of sloppy web pages, this is EXTREMELY tolerant"
	
	| c start end |
	"make sure there are at least two characters left"
	pos >= text size ifTrue: [ ^self nextChar asString ].

	"okay, peek at the first character"
	start := pos.
	c := text at: start.

	"check whether it's either kind of quote mark"
	(c = $" or: [ c = $' ]) ifTrue: [
		"yes--so find the matching quote mark"
		end := text indexOf: c startingAt: start+1 ifAbsent: [ text size + 1 ].
		pos := end+1.
		^text copyFrom: start to: end ].


	"no--go until a space or a $> is seen"
	end := text indexOfAnyOf: CSAttributeEnders startingAt: start ifAbsent: [ text size + 1 ].
	end := end - 1.
	pos := end + 1.
	^text copyFrom: start to: end.! !

!HtmlTokenizer methodsFor: 'private' stamp: 'sd 12/4/2010 22:51'!
nextChar
	| c |
	self atEnd ifTrue: [ ^nil ].
	c := text at: pos.
	pos := pos + 1.
	^c! !

!HtmlTokenizer methodsFor: 'private-tokenizing' stamp: 'sd 12/4/2010 22:52'!
nextComment
	"we've seen < and the next is a !!.  read until the whole comment is done"
	"this isn't perfectly correct--for instance <!!--> is considered a proper comment--but it should do for now.  It also picks up <!!DOCTYPE...> tags"
	
	| source c hyphens |
	self nextChar.   "swallow the $!!"
	source := WriteStream on: String new.
	source nextPutAll: '<!!'.
	self peekChar = $- ifFalse: [ 
		"this case is wierd.  go until we find a > at all and pray it's the correct end-of-'comment'"
		[	self atEnd or: [ self peekChar = $> ] 
		] whileFalse: [
			c := self nextChar.
			source nextPut: c 
		].
		self atEnd ifFalse: [ source nextPut: self nextChar ].
		^HtmlComment forSource: source contents ].
	hyphens := 0.

	[ 	c := self nextChar.
		c = nil or: [
			source nextPut: c.
			(hyphens >=2 and: [ c = $> ])]
	] whileFalse: [
		c = $- ifTrue: [ hyphens := hyphens + 1 ] ifFalse: [ hyphens := 0 ]
	].
		
	^HtmlComment forSource: source contents.
! !

!HtmlTokenizer methodsFor: 'private-tokenizing' stamp: 'sd 12/4/2010 22:57'!
nextName
	"return the next sequence of alphanumeric characters"
	"because of the plethora of sloppy web pages, this also accepts most non-space characters"
	| start end |
	start := pos.
	end := text indexOfAnyOf: CSNameEnders startingAt: start ifAbsent: [ text size + 1].
	end := end - 1.
	pos := end+1.
	^text copyFrom: start to: end! !

!HtmlTokenizer methodsFor: 'private-tokenizing' stamp: 'sd 12/4/2010 22:58'!
nextSpaces
	"read in as many consecutive space characters as possible"
	| start end |

	"short cut for a common case"
	self peekChar isSeparator not ifTrue: [ ^'' ].

	"find the start and end of the sequence of spaces"
	start := pos.
	end := text indexOfAnyOf: CSNonSeparators startingAt: start ifAbsent: [ text size + 1 ].
	end := end - 1.

	"update pos and return the sequence"
	pos := end + 1.
	^text copyFrom: start to: end! !

!HtmlTokenizer methodsFor: 'private-tokenizing' stamp: 'pb 5/31/2015 23:37'!
nextTag
	"we've seen a < and peek-ed something other than a !!.  Parse and return a tag"
	| source negated name attribs attribName attribValue sourceStart sourceEnd c |
	
	sourceStart := pos-1.
	attribs := Dictionary new.

	"determine if its negated"
	self peekChar = $/
		ifTrue: [ negated := true.  self nextChar. ]
		ifFalse: [ negated := false ].

	"read in the name"
	self skipSpaces.
	name := self nextName.
	name := name asLowercase.

	"read in any attributes"
	[ 	self skipSpaces.
		c := self peekChar.
		c = nil or: [c isLetter not ]
	] whileFalse: [
		attribName := self nextName.
		attribName := attribName asLowercase.
		self skipSpaces.
		self peekChar = $=
			ifTrue: [
				self nextChar.
				self skipSpaces.
				attribValue := self withoutQuoting: (self nextAttributeValue)  ]
			ifFalse: [ attribValue := '' ].
		attribs at: attribName  put: attribValue ].

	self skipSpaces.
	"determine if the tag is of the form <foo/>"
	self peekChar = $/ ifTrue: [ self nextChar. ].
	self skipSpaces.
	self peekChar = $> ifTrue: [ self nextChar ].

	sourceEnd := pos-1.
	source := text copyFrom: sourceStart to: sourceEnd.

	^HtmlTag source: source name: name asLowercase negated: negated attribs: attribs! !

!HtmlTokenizer methodsFor: 'private-tokenizing' stamp: 'ls 8/20/1998 05:31'!
nextTagOrComment
	"next character is a $<.  So read either a tag or a token"
	self nextChar.  "skip the $<"

	^self peekChar = $!! 
		ifTrue: [ self nextComment ]
		ifFalse: [ self nextTag ]

! !

!HtmlTokenizer methodsFor: 'private-tokenizing' stamp: 'ls 8/20/1998 05:34'!
nextText
	"returns the next textual segment"
	|start end|

	start := pos.
	end := (text indexOf: $< startingAt: start ifAbsent: [ text size + 1 ]) - 1.

	pos := end+1.
	^HtmlText forSource: (text copyFrom: start to: end)! !

!HtmlTokenizer methodsFor: 'private' stamp: 'ls 8/20/1998 05:14'!
peekChar
	self atEnd ifTrue: [ ^nil ].
	^text at: pos! !

!HtmlTokenizer methodsFor: 'private-tokenizing' stamp: 'ls 8/25/1998 05:26'!
skipSpaces
	"skip as many consecutive space characters as possible"
	pos := text indexOfAnyOf: CSNonSeparators startingAt: pos ifAbsent: [ text size + 1 ].! !

!HtmlTokenizer methodsFor: 'as yet unclassified' stamp: 'pb 6/1/2015 01:08'!
withoutQuoting: aString
	"Hack to reimplement String withoutQuoting..."
	"remove the initial and final quote marks, if present (and if matches nesting quotes)"
	"'''h''' withoutQuoting"
	| quote |
	self flag: #fixme.
	aString size < 2 ifTrue: [ ^ aString ].
	quote := aString first.
	(quote = aString last and: [
		quote = $' or: [ quote = $" ]])
		ifTrue: [ ^ aString
				copyFrom: 2
				to: aString size - 1 ]
		ifFalse: [ ^ aString ].! !

!HtmlTokenizer class methodsFor: 'initialization' stamp: 'pb 6/4/2012 16:41'!
initialize
	"HtmlTokenizer initialize"
	CSAttributeEnders := CharacterSet new.
	"CSAttributeEnders := CharacterSet empty."
	CSAttributeEnders addAll: Character separators.
	CSAttributeEnders add: $>.
	self flag: #pbfix.
	"broken"
	"CSNameEnders := CharacterSet empty."
	CSNameEnders := CharacterSet new.
	CSNameEnders addAll: '=>'.
	CSNameEnders addAll: Character separators.
	CSNonSeparators := CharacterSet separators complement.! !

!HtmlTokenizer class methodsFor: 'instance creation' stamp: 'ls 8/20/1998 05:13'!
on: aStream
	^super basicNew initialize: aStream contents! !
HtmlEntity initialize!
HtmlEmbedded initialize!
HtmlFormatter initialize!
HtmlTag initialize!
HtmlTokenizer initialize!
