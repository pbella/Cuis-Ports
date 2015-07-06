'From Cuis 4.2 of 25 July 2013 [latest update: #2400] on 6 July 2015 at 4:29:03.102449 am'!
'Description Originally based on OpenGL-Core-jrd.6.mcz and have merged in some of the changes from OpenGL-Core-bf.17.mcz.  Requires 3DTransform and FFI.
'!
!provides: 'OpenGL' 1 2!
!requires: 'FFI' 1 8 nil!
!requires: '3DTransform' 1 1 nil!
!classDefinition: #GLExtConstants category: #'OpenGL-Pools'!
SharedPool subclass: #GLExtConstants
	instanceVariableNames: ''
	classVariableNames: 'GLArrayBufferARB GLArrayBufferBindingARB GLArrayElementLockCountEXT GLArrayElementLockFirstEXT GLBoolARB GLBoolVec2ARB GLBoolVec3ARB GLBoolVec4ARB GLBufferAccessARB GLBufferMappedARB GLBufferSizeARB GLBufferUsageARB GLColorArrayBufferBindingARB GLColorAttachment0EXT GLColorAttachment10EXT GLColorAttachment11EXT GLColorAttachment12EXT GLColorAttachment13EXT GLColorAttachment14EXT GLColorAttachment15EXT GLColorAttachment1EXT GLColorAttachment2EXT GLColorAttachment3EXT GLColorAttachment4EXT GLColorAttachment5EXT GLColorAttachment6EXT GLColorAttachment7EXT GLColorAttachment8EXT GLColorAttachment9EXT GLColorSumARB GLCompressedAlphaARB GLCompressedIntensityARB GLCompressedLuminanceARB GLCompressedLuminanceAlphaARB GLCompressedRgbARB GLCompressedRgbS3tcDxt1EXT GLCompressedRgbaARB GLCompressedRgbaS3tcDxt1EXT GLCompressedRgbaS3tcDxt3EXT GLCompressedRgbaS3tcDxt5EXT GLCompressedTextureFormatsARB GLCurrentMatrixARB GLCurrentMatrixStackDepthARB GLCurrentVertexAttribARB GLDepthAttachmentEXT GLDrawFramebufferBindingEXT GLDrawFramebufferEXT GLDynamicCopyARB GLDynamicDrawARB GLDynamicReadARB GLEdgeFlagArrayBufferBindingARB GLElementArrayBufferARB GLElementArrayBufferBindingARB GLFloatMat2ARB GLFloatMat3ARB GLFloatMat4ARB GLFloatVec2ARB GLFloatVec3ARB GLFloatVec4ARB GLFogCoordinateArrayBufferBindingARB GLFragmentShaderARB GLFragmentShaderDerivativeHintARB GLFramebufferAttachmentObjectNameEXT GLFramebufferAttachmentObjectTypeEXT GLFramebufferAttachmentTexture3dZoffsetEXT GLFramebufferAttachmentTextureCubeMapFaceEXT GLFramebufferAttachmentTextureLevelEXT GLFramebufferBindingEXT GLFramebufferCompleteEXT GLFramebufferEXT GLFramebufferIncompleteAttachmentEXT GLFramebufferIncompleteDimensionsEXT GLFramebufferIncompleteDrawBufferEXT GLFramebufferIncompleteFormatsEXT GLFramebufferIncompleteMissingAttachmentEXT GLFramebufferIncompleteReadBufferEXT GLFramebufferUnsupportedEXT GLGenerateMipmapHintSGIS GLGenerateMipmapSGIS GLIndexArrayBufferBindingARB GLIntVec2ARB GLIntVec3ARB GLIntVec4ARB GLInvalidFramebufferOperationEXT GLMatrix0ARB GLMatrix10ARB GLMatrix11ARB GLMatrix12ARB GLMatrix13ARB GLMatrix14ARB GLMatrix15ARB GLMatrix16ARB GLMatrix17ARB GLMatrix18ARB GLMatrix19ARB GLMatrix1ARB GLMatrix20ARB GLMatrix21ARB GLMatrix22ARB GLMatrix23ARB GLMatrix24ARB GLMatrix25ARB GLMatrix26ARB GLMatrix27ARB GLMatrix28ARB GLMatrix29ARB GLMatrix2ARB GLMatrix30ARB GLMatrix31ARB GLMatrix3ARB GLMatrix4ARB GLMatrix5ARB GLMatrix6ARB GLMatrix7ARB GLMatrix8ARB GLMatrix9ARB GLMaxColorAttachmentsEXT GLMaxCombinedTextureImageUnitsARB GLMaxFragmentUniformComponentsARB GLMaxProgramAddressRegistersARB GLMaxProgramAttribsARB GLMaxProgramEnvParametersARB GLMaxProgramInstructionsARB GLMaxProgramLocalParametersARB GLMaxProgramMatricesARB GLMaxProgramMatrixStackDepthARB GLMaxProgramNativeAddressRegistersARB GLMaxProgramNativeAttribsARB GLMaxProgramNativeInstructionsARB GLMaxProgramNativeParametersARB GLMaxProgramNativeTemporariesARB GLMaxProgramParametersARB GLMaxProgramTemporariesARB GLMaxRenderbufferSizeEXT GLMaxTextureCoordsARB GLMaxTextureImageUnitsARB GLMaxVaryingFloatsARB GLMaxVertexAttribsARB GLMaxVertexTextureImageUnitsARB GLMaxVertexUniformComponentsARB GLMultisampleARB GLMultisampleFilterHintNV GLNormalArrayBufferBindingARB GLNumCompressedTextureFormatsARB GLObjectActiveAttributeMaxLengthARB GLObjectActiveAttributesARB GLObjectActiveUniformMaxLengthARB GLObjectActiveUniformsARB GLObjectAttachedObjectsARB GLObjectCompileStatusARB GLObjectDeleteStatusARB GLObjectInfoLogLengthARB GLObjectLinkStatusARB GLObjectShaderSourceLengthARB GLObjectSubtypeARB GLObjectTypeARB GLObjectValidateStatusARB GLProgramAddressRegistersARB GLProgramAttribsARB GLProgramBindingARB GLProgramErrorPositionARB GLProgramErrorStringARB GLProgramFormatARB GLProgramFormatAsciiARB GLProgramInstructionsARB GLProgramLengthARB GLProgramNativeAddressRegistersARB GLProgramNativeAttribsARB GLProgramNativeInstructionsARB GLProgramNativeParametersARB GLProgramNativeTemporariesARB GLProgramObjectARB GLProgramParametersARB GLProgramStringARB GLProgramTemporariesARB GLProgramUnderNativeLimitsARB GLReadFramebufferBindingEXT GLReadFramebufferEXT GLReadOnlyARB GLReadWriteARB GLRenderbufferAlphaSizeEXT GLRenderbufferBindingEXT GLRenderbufferBlueSizeEXT GLRenderbufferDepthSizeEXT GLRenderbufferEXT GLRenderbufferGreenSizeEXT GLRenderbufferHeightEXT GLRenderbufferInternalFormatEXT GLRenderbufferRedSizeEXT GLRenderbufferStencilSizeEXT GLRenderbufferWidthEXT GLSampler1dARB GLSampler1dShadowARB GLSampler2dARB GLSampler2dRectARB GLSampler2dRectShadowARB GLSampler2dShadowARB GLSampler3dARB GLSamplerCubeARB GLSecondaryColorArrayBufferBindingARB GLShaderObjectARB GLStaticCopyARB GLStaticDrawARB GLStaticReadARB GLStencilAttachmentEXT GLStencilIndex16EXT GLStencilIndex1EXT GLStencilIndex4EXT GLStencilIndex8EXT GLStreamCopyARB GLStreamDrawARB GLStreamReadARB GLTextureCompressedARB GLTextureCompressedImageSizeARB GLTextureCompressionHintARB GLTextureCoordArrayBufferBindingARB GLTransposeCurrentMatrixARB GLVertexArrayBufferBindingARB GLVertexAttribArrayBufferBindingARB GLVertexAttribArrayEnabledARB GLVertexAttribArrayNormalizedARB GLVertexAttribArrayPointerARB GLVertexAttribArraySizeARB GLVertexAttribArrayStrideARB GLVertexAttribArrayTypeARB GLVertexProgramARB GLVertexProgramPointSizeARB GLVertexProgramTwoSideARB GLVertexShaderARB GLWeightArrayBufferBindingARB GLWriteOnlyARB'
	poolDictionaries: ''
	category: 'OpenGL-Pools'!
!classDefinition: 'GLExtConstants class' category: #'OpenGL-Pools'!
GLExtConstants class
	instanceVariableNames: ''!

!classDefinition: #OpenGLConstants category: #'OpenGL-Pools'!
SharedPool subclass: #OpenGLConstants
	instanceVariableNames: ''
	classVariableNames: 'GL2Bytes GL2d GL3Bytes GL3d GL3dColor GL3dColorTexture GL4Bytes GL4dColorTexture GLAbgrExt GLAccum GLAccumAlphaBits GLAccumBlueBits GLAccumBufferBit GLAccumClearValue GLAccumGreenBits GLAccumRedBits GLActiveTextureArb GLAdd GLAddSignedExt GLAliasedLineWidthRange GLAliasedPointSizeRange GLAllAttribBits GLAlpha GLAlpha12 GLAlpha16 GLAlpha4 GLAlpha8 GLAlphaBias GLAlphaBits GLAlphaScale GLAlphaTest GLAlphaTestFunc GLAlphaTestRef GLAlways GLAmbient GLAmbientAndDiffuse GLAnd GLAndInverted GLAndReverse GLAttribStackDepth GLAutoNormal GLAux0 GLAux1 GLAux2 GLAux3 GLAuxBuffers GLBack GLBackLeft GLBackRight GLBgr GLBgra GLBitmap GLBitmapToken GLBlend GLBlendColor GLBlendColorExt GLBlendDst GLBlendEquation GLBlendEquationExt GLBlendSrc GLBlue GLBlueBias GLBlueBits GLBlueScale GLByte GLC3fV3f GLC4fN3fV3f GLC4ubV2f GLC4ubV3f GLCcw GLClamp GLClampToEdge GLClear GLClientActiveTextureArb GLClientAllAttribBits GLClientAttribStackDepth GLClientPixelStoreBit GLClientVertexArrayBit GLClipPlane0 GLClipPlane1 GLClipPlane2 GLClipPlane3 GLClipPlane4 GLClipPlane5 GLCoeff GLColor GLColorArray GLColorArrayPointer GLColorArraySize GLColorArrayStride GLColorArrayType GLColorBufferBit GLColorClearValue GLColorIndex GLColorIndexes GLColorLogicOp GLColorMaterial GLColorMaterialFace GLColorMaterialParameter GLColorMatrix GLColorMatrixStackDepth GLColorTable GLColorTableAlphaSize GLColorTableBias GLColorTableBlueSize GLColorTableFormat GLColorTableGreenSize GLColorTableIntensitySize GLColorTableLuminanceSize GLColorTableRedSize GLColorTableScale GLColorTableWidth GLColorWritemask GLCombineAlphaExt GLCombineExt GLCombineRgbExt GLCompile GLCompileAndExecute GLConstantAlpha GLConstantAlphaExt GLConstantAttenuation GLConstantBorder GLConstantColor GLConstantColorExt GLConstantExt GLConvolution1d GLConvolution2d GLConvolutionBorderColor GLConvolutionBorderMode GLConvolutionFilterBias GLConvolutionFilterScale GLConvolutionFormat GLConvolutionHeight GLConvolutionWidth GLCopy GLCopyInverted GLCopyPixelToken GLCullFace GLCullFaceMode GLCurrentBit GLCurrentColor GLCurrentIndex GLCurrentNormal GLCurrentRasterColor GLCurrentRasterDistance GLCurrentRasterIndex GLCurrentRasterPosition GLCurrentRasterPositionValid GLCurrentRasterTextureCoords GLCurrentTextureCoords GLCw GLDecal GLDecr GLDepth GLDepthBias GLDepthBits GLDepthBufferBit GLDepthClearValue GLDepthComponent GLDepthFunc GLDepthRange GLDepthScale GLDepthTest GLDepthWritemask GLDiffuse GLDither GLDomain GLDontCare GLDouble GLDoublebuffer GLDrawBuffer GLDrawPixelToken GLDstAlpha GLDstColor GLEdgeFlag GLEdgeFlagArray GLEdgeFlagArrayPointer GLEdgeFlagArrayStride GLEmission GLEnableBit GLEqual GLEquiv GLEvalBit GLExp GLExp2 GLExtAbgr GLExtBlendColor GLExtBlendMinmax GLExtBlendSubtract GLExtTextureEnvAdd GLExtTextureEnvCombine GLExtensions GLEyeLinear GLEyePlane GLFalse GLFastest GLFeedback GLFeedbackBufferPointer GLFeedbackBufferSize GLFeedbackBufferType GLFill GLFlat GLFloat GLFog GLFogBit GLFogColor GLFogDensity GLFogEnd GLFogHint GLFogIndex GLFogMode GLFogStart GLFront GLFrontAndBack GLFrontFace GLFrontLeft GLFrontRight GLFuncAdd GLFuncAddExt GLFuncReverseSubtract GLFuncReverseSubtractExt GLFuncSubtract GLFuncSubtractExt GLGequal GLGreater GLGreen GLGreenBias GLGreenBits GLGreenScale GLHintBit GLHistogram GLHistogramAlphaSize GLHistogramBlueSize GLHistogramFormat GLHistogramGreenSize GLHistogramLuminanceSize GLHistogramRedSize GLHistogramSink GLHistogramWidth GLIncr GLIndexArray GLIndexArrayPointer GLIndexArrayStride GLIndexArrayType GLIndexBits GLIndexClearValue GLIndexLogicOp GLIndexMode GLIndexOffset GLIndexShift GLIndexWritemask GLInt GLIntensity GLIntensity12 GLIntensity16 GLIntensity4 GLIntensity8 GLInterpolateExt GLInvalidEnum GLInvalidOperation GLInvalidValue GLInvert GLKeep GLLeft GLLequal GLLess GLLight0 GLLight1 GLLight2 GLLight3 GLLight4 GLLight5 GLLight6 GLLight7 GLLightModelAmbient GLLightModelColorControl GLLightModelLocalViewer GLLightModelTwoSide GLLighting GLLightingBit GLLine GLLineBit GLLineLoop GLLineResetToken GLLineSmooth GLLineSmoothHint GLLineStipple GLLineStipplePattern GLLineStippleRepeat GLLineStrip GLLineToken GLLineWidth GLLineWidthGranularity GLLineWidthRange GLLinear GLLinearAttenuation GLLinearMipmapLinear GLLinearMipmapNearest GLLines GLListBase GLListBit GLListIndex GLListMode GLLoad GLLogicOp GLLogicOpMode GLLuminance GLLuminance12 GLLuminance12Alpha12 GLLuminance12Alpha4 GLLuminance16 GLLuminance16Alpha16 GLLuminance4 GLLuminance4Alpha4 GLLuminance6Alpha2 GLLuminance8 GLLuminance8Alpha8 GLLuminanceAlpha GLMap1Color4 GLMap1GridDomain GLMap1GridSegments GLMap1Index GLMap1Normal GLMap1TextureCoord1 GLMap1TextureCoord2 GLMap1TextureCoord3 GLMap1TextureCoord4 GLMap1Vertex3 GLMap1Vertex4 GLMap2Color4 GLMap2GridDomain GLMap2GridSegments GLMap2Index GLMap2Normal GLMap2TextureCoord1 GLMap2TextureCoord2 GLMap2TextureCoord3 GLMap2TextureCoord4 GLMap2Vertex3 GLMap2Vertex4 GLMapColor GLMapStencil GLMatrixMode GLMax GLMax3dTextureSize GLMaxAttribStackDepth GLMaxClientAttribStackDepth GLMaxClipPlanes GLMaxColorMatrixStackDepth GLMaxConvolutionHeight GLMaxConvolutionWidth GLMaxElementsIndices GLMaxElementsVertices GLMaxEvalOrder GLMaxExt GLMaxLights GLMaxListNesting GLMaxModelviewStackDepth GLMaxNameStackDepth GLMaxPixelMapTable GLMaxProjectionStackDepth GLMaxTextureSize GLMaxTextureStackDepth GLMaxTextureUnitsArb GLMaxViewportDims GLMin GLMinExt GLMinmax GLMinmaxFormat GLMinmaxSink GLModelview GLModelviewMatrix GLModelviewStackDepth GLModulate GLMult GLN3fV3f GLNameStackDepth GLNand GLNearest GLNearestMipmapLinear GLNearestMipmapNearest GLNever GLNicest GLNoError GLNone GLNoop GLNor GLNormalArray GLNormalArrayPointer GLNormalArrayStride GLNormalArrayType GLNormalize GLNotequal GLObjectLinear GLObjectPlane GLOne GLOneMinusConstantAlpha GLOneMinusConstantAlphaExt GLOneMinusConstantColor GLOneMinusConstantColorExt GLOneMinusDstAlpha GLOneMinusDstColor GLOneMinusSrcAlpha GLOneMinusSrcColor GLOperand0AlphaExt GLOperand0RgbExt GLOperand1AlphaExt GLOperand1RgbExt GLOperand2AlphaExt GLOperand2RgbExt GLOr GLOrInverted GLOrReverse GLOrder GLOutOfMemory GLPackAlignment GLPackImageHeight GLPackLsbFirst GLPackRowLength GLPackSkipImages GLPackSkipPixels GLPackSkipRows GLPackSwapBytes GLPassThroughToken GLPerspectiveCorrectionHint GLPixelMapAToA GLPixelMapAToASize GLPixelMapBToB GLPixelMapBToBSize GLPixelMapGToG GLPixelMapGToGSize GLPixelMapIToA GLPixelMapIToASize GLPixelMapIToB GLPixelMapIToBSize GLPixelMapIToG GLPixelMapIToGSize GLPixelMapIToI GLPixelMapIToISize GLPixelMapIToR GLPixelMapIToRSize GLPixelMapRToR GLPixelMapRToRSize GLPixelMapSToS GLPixelMapSToSSize GLPixelModeBit GLPoint GLPointBit GLPointSize GLPointSizeGranularity GLPointSizeRange GLPointSmooth GLPointSmoothHint GLPointToken GLPoints GLPolygon GLPolygonBit GLPolygonMode GLPolygonOffsetFactor GLPolygonOffsetFill GLPolygonOffsetLine GLPolygonOffsetPoint GLPolygonOffsetUnits GLPolygonSmooth GLPolygonSmoothHint GLPolygonStipple GLPolygonStippleBit GLPolygonToken GLPosition GLPostColorMatrixAlphaBias GLPostColorMatrixAlphaScale GLPostColorMatrixBlueBias GLPostColorMatrixBlueScale GLPostColorMatrixColorTable GLPostColorMatrixGreenBias GLPostColorMatrixGreenScale GLPostColorMatrixRedBias GLPostColorMatrixRedScale GLPostConvolutionAlphaBias GLPostConvolutionAlphaScale GLPostConvolutionBlueBias GLPostConvolutionBlueScale GLPostConvolutionColorTable GLPostConvolutionGreenBias GLPostConvolutionGreenScale GLPostConvolutionRedBias GLPostConvolutionRedScale GLPreviousExt GLPrimaryColorExt GLProjection GLProjectionMatrix GLProjectionStackDepth GLProxyColorTable GLProxyHistogram GLProxyPostColorMatrixColorTable GLProxyPostConvolutionColorTable GLProxyTexture1d GLProxyTexture2d GLProxyTexture3d GLQ GLQuadStrip GLQuadraticAttenuation GLQuads GLR GLR3G3B2 GLReadBuffer GLRed GLRedBias GLRedBits GLRedScale GLReduce GLRender GLRenderMode GLRenderer GLRepeat GLReplace GLReplicateBorder GLRescaleNormal GLReturn GLRgb GLRgb10 GLRgb10A2 GLRgb12 GLRgb16 GLRgb4 GLRgb5 GLRgb5A1 GLRgb8 GLRgbScaleExt GLRgba GLRgba12 GLRgba16 GLRgba2 GLRgba4 GLRgba8 GLRgbaMode GLRight GLS GLScissorBit GLScissorBox GLScissorTest GLSelect GLSelectionBufferPointer GLSelectionBufferSize GLSeparable2d GLSeparateSpecularColor GLSet GLShadeModel GLShininess GLShort GLSingleColor GLSmooth GLSmoothLineWidthGranularity GLSmoothLineWidthRange GLSmoothPointSizeGranularity GLSmoothPointSizeRange GLSource0AlphaExt GLSource0RgbExt GLSource1AlphaExt GLSource1RgbExt GLSource2AlphaExt GLSource2RgbExt GLSpecular GLSphereMap GLSpotCutoff GLSpotDirection GLSpotExponent GLSrcAlpha GLSrcAlphaSaturate GLSrcColor GLStackOverflow GLStackUnderflow GLStencil GLStencilBits GLStencilBufferBit GLStencilClearValue GLStencilFail GLStencilFunc GLStencilIndex GLStencilPassDepthFail GLStencilPassDepthPass GLStencilRef GLStencilTest GLStencilValueMask GLStencilWritemask GLStereo GLSubpixelBits GLT GLT2fC3fV3f GLT2fC4fN3fV3f GLT2fC4ubV3f GLT2fN3fV3f GLT2fV3f GLT4fC4fN3fV4f GLT4fV4f GLTableTooLarge GLTexture GLTexture0Arb GLTexture10Arb GLTexture11Arb GLTexture12Arb GLTexture13Arb GLTexture14Arb GLTexture15Arb GLTexture16Arb GLTexture17Arb GLTexture18Arb GLTexture19Arb GLTexture1Arb GLTexture1d GLTexture20Arb GLTexture21Arb GLTexture22Arb GLTexture23Arb GLTexture24Arb GLTexture25Arb GLTexture26Arb GLTexture27Arb GLTexture28Arb GLTexture29Arb GLTexture2Arb GLTexture2d GLTexture30Arb GLTexture31Arb GLTexture3Arb GLTexture3d GLTexture4Arb GLTexture5Arb GLTexture6Arb GLTexture7Arb GLTexture8Arb GLTexture9Arb GLTextureAlphaSize GLTextureBaseLevel GLTextureBinding1d GLTextureBinding2d GLTextureBinding3d GLTextureBit GLTextureBlueSize GLTextureBorder GLTextureBorderColor GLTextureComponents GLTextureCoordArray GLTextureCoordArrayPointer GLTextureCoordArraySize GLTextureCoordArrayStride GLTextureCoordArrayType GLTextureDepth GLTextureEnv GLTextureEnvColor GLTextureEnvMode GLTextureGenMode GLTextureGenQ GLTextureGenR GLTextureGenS GLTextureGenT GLTextureGreenSize GLTextureHeight GLTextureIntensitySize GLTextureInternalFormat GLTextureLuminanceSize GLTextureMagFilter GLTextureMatrix GLTextureMaxLevel GLTextureMaxLod GLTextureMinFilter GLTextureMinLod GLTexturePriority GLTextureRedSize GLTextureResident GLTextureStackDepth GLTextureWidth GLTextureWrapR GLTextureWrapS GLTextureWrapT GLTransformBit GLTriangleFan GLTriangleStrip GLTriangles GLTrue GLUnpackAlignment GLUnpackImageHeight GLUnpackLsbFirst GLUnpackRowLength GLUnpackSkipImages GLUnpackSkipPixels GLUnpackSkipRows GLUnpackSwapBytes GLUnsignedByte GLUnsignedByte233Rev GLUnsignedByte332 GLUnsignedInt GLUnsignedInt1010102 GLUnsignedInt2101010Rev GLUnsignedInt8888 GLUnsignedInt8888Rev GLUnsignedShort GLUnsignedShort1555Rev GLUnsignedShort4444 GLUnsignedShort4444Rev GLUnsignedShort5551 GLUnsignedShort565 GLUnsignedShort565Rev GLV2f GLV3f GLVendor GLVersion GLVertexArray GLVertexArrayPointer GLVertexArraySize GLVertexArrayStride GLVertexArrayType GLViewport GLViewportBit GLXor GLZero GLZoomX GLZoomY TAlpha TAlphaNoAlpha TNoAlpha'
	poolDictionaries: ''
	category: 'OpenGL-Pools'!
!classDefinition: 'OpenGLConstants class' category: #'OpenGL-Pools'!
OpenGLConstants class
	instanceVariableNames: ''!

!classDefinition: #OGLExtManager category: #'OpenGL-Core'!
Object subclass: #OGLExtManager
	instanceVariableNames: 'ogl'
	classVariableNames: ''
	poolDictionaries: 'GLExtConstants'
	category: 'OpenGL-Core'!
!classDefinition: 'OGLExtManager class' category: #'OpenGL-Core'!
OGLExtManager class
	instanceVariableNames: ''!

!classDefinition: #OGLFontManager category: #'OpenGL-Core'!
Object subclass: #OGLFontManager
	instanceVariableNames: 'glx fonts charList'
	classVariableNames: ''
	poolDictionaries: 'OpenGLConstants'
	category: 'OpenGL-Core'!
!classDefinition: 'OGLFontManager class' category: #'OpenGL-Core'!
OGLFontManager class
	instanceVariableNames: ''!

!classDefinition: #OGLShaderManager category: #'OpenGL-Core'!
Object subclass: #OGLShaderManager
	instanceVariableNames: 'ogl programObjects'
	classVariableNames: ''
	poolDictionaries: 'OpenGLConstants GLExtConstants'
	category: 'OpenGL-Core'!
!classDefinition: 'OGLShaderManager class' category: #'OpenGL-Core'!
OGLShaderManager class
	instanceVariableNames: ''!

!classDefinition: #OGLTextureHandle category: #'OpenGL-Core'!
Object subclass: #OGLTextureHandle
	instanceVariableNames: 'glID objectID target timeStamp scaledSize bytesUsed allocated'
	classVariableNames: ''
	poolDictionaries: 'OpenGLConstants'
	category: 'OpenGL-Core'!
!classDefinition: 'OGLTextureHandle class' category: #'OpenGL-Core'!
OGLTextureHandle class
	instanceVariableNames: ''!

!classDefinition: #OGLTextureManager category: #'OpenGL-Core'!
Object subclass: #OGLTextureManager
	instanceVariableNames: 'ogl textures memAvail memUsed memFrame compressTextures'
	classVariableNames: ''
	poolDictionaries: 'OpenGLConstants GLExtConstants'
	category: 'OpenGL-Core'!
!classDefinition: 'OGLTextureManager class' category: #'OpenGL-Core'!
OGLTextureManager class
	instanceVariableNames: ''!

!classDefinition: #OpenGL category: #'OpenGL-Core'!
Object subclass: #OpenGL
	instanceVariableNames: 'handle bufRect glExt extensions frontFace maxPortalDepth changeTexture test timeStamp formManager textureManager fontManager shaderManager glListRegistry isMirror inPortal3D camera forceWire numVtx numPrims inGLBlock distance harness eventPointer avatar forcePick forceHilite suppressPortals noSwap fogOn transparency matrixStack portalDepth bufferObjects'
	classVariableNames: 'InstalledOpenGLLibrary'
	poolDictionaries: 'OpenGLConstants GLExtConstants'
	category: 'OpenGL-Core'!
!classDefinition: 'OpenGL class' category: #'OpenGL-Core'!
OpenGL class
	instanceVariableNames: 'glErrors'!

!classDefinition: #OGLMacOSX category: #'OpenGL-Core'!
OpenGL subclass: #OGLMacOSX
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenGL-Core'!
!classDefinition: 'OGLMacOSX class' category: #'OpenGL-Core'!
OGLMacOSX class
	instanceVariableNames: ''!

!classDefinition: #OGLMacOS9 category: #'OpenGL-Core'!
OGLMacOSX subclass: #OGLMacOS9
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenGL-Core'!
!classDefinition: 'OGLMacOS9 class' category: #'OpenGL-Core'!
OGLMacOS9 class
	instanceVariableNames: ''!

!classDefinition: #OGLUnix category: #'OpenGL-Core'!
OpenGL subclass: #OGLUnix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenGL-Core'!
!classDefinition: 'OGLUnix class' category: #'OpenGL-Core'!
OGLUnix class
	instanceVariableNames: ''!

!classDefinition: #OGLUnixX11BE category: #'OpenGL-Core'!
OGLUnix subclass: #OGLUnixX11BE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenGL-Core'!
!classDefinition: 'OGLUnixX11BE class' category: #'OpenGL-Core'!
OGLUnixX11BE class
	instanceVariableNames: ''!

!classDefinition: #OGLUnixX11LE category: #'OpenGL-Core'!
OGLUnix subclass: #OGLUnixX11LE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenGL-Core'!
!classDefinition: 'OGLUnixX11LE class' category: #'OpenGL-Core'!
OGLUnixX11LE class
	instanceVariableNames: ''!

!classDefinition: #OGLUnixQuartz category: #'OpenGL-Core'!
OpenGL subclass: #OGLUnixQuartz
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenGL-Core'!
!classDefinition: 'OGLUnixQuartz class' category: #'OpenGL-Core'!
OGLUnixQuartz class
	instanceVariableNames: ''!

!classDefinition: #OGLWin32 category: #'OpenGL-Core'!
OpenGL subclass: #OGLWin32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OpenGL-Core'!
!classDefinition: 'OGLWin32 class' category: #'OpenGL-Core'!
OGLWin32 class
	instanceVariableNames: ''!


!OGLExtManager commentStamp: '<historical>' prior: 0!
OGLExtManager handles OpenGL extension functions for OGL. Since extensions are specific for particular renderer/drivers all extensions must be looked up dynamically. I provide the technical means to handle extensions transparently for any number of contexts.

Declaring Extensions:
========================
Extension functions (and constants) have to be specifically declared. To declare functions and constants for an extension you need to do the following:
#1: Go to the CLASS side of OGLExtManager and add a category that has the same name as listed in glGet(GL_EXTENSIONS). For example, to use the ARB imaging extension this category must be named GL_ARB_imaging (even though the extension is referred to as 'ARB imaging' glGet will tell us that the name GL_ARB_imaging so that is what you need to use).
WARNING: The name must match EXACTLY, no extra spaces, watch for small and capital letters etc.

#2: Add a method which initializes the constants in this extension. The method itself MUST follow the convention to begin with 'initialize' and should then use the extension name. E.g., for initializing the constants in the ARB_imaging it should be called 'initializeArbImaging'.

The constants itself can be initialized by just copying them from the spec describing them and use the provided utility methods for initialization (just look over a few existing extensions). I am trying to make it easy for you to just copy those constants.

Note that all constants appear ONLY in the GLExtConstants - OpenGLConstants is exclusively used for standard OpenGL constants.

#3: Add the functions the extension defines. Generally, these should just be plain ffi call methods but there are three important issues:
	a) NEVER provide a 'module' for these functions. Since they are looked up by opengl specific means other ways are used and you MUST NOT provide a module. The extension this particular function is contained in is defined by the category and not by the module (this is to prevent confusion about 'I have no GL_ARB_imaging.dll' or even worse the possibility that on some system any such thing even exists (!!)).
	b) NEVER do anything but just the plain FFI call (optionally followed by a plain return or call to #externalCallFailed). The method you are writing will actually be run in an entirely different place - you are only providing a template for the sake of your convenience (and speed). If you need more sophisticated error handling do this in the place where you call the method or provide a helper in OGL or something similar, but NEVER EVER DO THIS HERE.
	c) The calling convention is effectively ignored. Since it is platform specific it will in fact be replaced by the appropriate OS calling convention when used.

#4: Evaluate 'OGLExtManager initialize' which will add the constants to GLExtConstants and compile forwarder methods in OGL.

Using Extensions:
====================
Once you declared the extension, using it is simple. Since the constants defined by the extension are accessible through the GLExtConstants pool, you can just refer to them by name.

To invoke an extension method you simply invoke it via ogl, e.g., in order to invoke the 'glUnlockArraysEXT' function you would use something like:
	ogl glUnlockArraysEXT
etc.

IMPORTANT: You must never ever attempt to create a OGLExtManager explicitly. The OGLExtManager is transparently wrapped in OGL.

Implementation details:
===========================
In order to implement the dynamic lookup mechanism in the most convenient way, the OGLExtManager is always created as a 'unique subclass' of OGLExtManager. That is when you ask for a new extension manager using 'OGLExtManager new' you actually get a subclass of OGLExtManager which is denoted by an asterisk in front of it so it looks as *OGLExtManager.

The *OGLExtManager does not understand any of the function you have provided. When it runs into a message which it doesn't understand it performs the following functions:
1: First it looks if that method is in fact an extension method you defined.
2: If it is, it looks if the extension this method belongs to is present in the renderer it is bound to (e.g., the OGLExtManager's ogl inst var)
3: If the extension is present it looks at the functions you declared for this extension, and for each function
	- it copies the template method
	- it looks up calling convention / address of the function
	- it installs a new ffi call spec for that method
	- it adds this method to *OGLExtManager
4: Once all the function for the extension have been loaded it reinvokes the message which failed.

Preloading extensions:
=========================
As you can see from the implementation details, there is a certain overhead involved in handling extensions. In particular, there can be a noticable speed impact when an extension is loaded 'on demand' (e.g., when a message is not understood). To compensate for this, extensions can be loaded explicitly, by using, e.g.,
	ogl loadExtension: #'GL_ARB_imaging'.

Providing extensions as changesets:
=========================================
When creating a changeset to add a particular OpenGL extension, you should not include any of the auto-generated changes (neither methods nor constants) to OpenGL, GLExtConstants, or OGLExtManager.  In particular, including the changed GLExtConstants class definition will cause those who load the changeset to lose the constants for other extensions they may have loaded.

Instead, only include the initialization methods on the class side of OGLExtManager (eg: #initializeArbVertexProgram and #generateArbVertexProgramFunctions), and perhaps a test for the extension in OpenGL (eg: OpenGL>>hasArbVertexProgram).  Use code in the changeset postscript to generate extension functions and constants.  Continuing with the vertex program example, the postscript might look like this:

"Postscript:
Generate extension functions and initialize extension constants. Use a temporary changeset so we do not clutter up this one. And provide initials to prevent asking the user"

Utilities useAuthorInitials: 'generated' during: [
	| cs |
	cs _ Utilities useChangeSetNamed: 'generated' during: [
		OGLExtManager 
			generateArbVertexProgramFunctions;
			initialize].
	cs preambleString: '"catch-all changeset for generated functions"'.
]!

!OGLFontManager commentStamp: '<historical>' prior: 0!
I manage fonts (and their caching) on a particular OpenGL object.!

!OGLTextureHandle commentStamp: '<historical>' prior: 0!
Class TextureHandle represents a texture handle for internal use by the texture manager.

Instance variables:
	glID			<Integer>	The 'texture name' of OpenGL associated with this texture
	target		<Integer>	The OpenGL target (e.g., GLTexture2d)
	timeStamp	<Integer>	The stamp for the frame when this texture was last used
	scaledSize	<Point>		The ultimate size the texture needs to be scaled to (power of two)
	bytesUsed	<Integer>	The number of bytes associated with this texture on the graphics hardware
	allocated 	<Boolean>	True if the texture is currently allocated on the graphics hardware, false if not.
	displayLists <Set> 		Set of all of the OGL display lists that use this texture!

!OpenGL commentStamp: 'das 12/27/2005 23:43' prior: 0!
This is the main interface to the OpenGL library. 

displayList
displayListAlpha

David A. Smith!

!OGLMacOSX commentStamp: '<historical>' prior: 0!
I represent the Mac OS X OpenGL version.!

!OGLMacOS9 commentStamp: '<historical>' prior: 0!
I represent the Mac OS 9 OpenGL version.!

!OGLUnix commentStamp: '<historical>' prior: 0!
I represent the generic Unix OpenGL version.!

!OGLUnixX11BE commentStamp: '<historical>' prior: 0!
I represent the big endian Unix OpenGL version.!

!OGLUnixX11LE commentStamp: '<historical>' prior: 0!
I represent the little endian Unix OpenGL version.!

!OGLUnixQuartz commentStamp: '<historical>' prior: 0!
I represent the Mac Unix (Quartz) OpenGL version.!

!OGLWin32 commentStamp: '<historical>' prior: 0!
I represent the Win32 OpenGL version.!

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'ar 5/20/2003 11:11'!
initialize
	"GLExtConstants initialize"
	self class selectors do:[:each|
		(each ~~ #initialize and:[each beginsWith: 'initialize']) ifTrue:[
			self perform: each.
		].
	].! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 2/14/2010 15:33'!
initializeArbFragmentShader
	"This method was automatically generated by OGLExtManager"
	GLFragmentShaderARB := 35632.
	GLMaxFragmentUniformComponentsARB := 35657.
	GLMaxTextureCoordsARB := 34929.
	GLMaxTextureImageUnitsARB := 34930.
	GLFragmentShaderDerivativeHintARB := 35723.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 2/14/2010 15:33'!
initializeArbMutlisample
	"This method was automatically generated by OGLExtManager"
	GLMultisampleARB := 32925.
	GLMultisampleFilterHintNV := 34100.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 2/14/2010 15:33'!
initializeArbShaderObjects
	"This method was automatically generated by OGLExtManager"
	GLProgramObjectARB := 35648.
	GLShaderObjectARB := 35656.
	GLObjectTypeARB := 35662.
	GLObjectSubtypeARB := 35663.
	GLFloatVec2ARB := 35664.
	GLFloatVec3ARB := 35665.
	GLFloatVec4ARB := 35666.
	GLIntVec2ARB := 35667.
	GLIntVec3ARB := 35668.
	GLIntVec4ARB := 35669.
	GLBoolARB := 35670.
	GLBoolVec2ARB := 35671.
	GLBoolVec3ARB := 35672.
	GLBoolVec4ARB := 35673.
	GLFloatMat2ARB := 35674.
	GLFloatMat3ARB := 35675.
	GLFloatMat4ARB := 35676.
	GLSampler1dARB := 35677.
	GLSampler2dARB := 35678.
	GLSampler3dARB := 35679.
	GLSamplerCubeARB := 35680.
	GLSampler1dShadowARB := 35681.
	GLSampler2dShadowARB := 35682.
	GLSampler2dRectARB := 35683.
	GLSampler2dRectShadowARB := 35684.
	GLObjectDeleteStatusARB := 35712.
	GLObjectCompileStatusARB := 35713.
	GLObjectLinkStatusARB := 35714.
	GLObjectValidateStatusARB := 35715.
	GLObjectInfoLogLengthARB := 35716.
	GLObjectAttachedObjectsARB := 35717.
	GLObjectActiveUniformsARB := 35718.
	GLObjectActiveUniformMaxLengthARB := 35719.
	GLObjectShaderSourceLengthARB := 35720.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 6/5/2012 22:53'!
initializeArbTextureCompression
	"This method was automatically generated by OGLExtManager"
	GLCompressedAlphaARB := 34025.
	GLCompressedLuminanceARB := 34026.
	GLCompressedLuminanceAlphaARB := 34027.
	GLCompressedIntensityARB := 34028.
	GLCompressedRgbARB := 34029.
	GLCompressedRgbaARB := 34030.
	GLTextureCompressionHintARB := 34031.
	GLTextureCompressedImageSizeARB := 34464.
	GLTextureCompressedARB := 34465.
	GLNumCompressedTextureFormatsARB := 34466.
	GLCompressedTextureFormatsARB := 34467.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 6/5/2012 22:53'!
initializeArbTransposeMatrix
	"This method was automatically generated by OGLExtManager"! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 6/5/2012 22:53'!
initializeArbVertexBufferObject
	"This method was automatically generated by OGLExtManager"
	GLArrayBufferARB := 34962.
	GLElementArrayBufferARB := 34963.
	GLArrayBufferBindingARB := 34964.
	GLElementArrayBufferBindingARB := 34965.
	GLVertexArrayBufferBindingARB := 34966.
	GLNormalArrayBufferBindingARB := 34967.
	GLColorArrayBufferBindingARB := 34968.
	GLIndexArrayBufferBindingARB := 34969.
	GLTextureCoordArrayBufferBindingARB := 34970.
	GLEdgeFlagArrayBufferBindingARB := 34971.
	GLSecondaryColorArrayBufferBindingARB := 34972.
	GLFogCoordinateArrayBufferBindingARB := 34973.
	GLWeightArrayBufferBindingARB := 34974.
	GLVertexAttribArrayBufferBindingARB := 34975.
	GLStreamDrawARB := 35040.
	GLStreamReadARB := 35041.
	GLStreamCopyARB := 35042.
	GLStaticDrawARB := 35044.
	GLStaticReadARB := 35045.
	GLStaticCopyARB := 35046.
	GLDynamicDrawARB := 35048.
	GLDynamicReadARB := 35049.
	GLDynamicCopyARB := 35050.
	GLReadOnlyARB := 35000.
	GLWriteOnlyARB := 35001.
	GLReadWriteARB := 35002.
	GLBufferSizeARB := 34660.
	GLBufferUsageARB := 34661.
	GLBufferAccessARB := 35003.
	GLBufferMappedARB := 35004.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 6/5/2012 22:53'!
initializeArbVertexProgram
	"This method was automatically generated by OGLExtManager"
	GLVertexProgramARB := 34336.
	GLVertexProgramPointSizeARB := 34370.
	GLVertexProgramTwoSideARB := 34371.
	GLColorSumARB := 33880.
	GLProgramFormatAsciiARB := 34933.
	GLVertexAttribArrayEnabledARB := 34338.
	GLVertexAttribArraySizeARB := 34339.
	GLVertexAttribArrayStrideARB := 34340.
	GLVertexAttribArrayTypeARB := 34341.
	GLVertexAttribArrayNormalizedARB := 34922.
	GLCurrentVertexAttribARB := 34342.
	GLVertexAttribArrayPointerARB := 34373.
	GLProgramLengthARB := 34343.
	GLProgramFormatARB := 34934.
	GLProgramBindingARB := 34423.
	GLProgramInstructionsARB := 34976.
	GLMaxProgramInstructionsARB := 34977.
	GLProgramNativeInstructionsARB := 34978.
	GLMaxProgramNativeInstructionsARB := 34979.
	GLProgramTemporariesARB := 34980.
	GLMaxProgramTemporariesARB := 34981.
	GLProgramNativeTemporariesARB := 34982.
	GLMaxProgramNativeTemporariesARB := 34983.
	GLProgramParametersARB := 34984.
	GLMaxProgramParametersARB := 34985.
	GLProgramNativeParametersARB := 34986.
	GLMaxProgramNativeParametersARB := 34987.
	GLProgramAttribsARB := 34988.
	GLMaxProgramAttribsARB := 34989.
	GLProgramNativeAttribsARB := 34990.
	GLMaxProgramNativeAttribsARB := 34991.
	GLProgramAddressRegistersARB := 34992.
	GLMaxProgramAddressRegistersARB := 34993.
	GLProgramNativeAddressRegistersARB := 34994.
	GLMaxProgramNativeAddressRegistersARB := 34995.
	GLMaxProgramLocalParametersARB := 34996.
	GLMaxProgramEnvParametersARB := 34997.
	GLProgramUnderNativeLimitsARB := 34998.
	GLProgramStringARB := 34344.
	GLProgramErrorPositionARB := 34379.
	GLCurrentMatrixARB := 34369.
	GLTransposeCurrentMatrixARB := 34999.
	GLCurrentMatrixStackDepthARB := 34368.
	GLMaxVertexAttribsARB := 34921.
	GLMaxProgramMatricesARB := 34351.
	GLMaxProgramMatrixStackDepthARB := 34350.
	GLProgramErrorStringARB := 34932.
	GLMatrix0ARB := 35008.
	GLMatrix1ARB := 35009.
	GLMatrix2ARB := 35010.
	GLMatrix3ARB := 35011.
	GLMatrix4ARB := 35012.
	GLMatrix5ARB := 35013.
	GLMatrix6ARB := 35014.
	GLMatrix7ARB := 35015.
	GLMatrix8ARB := 35016.
	GLMatrix9ARB := 35017.
	GLMatrix10ARB := 35018.
	GLMatrix11ARB := 35019.
	GLMatrix12ARB := 35020.
	GLMatrix13ARB := 35021.
	GLMatrix14ARB := 35022.
	GLMatrix15ARB := 35023.
	GLMatrix16ARB := 35024.
	GLMatrix17ARB := 35025.
	GLMatrix18ARB := 35026.
	GLMatrix19ARB := 35027.
	GLMatrix20ARB := 35028.
	GLMatrix21ARB := 35029.
	GLMatrix22ARB := 35030.
	GLMatrix23ARB := 35031.
	GLMatrix24ARB := 35032.
	GLMatrix25ARB := 35033.
	GLMatrix26ARB := 35034.
	GLMatrix27ARB := 35035.
	GLMatrix28ARB := 35036.
	GLMatrix29ARB := 35037.
	GLMatrix30ARB := 35038.
	GLMatrix31ARB := 35039.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 2/14/2010 15:33'!
initializeArbVertexShader
	"This method was automatically generated by OGLExtManager"
	GLVertexShaderARB := 35633.
	GLMaxVertexUniformComponentsARB := 35658.
	GLMaxVaryingFloatsARB := 35659.
	GLMaxVertexTextureImageUnitsARB := 35660.
	GLMaxCombinedTextureImageUnitsARB := 35661.
	GLObjectActiveAttributesARB := 35721.
	GLObjectActiveAttributeMaxLengthARB := 35722.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 6/5/2012 22:53'!
initializeExtCompiledVertexArray
	"This method was automatically generated by OGLExtManager"
	GLArrayElementLockCountEXT := 33193.
	GLArrayElementLockFirstEXT := 33192.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 2/14/2010 15:33'!
initializeExtFramebufferBlit
	"This method was automatically generated by OGLExtManager"
	GLReadFramebufferEXT := 36008.
	GLDrawFramebufferEXT := 36009.
	GLReadFramebufferBindingEXT := 36006.
	GLDrawFramebufferBindingEXT := 36010.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 2/14/2010 15:33'!
initializeExtFramebufferObjects
	"This method was automatically generated by OGLExtManager"
	GLInvalidFramebufferOperationEXT := 1286.
	GLMaxRenderbufferSizeEXT := 34024.
	GLFramebufferBindingEXT := 36006.
	GLRenderbufferBindingEXT := 36007.
	GLFramebufferAttachmentObjectTypeEXT := 36048.
	GLFramebufferAttachmentObjectNameEXT := 36049.
	GLFramebufferAttachmentTextureLevelEXT := 36050.
	GLFramebufferAttachmentTextureCubeMapFaceEXT := 36051.
	GLFramebufferAttachmentTexture3dZoffsetEXT := 36052.
	GLFramebufferCompleteEXT := 36053.
	GLFramebufferIncompleteAttachmentEXT := 36054.
	GLFramebufferIncompleteMissingAttachmentEXT := 36055.
	GLFramebufferIncompleteDimensionsEXT := 36057.
	GLFramebufferIncompleteFormatsEXT := 36058.
	GLFramebufferIncompleteDrawBufferEXT := 36059.
	GLFramebufferIncompleteReadBufferEXT := 36060.
	GLFramebufferUnsupportedEXT := 36061.
	GLMaxColorAttachmentsEXT := 36063.
	GLColorAttachment0EXT := 36064.
	GLColorAttachment1EXT := 36065.
	GLColorAttachment2EXT := 36066.
	GLColorAttachment3EXT := 36067.
	GLColorAttachment4EXT := 36068.
	GLColorAttachment5EXT := 36069.
	GLColorAttachment6EXT := 36070.
	GLColorAttachment7EXT := 36071.
	GLColorAttachment8EXT := 36072.
	GLColorAttachment9EXT := 36073.
	GLColorAttachment10EXT := 36074.
	GLColorAttachment11EXT := 36075.
	GLColorAttachment12EXT := 36076.
	GLColorAttachment13EXT := 36077.
	GLColorAttachment14EXT := 36078.
	GLColorAttachment15EXT := 36079.
	GLDepthAttachmentEXT := 36096.
	GLStencilAttachmentEXT := 36128.
	GLFramebufferEXT := 36160.
	GLRenderbufferEXT := 36161.
	GLRenderbufferWidthEXT := 36162.
	GLRenderbufferHeightEXT := 36163.
	GLRenderbufferInternalFormatEXT := 36164.
	GLStencilIndex1EXT := 36166.
	GLStencilIndex4EXT := 36167.
	GLStencilIndex8EXT := 36168.
	GLStencilIndex16EXT := 36169.
	GLRenderbufferRedSizeEXT := 36176.
	GLRenderbufferGreenSizeEXT := 36177.
	GLRenderbufferBlueSizeEXT := 36178.
	GLRenderbufferAlphaSizeEXT := 36179.
	GLRenderbufferDepthSizeEXT := 36180.
	GLRenderbufferStencilSizeEXT := 36181.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 6/5/2012 22:53'!
initializeExtTextureCompressionS3tc
	"This method was automatically generated by OGLExtManager"
	GLCompressedRgbS3tcDxt1EXT := 33776.
	GLCompressedRgbaS3tcDxt1EXT := 33777.
	GLCompressedRgbaS3tcDxt3EXT := 33778.
	GLCompressedRgbaS3tcDxt5EXT := 33779.! !

!GLExtConstants class methodsFor: 'pool initialization' stamp: 'pb 6/5/2012 22:53'!
initializeSgisGenerateMipmap
	"This method was automatically generated by OGLExtManager"
	GLGenerateMipmapSGIS := 33169.
	GLGenerateMipmapHintSGIS := 33170.! !

!OpenGLConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 17:03'!
init1
	"OpenGLConstants initialize"
	TAlpha := 1.
	TNoAlpha := 2.
	TAlphaNoAlpha := 3.
	GLAccum := 16r0100.
	GLLoad := 16r0101.
	GLReturn := 16r0102.
	GLMult := 16r0103.
	GLAdd := 16r0104.
	GLNever := 16r0200.
	GLLess := 16r0201.
	GLEqual := 16r0202.
	GLLequal := 16r0203.
	GLGreater := 16r0204.
	GLNotequal := 16r0205.
	GLGequal := 16r0206.
	GLAlways := 16r0207.
	GLCurrentBit := 16r00000001.
	GLPointBit := 16r00000002.
	GLLineBit := 16r00000004.
	GLPolygonBit := 16r00000008.
	GLPolygonStippleBit := 16r00000010.
	GLPixelModeBit := 16r00000020.
	GLLightingBit := 16r00000040.
	GLFogBit := 16r00000080.
	GLDepthBufferBit := 16r00000100.
	GLAccumBufferBit := 16r00000200.
	GLStencilBufferBit := 16r00000400.
	GLViewportBit := 16r00000800.
	GLTransformBit := 16r00001000.
	GLEnableBit := 16r00002000.
	GLColorBufferBit := 16r00004000.
	GLHintBit := 16r00008000.
	GLEvalBit := 16r00010000.
	GLListBit := 16r00020000.
	GLTextureBit := 16r00040000.
	GLScissorBit := 16r00080000.
	GLAllAttribBits := 16r000FFFF.
	GLPoints := 16r0000.
	GLLines := 16r0001.
	GLLineLoop := 16r0002.
	GLLineStrip := 16r0003.
	GLTriangles := 16r0004.
	GLTriangleStrip := 16r0005.
	GLTriangleFan := 16r0006.
	GLQuads := 16r0007.
	GLQuadStrip := 16r0008.
	GLPolygon := 16r0009.
	GLZero := 0.
	GLOne := 1.
	GLSrcColor := 16r0300.
	GLOneMinusSrcColor := 16r0301.
	GLSrcAlpha := 16r0302.
	GLOneMinusSrcAlpha := 16r0303.
	GLDstAlpha := 16r0304.
	GLOneMinusDstAlpha := 16r0305.
	GLDstColor := 16r0306.
	GLOneMinusDstColor := 16r0307.
	GLSrcAlphaSaturate := 16r0308.
	GLTrue := 1.
	GLFalse := 0.
	GLClipPlane0 := 16r3000.
	GLClipPlane1 := 16r3001.
	GLClipPlane2 := 16r3002.
	GLClipPlane3 := 16r3003.
	GLClipPlane4 := 16r3004.
	GLClipPlane5 := 16r3005.
	GLByte := 16r1400.
	GLUnsignedByte := 16r1401.
	GLShort := 16r1402.
	GLUnsignedShort := 16r1403.
	GLInt := 16r1404.
	GLUnsignedInt := 16r1405.
	GLFloat := 16r1406.
	GL2Bytes := 16r1407.
	GL3Bytes := 16r1408.
	GL4Bytes := 16r1409.
	GLDouble := 16r140A.
	GLNone := 0.
	GLFrontLeft := 16r0400.
	GLFrontRight := 16r0401.
	GLBackLeft := 16r0402.
	GLBackRight := 16r0403.
	GLFront := 16r0404.
	GLBack := 16r0405.
	GLLeft := 16r0406.
	GLRight := 16r0407.
	GLFrontAndBack := 16r0408.
	GLAux0 := 16r0409.
	GLAux1 := 16r040A.
	GLAux2 := 16r040B.
	GLAux3 := 16r040C.
	GLNoError := 0.
	GLInvalidEnum := 16r0500.
	GLInvalidValue := 16r0501.
	GLInvalidOperation := 16r0502.
	GLStackOverflow := 16r0503.
	GLStackUnderflow := 16r0504.
	GLOutOfMemory := 16r0505.
	GL2d := 16r0600.
	GL3d := 16r0601.
	GL3dColor := 16r0602.
	GL3dColorTexture := 16r0603.
	GL4dColorTexture := 16r0604.
	GLPassThroughToken := 16r0700.
	GLPointToken := 16r0701.
	GLLineToken := 16r0702.
	GLPolygonToken := 16r0703.
	GLBitmapToken := 16r0704.
	GLDrawPixelToken := 16r0705.
	GLCopyPixelToken := 16r0706.
	GLLineResetToken := 16r0707.
	GLExp := 16r0800.
	GLExp2 := 16r0801.
	GLCw := 16r0900.
	GLCcw := 16r0901.
	GLCoeff := 16r0A00.
	GLOrder := 16r0A01.
	GLDomain := 16r0A02.
	GLCurrentColor := 16r0B00.
	GLCurrentIndex := 16r0B01.
	GLCurrentNormal := 16r0B02.
	GLCurrentTextureCoords := 16r0B03.
	GLCurrentRasterColor := 16r0B04.
	GLCurrentRasterIndex := 16r0B05.
	GLCurrentRasterTextureCoords := 16r0B06.
	GLCurrentRasterPosition := 16r0B07.
	GLCurrentRasterPositionValid := 16r0B08.
	GLCurrentRasterDistance := 16r0B09.
	GLPointSmooth := 16r0B10.
	GLPointSize := 16r0B11.
	GLPointSizeRange := 16r0B12.
	GLPointSizeGranularity := 16r0B13.
! !

!OpenGLConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 16:59'!
init2
	GLLineSmooth := 16r0B20.
	GLLineWidth := 16r0B21.
	GLLineWidthRange := 16r0B22.
	GLLineWidthGranularity := 16r0B23.
	GLLineStipple := 16r0B24.
	GLLineStipplePattern := 16r0B25.
	GLLineStippleRepeat := 16r0B26.
	GLListMode := 16r0B30.
	GLMaxListNesting := 16r0B31.
	GLListBase := 16r0B32.
	GLListIndex := 16r0B33.
	GLPolygonMode := 16r0B40.
	GLPolygonSmooth := 16r0B41.
	GLPolygonStipple := 16r0B42.
	GLEdgeFlag := 16r0B43.
	GLCullFace := 16r0B44.
	GLCullFaceMode := 16r0B45.
	GLFrontFace := 16r0B46.
	GLLighting := 16r0B50.
	GLLightModelLocalViewer := 16r0B51.
	GLLightModelTwoSide := 16r0B52.
	GLLightModelAmbient := 16r0B53.
	GLShadeModel := 16r0B54.
	GLColorMaterialFace := 16r0B55.
	GLColorMaterialParameter := 16r0B56.
	GLColorMaterial := 16r0B57.
	GLFog := 16r0B60.
	GLFogIndex := 16r0B61.
	GLFogDensity := 16r0B62.
	GLFogStart := 16r0B63.
	GLFogEnd := 16r0B64.
	GLFogMode := 16r0B65.
	GLFogColor := 16r0B66.
	GLDepthRange := 16r0B70.
	GLDepthTest := 16r0B71.
	GLDepthWritemask := 16r0B72.
	GLDepthClearValue := 16r0B73.
	GLDepthFunc := 16r0B74.
	GLAccumClearValue := 16r0B80.
	GLStencilTest := 16r0B90.
	GLStencilClearValue := 16r0B91.
	GLStencilFunc := 16r0B92.
	GLStencilValueMask := 16r0B93.
	GLStencilFail := 16r0B94.
	GLStencilPassDepthFail := 16r0B95.
	GLStencilPassDepthPass := 16r0B96.
	GLStencilRef := 16r0B97.
	GLStencilWritemask := 16r0B98.
	GLMatrixMode := 16r0BA0.
	GLNormalize := 16r0BA1.
	GLViewport := 16r0BA2.
	GLModelviewStackDepth := 16r0BA3.
	GLProjectionStackDepth := 16r0BA4.
	GLTextureStackDepth := 16r0BA5.
	GLModelviewMatrix := 16r0BA6.
	GLProjectionMatrix := 16r0BA7.
	GLTextureMatrix := 16r0BA8.
	GLAttribStackDepth := 16r0BB0.
	GLClientAttribStackDepth := 16r0BB1.
	GLAlphaTest := 16r0BC0.
	GLAlphaTestFunc := 16r0BC1.
	GLAlphaTestRef := 16r0BC2.
	GLDither := 16r0BD0.
	GLBlendDst := 16r0BE0.
	GLBlendSrc := 16r0BE1.
	GLBlend := 16r0BE2.
	GLLogicOpMode := 16r0BF0.
	GLIndexLogicOp := 16r0BF1.
	GLColorLogicOp := 16r0BF2.
	GLAuxBuffers := 16r0C00.
	GLDrawBuffer := 16r0C01.
	GLReadBuffer := 16r0C02.
	GLScissorBox := 16r0C10.
	GLScissorTest := 16r0C11.
	GLIndexClearValue := 16r0C20.
	GLIndexWritemask := 16r0C21.
	GLColorClearValue := 16r0C22.
	GLColorWritemask := 16r0C23.
	GLIndexMode := 16r0C30.
	GLRgbaMode := 16r0C31.
	GLDoublebuffer := 16r0C32.
	GLStereo := 16r0C33.
	GLRenderMode := 16r0C40.
	GLPerspectiveCorrectionHint := 16r0C50.
	GLPointSmoothHint := 16r0C51.
	GLLineSmoothHint := 16r0C52.
	GLPolygonSmoothHint := 16r0C53.
	GLFogHint := 16r0C54.
	GLTextureGenS := 16r0C60.
	GLTextureGenT := 16r0C61.
	GLTextureGenR := 16r0C62.
	GLTextureGenQ := 16r0C63.
	GLPixelMapIToI := 16r0C70.
	GLPixelMapSToS := 16r0C71.
	GLPixelMapIToR := 16r0C72.
	GLPixelMapIToG := 16r0C73.
	GLPixelMapIToB := 16r0C74.
	GLPixelMapIToA := 16r0C75.
	GLPixelMapRToR := 16r0C76.
	GLPixelMapGToG := 16r0C77.
	GLPixelMapBToB := 16r0C78.
	GLPixelMapAToA := 16r0C79.
	GLPixelMapIToISize := 16r0CB0.
	GLPixelMapSToSSize := 16r0CB1.
	GLPixelMapIToRSize := 16r0CB2.
	GLPixelMapIToGSize := 16r0CB3.
	GLPixelMapIToBSize := 16r0CB4.
	GLPixelMapIToASize := 16r0CB5.
	GLPixelMapRToRSize := 16r0CB6.
	GLPixelMapGToGSize := 16r0CB7.
	GLPixelMapBToBSize := 16r0CB8.
	GLPixelMapAToASize := 16r0CB9.
	GLUnpackSwapBytes := 16r0CF0.
	GLUnpackLsbFirst := 16r0CF1.
	GLUnpackRowLength := 16r0CF2.
	GLUnpackSkipRows := 16r0CF3.
	GLUnpackSkipPixels := 16r0CF4.
	GLUnpackAlignment := 16r0CF5.
	GLPackSwapBytes := 16r0D00.
	GLPackLsbFirst := 16r0D01.
	GLPackRowLength := 16r0D02.
	GLPackSkipRows := 16r0D03.
	GLPackSkipPixels := 16r0D04.
	GLPackAlignment := 16r0D05.
! !

!OpenGLConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 17:00'!
init3
	GLMapColor := 16r0D10.
	GLMapStencil := 16r0D11.
	GLIndexShift := 16r0D12.
	GLIndexOffset := 16r0D13.
	GLRedScale := 16r0D14.
	GLRedBias := 16r0D15.
	GLZoomX := 16r0D16.
	GLZoomY := 16r0D17.
	GLGreenScale := 16r0D18.
	GLGreenBias := 16r0D19.
	GLBlueScale := 16r0D1A.
	GLBlueBias := 16r0D1B.
	GLAlphaScale := 16r0D1C.
	GLAlphaBias := 16r0D1D.
	GLDepthScale := 16r0D1E.
	GLDepthBias := 16r0D1F.
	GLMaxEvalOrder := 16r0D30.
	GLMaxLights := 16r0D31.
	GLMaxClipPlanes := 16r0D32.
	GLMaxTextureSize := 16r0D33.
	GLMaxPixelMapTable := 16r0D34.
	GLMaxAttribStackDepth := 16r0D35.
	GLMaxModelviewStackDepth := 16r0D36.
	GLMaxNameStackDepth := 16r0D37.
	GLMaxProjectionStackDepth := 16r0D38.
	GLMaxTextureStackDepth := 16r0D39.
	GLMaxViewportDims := 16r0D3A.
	GLMaxClientAttribStackDepth := 16r0D3B.
	GLSubpixelBits := 16r0D50.
	GLIndexBits := 16r0D51.
	GLRedBits := 16r0D52.
	GLGreenBits := 16r0D53.
	GLBlueBits := 16r0D54.
	GLAlphaBits := 16r0D55.
	GLDepthBits := 16r0D56.
	GLStencilBits := 16r0D57.
	GLAccumRedBits := 16r0D58.
	GLAccumGreenBits := 16r0D59.
	GLAccumBlueBits := 16r0D5A.
	GLAccumAlphaBits := 16r0D5B.
	GLNameStackDepth := 16r0D70.
	GLAutoNormal := 16r0D80.
	GLMap1Color4 := 16r0D90.
	GLMap1Index := 16r0D91.
	GLMap1Normal := 16r0D92.
	GLMap1TextureCoord1 := 16r0D93.
	GLMap1TextureCoord2 := 16r0D94.
	GLMap1TextureCoord3 := 16r0D95.
	GLMap1TextureCoord4 := 16r0D96.
	GLMap1Vertex3 := 16r0D97.
	GLMap1Vertex4 := 16r0D98.
	GLMap2Color4 := 16r0DB0.
	GLMap2Index := 16r0DB1.
	GLMap2Normal := 16r0DB2.
	GLMap2TextureCoord1 := 16r0DB3.
	GLMap2TextureCoord2 := 16r0DB4.
	GLMap2TextureCoord3 := 16r0DB5.
	GLMap2TextureCoord4 := 16r0DB6.
	GLMap2Vertex3 := 16r0DB7.
	GLMap2Vertex4 := 16r0DB8.
	GLMap1GridDomain := 16r0DD0.
	GLMap1GridSegments := 16r0DD1.
	GLMap2GridDomain := 16r0DD2.
	GLMap2GridSegments := 16r0DD3.
	GLTexture1d := 16r0DE0.
	GLTexture2d := 16r0DE1.

	GLFeedbackBufferPointer := 16r0DF0.
	GLFeedbackBufferSize := 16r0DF1.
	GLFeedbackBufferType := 16r0DF2.
	GLSelectionBufferPointer := 16r0DF3.
	GLSelectionBufferSize := 16r0DF4.
	GLTextureWidth := 16r1000.
	GLTextureHeight := 16r1001.
	GLTextureInternalFormat := 16r1003.
	GLTextureBorderColor := 16r1004.
	GLTextureBorder := 16r1005.
	GLDontCare := 16r1100.
	GLFastest := 16r1101.
	GLNicest := 16r1102.
	GLLight0 := 16r4000.
	GLLight1 := 16r4001.
	GLLight2 := 16r4002.
	GLLight3 := 16r4003.
	GLLight4 := 16r4004.
	GLLight5 := 16r4005.
	GLLight6 := 16r4006.
	GLLight7 := 16r4007.
	GLAmbient := 16r1200.
	GLDiffuse := 16r1201.
	GLSpecular := 16r1202.
	GLPosition := 16r1203.
	GLSpotDirection := 16r1204.
	GLSpotExponent := 16r1205.
	GLSpotCutoff := 16r1206.
	GLConstantAttenuation := 16r1207.
	GLLinearAttenuation := 16r1208.
	GLQuadraticAttenuation := 16r1209.
	GLCompile := 16r1300.
	GLCompileAndExecute := 16r1301.
	GLClear := 16r1500.
	GLAnd := 16r1501.
	GLAndReverse := 16r1502.
	GLCopy := 16r1503.
	GLAndInverted := 16r1504.
	GLNoop := 16r1505.
	GLXor := 16r1506.
	GLOr := 16r1507.
	GLNor := 16r1508.
	GLEquiv := 16r1509.
	GLInvert := 16r150A.
	GLOrReverse := 16r150B.
	GLCopyInverted := 16r150C.
	GLOrInverted := 16r150D.
	GLNand := 16r150E.
	GLSet := 16r150F.
! !

!OpenGLConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 17:00'!
init4
	GLEmission := 16r1600.
	GLShininess := 16r1601.
	GLAmbientAndDiffuse := 16r1602.
	GLColorIndexes := 16r1603.
	GLModelview := 16r1700.
	GLProjection := 16r1701.
	GLTexture := 16r1702.
	GLColor := 16r1800.
	GLDepth := 16r1801.
	GLStencil := 16r1802.
	GLColorIndex := 16r1900.
	GLStencilIndex := 16r1901.
	GLDepthComponent := 16r1902.
	GLRed := 16r1903.
	GLGreen := 16r1904.
	GLBlue := 16r1905.
	GLAlpha := 16r1906.
	GLRgb := 16r1907.
	GLRgba := 16r1908.
	GLLuminance := 16r1909.
	GLLuminanceAlpha := 16r190A.
	GLBitmap := 16r1A00.
	GLPoint := 16r1B00.
	GLLine := 16r1B01.
	GLFill := 16r1B02.
	GLRender := 16r1C00.
	GLFeedback := 16r1C01.
	GLSelect := 16r1C02.
	GLFlat := 16r1D00.
	GLSmooth := 16r1D01.
	GLKeep := 16r1E00.
	GLReplace := 16r1E01.
	GLIncr := 16r1E02.
	GLDecr := 16r1E03.
	GLVendor := 16r1F00.
	GLRenderer := 16r1F01.
	GLVersion := 16r1F02.
	GLExtensions := 16r1F03.
	GLS := 16r2000.
	GLT := 16r2001.
	GLR := 16r2002.
	GLQ := 16r2003.
	GLModulate := 16r2100.
	GLDecal := 16r2101.
	GLTextureEnvMode := 16r2200.
	GLTextureEnvColor := 16r2201.
	GLTextureEnv := 16r2300.
	GLEyeLinear := 16r2400.
	GLObjectLinear := 16r2401.
	GLSphereMap := 16r2402.
	GLTextureGenMode := 16r2500.
	GLObjectPlane := 16r2501.
	GLEyePlane := 16r2502.
	GLNearest := 16r2600.
	GLLinear := 16r2601.
	GLNearestMipmapNearest := 16r2700.
	GLLinearMipmapNearest := 16r2701.
	GLNearestMipmapLinear := 16r2702.
	GLLinearMipmapLinear := 16r2703.
	GLTextureMagFilter := 16r2800.
	GLTextureMinFilter := 16r2801.
	GLTextureWrapS := 16r2802.
	GLTextureWrapT := 16r2803.
	GLClamp := 16r2900.
	GLRepeat := 16r2901.
	GLClientPixelStoreBit := 16r00000001.
	GLClientVertexArrayBit := 16r00000002.
	GLClientAllAttribBits := 16rFFFFFFFF.
	GLPolygonOffsetFactor := 16r8038.
	GLPolygonOffsetUnits := 16r2A00.
	GLPolygonOffsetPoint := 16r2A01.
	GLPolygonOffsetLine := 16r2A02.
	GLPolygonOffsetFill := 16r8037.
	GLAlpha4 := 16r803B.
	GLAlpha8 := 16r803C.
	GLAlpha12 := 16r803D.
	GLAlpha16 := 16r803E.
	GLLuminance4 := 16r803F.
	GLLuminance8 := 16r8040.
	GLLuminance12 := 16r8041.
	GLLuminance16 := 16r8042.
	GLLuminance4Alpha4 := 16r8043.
	GLLuminance6Alpha2 := 16r8044.
	GLLuminance8Alpha8 := 16r8045.
	GLLuminance12Alpha4 := 16r8046.
	GLLuminance12Alpha12 := 16r8047.
	GLLuminance16Alpha16 := 16r8048.
	GLIntensity := 16r8049.
	GLIntensity4 := 16r804A.
	GLIntensity8 := 16r804B.
	GLIntensity12 := 16r804C.
	GLIntensity16 := 16r804D.
	GLR3G3B2 := 16r2A10.
	GLRgb4 := 16r804F.
	GLRgb5 := 16r8050.
	GLRgb8 := 16r8051.
	GLRgb10 := 16r8052.
	GLRgb12 := 16r8053.
	GLRgb16 := 16r8054.
	GLRgba2 := 16r8055.
	GLRgba4 := 16r8056.
	GLRgb5A1 := 16r8057.
	GLRgba8 := 16r8058.
	GLRgb10A2 := 16r8059.
	GLRgba12 := 16r805A.
	GLRgba16 := 16r805B.
	GLTextureRedSize := 16r805C.
	GLTextureGreenSize := 16r805D.
	GLTextureBlueSize := 16r805E.
	GLTextureAlphaSize := 16r805F.
	GLTextureLuminanceSize := 16r8060.
	GLTextureIntensitySize := 16r8061.
	GLProxyTexture1d := 16r8063.
	GLProxyTexture2d := 16r8064.
	GLTexturePriority := 16r8066.
	GLTextureResident := 16r8067.
	GLTextureBinding1d := 16r8068.
	GLTextureBinding2d := 16r8069.
	GLTextureBinding3d := 16r806A.
! !

!OpenGLConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 17:01'!
init5
	GLVertexArray := 16r8074.
	GLNormalArray := 16r8075.
	GLColorArray := 16r8076.
	GLIndexArray := 16r8077.
	GLTextureCoordArray := 16r8078.
	GLEdgeFlagArray := 16r8079.
	GLVertexArraySize := 16r807A.
	GLVertexArrayType := 16r807B.
	GLVertexArrayStride := 16r807C.
	GLNormalArrayType := 16r807E.
	GLNormalArrayStride := 16r807F.
	GLColorArraySize := 16r8081.
	GLColorArrayType := 16r8082.
	GLColorArrayStride := 16r8083.
	GLIndexArrayType := 16r8085.
	GLIndexArrayStride := 16r8086.
	GLTextureCoordArraySize := 16r8088.
	GLTextureCoordArrayType := 16r8089.
	GLTextureCoordArrayStride := 16r808A.
	GLEdgeFlagArrayStride := 16r808C.
	GLVertexArrayPointer := 16r808E.
	GLNormalArrayPointer := 16r808F.
	GLColorArrayPointer := 16r8090.
	GLIndexArrayPointer := 16r8091.
	GLTextureCoordArrayPointer := 16r8092.
	GLEdgeFlagArrayPointer := 16r8093.
	GLV2f := 16r2A20.
	GLV3f := 16r2A21.
	GLC4ubV2f := 16r2A22.
	GLC4ubV3f := 16r2A23.
	GLC3fV3f := 16r2A24.
	GLN3fV3f := 16r2A25.
	GLC4fN3fV3f := 16r2A26.
	GLT2fV3f := 16r2A27.
	GLT4fV4f := 16r2A28.
	GLT2fC4ubV3f := 16r2A29.
	GLT2fC3fV3f := 16r2A2A.
	GLT2fN3fV3f := 16r2A2B.
	GLT2fC4fN3fV3f := 16r2A2C.
	GLT4fC4fN3fV4f := 16r2A2D.
	GLBgr := 16r80E0.
	GLBgra := 16r80E1.
	GLConstantColor := 16r8001.
	GLOneMinusConstantColor := 16r8002.
	GLConstantAlpha := 16r8003.
	GLOneMinusConstantAlpha := 16r8004.
	GLBlendColor := 16r8005.
	GLFuncAdd := 16r8006.
	GLMin := 16r8007.
	GLMax := 16r8008.
	GLBlendEquation := 16r8009.
	GLFuncSubtract := 16r800A.
	GLFuncReverseSubtract := 16r800B.
	GLColorMatrix := 16r80B1.
	GLColorMatrixStackDepth := 16r80B2.
	GLMaxColorMatrixStackDepth := 16r80B3.
	GLPostColorMatrixRedScale := 16r80B4.
	GLPostColorMatrixGreenScale := 16r80B5.
	GLPostColorMatrixBlueScale := 16r80B6.
	GLPostColorMatrixAlphaScale := 16r80B7.
	GLPostColorMatrixRedBias := 16r80B8.
	GLPostColorMatrixGreenBias := 16r80B9.
	GLPostColorMatrixBlueBias := 16r80BA.
	GLPostColorMatrixAlphaBias := 16r80BB.
	GLColorTable := 16r80D0.
	GLPostConvolutionColorTable := 16r80D1.
	GLPostColorMatrixColorTable := 16r80D2.
	GLProxyColorTable := 16r80D3.
	GLProxyPostConvolutionColorTable := 16r80D4.
	GLProxyPostColorMatrixColorTable := 16r80D5.
	GLColorTableScale := 16r80D6.
	GLColorTableBias := 16r80D7.
	GLColorTableFormat := 16r80D8.
	GLColorTableWidth := 16r80D9.
	GLColorTableRedSize := 16r80DA.
	GLColorTableGreenSize := 16r80DB.
	GLColorTableBlueSize := 16r80DC.
	GLColorTableAlphaSize := 16r80DD.
	GLColorTableLuminanceSize := 16r80DE.
	GLColorTableIntensitySize := 16r80DF.
	GLConvolution1d := 16r8010.
	GLConvolution2d := 16r8011.
	GLSeparable2d := 16r8012.
	GLConvolutionBorderMode := 16r8013.
	GLConvolutionFilterScale := 16r8014.
	GLConvolutionFilterBias := 16r8015.
	GLReduce := 16r8016.
	GLConvolutionFormat := 16r8017.
	GLConvolutionWidth := 16r8018.
	GLConvolutionHeight := 16r8019.
	GLMaxConvolutionWidth := 16r801A.
	GLMaxConvolutionHeight := 16r801B.
	GLPostConvolutionRedScale := 16r801C.
	GLPostConvolutionGreenScale := 16r801D.
	GLPostConvolutionBlueScale := 16r801E.
	GLPostConvolutionAlphaScale := 16r801F.
	GLPostConvolutionRedBias := 16r8020.
	GLPostConvolutionGreenBias := 16r8021.
	GLPostConvolutionBlueBias := 16r8022.
	GLPostConvolutionAlphaBias := 16r8023.
	GLConstantBorder := 16r8151.
	GLReplicateBorder := 16r8153.
	GLConvolutionBorderColor := 16r8154.
	GLMaxElementsVertices := 16r80E8.
	GLMaxElementsIndices := 16r80E9.
	GLHistogram := 16r8024.
	GLProxyHistogram := 16r8025.
	GLHistogramWidth := 16r8026.
	GLHistogramFormat := 16r8027.
	GLHistogramRedSize := 16r8028.
	GLHistogramGreenSize := 16r8029.
	GLHistogramBlueSize := 16r802A.
	GLHistogramAlphaSize := 16r802B.
	GLHistogramLuminanceSize := 16r802C.
	GLHistogramSink := 16r802D.
	GLMinmax := 16r802E.
	GLMinmaxFormat := 16r802F.
	GLMinmaxSink := 16r8030.
	GLTableTooLarge := 16r8031.
! !

!OpenGLConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 17:02'!
init6
	GLUnsignedByte332 := 16r8032.
	GLUnsignedShort4444 := 16r8033.
	GLUnsignedShort5551 := 16r8034.
	GLUnsignedInt8888 := 16r8035.
	GLUnsignedInt1010102 := 16r8036.
	GLUnsignedByte233Rev := 16r8362.
	GLUnsignedShort565 := 16r8363.
	GLUnsignedShort565Rev := 16r8364.
	GLUnsignedShort4444Rev := 16r8365.
	GLUnsignedShort1555Rev := 16r8366.
	GLUnsignedInt8888Rev := 16r8367.
	GLUnsignedInt2101010Rev := 16r8368.
	GLRescaleNormal := 16r803A.
	GLLightModelColorControl := 16r81F8.
	GLSingleColor := 16r81F9.
	GLSeparateSpecularColor := 16r81FA.
	GLPackSkipImages := 16r806B.
	GLPackImageHeight := 16r806C.
	GLUnpackSkipImages := 16r806D.
	GLUnpackImageHeight := 16r806E.
	GLTexture3d := 16r806F.
	GLProxyTexture3d := 16r8070.
	GLTextureDepth := 16r8071.
	GLTextureWrapR := 16r8072.
	GLMax3dTextureSize := 16r8073.
	GLClampToEdge := 16r812F.
	GLTextureMinLod := 16r813A.
	GLTextureMaxLod := 16r813B.
	GLTextureBaseLevel := 16r813C.
	GLTextureMaxLevel := 16r813D.
	GLSmoothPointSizeRange := 16r0B12.
	GLSmoothPointSizeGranularity := 16r0B13.
	GLSmoothLineWidthRange := 16r0B22.
	GLSmoothLineWidthGranularity := 16r0B23.
	GLAliasedPointSizeRange := 16r846D.
	GLAliasedLineWidthRange := 16r846E.
	GLTexture0Arb := 16r84C0.
	GLTexture1Arb := 16r84C1.
	GLTexture2Arb := 16r84C2.
	GLTexture3Arb := 16r84C3.
	GLTexture4Arb := 16r84C4.
	GLTexture5Arb := 16r84C5.
	GLTexture6Arb := 16r84C6.
	GLTexture7Arb := 16r84C7.
	GLTexture8Arb := 16r84C8.
	GLTexture9Arb := 16r84C9.
	GLTexture10Arb := 16r84CA.
	GLTexture11Arb := 16r84CB.
	GLTexture12Arb := 16r84CC.
	GLTexture13Arb := 16r84CD.
	GLTexture14Arb := 16r84CE.
	GLTexture15Arb := 16r84CF.
	GLTexture16Arb := 16r84D0.
	GLTexture17Arb := 16r84D1.
	GLTexture18Arb := 16r84D2.
	GLTexture19Arb := 16r84D3.
	GLTexture20Arb := 16r84D4.
	GLTexture21Arb := 16r84D5.
	GLTexture22Arb := 16r84D6.
	GLTexture23Arb := 16r84D7.
	GLTexture24Arb := 16r84D8.
	GLTexture25Arb := 16r84D9.
	GLTexture26Arb := 16r84DA.
	GLTexture27Arb := 16r84DB.
	GLTexture28Arb := 16r84DC.
	GLTexture29Arb := 16r84DD.
	GLTexture30Arb := 16r84DE.
	GLTexture31Arb := 16r84DF.
	GLActiveTextureArb := 16r84E0.
	GLClientActiveTextureArb := 16r84E1.
	GLMaxTextureUnitsArb := 16r84E2.
	GLExtAbgr := 1.
	GLExtBlendColor := 1.
	GLExtBlendMinmax := 1.
	GLExtBlendSubtract := 1.
	GLExtTextureEnvCombine := 1.
	GLExtTextureEnvAdd := 1.
	GLAbgrExt := 16r8000.
	GLConstantColorExt := 16r8001.
	GLOneMinusConstantColorExt := 16r8002.
	GLConstantAlphaExt := 16r8003.
	GLOneMinusConstantAlphaExt := 16r8004.
	GLBlendColorExt := 16r8005.
	GLFuncAddExt := 16r8006.
	GLMinExt := 16r8007.
	GLMaxExt := 16r8008.
	GLBlendEquationExt := 16r8009.
	GLFuncSubtractExt := 16r800A.
	GLFuncReverseSubtractExt := 16r800B.
	GLCombineExt := 16r8570.
	GLCombineRgbExt := 16r8571.
	GLCombineAlphaExt := 16r8572.
	GLRgbScaleExt := 16r8573.
	GLAddSignedExt := 16r8574.
	GLInterpolateExt := 16r8575.
	GLConstantExt := 16r8576.
	GLPrimaryColorExt := 16r8577.
	GLPreviousExt := 16r8578.
	GLSource0RgbExt := 16r8580.
	GLSource1RgbExt := 16r8581.
	GLSource2RgbExt := 16r8582.
	GLSource0AlphaExt := 16r8588.
	GLSource1AlphaExt := 16r8589.
	GLSource2AlphaExt := 16r858A.
	GLOperand0RgbExt := 16r8590.
	GLOperand1RgbExt := 16r8591.
	GLOperand2RgbExt := 16r8592.
	GLOperand0AlphaExt := 16r8598.
	GLOperand1AlphaExt := 16r8599.
	GLOperand2AlphaExt := 16r859A.

	GLLogicOp := GLIndexLogicOp.
	GLTextureComponents := GLTextureInternalFormat.
! !

!OpenGLConstants class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 17:03'!
initialize
	"OpenGLConstants initialize"
	self init1.
	self init2.
	self init3.
	self init4.
	self init5.
	self init6.
	"check the class pool"
	self classPool keysAndValuesDo:[:key :val|
		val ifNil:[self error: key printString,' undefined'].
	].! !

!OGLExtManager methodsFor: 'dynamic lookup' stamp: 'das 3/12/2004 14:19'!
doesNotUnderstand: aMessage
	"WARNING: DON'T TRY THIS AT HOME!!
	If you grok this method you are seriously ready for meta-programming.
	If you don't you shouldn't worry either - it's not your fault ;-)"
	| method ffiCall extension |

	"see if that method is really there"
	method := OGLExtManager class "(!!)" compiledMethodAt: aMessage selector ifAbsent:[nil].
	method ifNil:[^super doesNotUnderstand: aMessage].

	"see if it's an FFI call"
	(method numLiterals > 0 and:[(method literalAt: 1) isKindOf: ExternalFunction])
		ifFalse:[^super doesNotUnderstand: aMessage].

	"grab the ffi call from the method"
	ffiCall := method literalAt: 1.
	"see if it describes some extension (module needs to be empty)"
	ffiCall module isNil ifFalse:[^super doesNotUnderstand: aMessage].

	"look up the extension"
	extension := OGLExtManager class organization categoryOfElement: aMessage selector.

	"load the extension"
	self loadExtension: extension.

	"and reinvoke method"
	(self respondsTo: aMessage selector) ifTrue:[
		^self perform: aMessage selector withArguments: aMessage arguments
	].
	^super doesNotUnderstand: aMessage! !

!OGLExtManager methodsFor: 'dynamic lookup' stamp: 'pb 7/6/2015 04:28'!
loadExtension: extName
	"Load and install the extension with the given name"
	| selectors callType method ffiCall ffiAddr methodCopy |
	ogl ifNil: [ ^ self error: 'OGLExtManager must be bound to a specific context' ].
	"see if the extension is present"
	(ogl extensions includes: extName) ifFalse: [ ^ self error: extName , ' is not present' ].
	"verify that we're an anonymous subclass of OGLExtManager"
	(self class superclass ~~ OGLExtManager or: [ OGLExtManager subclasses includes: self class ]) ifTrue: [ ^ self error: 'Must be anonymous' ].
	"pick up the selectors"
	selectors := OGLExtManager class organization listAtCategoryNamed: extName.
	"!!"
	"get the calling convention"
	callType := ogl glExtCallingConvention.
	"install the methods"
	selectors do: [ :sel |
		method := OGLExtManager class
			compiledMethodAt: sel
			ifAbsent: [ ].
		(method notNil and: [
			method numLiterals > 0 and: [ (ffiCall := method literalAt: 1) isKindOf: ExternalLibraryFunction ]]) ifTrue: [
			"clone the method"
			methodCopy := method shallowCopy.
			"set up a new ffi function"
			ffiCall := ExternalLibraryFunction new
				name: ffiCall name
				module: (ogl glExtModule ifNil: [ ffiCall module ])
				flags: callType
				argTypes: ffiCall argTypes.
			"see glExtModule"
			"Lookup address through OpenGL, unless we use a regular FFI lookup."
			ogl glExtModule ifNil: [
				"get the address of the extension function"
				ffiAddr := ogl glExtGetProcAddress: ffiCall name asString.
				(ffiAddr == nil or: [ ffiAddr = 0 ])
					ifTrue: [ self error: 'Failed to find ' , ffiCall name ]
					ifFalse: [ ffiAddr := ExternalAddress new fromInteger: ffiAddr ].
				ffiCall setHandle: ffiAddr ].
			"install the new ffi call"
			methodCopy
				literalAt: 1
				put: ffiCall.
			"add the copy to our method dictionary"
			self class
				addSelectorSilently: sel
				withMethod: methodCopy ]].! !

!OGLExtManager methodsFor: 'accessing' stamp: 'das 3/12/2004 12:44'!
ogl
	^ogl! !

!OGLExtManager methodsFor: 'accessing' stamp: 'das 3/12/2004 12:44'!
ogl: aOGL
	ogl := aOGL.! !

!OGLExtManager class methodsFor: 'class initialization' stamp: 'pb 6/1/2015 01:55'!
forwardExtMethod: aSelector
	"Compile a forwarder method in OpenGL"
	| code parser fwdCode |
	code := self class sourceCodeAt: aSelector.
	"FIXME - no longer works in Cuis 4.2
	parser := self parserClass new.
	"
	self flag: #fixme.
	parser := Parser new.
	parser parseSelector: code.
	code := code copyFrom: 1 to: (parser endOfLastToken min: code size).
	fwdCode := String streamContents:[:s|
		s nextPutAll: code.
		s newLineTab: 1; nextPutAll: '"This method was automatically generated"'.
		s newLineTab: 1; nextPutAll: '^glExt '; nextPutAll: code.
	].
	(OpenGL sourceCodeAt: aSelector ifAbsent:['']) = fwdCode ifTrue:[^self]. "no change"
	OpenGL compile: fwdCode classified: (self class organization categoryOfElement: aSelector).
! !

!OGLExtManager class methodsFor: 'function helpers' stamp: 'ar 4/4/2006 21:04'!
generate: extension types: typeList functions: list
	"Take a list of strings comprising open gl functions, and compile FFI functions here.
	See also parseEXTFunctions:do:."
	
	| typeMap signature1 signature2 body |
	(extension beginsWith: 'GL_')
		ifFalse: [^self error: 'suspicious extension name'].

	typeMap := Dictionary new.
	typeList pairsDo: [:string :exttype | typeMap at: string put: exttype].

	self parseEXTFunctions: list do: [:return :function :args |
		signature1 := String streamContents: [:s |
			s nextPutAll: function; nextPut: $(.
			args do: [:arg | s nextPutAll: arg second]
				separatedBy: [s nextPutAll: ', '].
			s nextPut: $)].
		signature2 := String streamContents: [:s |
			s nextPutAll: function.
			args isEmpty ifFalse: [
				s nextPutAll: ': '.
				args do: [:arg | s nextPutAll: arg second]
					separatedBy: [s nextPutAll: ' with: ']]].
		body := String streamContents: [:s |
			s newLineTab: 1; nextPutAll: '"This method was generated by ', name, '"'.
			s newLineTab: 1; nextPutAll: '<apicall: '.
			s nextPutAll: (self mapType: return using: typeMap); space.
			s print: function; space.
			s nextPutAll: '('.
			args do: [:arg | s nextPutAll: (self mapType: arg first using: typeMap)]
				separatedBy: [s space].
			s nextPutAll: ')>'; newLineTab: 1.
			s nextPutAll: '^self externalCallFailed'.
		].
		self class "!!" 
			compile: signature1,body classified: extension;
			compile: signature2,body classified: extension]! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'tog 3/30/2005 16:44'!
generateArbShaderObjectsFunctions
	"self generateArbShaderObjectsFunctions"

	self generate: 'GL_ARB_shader_objects'
	types: #(
		'handleARB' ulong
		'charARB' char
		'uint' ulong
		'short' short
		'float' float
		'double' double
		'ubyte' unsignedByte
		'byte' byte
		'int' long
		'ushort' unsignedShort
		'enum' long
		'boolean' long
		'sizei' long
		'void' void
	)
	functions: #(
		'void DeleteObjectARB(handleARB obj)'
		'handleARB GetHandleARB(enum pname)'
		'void DetachObjectARB(handleARB containerObj, handleARB attachedObj)'
		'handleARB CreateShaderObjectARB(enum shaderType)'
"
		'void ShaderSourceARB(handleARB shaderObj, sizei count, const charARB **string, const int *length)'
"		
		'void ShaderSourceARB(handleARB shaderObj, sizei count, const charARB* string, const int *length)'

		'void CompileShaderARB(handleARB shaderObj)'
		'handleARB CreateProgramObjectARB()'
		'void AttachObjectARB(handleARB containerObj, handleARB obj)'
		'void LinkProgramARB(handleARB programObj)'
		'void UseProgramObjectARB(handleARB programObj)'
		'void ValidateProgramARB(handleARB programObj)'
		'void Uniform1fARB(int location, float v0)'
		'void Uniform2fARB(int location, float v0, float v1)'
		'void Uniform3fARB(int location, float v0, float v1, float v2)'
		'void Uniform4fARB(int location, float v0, float v1, float v2, float v3)'
		'void Uniform1iARB(int location, int v0)'
		'void Uniform2iARB(int location, int v0, int v1)'
		'void Uniform3iARB(int location, int v0, int v1, int v2)'
		'void Uniform4iARB(int location, int v0, int v1, int v2, int v3)'
		'void Uniform1fvARB(int location, sizei count, const float *value)'
		'void Uniform2fvARB(int location, sizei count, const float *value)'
		'void Uniform3fvARB(int location, sizei count, const float *value)'
		'void Uniform4fvARB(int location, sizei count, const float *value)'
		'void Uniform1ivARB(int location, sizei count, const int *value)'
		'void Uniform2ivARB(int location, sizei count, const int *value)'
		'void Uniform3ivARB(int location, sizei count, const int *value)'
		'void Uniform4ivARB(int location, sizei count, const int *value)'
		'void UniformMatrix2fvARB(int location, sizei count, boolean transpose, const float *value)'
		'void UniformMatrix3fvARB(int location, sizei count, boolean transpose, const float *value)'
		'void UniformMatrix4fvARB(int location, sizei count, boolean transpose, const float *value)'
		'void GetObjectParameterfvARB(handleARB obj, enum pname, float *params)'
		'void GetObjectParameterivARB(handleARB obj, enum pname, int *params)'
		'void GetInfoLogARB(handleARB obj, sizei maxLength, sizei *length, charARB *infoLog)'
		'void GetAttachedObjectsARB(handleARB containerObj, sizei maxCount, sizei *count, handleARB *obj)'
		'int GetUniformLocationARB(handleARB programObj, const charARB *name)'
		'void GetActiveUniformARB(handleARB programObj, uint index, sizei maxLength, sizei *length, int *size, enum *type, charARB *name)'
		'void GetUniformfvARB(handleARB programObj, int location, float *params)'
		'void GetUniformivARB(handleARB programObj, int location, int *params)'
		'void GetShaderSourceARB(handleARB obj, sizei maxLength, sizei *length, charARB *source)'
	)
! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 5/10/2005 23:47'!
generateArbVertexBufferObjectFunctions
	"self generateArbVertexBufferObjectFunctions"

Utilities useAuthorInitials: 'jcg' during: [
	self generate: 'GL_ARB_vertex_buffer_object'
		types: #(
			'uint' ulong
			'short' short
			'float' float
			'double' double
			'ubyte' unsignedByte
			'byte' byte
			'int' long
			'ushort' unsignedShort
			'enum' long
			'boolean' long
			'sizei' long
			'void' void
			'void*' long
			'long' long
			'sizeiptrARB' long
			'intptrARB' long
	)
	functions: #(
		'void BindBufferARB(enum target, uint buffer);'
		'void DeleteBuffersARB(sizei n, const uint *buffers);'
		'void GenBuffersARB(sizei n, uint *buffers);'
		'boolean IsBufferARB(uint buffer);'
		'void BufferDataARB(enum target, sizeiptrARB size, const void *data, enum usage);'
		'void BufferSubDataARB(enum target, intptrARB offset, sizeiptrARB size, const void *data);'
		'void GetBufferSubDataARB(enum target, intptrARB offset, sizeiptrARB size, void *data);'
		'long MapBufferARB(enum target, enum access);'
		'boolean UnmapBufferARB(enum target);'
		'void GetBufferParameterivARB(enum target, enum pname, int *params);'
		'void BindBufferARB(enum target, uint buffer);'
		'void DeleteBuffersARB(sizei n, const uint *buffers);'
		'void GenBuffersARB(sizei n, uint *buffers);'
		'boolean IsBufferARB(uint buffer);'
		'void BufferDataARB(enum target, sizeiptrARB size, const void *data, enum usage);'
		'void BufferSubDataARB(enum target, intptrARB offset, sizeiptrARB size, const void *data);'
		'void GetBufferSubDataARB(enum target, intptrARB offset, sizeiptrARB size, void *data);'
		'long MapBufferARB(enum target, enum access);'
		'boolean UnmapBufferARB(enum target);'
		'void GetBufferParameterivARB(enum target, enum pname, int *params);'
		'void GetBufferPointervARB(enum target, enum pname, void **params);'
	)
]! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 10/28/2004 15:57'!
generateArbVertexProgramFunctions
	"self generateArbVertexProgramFunctions"

Utilities useAuthorInitials: 'bf' during: [
	self generate: 'GL_ARB_vertex_program'
	types: #(
		'uint' ulong
		'short' short
		'float' float
		'double' double
		'ubyte' unsignedByte
		'byte' byte
		'int' long
		'ushort' unsignedShort
		'enum' long
		'boolean' long
		'sizei' long
		'void' void
	)
	functions: #(
		'void VertexAttrib1sARB(uint index, short x);'
		'void VertexAttrib1fARB(uint index, float x);'
		'void VertexAttrib1dARB(uint index, double x);'
		'void VertexAttrib2sARB(uint index, short x, short y);'
		'void VertexAttrib2fARB(uint index, float x, float y);'
		'void VertexAttrib2dARB(uint index, double x, double y);'
		'void VertexAttrib3sARB(uint index, short x, short y, short z);'
		'void VertexAttrib3fARB(uint index, float x, float y, float z);'
		'void VertexAttrib3dARB(uint index, double x, double y, double z);'
		'void VertexAttrib4sARB(uint index, short x, short y, short z, short w);'
		'void VertexAttrib4fARB(uint index, float x, float y, float z, float w);'
		'void VertexAttrib4dARB(uint index, double x, double y, double z, double w);'
		'void VertexAttrib4NubARB(uint index, ubyte x, ubyte y, ubyte z, ubyte w);'
		'void VertexAttrib1svARB(uint index, const short *v);'
		'void VertexAttrib1fvARB(uint index, const float *v);'
		'void VertexAttrib1dvARB(uint index, const double *v);'
		'void VertexAttrib2svARB(uint index, const short *v);'
		'void VertexAttrib2fvARB(uint index, const float *v);'
		'void VertexAttrib2dvARB(uint index, const double *v);'
		'void VertexAttrib3svARB(uint index, const short *v);'
		'void VertexAttrib3fvARB(uint index, const float *v);'
		'void VertexAttrib3dvARB(uint index, const double *v);'
		'void VertexAttrib4bvARB(uint index, const byte *v);'
		'void VertexAttrib4svARB(uint index, const short *v);'
		'void VertexAttrib4ivARB(uint index, const int *v);'
		'void VertexAttrib4ubvARB(uint index, const ubyte *v);'
		'void VertexAttrib4usvARB(uint index, const ushort *v);'
		'void VertexAttrib4uivARB(uint index, const uint *v);'
		'void VertexAttrib4fvARB(uint index, const float *v);'
		'void VertexAttrib4dvARB(uint index, const double *v);'
		'void VertexAttrib4NbvARB(uint index, const byte *v);'
		'void VertexAttrib4NsvARB(uint index, const short *v);'
		'void VertexAttrib4NivARB(uint index, const int *v);'
		'void VertexAttrib4NubvARB(uint index, const ubyte *v);'
		'void VertexAttrib4NusvARB(uint index, const ushort *v);'
		'void VertexAttrib4NuivARB(uint index, const uint *v);'
		'void VertexAttribPointerARB(uint index, int size, enum type, boolean normalized, sizei stride, const void *pointer);'
		'void EnableVertexAttribArrayARB(uint index);'
		'void DisableVertexAttribArrayARB(uint index);'
		'void ProgramStringARB(enum target, enum format, sizei len, const void *string);'
		'void BindProgramARB(enum target, uint program);'
		'void DeleteProgramsARB(sizei n, const uint *programs);'
		'void GenProgramsARB(sizei n, uint *programs);'
		'void ProgramEnvParameter4dARB(enum target, uint index, double x, double y, double z, double w);'
		'void ProgramEnvParameter4dvARB(enum target, uint index, const double *params);'
		'void ProgramEnvParameter4fARB(enum target, uint index, float x, float y, float z, float w);'
		'void ProgramEnvParameter4fvARB(enum target, uint index, const float *params);'
		'void ProgramLocalParameter4dARB(enum target, uint index, double x, double y, double z, double w);'
		'void ProgramLocalParameter4dvARB(enum target, uint index, const double *params);'
		'void ProgramLocalParameter4fARB(enum target, uint index, float x, float y, float z, float w);'
		'void ProgramLocalParameter4fvARB(enum target, uint index, const float *params);'
		'void GetProgramEnvParameterdvARB(enum target, uint index, double *params);'
		'void GetProgramEnvParameterfvARB(enum target, uint index, float *params);'
		'void GetProgramLocalParameterdvARB(enum target, uint index, double *params);'
		'void GetProgramLocalParameterfvARB(enum target, uint index, float *params);'
		'void GetProgramivARB(enum target, enum pname, int *params);'
		'void GetProgramStringARB(enum target, enum pname, void *string);'
		'void GetVertexAttribdvARB(uint index, enum pname, double *params);'
		'void GetVertexAttribfvARB(uint index, enum pname, float *params);'
		'void GetVertexAttribivARB(uint index, enum pname, int *params);'
		'void GetVertexAttribPointervARB(uint index, enum pname, void **pointer);'
		'boolean IsProgramARB(uint program);'
	)
]! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_shader' stamp: 'tog 3/29/2005 15:16'!
generateArbVertexShaderFunctions
	"self generateArbVertexShaderFunctions"

	self generate: 'GL_ARB_vertex_shader'
	types: #(
		'handleARB' ulong
		'charARB' char
		'uint' ulong
		'short' short
		'float' float
		'double' double
		'ubyte' unsignedByte
		'byte' byte
		'int' long
		'ushort' unsignedShort
		'enum' long
		'boolean' long
		'sizei' long
		'void' void
	)
	functions: #(
    		'void BindAttribLocationARB(handleARB programObj, uint index, const charARB *name);'
		'void GetActiveAttribARB(handleARB programObj, uint index, sizei maxLength, sizei *length, int *size, enum *type, charARB *name);'
		'int GetAttribLocationARB(handleARB programObj, const charARB *name);'
	)
! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_blit' stamp: 'daf 4/1/2007 11:59'!
generateExtFramebufferBlitFunctions
	"self generateExtFramebufferBlitFunctions"

	self generate: 'GL_EXT_framebuffer_blit'
	types: #(
		'handleARB' ulong
		'charARB' char
		'uint' ulong
		'short' short
		'float' float
		'double' double
		'ubyte' unsignedByte
		'byte' byte
		'int' long
		'ushort' unsignedShort
		'enum' long
		'boolean' long
		'bitfield' long
		'sizei' long
		'void' void
	)
	functions: #(
		'void BlitFramebufferEXT(int srcX0, int srcY0, int srcX1, int srcY1, int dstX0, int dstY0, int dstX1, int dstY1, bitfield mask, enum filter)'
	)
! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 11:55'!
generateExtFramebufferObjectFunctions
	"self generateExtFramebufferObjectFunctions"

	self generate: 'GL_EXT_framebuffer_object'
	types: #(
		'handleARB' ulong
		'charARB' char
		'uint' ulong
		'short' short
		'float' float
		'double' double
		'ubyte' unsignedByte
		'byte' byte
		'int' long
		'ushort' unsignedShort
		'enum' long
		'boolean' long
		'sizei' long
		'void' void
	)
	functions: #(
		'boolean IsRenderbufferEXT (uint buffer)'
		'void BindRenderbufferEXT (enum target, uint buffer)'
		'void DeleteRenderbuffersEXT (sizei n, const uint *buffers)'
		'void GenRenderbuffersEXT (sizei n, uint *buffers)'
		'void RenderbufferStorageEXT (enum target, enum internalformat, sizei width, sizei height)'
		'void GetRenderbufferParameterivEXT (enum target, enum pnames, int *params)'
		'boolean IsFramebufferEXT (uint buffer)'
		'void BindFramebufferEXT (enum target, uint buffer)'
		'void DeleteFramebuffersEXT (sizei n, const uint *buffers)'
		'void GenFramebuffersEXT (sizei n, uint *buffers)'
		'enum CheckFramebufferStatusEXT (enum target)'
		'void FramebufferTexture1DEXT (enum target, enum attachment, enum textarget, uint texture, int level)'
		'void FramebufferTexture2DEXT (enum target, enum attachment, enum textarget, uint texture, int level)'
		'void FramebufferTexture3DEXT (enum target, enum attachment, enum textarget, uint texture, int level, int zoffset)'
		'void FramebufferRenderbufferEXT (enum target, enum attachment, enum renderbuffertarget, uint renderbuffer)'
		'void GetFramebufferAttachmentParameterivEXT (enum target, enum attachment, enum pname, int *params)'
		'void GenerateMipmapEXT (enum target)'
	)
! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glAttachObjectARB: containerObj with: obj
	"This method was generated by OGLExtManager"
	<apicall: void 'glAttachObjectARB' (ulong ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_shader' stamp: 'daf 4/1/2007 20:24'!
glBindAttribLocationARB: programObj with: index with: name
	"This method was generated by OGLExtManager"
	<apicall: void 'glBindAttribLocationARB' (ulong ulong char*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glBindBufferARB: target with: buffer
	"This method was generated by OGLExtManager"
	<apicall: void 'glBindBufferARB' (long ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glBindFramebufferEXT: target with: buffer
	"This method was generated by OGLExtManager"
	<apicall: void 'glBindFramebufferEXT' (long ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glBindProgramARB: target with: program
	"This method was generated by OGLExtManager"
	<apicall: void 'glBindProgramARB' (long ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glBindRenderbufferEXT: target with: buffer
	"This method was generated by OGLExtManager"
	<apicall: void 'glBindRenderbufferEXT' (long ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_blit' stamp: 'daf 4/1/2007 20:24'!
glBlitFramebufferEXT: srcX0 with: srcY0 with: srcX1 with: srcY1 with: dstX0 with: dstY0 with: dstX1 with: dstY1 with: mask with: filter
	"This method was generated by OGLExtManager"
	<apicall: void 'glBlitFramebufferEXT' (long long long long long long long long long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glBufferDataARB: target with: size with: data with: usage
	"This method was generated by OGLExtManager"
	<apicall: void 'glBufferDataARB' (long long void* long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glBufferSubDataARB: target with: offset with: size with: data
	"This method was generated by OGLExtManager"
	<apicall: void 'glBufferSubDataARB' (long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glCheckFramebufferStatusEXT: target
	"This method was generated by OGLExtManager"
	<apicall: long 'glCheckFramebufferStatusEXT' (long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_multitexture' stamp: 'dsa 8/2/2007 14:27'!
glClientActiveTextureARB: texture
	"This method was automatically generated."
	"void glClientActiveTextureARB(GLenum texture);"
	<apicall: void 'glClientActiveTextureARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glCompileShaderARB: shaderObj
	"This method was generated by OGLExtManager"
	<apicall: void 'glCompileShaderARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/26/2003 02:06'!
glCompressedTexImage1DARB: target with: level with: internalformat with: width with: border with: imageSize with: data
	"void glCompressedTexImage1DARB(enum target, int level,
                                 enum internalformat, sizei width,
                                 int border, sizei imageSize,
                                 const void *data);"
	<apicall: void 'glCompressedTexImage1DARB' (long long long long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/26/2003 02:06'!
glCompressedTexImage2DARB: target with: level with: internalformat with: width with: height with: border with: imageSize with: data
	"void glCompressedTexImage2DARB(enum target, int level,
                                 enum internalformat, sizei width,
                                 sizei height,
                                 int border, sizei imageSize,
                                 const void *data);"
	<apicall: void 'glCompressedTexImage2DARB' (long long long long long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/26/2003 02:04'!
glCompressedTexImage3DARB: target with: level with: internalformat with: width with: height with: depth with: border with: imageSize with: data
	"void glCompressedTexImage3DARB(enum target, int level,
                                 enum internalformat, sizei width,
                                 sizei height, sizei depth,
                                 int border, sizei imageSize,
                                 const void *data);"
	<apicall: void 'glCompressedTexImage3DARB' (long long long long long long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/26/2003 02:09'!
glCompressedTexSubImage1DARB: target with: level with: xoffset with: width with: aFormat with: imageSize with: data
	"void CompressedTexSubImage1DARB(enum target, int level, 
                                    int xoffset, sizei height,
                                    enum format, sizei imageSize,
                                    const void *data);"
	<apicall: void 'glCompressedTexImage2DARB' (long long long long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/26/2003 02:08'!
glCompressedTexSubImage2DARB: target with: level with: xoffset with: yoffset with: width with: height with: aFormat with: imageSize with: data
	"void CompressedTexSubImage2DARB(enum target, int level, 
                                    int xoffset, int yoffset,
                                    sizei width, sizei height,
                                    enum format, sizei imageSize,
                                    const void *data);"
	<apicall: void 'glCompressedTexImage2DARB' (long long long long long long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/26/2003 02:08'!
glCompressedTexSubImage3DARB: target with: level with: xoffset with: yoffset with: zoffset with: width with: height with: depth with: aFormat with: imageSize with: data
	"void CompressedTexSubImage3DARB(enum target, int level, 
                                    int xoffset, int yoffset,
                                    int zoffset, sizei width,
                                    sizei height, sizei depth,
                                    enum format, sizei imageSize,
                                    const void *data);"
	<apicall: void 'glCompressedTexImage2DARB' (long long long long long long long long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glCreateProgramObjectARB
	"This method was generated by OGLExtManager"
	<apicall: ulong 'glCreateProgramObjectARB' ()>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glCreateShaderObjectARB: shaderType
	"This method was generated by OGLExtManager"
	<apicall: ulong 'glCreateShaderObjectARB' (long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glDeleteBuffersARB: n with: buffers
	"This method was generated by OGLExtManager"
	<apicall: void 'glDeleteBuffersARB' (long ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glDeleteFramebuffersEXT: n with: buffers
	"This method was generated by OGLExtManager"
	<apicall: void 'glDeleteFramebuffersEXT' (long ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glDeleteObjectARB: obj
	"This method was generated by OGLExtManager"
	<apicall: void 'glDeleteObjectARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glDeleteProgramsARB: n with: programs
	"This method was generated by OGLExtManager"
	<apicall: void 'glDeleteProgramsARB' (long ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glDeleteRenderbuffersEXT: n with: buffers
	"This method was generated by OGLExtManager"
	<apicall: void 'glDeleteRenderbuffersEXT' (long ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glDetachObjectARB: containerObj with: attachedObj
	"This method was generated by OGLExtManager"
	<apicall: void 'glDetachObjectARB' (ulong ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glDisableVertexAttribArrayARB: index
	"This method was generated by OGLExtManager"
	<apicall: void 'glDisableVertexAttribArrayARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glEnableVertexAttribArrayARB: index
	"This method was generated by OGLExtManager"
	<apicall: void 'glEnableVertexAttribArrayARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glFramebufferRenderbufferEXT: target with: attachment with: renderbuffertarget with: renderbuffer
	"This method was generated by OGLExtManager"
	<apicall: void 'glFramebufferRenderbufferEXT' (long long long ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glFramebufferTexture1DEXT: target with: attachment with: textarget with: texture with: level
	"This method was generated by OGLExtManager"
	<apicall: void 'glFramebufferTexture1DEXT' (long long long ulong long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glFramebufferTexture2DEXT: target with: attachment with: textarget with: texture with: level
	"This method was generated by OGLExtManager"
	<apicall: void 'glFramebufferTexture2DEXT' (long long long ulong long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glFramebufferTexture3DEXT: target with: attachment with: textarget with: texture with: level with: zoffset
	"This method was generated by OGLExtManager"
	<apicall: void 'glFramebufferTexture3DEXT' (long long long ulong long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glGenBuffersARB: n with: buffers
	"This method was generated by OGLExtManager"
	<apicall: void 'glGenBuffersARB' (long ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGenFramebuffersEXT: n with: buffers
	"This method was generated by OGLExtManager"
	<apicall: void 'glGenFramebuffersEXT' (long ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGenProgramsARB: n with: programs
	"This method was generated by OGLExtManager"
	<apicall: void 'glGenProgramsARB' (long ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGenRenderbuffersEXT: n with: buffers
	"This method was generated by OGLExtManager"
	<apicall: void 'glGenRenderbuffersEXT' (long ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGenerateMipmapEXT: target
	"This method was generated by OGLExtManager"
	<apicall: void 'glGenerateMipmapEXT' (long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_shader' stamp: 'daf 4/1/2007 20:24'!
glGetActiveAttribARB: programObj with: index with: maxLength with: length with: size with: type with: name
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetActiveAttribARB' (ulong ulong long long* long* long* char*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetActiveUniformARB: programObj with: index with: maxLength with: length with: size with: type with: name
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetActiveUniformARB' (ulong ulong long long* long* long* char*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetAttachedObjectsARB: containerObj with: maxCount with: count with: obj
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetAttachedObjectsARB' (ulong long long* ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_shader' stamp: 'daf 4/1/2007 20:24'!
glGetAttribLocationARB: programObj with: name
	"This method was generated by OGLExtManager"
	<apicall: long 'glGetAttribLocationARB' (ulong char*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glGetBufferParameterivARB: target with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetBufferParameterivARB' (long long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glGetBufferPointervARB: target with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetBufferPointervARB' (long long void**)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glGetBufferSubDataARB: target with: offset with: size with: data
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetBufferSubDataARB' (long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/26/2003 02:18'!
glGetCompressedTexImageARB: target with: lod with: img
	"void GetCompressedTexImageARB(enum target, int lod,
                                    void *img);"
	<apicall: void 'glCompressedTexImage1DARB' (long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGetFramebufferAttachmentParameterivEXT: target with: attachment with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetFramebufferAttachmentParameterivEXT' (long long long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetHandleARB: pname
	"This method was generated by OGLExtManager"
	<apicall: ulong 'glGetHandleARB' (long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetInfoLogARB: obj with: maxLength with: length with: infoLog
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetInfoLogARB' (ulong long long* char*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetObjectParameterfvARB: obj with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetObjectParameterfvARB' (ulong long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetObjectParameterivARB: obj with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetObjectParameterivARB' (ulong long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetProgramEnvParameterdvARB: target with: index with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetProgramEnvParameterdvARB' (long ulong double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetProgramEnvParameterfvARB: target with: index with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetProgramEnvParameterfvARB' (long ulong float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetProgramLocalParameterdvARB: target with: index with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetProgramLocalParameterdvARB' (long ulong double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetProgramLocalParameterfvARB: target with: index with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetProgramLocalParameterfvARB' (long ulong float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetProgramStringARB: target with: pname with: string
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetProgramStringARB' (long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetProgramivARB: target with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetProgramivARB' (long long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGetRenderbufferParameterivEXT: target with: pnames with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetRenderbufferParameterivEXT' (long long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetShaderSourceARB: obj with: maxLength with: length with: source
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetShaderSourceARB' (ulong long long* char*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetUniformLocationARB: programObj with: name
	"This method was generated by OGLExtManager"
	<apicall: long 'glGetUniformLocationARB' (ulong char*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetUniformfvARB: programObj with: location with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetUniformfvARB' (ulong long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetUniformivARB: programObj with: location with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetUniformivARB' (ulong long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetVertexAttribPointervARB: index with: pname with: pointer
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetVertexAttribPointervARB' (ulong long void**)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetVertexAttribdvARB: index with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetVertexAttribdvARB' (ulong long double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetVertexAttribfvARB: index with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetVertexAttribfvARB' (ulong long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glGetVertexAttribivARB: index with: pname with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glGetVertexAttribivARB' (ulong long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glIsBufferARB: buffer
	"This method was generated by OGLExtManager"
	<apicall: long 'glIsBufferARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glIsFramebufferEXT: buffer
	"This method was generated by OGLExtManager"
	<apicall: long 'glIsFramebufferEXT' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glIsProgramARB: program
	"This method was generated by OGLExtManager"
	<apicall: long 'glIsProgramARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glIsRenderbufferEXT: buffer
	"This method was generated by OGLExtManager"
	<apicall: long 'glIsRenderbufferEXT' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glLinkProgramARB: programObj
	"This method was generated by OGLExtManager"
	<apicall: void 'glLinkProgramARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_transpose_matrix' stamp: 'das 6/8/2005 15:06'!
glLoadTransposeMatrixf: m

	"This method was automatically generated."
	"void glLoadTransposeMatrixf(GLfloat* m);"
	<apicall: void 'glLoadTransposeMatrixf' (float*) >
	^self externalCallFailed
"	^self glLoadMatrixf: m transposed."
! !

!OGLExtManager class methodsFor: 'GL_EXT_compiled_vertex_array' stamp: 'ar 1/4/2003 00:00'!
glLockArraysEXT: first with: count
	"void glLockArraysEXT (int first, sizei count)"
	<apicall: void 'glLockArraysEXT' (long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glMapBufferARB: target with: access
	"This method was generated by OGLExtManager"
	<apicall: long 'glMapBufferARB' (long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_transpose_matrix' stamp: 'das 6/8/2005 15:06'!
glMultTransposeMatrixf: m

	"This method was automatically generated."
	"void glMultTransposeMatrixf(GLfloat* m);"
	<apicall: void 'glMultTransposeMatrixf' (float*) >
	^self externalCallFailed
"	^self glMultMatrixf: m transposed."! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramEnvParameter4dARB: target with: index with: x with: y with: z with: w
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramEnvParameter4dARB' (long ulong double double double double)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramEnvParameter4dvARB: target with: index with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramEnvParameter4dvARB' (long ulong double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramEnvParameter4fARB: target with: index with: x with: y with: z with: w
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramEnvParameter4fARB' (long ulong float float float float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramEnvParameter4fvARB: target with: index with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramEnvParameter4fvARB' (long ulong float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramLocalParameter4dARB: target with: index with: x with: y with: z with: w
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramLocalParameter4dARB' (long ulong double double double double)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramLocalParameter4dvARB: target with: index with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramLocalParameter4dvARB' (long ulong double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramLocalParameter4fARB: target with: index with: x with: y with: z with: w
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramLocalParameter4fARB' (long ulong float float float float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramLocalParameter4fvARB: target with: index with: params
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramLocalParameter4fvARB' (long ulong float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glProgramStringARB: target with: format with: len with: string
	"This method was generated by OGLExtManager"
	<apicall: void 'glProgramStringARB' (long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glRenderbufferStorageEXT: target with: internalformat with: width with: height
	"This method was generated by OGLExtManager"
	<apicall: void 'glRenderbufferStorageEXT' (long long long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glShaderSourceARB: shaderObj with: count with: string with: length
	"This method was generated by OGLExtManager"
	<apicall: void 'glShaderSourceARB' (ulong long char* long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform1fARB: location with: v0
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform1fARB' (long float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform1fvARB: location with: count with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform1fvARB' (long long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform1iARB: location with: v0
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform1iARB' (long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform1ivARB: location with: count with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform1ivARB' (long long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform2fARB: location with: v0 with: v1
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform2fARB' (long float float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform2fvARB: location with: count with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform2fvARB' (long long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform2iARB: location with: v0 with: v1
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform2iARB' (long long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform2ivARB: location with: count with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform2ivARB' (long long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform3fARB: location with: v0 with: v1 with: v2
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform3fARB' (long float float float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform3fvARB: location with: count with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform3fvARB' (long long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform3iARB: location with: v0 with: v1 with: v2
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform3iARB' (long long long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform3ivARB: location with: count with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform3ivARB' (long long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform4fARB: location with: v0 with: v1 with: v2 with: v3
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform4fARB' (long float float float float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform4fvARB: location with: count with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform4fvARB' (long long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform4iARB: location with: v0 with: v1 with: v2 with: v3
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform4iARB' (long long long long long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform4ivARB: location with: count with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniform4ivARB' (long long long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniformMatrix2fvARB: location with: count with: transpose with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniformMatrix2fvARB' (long long long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniformMatrix3fvARB: location with: count with: transpose with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniformMatrix3fvARB' (long long long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniformMatrix4fvARB: location with: count with: transpose with: value
	"This method was generated by OGLExtManager"
	<apicall: void 'glUniformMatrix4fvARB' (long long long float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_EXT_compiled_vertex_array' stamp: 'ar 1/4/2003 00:00'!
glUnlockArraysEXT
	"void glUnlockArraysEXT (void)"
	<apicall: void 'glUnlockArraysEXT' (void)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'jcg 6/18/2005 23:44'!
glUnmapBufferARB: target
	"This method was generated by OGLExtManager"
	<apicall: long 'glUnmapBufferARB' (long)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUseProgramObjectARB: programObj
	"This method was generated by OGLExtManager"
	<apicall: void 'glUseProgramObjectARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glValidateProgramARB: programObj
	"This method was generated by OGLExtManager"
	<apicall: void 'glValidateProgramARB' (ulong)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib1dARB: index with: x
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib1dARB' (ulong double)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib1dvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib1dvARB' (ulong double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib1fARB: index with: x
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib1fARB' (ulong float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib1fvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib1fvARB' (ulong float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib1sARB: index with: x
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib1sARB' (ulong short)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib1svARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib1svARB' (ulong short*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib2dARB: index with: x with: y
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib2dARB' (ulong double double)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib2dvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib2dvARB' (ulong double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib2fARB: index with: x with: y
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib2fARB' (ulong float float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib2fvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib2fvARB' (ulong float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib2sARB: index with: x with: y
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib2sARB' (ulong short short)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib2svARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib2svARB' (ulong short*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib3dARB: index with: x with: y with: z
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib3dARB' (ulong double double double)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib3dvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib3dvARB' (ulong double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib3fARB: index with: x with: y with: z
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib3fARB' (ulong float float float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib3fvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib3fvARB' (ulong float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib3sARB: index with: x with: y with: z
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib3sARB' (ulong short short short)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib3svARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib3svARB' (ulong short*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4NbvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4NbvARB' (ulong byte*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4NivARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4NivARB' (ulong long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4NsvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4NsvARB' (ulong short*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4NubARB: index with: x with: y with: z with: w
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4NubARB' (ulong unsignedByte unsignedByte unsignedByte unsignedByte)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4NubvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4NubvARB' (ulong unsignedByte*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4NuivARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4NuivARB' (ulong ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4NusvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4NusvARB' (ulong unsignedShort*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4bvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4bvARB' (ulong byte*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4dARB: index with: x with: y with: z with: w
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4dARB' (ulong double double double double)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4dvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4dvARB' (ulong double*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4fARB: index with: x with: y with: z with: w
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4fARB' (ulong float float float float)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4fvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4fvARB' (ulong float*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4ivARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4ivARB' (ulong long*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4sARB: index with: x with: y with: z with: w
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4sARB' (ulong short short short short)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4svARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4svARB' (ulong short*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4ubvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4ubvARB' (ulong unsignedByte*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4uivARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4uivARB' (ulong ulong*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttrib4usvARB: index with: v
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttrib4usvARB' (ulong unsignedShort*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'bf 4/12/2005 10:11'!
glVertexAttribPointerARB: index with: size with: type with: normalized with: stride with: pointer
	"This method was generated by OGLExtManager"
	<apicall: void 'glVertexAttribPointerARB' (ulong long long long long void*)>
	^self externalCallFailed! !

!OGLExtManager class methodsFor: 'class initialization' stamp: 'das 3/13/2004 14:49'!
initialize
	"OGLExtManager initialize"
	"Initialize all registered extensions. Please see the class comment."
	self class selectorsAndMethodsDo:[:sel :meth|
		(sel ~= #initialize and:[sel beginsWith: 'initialize']) ifTrue:[self perform: sel].
		((sel beginsWith: 'gl') and:[meth numLiterals > 0 and:[(meth literalAt: 1) isKindOf: ExternalLibraryFunction]]) ifTrue:[
			self forwardExtMethod: sel.
		].
	].
	GLExtConstants initialize.! !

!OGLExtManager class methodsFor: 'GL_ARB_fragment_shader' stamp: 'tog 3/31/2005 14:25'!
initializeArbFragmentShader
	"self initializeArbFragmentShader"
	"Initialize ARB_fragment_shader constants"

	self install: 'ArbFragmentShader' withEXTConstants: #(
		'FRAGMENT_SHADER_ARB				0x8B30'
		'MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB		0x8B49'
		'MAX_TEXTURE_COORDS_ARB				0x8871'
		'MAX_TEXTURE_IMAGE_UNITS_ARB			0x8872'
		'FRAGMENT_SHADER_DERIVATIVE_HINT_ARB		0x8B8B'
	).! !

!OGLExtManager class methodsFor: 'GL_ARB_multisample' stamp: 'bgf 6/14/2006 15:09'!
initializeArbMultisample
	"GLExtensions initialize"
	"Initialize GL_ARB_MULTISAMPLE constants; this is only a small subset"
	self install: 'ArbMutlisample' withEXTConstants: #(
		'MULTISAMPLE_ARB	0x809D'
		'MULTISAMPLE_FILTER_HINT_NV	0x8534'
	)
! !

!OGLExtManager class methodsFor: 'GL_ARB_shader_objects' stamp: 'tog 3/29/2005 13:20'!
initializeArbShaderObjects
	"self initializeArbShaderObjects"
	"Initialize ARB_shader_objects constants"

	self install: 'ArbShaderObjects' withEXTConstants: #(
		'PROGRAM_OBJECT_ARB 	0x8B40
'
		'SHADER_OBJECT_ARB 	0x8B48'
		'OBJECT_TYPE_ARB 	0x8B4E'
		'OBJECT_SUBTYPE_ARB 	0x8B4F'
		'FLOAT_VEC2_ARB 	0x8B50'
		'FLOAT_VEC3_ARB 	0x8B51'
		'FLOAT_VEC4_ARB 	0x8B52'
		'INT_VEC2_ARB 	0x8B53'
		'INT_VEC3_ARB 	0x8B54'
		'INT_VEC4_ARB 	0x8B55'
		'BOOL_ARB 	0x8B56'
		'BOOL_VEC2_ARB 	0x8B57'
		'BOOL_VEC3_ARB 	0x8B58'
		'BOOL_VEC4_ARB 	0x8B59'
		'FLOAT_MAT2_ARB 	0x8B5A'
		'FLOAT_MAT3_ARB 	0x8B5B'
		'FLOAT_MAT4_ARB 	0x8B5C'
		'SAMPLER_1D_ARB 	0x8B5D'
		'SAMPLER_2D_ARB 	0x8B5E'
		'SAMPLER_3D_ARB 	0x8B5F'
		'SAMPLER_CUBE_ARB 	0x8B60'
		'SAMPLER_1D_SHADOW_ARB 	0x8B61'
		'SAMPLER_2D_SHADOW_ARB 	0x8B62'
		'SAMPLER_2D_RECT_ARB 	0x8B63'
		'SAMPLER_2D_RECT_SHADOW_ARB 	0x8B64'
		'OBJECT_DELETE_STATUS_ARB 	0x8B80'
		'OBJECT_COMPILE_STATUS_ARB 	0x8B81'
		'OBJECT_LINK_STATUS_ARB 	0x8B82'
		'OBJECT_VALIDATE_STATUS_ARB 	0x8B83'
		'OBJECT_INFO_LOG_LENGTH_ARB 	0x8B84'
		'OBJECT_ATTACHED_OBJECTS_ARB 	0x8B85'
		'OBJECT_ACTIVE_UNIFORMS_ARB 	0x8B86'
		'OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB 	0x8B87'
		'OBJECT_SHADER_SOURCE_LENGTH_ARB 		0x8B88'
	)! !

!OGLExtManager class methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/5/2006 21:15'!
initializeArbTextureCompression
	"OGLExtManager initialize"
	"Initialize ARB_texture_compression constants"
	self install: 'ArbTextureCompression' withEXTConstants: #(
		"glTexImage/glCopyTexImage"
		'COMPRESSED_ALPHA_ARB				0x84E9'
		'COMPRESSED_LUMINANCE_ARB			0x84EA'
		'COMPRESSED_LUMINANCE_ALPHA_ARB			0x84EB'
		'COMPRESSED_INTENSITY_ARB			0x84EC'
		'COMPRESSED_RGB_ARB				0x84ED'
		'COMPRESSED_RGBA_ARB				0x84EE'

		"glHint/glGet"
		'TEXTURE_COMPRESSION_HINT_ARB			0x84EF'

		"glGetTexLevelParameter"
		'TEXTURE_COMPRESSED_IMAGE_SIZE_ARB		0x86A0'
		'TEXTURE_COMPRESSED_ARB				0x86A1'

		"glGet*"
		'NUM_COMPRESSED_TEXTURE_FORMATS_ARB		0x86A2'
		'COMPRESSED_TEXTURE_FORMATS_ARB			0x86A3'
	)
! !

!OGLExtManager class methodsFor: 'GL_ARB_transpose_matrix' stamp: 'das 6/8/2005 14:06'!
initializeArbTransposeMatrix
	"OGLExtManager initialize"
	"Initialize ARB:=texture:=compression constants"
	self install: 'ArbTransposeMatrix' withEXTConstants: #(	)
! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:15'!
initializeArbVertexBufferObject
		"self initializeArbVertexBufferObject"
	"Initialize ARB_vertex_buffer_object constants"
	self install: 'ArbVertexBufferObject' withEXTConstants: #(
		"glBindBufferARB/glBufferDataARB/glBufferSubDataARB/etc."
		'ARRAY_BUFFER_ARB				0x8892'
		'ELEMENT_ARRAY_BUFFER_ARB 		0x8893'

		"glGet"
		'ARRAY_BUFFER_BINDING_ARB						0x8894'
		'ELEMENT_ARRAY_BUFFER_BINDING_ARB				0x8895'
		'VERTEX_ARRAY_BUFFER_BINDING_ARB				0x8896'
		'NORMAL_ARRAY_BUFFER_BINDING_ARB				0x8897'
		'COLOR_ARRAY_BUFFER_BINDING_ARB				0x8898'
		'INDEX_ARRAY_BUFFER_BINDING_ARB					0x8899'
		'TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB		0x889A'
		'EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB			0x889B'
		'SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB		0x889C'
		'FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB		0x889D'
		'WEIGHT_ARRAY_BUFFER_BINDING_ARB				0x889E'

		"glGetVertexAttribivARB"
		'VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB		0x889F'

		"glBufferDataARB"
		'STREAM_DRAW_ARB									0x88E0'
		'STREAM_READ_ARB									0x88E1'
		'STREAM_COPY_ARB									0x88E2'
		'STATIC_DRAW_ARB									0x88E4'
		'STATIC_READ_ARB									0x88E5'
		'STATIC_COPY_ARB									0x88E6'
		'DYNAMIC_DRAW_ARB								0x88E8'
		'DYNAMIC_READ_ARB								0x88E9'
		'DYNAMIC_COPY_ARB								0x88EA'

		"glMapBufferARB"
		'READ_ONLY_ARB							0x88B8'
		'WRITE_ONLY_ARB							0x88B9'
		'READ_WRITE_ARB							0x88BA'
	
		"glGetBufferParameterivARB"
		'BUFFER_SIZE_ARB							0x8764'
		'BUFFER_USAGE_ARB							0x8765'
		'BUFFER_ACCESS_ARB						0x88BB'
		'BUFFER_MAPPED_ARB						0x88BC'
	)
! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:16'!
initializeArbVertexProgram
	"self initializeArbVertexProgram"
	"Initialize ARB_vertex_program constants"

	self install: 'ArbVertexProgram' withEXTConstants: #(
		'VERTEX_PROGRAM_ARB		0x8620'
		'VERTEX_PROGRAM_POINT_SIZE_ARB		0x8642'
		'VERTEX_PROGRAM_TWO_SIDE_ARB		0x8643'
		'COLOR_SUM_ARB		0x8458'
		'PROGRAM_FORMAT_ASCII_ARB		0x8875'
		'VERTEX_ATTRIB_ARRAY_ENABLED_ARB		0x8622'
		'VERTEX_ATTRIB_ARRAY_SIZE_ARB		0x8623'
		'VERTEX_ATTRIB_ARRAY_STRIDE_ARB		0x8624'
		'VERTEX_ATTRIB_ARRAY_TYPE_ARB		0x8625'
		'VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB		0x886A'
		'CURRENT_VERTEX_ATTRIB_ARB		0x8626'
		'VERTEX_ATTRIB_ARRAY_POINTER_ARB		0x8645'
		'PROGRAM_LENGTH_ARB		0x8627'
		'PROGRAM_FORMAT_ARB		0x8876'
		'PROGRAM_BINDING_ARB		0x8677'
		'PROGRAM_INSTRUCTIONS_ARB		0x88A0'
		'MAX_PROGRAM_INSTRUCTIONS_ARB		0x88A1'
		'PROGRAM_NATIVE_INSTRUCTIONS_ARB		0x88A2'
		'MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB		0x88A3'
		'PROGRAM_TEMPORARIES_ARB		0x88A4'
		'MAX_PROGRAM_TEMPORARIES_ARB		0x88A5'
		'PROGRAM_NATIVE_TEMPORARIES_ARB		0x88A6'
		'MAX_PROGRAM_NATIVE_TEMPORARIES_ARB		0x88A7'
		'PROGRAM_PARAMETERS_ARB		0x88A8'
		'MAX_PROGRAM_PARAMETERS_ARB		0x88A9'
		'PROGRAM_NATIVE_PARAMETERS_ARB		0x88AA'
		'MAX_PROGRAM_NATIVE_PARAMETERS_ARB		0x88AB'
		'PROGRAM_ATTRIBS_ARB		0x88AC'
		'MAX_PROGRAM_ATTRIBS_ARB		0x88AD'
		'PROGRAM_NATIVE_ATTRIBS_ARB		0x88AE'
		'MAX_PROGRAM_NATIVE_ATTRIBS_ARB		0x88AF'
		'PROGRAM_ADDRESS_REGISTERS_ARB		0x88B0'
		'MAX_PROGRAM_ADDRESS_REGISTERS_ARB		0x88B1'
		'PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB		0x88B2'
		'MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB		0x88B3'
		'MAX_PROGRAM_LOCAL_PARAMETERS_ARB		0x88B4'
		'MAX_PROGRAM_ENV_PARAMETERS_ARB		0x88B5'
		'PROGRAM_UNDER_NATIVE_LIMITS_ARB		0x88B6'
		'PROGRAM_STRING_ARB		0x8628'
		'PROGRAM_ERROR_POSITION_ARB		0x864B'
		'CURRENT_MATRIX_ARB		0x8641'
		'TRANSPOSE_CURRENT_MATRIX_ARB		0x88B7'
		'CURRENT_MATRIX_STACK_DEPTH_ARB		0x8640'
		'MAX_VERTEX_ATTRIBS_ARB		0x8869'
		'MAX_PROGRAM_MATRICES_ARB		0x862F'
		'MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB		0x862E'
		'PROGRAM_ERROR_STRING_ARB		0x8874'
		'MATRIX0_ARB		0x88C0'
		'MATRIX1_ARB		0x88C1'
		'MATRIX2_ARB		0x88C2'
		'MATRIX3_ARB		0x88C3'
		'MATRIX4_ARB		0x88C4'
		'MATRIX5_ARB		0x88C5'
		'MATRIX6_ARB		0x88C6'
		'MATRIX7_ARB		0x88C7'
		'MATRIX8_ARB		0x88C8'
		'MATRIX9_ARB		0x88C9'
		'MATRIX10_ARB		0x88CA'
		'MATRIX11_ARB		0x88CB'
		'MATRIX12_ARB		0x88CC'
		'MATRIX13_ARB		0x88CD'
		'MATRIX14_ARB		0x88CE'
		'MATRIX15_ARB		0x88CF'
		'MATRIX16_ARB		0x88D0'
		'MATRIX17_ARB		0x88D1'
		'MATRIX18_ARB		0x88D2'
		'MATRIX19_ARB		0x88D3'
		'MATRIX20_ARB		0x88D4'
		'MATRIX21_ARB		0x88D5'
		'MATRIX22_ARB		0x88D6'
		'MATRIX23_ARB		0x88D7'
		'MATRIX24_ARB		0x88D8'
		'MATRIX25_ARB		0x88D9'
		'MATRIX26_ARB		0x88DA'
		'MATRIX27_ARB		0x88DB'
		'MATRIX28_ARB		0x88DC'
		'MATRIX29_ARB		0x88DD'
		'MATRIX30_ARB		0x88DE'
		'MATRIX31_ARB		0x88DF'
	)! !

!OGLExtManager class methodsFor: 'GL_ARB_vertex_shader' stamp: 'tog 3/29/2005 15:32'!
initializeArbVertexShader
	"self initializeArbVertexShader"
	"Initialize ARB_vertex_shader constants"

	self install: 'ArbVertexShader' withEXTConstants: #(
		'VERTEX_SHADER_ARB 	0x8B31'
		'MAX_VERTEX_UNIFORM_COMPONENTS_ARB 	0x8B4A'
		'MAX_VARYING_FLOATS_ARB 	0x8B4B'
		'MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB 	0x8B4C'
		'MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB 	0x8B4D'
		'OBJECT_ACTIVE_ATTRIBUTES_ARB 	0x8B89'
		'OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB 	0x8B8A  '
)! !

!OGLExtManager class methodsFor: 'GL_EXT_compiled_vertex_array' stamp: 'ar 4/5/2006 21:16'!
initializeExtCompiledVertexArray
	self install: 'ExtCompiledVertexArray' withEXTConstants: #(
		'ARRAY_ELEMENT_LOCK_COUNT_EXT	0x81A9'
		'ARRAY_ELEMENT_LOCK_FIRST_EXT	0x81A8'
	).
! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_blit' stamp: 'daf 4/1/2007 12:02'!
initializeExtFramebufferBlit
	"self initializeExtFramebufferBlit"
	"Initialize EXT_framebuffer_blit constants"

	self install: 'ExtFramebufferBlit' withEXTConstants: #(
		'READ_FRAMEBUFFER_EXT           0x8CA8'
		'DRAW_FRAMEBUFFER_EXT           0x8CA9'
		'READ_FRAMEBUFFER_BINDING_EXT   0x8CA6'
		'DRAW_FRAMEBUFFER_BINDING_EXT   0x8CAA'
	)
! !

!OGLExtManager class methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 11:57'!
initializeExtFramebufferObjects
	"self initializeExtFramebufferObjects"
	"Initialize EXT_framebuffer_objects constants"

	self install: 'ExtFramebufferObjects' withEXTConstants: #(
		'INVALID_FRAMEBUFFER_OPERATION_EXT 0x0506'
		'MAX_RENDERBUFFER_SIZE_EXT      0x84E8'
		'FRAMEBUFFER_BINDING_EXT        0x8CA6'
		'RENDERBUFFER_BINDING_EXT       0x8CA7'
		'FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT 0x8CD0'
		'FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT 0x8CD1'
		'FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT 0x8CD2'
		'FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT 0x8CD3'
		'FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT 0x8CD4'
		'FRAMEBUFFER_COMPLETE_EXT       0x8CD5'
		'FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT 0x8CD6'
		'FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT 0x8CD7'
		'FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT 0x8CD9'
		'FRAMEBUFFER_INCOMPLETE_FORMATS_EXT 0x8CDA'
		'FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT 0x8CDB'
		'FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT 0x8CDC'
		'FRAMEBUFFER_UNSUPPORTED_EXT    0x8CDD'
		'MAX_COLOR_ATTACHMENTS_EXT      0x8CDF'
		'COLOR_ATTACHMENT0_EXT          0x8CE0'
		'COLOR_ATTACHMENT1_EXT          0x8CE1'
		'COLOR_ATTACHMENT2_EXT          0x8CE2'
		'COLOR_ATTACHMENT3_EXT          0x8CE3'
		'COLOR_ATTACHMENT4_EXT          0x8CE4'
		'COLOR_ATTACHMENT5_EXT          0x8CE5'
		'COLOR_ATTACHMENT6_EXT          0x8CE6'
		'COLOR_ATTACHMENT7_EXT          0x8CE7'
		'COLOR_ATTACHMENT8_EXT          0x8CE8'
		'COLOR_ATTACHMENT9_EXT          0x8CE9'
		'COLOR_ATTACHMENT10_EXT         0x8CEA'
		'COLOR_ATTACHMENT11_EXT         0x8CEB'
		'COLOR_ATTACHMENT12_EXT         0x8CEC'
		'COLOR_ATTACHMENT13_EXT         0x8CED'
		'COLOR_ATTACHMENT14_EXT         0x8CEE'
		'COLOR_ATTACHMENT15_EXT         0x8CEF'
		'DEPTH_ATTACHMENT_EXT           0x8D00'
		'STENCIL_ATTACHMENT_EXT         0x8D20'
		'FRAMEBUFFER_EXT                0x8D40'
		'RENDERBUFFER_EXT               0x8D41'
		'RENDERBUFFER_WIDTH_EXT         0x8D42'
		'RENDERBUFFER_HEIGHT_EXT        0x8D43'
		'RENDERBUFFER_INTERNAL_FORMAT_EXT 0x8D44'
		'STENCIL_INDEX1_EXT             0x8D46'
		'STENCIL_INDEX4_EXT             0x8D47'
		'STENCIL_INDEX8_EXT             0x8D48'
		'STENCIL_INDEX16_EXT            0x8D49'
		'RENDERBUFFER_RED_SIZE_EXT      0x8D50'
		'RENDERBUFFER_GREEN_SIZE_EXT    0x8D51'
		'RENDERBUFFER_BLUE_SIZE_EXT     0x8D52'
		'RENDERBUFFER_ALPHA_SIZE_EXT    0x8D53'
		'RENDERBUFFER_DEPTH_SIZE_EXT    0x8D54'
		'RENDERBUFFER_STENCIL_SIZE_EXT  0x8D55'
	)
! !

!OGLExtManager class methodsFor: 'GL_EXT_texture_compression_s3tc' stamp: 'ar 4/5/2006 21:16'!
initializeExtTextureCompressionS3tc
	"OGLExtManager initialize"
	"Initialize EXT_texture_compression_s3tc constants"
	self install: 'ExtTextureCompressionS3tc' withEXTConstants: #(
		'COMPRESSED_RGB_S3TC_DXT1_EXT                   0x83F0'
		'COMPRESSED_RGBA_S3TC_DXT1_EXT                  0x83F1'
		'COMPRESSED_RGBA_S3TC_DXT3_EXT                  0x83F2'
		'COMPRESSED_RGBA_S3TC_DXT5_EXT                  0x83F3'
	)
! !

!OGLExtManager class methodsFor: 'GL_SGIS_generate_mipmap' stamp: 'ar 4/5/2006 21:16'!
initializeSgisGenerateMipmap
	"GLExtensions initialize"
	"Initialize SGIS_GENERATE_MIPMAP constants"
	self install: 'SgisGenerateMipmap' withEXTConstants: #(
		'GENERATE_MIPMAP_SGIS		0x8191'
		'GENERATE_MIPMAP_HINT_SGIS	0x8192'
	)
! !

!OGLExtManager class methodsFor: 'class initialization' stamp: 'ar 4/11/2006 15:52'!
install: extension withEXTConstants: list
	"Take a list of strings comprising open gl constants, and install them into GLEXTConstants.
	See also parseSpecConstants:."
	| code selector |
	selector := ('initialize', extension) asSymbol.
	code := String new writeStream.
	code nextPutAll: selector.
	code newLineTab: 1; nextPutAll: '"This method was automatically generated by OGLExtManager"'.
	(self parseEXTConstants: list) do:[:binding|
		(GLExtConstants bindingOf: binding key) ifNil:[
			GLExtConstants addClassVarName: binding key.
		].
		code newLineTab: 1; nextPutAll: binding key; nextPutAll: ' := '.
		code print: binding value; nextPutAll: '.'.
	].
	code := code contents.
	(GLExtConstants class sourceCodeAt: selector ifAbsent:['']) = code ifTrue:[^self]. "no change"
	GLExtConstants class compile: code classified: 'pool initialization'.! !

!OGLExtManager class methodsFor: 'function helpers' stamp: 'bf 10/26/2004 22:46'!
mapType: aString using: aDictionary
	| base |
	base := aString copyWithout: $*.
	^String streamContents: [:s |
		s nextPutAll: (aDictionary at: base ifAbsent: [self error: 'type not mapped: ', base]).
		s next: (aString occurrencesOf: $*) put: $*]
! !

!OGLExtManager class methodsFor: 'instance creation' stamp: 'ar 10/29/2004 22:28'!
new
	"To use OGLExtManager we must always create a new anonymous subclass"
	| baseClass newClass |
	self == OGLExtManager ifFalse:[self error: 'OGLExtManager: Cannot create instances of subclasses'].
	baseClass := OGLExtManager. "hardcoded for a reason"
	"Create a new anonymous subclass"
	newClass := ClassBuilder new
		newSubclassOf: baseClass
		type: baseClass typeOfClass
		instanceVariables: ''
		from: nil.
	baseClass removeSubclass: newClass.
	baseClass addObsoleteSubclass: newClass.
	newClass setName: '*', baseClass name.
	^newClass basicNew
! !

!OGLExtManager class methodsFor: 'class initialization' stamp: 'ar 1/4/2003 00:00'!
parseEXTConstants: list
	"Take a list of strings comprising open gl constants, and answer them converted into suitable constant bindings. The elements of the list are expected to follow the semi-standardized rules found in most of the extensions documents, e.g., something like:
		MAX_PALETTE_MATRICES_ARB	0x8842
			=> GLMaxPaletteMatricesARB
		GENERATE_MIPMAP_SGIS			0x8191
			=> GLGenerateMipMapSGIS
		ELEMENT_ARRAY_APPLE			0x8768
			=> GLElementArrayAPPLE
		PIXEL_COUNT_NV					0x8866
			=> GLPixelCountNV
	"
	| parts constName constValue |
	^list collect:[:string|
		parts := string withBlanksTrimmed findTokens:
			(String with: Character space with: Character tab).
		constName := parts first.
		constValue := parts last.
		(constValue beginsWith: '0x') ifTrue:[
			constValue := constValue copyFrom: 3 to: constValue size.
			constValue := Integer readFrom: (ReadStream on: constValue) base: 16.
		] ifFalse:[
			constValue := Number readFrom: (ReadStream on: constValue).
		].
		parts := constName findTokens: '_'.
		constName := String streamContents:[:s|
			s nextPutAll: 'GL'.
			parts allButLast do:[:str| 
				s nextPut: str first asUppercase.
				s nextPutAll: str allButFirst asLowercase.
			].
			s nextPutAll: parts last.
		].
		constName asSymbol -> constValue
	].
! !

!OGLExtManager class methodsFor: 'function helpers' stamp: 'ar 4/4/2006 21:04'!
parseEXTFunctions: list do: aBlock
	"Take a list of strings comprising open gl functions, and answer them converted into array."
	| s return function args i type param |
	list do: [:string|
		s := string readStream.
		return := (s upTo: Character space) withBlanksTrimmed.
		function := 'gl', (s upTo: $() withBlanksTrimmed.
		args := ((s upTo: $)) findTokens: $,) collect: [:arg | 
			i := arg findLast: [:c | (c isAlphaNumeric or: [c = $_]) not].
			type := arg copyFrom: 1 to: i.
			type := type copyReplaceAll: 'const' with: ''.
			type := type copyReplaceAll: ' *' with: '*'.
			param := arg copyFrom: i+1 to: arg size.
			Array with: type withBlanksTrimmed with: param withBlanksTrimmed].
		aBlock value: return value: function value: args.
	].
! !

!OGLFontManager methodsFor: 'displaying' stamp: 'ar 6/12/2002 04:56'!
drawString: aString from: firstIndex to: lastIndex font: aFont
	| sz glFontID |
	glFontID := fonts at: aFont ifAbsentPut:[self uploadFont: aFont].
	glx glListBase: glFontID.
	sz := lastIndex - firstIndex + 1.
	sz > charList size ifTrue:[charList := ByteArray new: sz].
	charList replaceFrom: 1 to: sz with: aString startingAt: firstIndex.
	glx glCallLists: sz with: GLByte with: charList
! !

!OGLFontManager methodsFor: 'displaying' stamp: 'ar 6/17/2002 00:08'!
drawString: aString from: firstIndex to: lastIndex font: aFont align: aSymbol
	| sz glFontID rect offset |
	glFontID := fonts at: aFont ifAbsentPut:[self uploadFont: aFont].
	glx glListBase: glFontID.
	sz := lastIndex - firstIndex + 1.
	aSymbol ifNotNil:[
		rect := 0@0 extent: (aFont widthOfString: aString) @ aFont height.
		offset := (rect perform: aSymbol).
		glx glBitmap: 0 with: 0 with: 0 with: 0
			with: 0 - offset x
			with: offset y - aFont height
			with: nil.
	].
	sz > charList size ifTrue:[charList := ByteArray new: sz].
	charList replaceFrom: 1 to: sz with: aString startingAt: firstIndex.
	glx glCallLists: sz with: GLByte with: charList
! !

!OGLFontManager methodsFor: 'displaying' stamp: 'ar 6/23/2002 23:27'!
drawStringRect: rect align: aSymbol
	| offset extent |
	aSymbol ifNotNil:[
		offset := (rect perform: aSymbol).
		glx glBitmap: 0 with: 0 with: 0 with: 0
			with: rect left - offset x
			with: offset y - rect height
			with: nil.
	].
	extent := rect extent.
	glx glBitmap: extent x with: extent y with: 0 with: 0
			with: extent x
			with: extent y
			with: ((Form extent: extent depth: 1) bits atAllPut: 16rFFFFFFFF).
! !

!OGLFontManager methodsFor: 'displaying' stamp: 'ar 6/23/2002 23:28'!
frameStringRect: rect align: aSymbol
	| offset form extent |
	aSymbol ifNotNil:[
		offset := (rect perform: aSymbol).
		glx glBitmap: 0 with: 0 with: 0 with: 0
			with: rect left - offset x
			with: offset y - rect height
			with: nil.
	].
	extent := rect extent.
	form := (Form extent: extent depth: 1).
	form getCanvas frameRectangle: form boundingBox width: 1 color: Color black.
	form flipVertically.
	form bits swapEndianness.
	glx glBitmap: extent x with: extent y with: 0 with: 0
			with: extent x
			with: extent y
			with: form bits.
! !

!OGLFontManager methodsFor: 'initialize' stamp: 'ar 6/12/2002 04:49'!
initialize: glX
	glx := glX.
	fonts := IdentityDictionary new.
	charList := ByteArray new: 256.! !

!OGLFontManager methodsFor: 'displaying' stamp: 'pb 7/6/2015 00:59'!
uploadFont: aFont
	| glFontID form |
	glFontID := glx glGenLists: 256.
	0 to: 255 do:[:i|
		"form := aFont characterFormAt: (i asCharacter)."
		"form := (aFont characterFormAt: (i asCharacter)) asFormOfDepth: 1."
		form := (aFont glyphAt: (i asCharacter)) asFormOfDepth: 1.
		self flag: #pbfix.
		"major issues"
		"
		form flipVertically.
		Smalltalk isLittleEndian ifTrue: [form bits swapEndianness].
		"
		glx glNewList: glFontID+i with: GLCompile.
		glx glBitmap: form width
			with: form height
			with: 0
			with: 0
			with: form width
			with: 0
			with: form bits.
		glx glEndList.
		"
		form flipVertically.
		Smalltalk isLittleEndian ifTrue: [form bits swapEndianness].
		"
	].
	^glFontID! !

!OGLShaderManager methodsFor: 'as yet unclassified' stamp: 'pbm 8/20/2007 11:33'!
fetchShader: shaderName
	^ programObjects at: shaderName ifAbsent: [ nil ].! !

!OGLShaderManager methodsFor: 'as yet unclassified' stamp: 'dsa 7/25/2007 16:11'!
fetchShader: vertexShaderPath fragment: fragmentShaderPath
	^ programObjects at: (vertexShaderPath, fragmentShaderPath) ifAbsent: [ nil ].! !

!OGLShaderManager methodsFor: 'initialize' stamp: 'dsa 7/25/2007 16:00'!
initialize: gl
	ogl := gl.
	programObjects := Dictionary new.! !

!OGLShaderManager methodsFor: 'accessing' stamp: 'dsa 7/25/2007 16:00'!
programObjects
	^ programObjects! !

!OGLTextureHandle methodsFor: 'accessing'!
bytesUsed
	^bytesUsed! !

!OGLTextureHandle methodsFor: 'accessing'!
bytesUsed: nBytes
	bytesUsed := nBytes! !

!OGLTextureHandle methodsFor: 'accessing'!
glID
	^glID! !

!OGLTextureHandle methodsFor: 'accessing'!
glID: aNumber
	glID := aNumber! !

!OGLTextureHandle methodsFor: 'accessing' stamp: 'ar 9/23/2002 23:20'!
isAllocated
	^allocated! !

!OGLTextureHandle methodsFor: 'accessing' stamp: 'ar 9/23/2002 23:20'!
isAllocated: aBool
	allocated := aBool! !

!OGLTextureHandle methodsFor: 'accessing' stamp: 'das 1/2/2006 19:57'!
objectID

	^objectID.! !

!OGLTextureHandle methodsFor: 'accessing' stamp: 'das 1/2/2006 19:57'!
objectID: oid

	objectID := oid.! !

!OGLTextureHandle methodsFor: 'printing' stamp: 'ar 10/1/2004 15:19'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll:'('; print: glID; nextPutAll:', '; print: scaledSize; nextPutAll:', '; print: timeStamp; nextPutAll:')'.! !

!OGLTextureHandle methodsFor: 'accessing'!
scaledSize
	^scaledSize! !

!OGLTextureHandle methodsFor: 'accessing'!
scaledSize: aPoint
	scaledSize := aPoint! !

!OGLTextureHandle methodsFor: 'accessing' stamp: 'ar 9/23/2002 23:42'!
target
	^target! !

!OGLTextureHandle methodsFor: 'accessing' stamp: 'ar 9/23/2002 23:42'!
target: aNumber
	target := aNumber! !

!OGLTextureHandle methodsFor: 'accessing' stamp: 'ar 10/1/2004 15:19'!
timeStamp
	^timeStamp! !

!OGLTextureHandle methodsFor: 'accessing' stamp: 'ar 10/1/2004 15:19'!
timeStamp: aStamp
	timeStamp := aStamp! !

!OGLTextureManager methodsFor: 'initialize'!
beginFrame
	memFrame := 0.! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'jcg 6/12/2006 22:40'!
bindTexture: aTForm
	| handle |
	handle := self textureHandleOf: aTForm.
	handle timeStamp = ogl timeStamp ifFalse:[
		handle timeStamp: ogl timeStamp.
		memFrame := memFrame + handle bytesUsed.
	].
	ogl glBindTexture: aTForm target with: handle glID.! !

!OGLTextureManager methodsFor: 'initialize' stamp: 'das 1/8/2006 11:10'!
cleanup
	"clean up any lost references"
	| |
	"first pass - clean up any unreferenced textures"
"	any := false.
	textures associationsDo:[:assoc|
		assoc key == nil ifTrue:[
			handle := assoc value.
			ogl glDeleteTextures: 1 with: (WordArray with: handle glID).
			memUsed := memUsed - handle bytesUsed.
			any := true].
	].
	any ifTrue:[textures finalizeValues]."
	"second pass - if we exceed maxMem of texture space clean up oldest (unused) textures"
	self releaseTextureMemoryIfNeeded.
! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'dsa 7/26/2007 11:03'!
compressTextures
	^ compressTextures! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'dsa 7/26/2007 11:04'!
compressTextures: ct
	compressTextures := ct! !

!OGLTextureManager methodsFor: 'private' stamp: 'das 1/2/2006 19:51'!
compressedSizeOf: aTForm at: level form: aForm
	(ogl glGetTexLevelParameteri: aTForm target
		with: level
		with: GLTextureCompressedARB) = 0 ifTrue:[
			"texture was not compressed"
			^aForm width * aForm height * 4.
	].
	^ogl glGetTexLevelParameteri: aTForm target
		with: level
		with: GLTextureCompressedImageSizeARB! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'jcg 7/28/2006 14:34'!
deleteTexture: aTForm
	| handle |
	handle := textures removeKey: aTForm sha ifAbsent:[nil].
	handle ifNotNil:[
		ogl glDeleteTextures: 1 with: (WordArray with: handle glID).
		memUsed := memUsed - handle bytesUsed.
	].! !

!OGLTextureManager methodsFor: 'initialize' stamp: 'ar 4/9/2002 22:51'!
endFrame
	self cleanup.
"	'Max: ', memUsed asStringWithCommas displayAt: 0@0.
	'Now: ', memFrame asStringWithCommas displayAt: 0@15.
	Display forceToScreen: (0@0 extent: 100@30).
"! !

!OGLTextureManager methodsFor: 'initialize' stamp: 'dsa 7/26/2007 11:10'!
initialize: gl
	ogl := gl.
	textures := Dictionary new.
	memAvail := 16*1024*1024.
	memUsed := 0.
	memFrame := 0.
	compressTextures := false.! !

!OGLTextureManager methodsFor: 'stats' stamp: 'ar 4/26/2003 01:38'!
memFrame
	^memFrame! !

!OGLTextureManager methodsFor: 'stats' stamp: 'ar 4/26/2003 01:38'!
memUsed
	^memUsed! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'das 1/2/2006 19:42'!
releaseTexture: aTForm
	| binding |
	binding := textures associationAt: aTForm sha ifAbsent:[^self].
	binding key: nil. "clean up after frame completed"
! !

!OGLTextureManager methodsFor: 'initialize' stamp: 'das 1/2/2006 20:25'!
releaseTextureMemoryIfNeeded
	"Release some textures if we exceed a certain space usage"
	| maxMem list sha |
	true ifTrue:[^self]. "doesn't *quite* work yet"
	"set upper memory bound"
	maxMem := 32*1024*1024. "for testing - set this to zero to force uploads on every frame"
	"Make sure we don't release anything if we needed it all in the current frame.
	Leave a bit of headroom for sudden changes in direction too."
	maxMem := maxMem max: memFrame + (4*1024*1024).
	memUsed > maxMem ifFalse:[^self].
	list := textures values asArray sort:[:h1 :h2| 
		h1 timeStamp = h2 timeStamp
			ifTrue:[h1 bytesUsed >= h2 bytesUsed]
			ifFalse:[h1 timeStamp < h2 timeStamp].
	].
	list do:[:handle|
		sha := textures keyAtIdentityValue: handle.
		textures removeKey: sha.
		ogl glDeleteTextures: 1 with: (WordArray with: handle glID).
		memUsed := memUsed - handle bytesUsed.
		memUsed < maxMem ifTrue:[^self].
	].
! !

!OGLTextureManager methodsFor: 'private'!
scaleTexture: form to: scaledSize
	| mmForm |
	form extent = scaledSize ifTrue:[^form].
	mmForm := Form extent: scaledSize depth: 32.
	(WarpBlt current toForm: mmForm)
		sourceForm: form destRect: mmForm boundingBox;
		combinationRule: 3;
		cellSize: 2;
		warpBits.
	^mmForm! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'das 1/2/2006 19:47'!
setTextureSizeOf: aTForm to: aPoint
	| handle |
	handle := self textureHandleOf: aTForm ifAbsent:[nil].
	handle ifNil:[^nil]. "unknown"
	^handle scaledSize: aPoint! !

!OGLTextureManager methodsFor: 'initialize' stamp: 'das 1/5/2006 01:14'!
setupHandle: aTForm

	| holder handle newMem |

	aTForm invalidate: aTForm boundingBox.
	holder := WordArray new: 1.
	ogl glGenTextures: 1 with: holder.
	handle := OGLTextureHandle new.
	handle objectID: aTForm objectID.
	handle glID: (holder at: 1).
	handle scaledSize: aTForm trueExtent.
	handle timeStamp: ogl timeStamp.
	newMem := handle scaledSize area * 4.
	aTForm bMipmap ifTrue:[newMem := newMem + (newMem // 3)].
	handle bytesUsed: newMem.
	memUsed := memUsed + newMem.
	memFrame := memFrame + newMem.
	^handle.
! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'jcg 6/12/2006 22:30'!
textureHandleOf: aTForm
	| handle |
	"Find (or make) the handle. If it has a different objectID from the TForm, remake it."
	handle := textures at: aTForm sha ifAbsentPut:[self setupHandle: aTForm].
	handle objectID = aTForm objectID ifFalse:[
		"To work around a bug in some OpenGL implementations (Panasonic R3 w/ Intel OGL drivers for example) we destroy and recreate the handle here rather than recycling it. That's because on the aforementioned machine we had very strange effects when we tried to switch texture resolutions while using GLGenerateMipmapSGIS when replacing the thumbnails with cached versions."
		textures removeKey: aTForm sha.
		ogl glDeleteTextures: 1 with: (WordArray with: handle glID).
		handle := self setupHandle: aTForm.
		textures at: aTForm sha put: handle.
	].
	^handle.! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'ar 1/30/2006 12:53'!
textureHandleOf: aTForm ifAbsent: aBlock
	"First, find the real texture if necessary"
	| tf |
	aTForm isThumb 
		ifTrue:[tf := ogl formManager resolve: aTForm distance:ogl distance]
		ifFalse:[tf := aTForm].
	^textures at: tf sha ifAbsent: aBlock! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'das 1/2/2006 19:47'!
textureSizeOf: aTForm
	| handle |
	handle := self textureHandleOf: aTForm ifAbsent:[nil].
	handle ifNil:[^nil]. "unknown"
	^handle scaledSize! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'jcg 6/12/2006 22:31'!
updateTextures: updateList
	updateList do:[:tf | 
		"upload the new texture for the current texture handle"
		self uploadTexture: tf. 
	].! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'das 1/2/2006 19:48'!
uploadTexture: aTForm
	| rect |
	rect := aTForm updateRect ifNil:[^self]. "nothing to upload"
	self uploadTexture: aTForm dirtyRect: rect.
	aTForm resetUpdateRect.! !

!OGLTextureManager methodsFor: 'accessing' stamp: 'dsa 7/26/2007 11:13'!
uploadTexture: aTForm dirtyRect: aRectangle
	"TODO: Later only upload the dirty portion"
	| handle form level scaledSize dirtyRect autoMip internalFormat byteSize format |

	autoMip := aTForm bMipmap and:[ogl autoMipmap].

	handle := self textureHandleOf: aTForm.
	handle timeStamp: ogl timeStamp.
	form := aTForm textureForm.
	form unhibernate.
	dirtyRect := aRectangle.
	aTForm isStatic ifTrue:[
		"Static (possibly compressed) textures typically require complete uploads;
		as they don't change often just upload everything."
		dirtyRect := 0@0 extent: aTForm textureForm extent.
	].
	dirtyRect := dirtyRect intersect: form boundingBox.
	ogl glBindTexture: aTForm target with: handle glID.
	ogl glTexParameteri: GLTexture2d with: GLTextureMinFilter with: aTForm minFilter.
	ogl glTexParameteri: GLTexture2d with: GLTextureMagFilter with: aTForm magFilter.
	autoMip ifTrue:[ogl glTexParameteri: GLTexture2d with: GLGenerateMipmapSGIS with: true].

	"TODO: This would be the right place to figure out what the initial size of the texture should be given that we're already low on texture memory."
	handle scaledSize: aTForm trueExtent.
	scaledSize := handle scaledSize.

	"specific optimization for morphic textures"
	(dirtyRect extent ~= form extent "needed since first-time upload *must* use glTexImage2D"
		and:[form depth = 32 "needed since we only upload 32bpp forms"
		and:[scaledSize = form extent "needed since we don't scale here"
		and:[autoMip or:[aTForm bMipmap not "only allow for non-mipmaps"]]]]) ifTrue:[
			ogl glPixelStorei: GLUnpackRowLength with: form width.
			ogl glPixelStorei: GLUnpackSkipRows with: dirtyRect top.
			ogl glPixelStorei: GLUnpackSkipPixels with: dirtyRect left.
			ogl glTexSubImage2D: aTForm target with: 0 
				with: dirtyRect left with: dirtyRect top 
				with: dirtyRect width with: dirtyRect height 
				with: ogl texturePixelFormat with: ogl texturePixelType 
				with: form bits.
			ogl glPixelStorei: GLUnpackRowLength with: 0.
			ogl glPixelStorei: GLUnpackSkipRows with: 0.
			ogl glPixelStorei: GLUnpackSkipPixels with: 0.
			^self].

	level := 0.
	"figure out the internal format to use"
	internalFormat := ogl textureInternalFormat.
	aTForm isStatic ifTrue:[
		(ogl hasExtension: #'GL_ARB_texture_compression') ifTrue:[
			"internalFormat := ogl textureInternalFormatCompressed."
		].
		byteSize := 0.
	].

	[
		format := internalFormat.

		(compressTextures) ifTrue: [ format := GLCompressedRgbaS3tcDxt5EXT ]. "$$ assumes RGBA, shouldn't actually do this for real-time textures"

		form := self scaleTexture: form to: scaledSize.
		ogl glTexImage2D: aTForm target 
			with: level 
			with: format
			with: form width with: form height 
			with: 0 
			with: ogl texturePixelFormat 
			with: ogl texturePixelType 
			with: form bits.
		aTForm isStatic ifTrue:[
			byteSize := byteSize + (self compressedSizeOf: aTForm at: level form: form).
		].
	autoMip not 
		and:[aTForm bMipmap 
		and:[scaledSize x > 1 or:[scaledSize y > 1]]]] whileTrue:[
			scaledSize := (scaledSize // 2) max: (1@1).
			level := level + 1.
	].

	aTForm isStatic ifTrue:[
		memUsed := memUsed - handle bytesUsed.
		memFrame := memFrame - handle bytesUsed.
		handle bytesUsed: byteSize.
		memUsed := memUsed + handle bytesUsed.
		memFrame := memFrame + handle bytesUsed.
	].
! !

!OpenGL methodsFor: 'testing' stamp: 'ar 1/3/2003 22:39'!
autoMipmap
	"Answer whether we support automatic mipmap generation"
	^self extensions includes: #'GL_SGIS_generate_mipmap'! !

!OpenGL methodsFor: 'accessing' stamp: 'das 1/20/2006 20:40'!
avatar

	^ avatar.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 1/20/2006 20:40'!
avatar: atar

	avatar := atar.! !

!OpenGL methodsFor: 'initialize' stamp: 'bgf 8/3/2006 19:39'!
beginFrame
	numVtx := numPrims := 0.
	textureManager beginFrame.
	forceHilite := nil.

"For future anti-aliasing work... 
 ARB_multisample requires changes in VM plugin glCreateRendererFlags.

	self glEnable: GLBlend.
	self glBlendFunc: GLSrcAlpha with: GLOneMinusSrcAlpha.
	self glEnable: GLMultisampleARB.  
	self glHint: GLMultisampleFilterHintNV with: GLNicest.
...."

	self glPixelStorei: GLUnpackAlignment with: 4.
	matrixStack := OrderedCollection new.
	portalDepth := 0.! !

!OpenGL methodsFor: 'rendering' stamp: 'das 7/6/2006 23:54'!
beginVertices: vertices normals: vtxNormals colors: vtxColors texCoords: vtxTexCoords
	| size |

	vtxColors ifNotNil:[
		self glEnableClientState: GLColorArray.
		self glEnable: GLColorMaterial.
		self glColorPointer: 4 with: GLFloat with: 16 with: vtxColors.
	].
	vtxNormals ifNotNil:[
		self glEnableClientState: GLNormalArray.
		self glNormalPointer: GLFloat with: 12 with: vtxNormals.
	].
	vtxTexCoords ifNotNil:[
		size := vtxTexCoords contentsSize.
		self glEnableClientState: GLTextureCoordArray.
		self glTexCoordPointer: size with: GLFloat with: size*4 with: vtxTexCoords.
	].
	self glEnableClientState: GLVertexArray.
	self glVertexPointer: 3 with: GLFloat with: 12 with: vertices.
! !

!OpenGL methodsFor: 'rendering' stamp: 'dsa 6/7/2007 13:16'!
beginVerticesVBO: vertices normals: vtxNormals colors: vtxColors texCoords: vtxTexCoords
	"this method accepts either the raw data or a bufferObjectID for each parameter"	

	vtxColors ifNotNil:[
		self glEnableClientState: GLColorArray.
		self glEnable: GLColorMaterial.

		(vtxColors isInteger) ifTrue: [
			self glBindBufferARB: GLArrayBufferARB with: vtxColors.
			self glColorPointer: 4 with: GLFloat with: 0 with: nil.
		]
		ifFalse: [ self glColorPointer: 4 with: GLFloat with: 0 with: vtxColors. ].
	].

	vtxNormals ifNotNil:[
		self glEnableClientState: GLNormalArray.

		(vtxNormals isInteger) ifTrue: [
			self glBindBufferARB: GLArrayBufferARB with: vtxNormals.
			self glNormalPointer: GLFloat with: 0 with: nil.
		] 	
		ifFalse: [ self glNormalPointer: GLFloat with: 0 with: vtxNormals. ].
	].

	vtxTexCoords ifNotNil:[
		self glEnableClientState: GLTextureCoordArray.

		(vtxTexCoords isInteger) ifTrue: [
			self glBindBufferARB: GLArrayBufferARB with: vtxTexCoords.
			self glTexCoordPointer: 2 with: GLFloat with: 0 with: nil. "$$ blah, assumes 2"
		]
		ifFalse: [ self glTexCoordPointer: 2 with: GLFloat with: 0 with: vtxTexCoords. ].
	].

	self glEnableClientState: GLVertexArray.

	(vertices isInteger) ifTrue: [
		self glBindBufferARB: GLArrayBufferARB with: vertices.
		self glVertexPointer: 3 with: GLFloat with: 0 with: nil.
	]
	ifFalse: [ self glVertexPointer: 3 with: GLFloat with: 0 with: vertices. ].
! !

!OpenGL methodsFor: 'shaders' stamp: 'dsa 6/13/2007 10:48'!
bindProgram: program
	self glUseProgramObjectARB: program.
	^ true! !

!OpenGL methodsFor: 'textures' stamp: 'das 1/5/2006 16:29'!
bindTexture: aTForm
	changeTexture ifFalse:[^false].
	textureManager bindTexture: aTForm.
	^ true.! !

!OpenGL methodsFor: 'accessing' stamp: 'dsa 6/7/2007 16:22'!
bufferObjects
	^ bufferObjects.! !

!OpenGL methodsFor: 'initialize' stamp: 'ar 5/6/2005 14:47'!
bufferRect: box
	"Set and validate the receiver's buffer rectangle"
	bufRect = box ifTrue:[^self].
	(self primRender: handle setBufferRectX: box left y: box top w: box width h: box height) ifNil:[
		"Failed to set the buffer rect. Destroy the receiver and start over."
		self destroy.
		^nil].
	"We did change the buffer rect; make sure our target is up to date"
	bufRect := box.
	^self! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 9/29/2004 15:12'!
camera
	^camera! !

!OpenGL methodsFor: 'accessing' stamp: 'das 10/28/2005 12:14'!
camera: cam
	camera := cam.! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 9/29/2004 15:12'!
changeTexture

	^ changeTexture.! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 9/29/2004 15:12'!
changeTexture: bool

	changeTexture := bool.! !

!OpenGL methodsFor: 'debugging' stamp: 'jcg 5/15/2005 22:07'!
checkForError
	"Check for any OpenGL errors, and raise an Error if any exist."
	| error |

	error := self glGetError.
	(error ~= GLNoError) ifTrue: [self error: error printString]
! !

!OpenGL methodsFor: 'as yet unclassified' stamp: 'pb 3/16/2010 19:28'!
checkForErrors
	| errNo |
	errNo := self glGetError.
	errNo ~= GLNoError
		ifTrue: [
			"errs := Dictionary new at: GLInvalidEnum put: 'Invalid Enum';
						 at: GLInvalidValue put: 'Invalid Value';
						 at: GLInvalidOperation put: 'Invalid Operation';
						 at: GLStackOverflow put: 'Stack Overflow';
						 at: GLStackUnderflow put: 'Stack Underflow';
						 at: GLOutOfMemory put: 'Out of Memory';
						 yourself.
			Error signal: 'OpenGL error: ' , errNo printString , ' description: ' , (errs at: errNo ifAbsent: 'Unknown error') printString"
			Error signal:((self class) errorStringWith: errNo)]! !

!OpenGL methodsFor: 'initialize' stamp: 'ar 9/6/2002 01:44'!
cleanup
	"Clean up any unused display list"
	| any listRange |
	any := false.
	glListRegistry associationsDo:[:assoc|
		assoc key == nil ifTrue:[
			listRange := assoc value.
			self glDeleteLists: listRange first with: listRange last.
			any := true].
	].
	any ifTrue:[glListRegistry finalizeValues].
! !

!OpenGL methodsFor: 'shaders' stamp: 'dsa 7/16/2007 13:24'!
createShaderObject: type path: path
	| shaderObject shaderFile shaderContents shaderArray |

	((type = GLVertexShaderARB) or: [type = GLFragmentShaderARB])
		ifFalse: [ Transcript show: 'unknown shader type'; cr. ^ 0 ].

	shaderObject := self glCreateShaderObjectARB: type.
	shaderFile := FileStream fileNamed: path.
	shaderContents := shaderFile contentsOfEntireFile.
	shaderFile close.

	shaderArray := NullTerminatedStringArray withStrings: (Array with: shaderContents).

	self glShaderSourceARB: shaderObject with: 1 with: (shaderArray pointers getHandle) with: nil.	
	shaderArray free.
	self glCompileShaderARB: shaderObject.
	self checkForErrors.

	^ shaderObject
	! !

!OpenGL methodsFor: 'shaders' stamp: 'pbm 8/20/2007 11:28'!
createShaderObject: type source: shaderSource
	| shaderObject  shaderArray |

	((type = GLVertexShaderARB) or: [type = GLFragmentShaderARB])
		ifFalse: [ Transcript show: 'unknown shader type'; cr. ^ 0 ].

	shaderObject := self glCreateShaderObjectARB: type.

	shaderArray := NullTerminatedStringArray withStrings: (Array with: shaderSource).

	self glShaderSourceARB: shaderObject with: 1 with: (shaderArray pointers getHandle) with: nil.	
	shaderArray free.
	self glCompileShaderARB: shaderObject.
	self checkForErrors.

	^ shaderObject
	! !

!OpenGL methodsFor: 'textures' stamp: 'das 1/5/2006 17:38'!
deleteTexture: aTForm
	
	textureManager deleteTexture: aTForm! !

!OpenGL methodsFor: 'initialize' stamp: 'ar 5/6/2005 14:47'!
destroy
	glExt ifNotNil:[
		glExt ogl: nil.
		glExt := nil].
	self primDestroyRenderer: handle.
	handle := nil.! !

!OpenGL methodsFor: 'textures' stamp: 'das 10/18/2002 21:00'!
disableTexture: aTexture
	changeTexture ifFalse:[^false].
	self glDisable: aTexture target.
	^ true.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 1/3/2006 12:44'!
distance

	^ distance.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 1/3/2006 12:44'!
distance: dist

	distance:= dist.! !

!OpenGL methodsFor: 'rendering' stamp: 'pbm 6/5/2007 10:06'!
drawIndexed: mode faces: faces 

	| realMode |

	realMode := forceWire ifTrue: [GLLineStrip] ifFalse: [mode].

	numVtx := numVtx + faces size.
	numPrims := numPrims + (self primCount: faces size forMode: realMode).

	self 
		glDrawElements: realMode
		with: faces basicSize
		with: GLUnsignedInt 
		with: faces.

! !

!OpenGL methodsFor: 'rendering' stamp: 'das 6/30/2006 10:38'!
drawIndexed: mode faces: faces vertices: vertices normals: vtxNormals colors: vtxColors texCoords: vtxTexCoords
	| size realMode |

	realMode := forceWire ifTrue: [GLLineStrip] ifFalse: [mode].

	numVtx := numVtx + faces size.
	numPrims := numPrims + (self primCount: faces size forMode: realMode).

	vtxColors ifNotNil:[
		self glEnableClientState: GLColorArray.
		self glEnable: GLColorMaterial.
		self glColorPointer: 4 with: GLFloat with: 16 with: vtxColors.
	].
	vtxNormals ifNotNil:[
		self glEnableClientState: GLNormalArray.
		self glNormalPointer: GLFloat with: 12 with: vtxNormals.
	].
	vtxTexCoords ifNotNil:[
		size := vtxTexCoords contentsSize.
		self glEnableClientState: GLTextureCoordArray.
		self glTexCoordPointer: size with: GLFloat with: size*4 with: vtxTexCoords.
	].
	self glEnableClientState: GLVertexArray.
	self glVertexPointer: 3 with: GLFloat with: 12 with: vertices.
	self 
		glDrawElements: realMode
		with: faces basicSize
		with: GLUnsignedInt 
		with: faces.

	vtxColors ifNotNil:[self glDisableClientState: GLColorArray].
	vtxNormals ifNotNil:[self glDisableClientState: GLNormalArray].
	vtxTexCoords ifNotNil:[self glDisableClientState: GLTextureCoordArray].
	self glDisableClientState: GLVertexArray.
	self glDisableClientState: GLColorArray.
	self glDisable: GLColorMaterial.
! !

!OpenGL methodsFor: 'rendering' stamp: 'dsa 6/7/2007 17:17'!
drawIndexedBufferObject: mode faces: faces elementBuffer: elementBufferObject 

	| realMode |

	realMode := forceWire ifTrue: [GLLineStrip] ifFalse: [mode].

	numVtx := numVtx + faces size.
	numPrims := numPrims + (self primCount: faces size forMode: realMode).

	self glBindBufferARB: GLElementArrayBufferARB with: elementBufferObject.

	self glDrawElements: realMode
		       with: faces byteSize
		       with: GLUnsignedInt 
		       with: nil.

	self glBindBufferARB: GLElementArrayBufferARB with: 0.

! !

!OpenGL methodsFor: 'text support' stamp: 'ar 6/12/2002 04:53'!
drawString: s at: pt

	^ self drawString: s at: pt font: nil color: Color blue! !

!OpenGL methodsFor: 'text support' stamp: 'pb 3/27/2010 13:57'!
drawString: s at: pt font: aFont color: aColor
	self glRasterPos3f: pt x with: pt y with: pt z.
	self glColor4f: aColor red with: aColor green with: aColor blue with: aColor alpha.
	self flag: #pbfix. "cuis doesn't have"
	"fontManager drawString: s from: 1 to: s size font: (aFont ifNil:[TextStyle defaultFont])."
	fontManager drawString: s from: 1 to: s size font: (aFont ifNil:[TextStyle default font])! !

!OpenGL methodsFor: 'text support' stamp: 'pb 6/6/2012 03:28'!
drawString: s at: pt font: aFont color: aColor align: align
	self glColor4f: aColor red with: aColor green with: aColor blue with: aColor alpha.
	self glRasterPos3f: pt x with: pt y with: pt z.
	self flag: #pbfix. "cuis doesn't have"
	"fontManager drawString: s from: 1 to: s size font: (aFont ifNil:[TextStyle defaultFont]) align: align."
	"fontManager drawString: s from: 1 to: s size font: (aFont ifNil:[TextStyle default font]) align: align."
	fontManager drawString: s from: 1 to: s size font: (aFont ifNil:[Preferences standardListFont]) align: align.
! !

!OpenGL methodsFor: 'text support' stamp: 'ar 6/23/2002 20:44'!
drawString: s from: startIndex to: stopIndex at: pt font: font
	self glRasterPos3f: pt x with: pt y with: pt z.
	fontManager drawString: s from: startIndex to: stopIndex font: (font ifNil:[TextStyle defaultFont]) align: nil.! !

!OpenGL methodsFor: 'text support' stamp: 'ar 6/30/2002 21:56'!
drawString: s from: startIndex to: stopIndex at: pt font: font align: align
	self glRasterPos3f: pt x with: pt y with: pt z.
	fontManager drawString: s from: startIndex to: stopIndex font: (font ifNil:[TextStyle defaultFont]) align: align.! !

!OpenGL methodsFor: 'text support' stamp: 'ar 6/30/2002 21:28'!
drawString: s from: startIndex to: stopIndex at: pt font: font color: aColor
	self glColor4f: aColor red with: aColor green with: aColor blue with: aColor alpha.
	self glRasterPos3f: pt x with: pt y with: pt z.
	fontManager drawString: s from: startIndex to: stopIndex font: (font ifNil:[TextStyle defaultFont]) align: nil.! !

!OpenGL methodsFor: 'text support' stamp: 'ar 6/23/2002 19:30'!
drawStringRect: extent at: pt color: aColor align: align
	self glColor4f: aColor red with: aColor green with: aColor blue with: aColor alpha.
	self glRasterPos3f: pt x with: pt y with: pt z.
	fontManager drawStringRect: extent align: align.! !

!OpenGL methodsFor: 'textures' stamp: 'das 10/18/2002 21:00'!
enableTexture: aTexture
	changeTexture ifFalse:[^false].
	self glEnable: aTexture target.
	^ true.! !

!OpenGL methodsFor: 'rendering' stamp: 'das 7/7/2006 00:09'!
end

	self glDisableClientState: GLColorArray.
	self glDisableClientState: GLNormalArray.
	self glDisableClientState: GLTextureCoordArray.
	self glDisableClientState: GLVertexArray.
	self glDisableClientState: GLColorArray.
	self glDisable: GLColorMaterial.
! !

!OpenGL methodsFor: 'initialize' stamp: 'bgf 7/9/2006 16:43'!
endFrame
	textureManager endFrame.
	self cleanup.
	matrixStack size > 0 ifTrue:[self error:'Unbalanced OGL matrix stack'].
	forceHilite ifNotNil: [self error: 'Unbalanced forceHilite.'].
! !

!OpenGL methodsFor: 'rendering' stamp: 'dsa 6/13/2007 14:42'!
endVBO

	self glBindBufferARB: GLArrayBufferARB with: 0.

	self glDisableClientState: GLColorArray.
	self glDisableClientState: GLNormalArray.
	self glDisableClientState: GLTextureCoordArray.
	self glDisableClientState: GLVertexArray.
	self glDisable: GLColorMaterial.
! !

!OpenGL methodsFor: 'rendering' stamp: 'das 7/6/2006 23:56'!
endVertices: vertices normals: vtxNormals colors: vtxColors texCoords: vtxTexCoords

	vtxColors ifNotNil:[self glDisableClientState: GLColorArray].
	vtxNormals ifNotNil:[self glDisableClientState: GLNormalArray].
	vtxTexCoords ifNotNil:[self glDisableClientState: GLTextureCoordArray].
	self glDisableClientState: GLVertexArray.
	self glDisableClientState: GLColorArray.
	self glDisable: GLColorMaterial.
! !

!OpenGL methodsFor: 'rendering' stamp: 'dsa 6/13/2007 14:42'!
endVerticesVBO: vertices normals: vtxNormals colors: vtxColors texCoords: vtxTexCoords

	self glBindBufferARB: GLArrayBufferARB with: 0.

	vtxColors ifNotNil:[self glDisableClientState: GLColorArray].
	vtxNormals ifNotNil:[self glDisableClientState: GLNormalArray].
	vtxTexCoords ifNotNil:[self glDisableClientState: GLTextureCoordArray].
	self glDisableClientState: GLVertexArray.
	self glDisable: GLColorMaterial.
! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 8/3/2006 19:36'!
enterPortal2D
	"Note that we're starting a render through a 2D portal."
	portalDepth := portalDepth + 1.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 12/9/2005 12:29'!
eventPointer

	^ eventPointer.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 12/9/2005 12:29'!
eventPointer: ep

	eventPointer := ep.! !

!OpenGL methodsFor: 'extensions' stamp: 'ar 10/7/2006 15:52'!
extensions
	"answer a set of extension strings for this renderer"
	| version |
	^extensions ifNil:[
		extensions := (self glGetString: GLExtensions) fromCString.
		extensions := extensions findTokens: ' '.
		extensions := extensions collect:[:str| str asSymbol].
		extensions := IdentitySet withAll: extensions.
		"Many extensions are implicit in the reported OpenGL version and may not be reported via the extensions. There seems to be no rule for this - I have seen some extensions listed, some not, so we simply add all of them based on the OpenGL version we find here."

		version := self version.

		version >= 1.1 ifTrue:[
			extensions addAll: #(
				#'GL_EXT_blend_logic_op'
				#'GL_EXT_copy_texture'
				#'GL_EXT_polygon_offset'
				#'GL_EXT_subtexture'
				#'GL_EXT_texture'
				#'GL_EXT_texture_object'
				#'GL_EXT_vertex_array'
			).
		].

		version >= 1.2 ifTrue:[
			extensions addAll: #(
				#'GL_EXT_bgra'
				#'GL_EXT_blend_color'
				#'GL_EXT_draw_range_elements'
				#'GL_EXT_packed_pixels'
				#'GL_EXT_rescale_normal'
				#'GL_EXT_separate_specular_color'
				#'GL_EXT_texture3D'
				#'GL_SGIS_texture_edge_clamp'
			).
		].

		version >= 1.3 ifTrue:[
			extensions addAll: #(
				#'GL_ARB_multisample'
				#'GL_ARB_multitexture'
				#'GL_ARB_texture_border_clamp'
				#'GL_ARB_texture_compression'
				#'GL_ARB_texture_cube_map'
				#'GL_ARB_texture_env_add'
				#'GL_ARB_texture_env_combine'
				#'GL_ARB_texture_env_dot3'
				#'GL_ARB_transpose_matrix'
			).
		].

		version >= 1.4 ifTrue:[
			extensions addAll: #(
				#'GL_ARB_point_parameters'
				#'GL_ARB_texture_env_crossbar'
				#'GL_ARB_window_pos'
				#'GL_EXT_blend_func_separate'
				#'GL_EXT_fog_coord'
				#'GL_EXT_multi_draw_arrays'
				#'GL_EXT_secondary_color'
				#'GL_EXT_stencil_wrap'
				#'GL_NV_blend_square'
				#'GL_SGIS_generate_mipmap'
				#'GL_ARB_depth_texture'
				#'GL_ARB_shadow'
				#'GL_ARB_texture_mirrored_repeat'
				#'GL_EXT_texture_lod_bias'
				#'GL_SGIS_texture_lod'
			).
		].

		version >= 1.5 ifTrue:[
			extensions addAll: #(
				#'GL_ARB_vertex_buffer_object'
				#'GL_ARB_occlusion_query'
				#'GL_EXT_shadow_funcs'
			).
		].

		extensions].! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 6/30/2002 21:31'!
extent
	^bufRect extent! !

!OpenGL methodsFor: 'accessing' stamp: 'das 5/25/2006 13:51'!
flipFace
	
	frontFace = GLCcw ifTrue:[frontFace := GLCw] ifFalse:[frontFace:= GLCcw].
	self setCull.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 5/24/2006 20:41'!
fogOn:bool

	(fogOn:= bool) ifTrue:[ self glEnable: GLFog.]
	ifFalse:[self glDisable: GLFog.].! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 6/23/2002 21:01'!
fontManager
	^fontManager! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 7/9/2006 16:43'!
forceHilite

	"If non-nil, the color returned is to override all material coloring...
       used for interactive highlighting."

	^ forceHilite! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 7/9/2006 20:59'!
forceHilite: aColor

	"Install the given color to force hiliting until cleared.
       Return true if accepted, false if the hilite state has already been set."

	aColor ifNotNil: [
		forceHilite ifNil: [
			forceHilite := aColor.
			^ true.
		] ifNotNil: [
			^ false.
		].
	] ifNil: [
		forceHilite ifNil: [
			^ false.
		] ifNotNil: [
			forceHilite := nil.
			^ true.
		].
	].! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 3/12/2006 12:18'!
forcePick
	^forcePick! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 2/24/2006 12:09'!
forcePick: aBoolean
	forcePick := aBoolean! !

!OpenGL methodsFor: 'accessing' stamp: 'das 8/12/2004 10:01'!
forceWire

	^ forceWire.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 8/12/2004 10:01'!
forceWire: bool

	forceWire := bool.! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 1/30/2006 12:52'!
formManager
	^formManager! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 1/30/2006 12:53'!
formManager: aTFormManager
	formManager := aTFormManager! !

!OpenGL methodsFor: 'text support' stamp: 'ar 6/23/2002 19:38'!
frameStringRect: extent at: pt color: aColor align: align
	self glColor4f: aColor red with: aColor green with: aColor blue with: aColor alpha.
	self glRasterPos3f: pt x with: pt y with: pt z.
	fontManager frameStringRect: extent align: align.! !

!OpenGL methodsFor: 'accessing'!
frontFace
	
	^ frontFace.! !

!OpenGL methodsFor: 'buffer objects' stamp: 'pbm 7/23/2007 13:30'!
generateBufferObject: bufferArray mode: mode
	^ self generateBufferObject: bufferArray type: GLArrayBufferARB mode: mode! !

!OpenGL methodsFor: 'buffer objects' stamp: 'pbm 7/23/2007 13:31'!
generateBufferObject: bufferArray type: type mode: mode
	| holder bufferID |

	bufferArray ifNil: [ ^ nil. ]. 

	holder := WordArray new: 1.
	self glGenBuffersARB: 1 with: holder.

	bufferID := holder at: 1.
	self glBindBufferARB: type with: bufferID.
	self glBufferDataARB: type 
		with: bufferArray byteSize with: bufferArray with: mode.

	^ bufferID! !

!OpenGL methodsFor: 'Keyword API' stamp: 'pb 6/6/2012 00:18'!
glAccum: op with: value
	"This method was automatically generated."
	"void glAccum(GLenum op, GLfloat value);"
	<apicall: void 'glAccum' (ulong float)  module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'pb 6/6/2012 00:19'!
glActiveTextureARB: texture
	"This method was automatically generated."
	"void glActiveTextureARB(GLenum texture);"
	<apicall: void 'glActiveTextureARB' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glAlphaFunc: func with: ref
	"This method was automatically generated."
	"void glAlphaFunc(GLenum func, GLclampf ref);"
	<apicall: void 'glAlphaFunc' (ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glAreTexturesResident: n with: textures with: residences
	"This method was automatically generated."
	"GLboolean glAreTexturesResident(GLsizei n, GLuint* textures, GLboolean* residences);"
	<apicall: bool 'glAreTexturesResident' (long ulong* ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glAreTexturesResidentEXT: n with: textures with: residences
	"This method was automatically generated."
	"GLboolean glAreTexturesResidentEXT(GLsizei n, GLuint* textures, GLboolean* residences);"
	<apicall: bool 'glAreTexturesResidentEXT' (long ulong* ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glArrayElement: i
	"This method was automatically generated."
	"void glArrayElement(GLint i);"
	<apicall: void 'glArrayElement' (long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glArrayElementEXT: i
	"This method was automatically generated."
	"void glArrayElementEXT(GLint i);"
	<apicall: void 'glArrayElementEXT' (long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glAttachObjectARB: containerObj with: obj
	"This method was automatically generated"
	^glExt glAttachObjectARB: containerObj with: obj! !

!OpenGL methodsFor: 'as yet unclassified' stamp: 'pb 4/7/2010 03:47'!
glBegin: mode

	inGLBlock := true.
	forceWire ifTrue:[
		self realglBegin: GLLineStrip.]
	ifFalse:[
		self realglBegin: mode.].! !

!OpenGL methodsFor: 'GL_ARB_vertex_shader' stamp: 'daf 4/1/2007 20:24'!
glBindAttribLocationARB: programObj with: index with: name
	"This method was automatically generated"
	^glExt glBindAttribLocationARB: programObj with: index with: name! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glBindBufferARB: target with: buffer
	"This method was automatically generated"
	^glExt glBindBufferARB: target with: buffer! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glBindFramebufferEXT: target with: buffer
	"This method was automatically generated"
	^glExt glBindFramebufferEXT: target with: buffer! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glBindProgramARB: target with: program
	"This method was automatically generated"
	^glExt glBindProgramARB: target with: program! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glBindRenderbufferEXT: target with: buffer
	"This method was automatically generated"
	^glExt glBindRenderbufferEXT: target with: buffer! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glBindTexture: target with: texture
	"This method was automatically generated."
	"void glBindTexture(GLenum target, GLuint texture);"
	<apicall: void 'glBindTexture' (ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glBitmap: width with: height with: xorig with: yorig with: xmove with: ymove with: bitmap
	"This method was automatically generated."
	"void glBitmap(GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, GLubyte* bitmap);"
	<apicall: void 'glBitmap' (long long float float float float void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glBlendColor: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glBlendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);"
	<apicall: void 'glBlendColor' (float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glBlendEquation: mode
	"This method was automatically generated."
	"void glBlendEquation(GLenum mode);"
	<apicall: void 'glBlendEquation' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glBlendFunc: sfactor with: dfactor
	"This method was automatically generated."
	"void glBlendFunc(GLenum sfactor, GLenum dfactor);"
	<apicall: void 'glBlendFunc' (ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_blit' stamp: 'daf 4/1/2007 20:24'!
glBlitFramebufferEXT: srcX0 with: srcY0 with: srcX1 with: srcY1 with: dstX0 with: dstY0 with: dstX1 with: dstY1 with: mask with: filter
	"This method was automatically generated"
	^glExt glBlitFramebufferEXT: srcX0 with: srcY0 with: srcX1 with: srcY1 with: dstX0 with: dstY0 with: dstX1 with: dstY1 with: mask with: filter! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glBufferDataARB: target with: size with: data with: usage
	"This method was automatically generated"
	^glExt glBufferDataARB: target with: size with: data with: usage! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glBufferSubDataARB: target with: offset with: size with: data
	"This method was automatically generated"
	^glExt glBufferSubDataARB: target with: offset with: size with: data! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glCallList: list
	"This method was automatically generated."
	"void glCallList(GLuint list);"
	<apicall: void 'glCallList' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glCallLists: n with: type with: lists
	"This method was automatically generated."
	"void glCallLists(GLsizei n, GLenum type, GLvoid* lists);"
	<apicall: void 'glCallLists' (long ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glCheckFramebufferStatusEXT: target
	"This method was automatically generated"
	^glExt glCheckFramebufferStatusEXT: target! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glClear: mask
	"This method was automatically generated."
	"void glClear(GLbitfield mask);"
	<apicall: void 'glClear' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glClearAccum: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glClearAccum(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);"
	<apicall: void 'glClearAccum' (float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'pb 6/6/2012 00:17'!
glClearColor: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glClearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);"
	<apicall: void 'glClearColor' (float float float float) module: '#openGLLibraryName'>
	"
	<apicall: void 'glClearColor' (float float float float) module: '#openGLLibraryName'>
	"
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glClearDepth: depth
	"This method was automatically generated."
	"void glClearDepth(GLclampd depth);"
	<apicall: void 'glClearDepth' (double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glClearIndex: c
	"This method was automatically generated."
	"void glClearIndex(GLfloat c);"
	<apicall: void 'glClearIndex' (float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glClearStencil: s
	"This method was automatically generated."
	"void glClearStencil(GLint s);"
	<apicall: void 'glClearStencil' (long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_multitexture' stamp: 'dsa 8/2/2007 14:33'!
glClientActiveTextureARB: texture
	"This method was automatically generated"
	^glExt glClientActiveTextureARB: texture! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glClipPlane: plane with: equation
	"This method was automatically generated."
	"void glClipPlane(GLenum plane, GLdouble* equation);"
	<apicall: void 'glClipPlane' (ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3b: red with: green with: blue
	"This method was automatically generated."
	"void glColor3b(GLbyte red, GLbyte green, GLbyte blue);"
	<apicall: void 'glColor3b' (byte byte byte) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3bv: v
	"This method was automatically generated."
	"void glColor3bv(GLbyte* v);"
	<apicall: void 'glColor3bv' (byte*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3d: red with: green with: blue
	"This method was automatically generated."
	"void glColor3d(GLdouble red, GLdouble green, GLdouble blue);"
	<apicall: void 'glColor3d' (double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3dv: v
	"This method was automatically generated."
	"void glColor3dv(GLdouble* v);"
	<apicall: void 'glColor3dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3f: red with: green with: blue
	"This method was automatically generated."
	"void glColor3f(GLfloat red, GLfloat green, GLfloat blue);"
	<apicall: void 'glColor3f' (float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3fv: v
	"This method was automatically generated."
	"void glColor3fv(GLfloat* v);"
	<apicall: void 'glColor3fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3i: red with: green with: blue
	"This method was automatically generated."
	"void glColor3i(GLint red, GLint green, GLint blue);"
	<apicall: void 'glColor3i' (long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3iv: v
	"This method was automatically generated."
	"void glColor3iv(GLint* v);"
	<apicall: void 'glColor3iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3s: red with: green with: blue
	"This method was automatically generated."
	"void glColor3s(GLshort red, GLshort green, GLshort blue);"
	<apicall: void 'glColor3s' (short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3sv: v
	"This method was automatically generated."
	"void glColor3sv(GLshort* v);"
	<apicall: void 'glColor3sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3ub: red with: green with: blue
	"This method was automatically generated."
	"void glColor3ub(GLubyte red, GLubyte green, GLubyte blue);"
	<apicall: void 'glColor3ub' (byte byte byte) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3ubv: v
	"This method was automatically generated."
	"void glColor3ubv(GLubyte* v);"
	<apicall: void 'glColor3ubv' (byte*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3ui: red with: green with: blue
	"This method was automatically generated."
	"void glColor3ui(GLuint red, GLuint green, GLuint blue);"
	<apicall: void 'glColor3ui' (ulong ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3uiv: v
	"This method was automatically generated."
	"void glColor3uiv(GLuint* v);"
	<apicall: void 'glColor3uiv' (ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3us: red with: green with: blue
	"This method was automatically generated."
	"void glColor3us(GLushort red, GLushort green, GLushort blue);"
	<apicall: void 'glColor3us' (ushort ushort ushort) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor3usv: v
	"This method was automatically generated."
	"void glColor3usv(GLushort* v);"
	<apicall: void 'glColor3usv' (ushort*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4b: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColor4b(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha);"
	<apicall: void 'glColor4b' (byte byte byte byte) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4bv: v
	"This method was automatically generated."
	"void glColor4bv(GLbyte* v);"
	<apicall: void 'glColor4bv' (byte*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4d: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColor4d(GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha);"
	<apicall: void 'glColor4d' (double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4dv: v
	"This method was automatically generated."
	"void glColor4dv(GLdouble* v);"
	<apicall: void 'glColor4dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4f: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColor4f(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);"
	<apicall: void 'glColor4f' (float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4fv: v
	"This method was automatically generated."
	"void glColor4fv(GLfloat* v);"
	<apicall: void 'glColor4fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4i: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColor4i(GLint red, GLint green, GLint blue, GLint alpha);"
	<apicall: void 'glColor4i' (long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4iv: v
	"This method was automatically generated."
	"void glColor4iv(GLint* v);"
	<apicall: void 'glColor4iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4s: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColor4s(GLshort red, GLshort green, GLshort blue, GLshort alpha);"
	<apicall: void 'glColor4s' (short short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4sv: v
	"This method was automatically generated."
	"void glColor4sv(GLshort* v);"
	<apicall: void 'glColor4sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4ub: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColor4ub(GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha);"
	<apicall: void 'glColor4ub' (byte byte byte byte) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4ubv: v
	"This method was automatically generated."
	"void glColor4ubv(GLubyte* v);"
	<apicall: void 'glColor4ubv' (byte*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4ui: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColor4ui(GLuint red, GLuint green, GLuint blue, GLuint alpha);"
	<apicall: void 'glColor4ui' (ulong ulong ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4uiv: v
	"This method was automatically generated."
	"void glColor4uiv(GLuint* v);"
	<apicall: void 'glColor4uiv' (ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4us: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColor4us(GLushort red, GLushort green, GLushort blue, GLushort alpha);"
	<apicall: void 'glColor4us' (ushort ushort ushort ushort) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColor4usv: v
	"This method was automatically generated."
	"void glColor4usv(GLushort* v);"
	<apicall: void 'glColor4usv' (ushort*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColorMask: red with: green with: blue with: alpha
	"This method was automatically generated."
	"void glColorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);"
	<apicall: void 'glColorMask' (bool bool bool bool) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColorMaterial: face with: mode
	"This method was automatically generated."
	"void glColorMaterial(GLenum face, GLenum mode);"
	<apicall: void 'glColorMaterial' (ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColorPointer: size with: type with: stride with: pointer
	"This method was automatically generated."
	"void glColorPointer(GLint size, GLenum type, GLsizei stride, GLvoid* pointer);"
	<apicall: void 'glColorPointer' (long ulong long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:58'!
glColorPointerEXT: size with: type with: stride with: count with: pointer
	"This method was automatically generated."
	"void glColorPointerEXT(GLint size, GLenum type, GLsizei stride, GLsizei count, GLvoid* pointer);"
	<apicall: void 'glColorPointerEXT' (long ulong long long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glColorSubTable: target with: start with: count with: format with: type with: data
	"This method was automatically generated."
	"void glColorSubTable(GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, GLvoid* data);"
	<apicall: void 'glColorSubTable' (ulong long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glColorTable: target with: internalformat with: width with: format with: type with: table
	"This method was automatically generated."
	"void glColorTable(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, GLvoid* table);"
	<apicall: void 'glColorTable' (ulong ulong long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glColorTableParameterfv: target with: pname with: params
	"This method was automatically generated."
	"void glColorTableParameterfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glColorTableParameterfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glColorTableParameteriv: target with: pname with: params
	"This method was automatically generated."
	"void glColorTableParameteriv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glColorTableParameteriv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glCompileShaderARB: shaderObj
	"This method was automatically generated"
	^glExt glCompileShaderARB: shaderObj! !

!OpenGL methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/5/2006 21:55'!
glCompressedTexImage1DARB: target with: level with: internalformat with: width with: border with: imageSize with: data
	"This method was automatically generated"
	^glExt glCompressedTexImage1DARB: target with: level with: internalformat with: width with: border with: imageSize with: data! !

!OpenGL methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/5/2006 21:55'!
glCompressedTexImage2DARB: target with: level with: internalformat with: width with: height with: border with: imageSize with: data
	"This method was automatically generated"
	^glExt glCompressedTexImage2DARB: target with: level with: internalformat with: width with: height with: border with: imageSize with: data! !

!OpenGL methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/5/2006 21:55'!
glCompressedTexImage3DARB: target with: level with: internalformat with: width with: height with: depth with: border with: imageSize with: data
	"This method was automatically generated"
	^glExt glCompressedTexImage3DARB: target with: level with: internalformat with: width with: height with: depth with: border with: imageSize with: data! !

!OpenGL methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/5/2006 21:55'!
glCompressedTexSubImage1DARB: target with: level with: xoffset with: width with: aFormat with: imageSize with: data
	"This method was automatically generated"
	^glExt glCompressedTexSubImage1DARB: target with: level with: xoffset with: width with: aFormat with: imageSize with: data! !

!OpenGL methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/5/2006 21:55'!
glCompressedTexSubImage2DARB: target with: level with: xoffset with: yoffset with: width with: height with: aFormat with: imageSize with: data
	"This method was automatically generated"
	^glExt glCompressedTexSubImage2DARB: target with: level with: xoffset with: yoffset with: width with: height with: aFormat with: imageSize with: data! !

!OpenGL methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/5/2006 21:55'!
glCompressedTexSubImage3DARB: target with: level with: xoffset with: yoffset with: zoffset with: width with: height with: depth with: aFormat with: imageSize with: data
	"This method was automatically generated"
	^glExt glCompressedTexSubImage3DARB: target with: level with: xoffset with: yoffset with: zoffset with: width with: height with: depth with: aFormat with: imageSize with: data! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glConvolutionFilter1D: target with: internalformat with: width with: format with: type with: image
	"This method was automatically generated."
	"void glConvolutionFilter1D(GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, GLvoid* image);"
	<apicall: void 'glConvolutionFilter1D' (ulong ulong long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glConvolutionFilter2D: target with: internalformat with: width with: height with: format with: type with: image
	"This method was automatically generated."
	"void glConvolutionFilter2D(GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid* image);"
	<apicall: void 'glConvolutionFilter2D' (ulong ulong long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glConvolutionParameterf: target with: pname with: params
	"This method was automatically generated."
	"void glConvolutionParameterf(GLenum target, GLenum pname, GLfloat params);"
	<apicall: void 'glConvolutionParameterf' (ulong ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glConvolutionParameterfv: target with: pname with: params
	"This method was automatically generated."
	"void glConvolutionParameterfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glConvolutionParameterfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glConvolutionParameteri: target with: pname with: params
	"This method was automatically generated."
	"void glConvolutionParameteri(GLenum target, GLenum pname, GLint params);"
	<apicall: void 'glConvolutionParameteri' (ulong ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glConvolutionParameteriv: target with: pname with: params
	"This method was automatically generated."
	"void glConvolutionParameteriv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glConvolutionParameteriv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyColorSubTable: target with: start with: x with: y with: width
	"This method was automatically generated."
	"void glCopyColorSubTable(GLenum target, GLsizei start, GLint x, GLint y, GLsizei width);"
	<apicall: void 'glCopyColorSubTable' (ulong long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyColorTable: target with: internalformat with: x with: y with: width
	"This method was automatically generated."
	"void glCopyColorTable(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width);"
	<apicall: void 'glCopyColorTable' (ulong ulong long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyConvolutionFilter1D: target with: internalformat with: x with: y with: width
	"This method was automatically generated."
	"void glCopyConvolutionFilter1D(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width);"
	<apicall: void 'glCopyConvolutionFilter1D' (ulong ulong long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyConvolutionFilter2D: target with: internalformat with: x with: y with: width with: height
	"This method was automatically generated."
	"void glCopyConvolutionFilter2D(GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height);"
	<apicall: void 'glCopyConvolutionFilter2D' (ulong ulong long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyPixels: x with: y with: width with: height with: type
	"This method was automatically generated."
	"void glCopyPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum type);"
	<apicall: void 'glCopyPixels' (long long long long ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyTexImage1D: target with: level with: internalformat with: x with: y with: width with: border
	"This method was automatically generated."
	"void glCopyTexImage1D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border);"
	<apicall: void 'glCopyTexImage1D' (ulong long ulong long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyTexImage2D: target with: level with: internalformat with: x with: y with: width with: height with: border
	"This method was automatically generated."
	"void glCopyTexImage2D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);"
	<apicall: void 'glCopyTexImage2D' (ulong long ulong long long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyTexSubImage1D: target with: level with: xoffset with: x with: y with: width
	"This method was automatically generated."
	"void glCopyTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);"
	<apicall: void 'glCopyTexSubImage1D' (ulong long long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyTexSubImage2D: target with: level with: xoffset with: yoffset with: x with: y with: width with: height
	"This method was automatically generated."
	"void glCopyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);"
	<apicall: void 'glCopyTexSubImage2D' (ulong long long long long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCopyTexSubImage3D: target with: level with: xoffset with: yoffset with: zoffset with: x with: y with: width with: height
	"This method was automatically generated."
	"void glCopyTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height);"
	<apicall: void 'glCopyTexSubImage3D' (ulong long long long long long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glCreateProgramObjectARB
	"This method was automatically generated"
	^glExt glCreateProgramObjectARB! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glCreateShaderObjectARB: shaderType
	"This method was automatically generated"
	^glExt glCreateShaderObjectARB: shaderType! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glCullFace: mode
	"This method was automatically generated."
	"void glCullFace(GLenum mode);"
	<apicall: void 'glCullFace' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glDeleteBuffersARB: n with: buffers
	"This method was automatically generated"
	^glExt glDeleteBuffersARB: n with: buffers! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glDeleteFramebuffersEXT: n with: buffers
	"This method was automatically generated"
	^glExt glDeleteFramebuffersEXT: n with: buffers! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDeleteLists: list with: range
	"This method was automatically generated."
	"void glDeleteLists(GLuint list, GLsizei range);"
	<apicall: void 'glDeleteLists' (ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glDeleteObjectARB: obj
	"This method was automatically generated"
	^glExt glDeleteObjectARB: obj! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glDeleteProgramsARB: n with: programs
	"This method was automatically generated"
	^glExt glDeleteProgramsARB: n with: programs! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glDeleteRenderbuffersEXT: n with: buffers
	"This method was automatically generated"
	^glExt glDeleteRenderbuffersEXT: n with: buffers! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDeleteTextures: n with: textures
	"This method was automatically generated."
	"void glDeleteTextures(GLsizei n, GLuint* textures);"
	<apicall: void 'glDeleteTextures' (long ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDeleteTexturesEXT: n with: textures
	"This method was automatically generated."
	"void glDeleteTexturesEXT(GLsizei n, GLuint* textures);"
	<apicall: void 'glDeleteTexturesEXT' (long ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDepthFunc: func
	"This method was automatically generated."
	"void glDepthFunc(GLenum func);"
	<apicall: void 'glDepthFunc' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDepthMask: flag
	"This method was automatically generated."
	"void glDepthMask(GLboolean flag);"
	<apicall: void 'glDepthMask' (bool) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDepthRange: zNear with: zFar
	"This method was automatically generated."
	"void glDepthRange(GLclampd zNear, GLclampd zFar);"
	<apicall: void 'glDepthRange' (double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glDetachObjectARB: containerObj with: attachedObj
	"This method was automatically generated"
	^glExt glDetachObjectARB: containerObj with: attachedObj! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDisable: cap
	"This method was automatically generated."
	"void glDisable(GLenum cap);"
	<apicall: void 'glDisable' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDisableClientState: array
	"This method was automatically generated."
	"void glDisableClientState(GLenum array);"
	<apicall: void 'glDisableClientState' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glDisableVertexAttribArrayARB: index
	"This method was automatically generated"
	^glExt glDisableVertexAttribArrayARB: index! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDrawArrays: mode with: first with: count
	"This method was automatically generated."
	"void glDrawArrays(GLenum mode, GLint first, GLsizei count);"
	<apicall: void 'glDrawArrays' (ulong long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDrawArraysEXT: mode with: first with: count
	"This method was automatically generated."
	"void glDrawArraysEXT(GLenum mode, GLint first, GLsizei count);"
	<apicall: void 'glDrawArraysEXT' (ulong long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDrawBuffer: mode
	"This method was automatically generated."
	"void glDrawBuffer(GLenum mode);"
	<apicall: void 'glDrawBuffer' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDrawElements: mode with: count with: type with: indices
	"This method was automatically generated."
	"void glDrawElements(GLenum mode, GLsizei count, GLenum type, GLvoid* indices);"
	<apicall: void 'glDrawElements' (ulong long ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDrawPixels: width with: height with: format with: type with: pixels
	"This method was automatically generated."
	"void glDrawPixels(GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glDrawPixels' (long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glDrawRangeElements: mode with: start with: end with: count with: type with: indices
	"This method was automatically generated."
	"void glDrawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, GLvoid* indices);"
	<apicall: void 'glDrawRangeElements' (ulong ulong ulong long ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEdgeFlag: flag
	"This method was automatically generated."
	"void glEdgeFlag(GLboolean flag);"
	<apicall: void 'glEdgeFlag' (bool) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEdgeFlagPointer: stride with: pointer
	"This method was automatically generated."
	"void glEdgeFlagPointer(GLsizei stride, GLboolean* pointer);"
	<apicall: void 'glEdgeFlagPointer' (long ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEdgeFlagPointerEXT: stride with: count with: pointer
	"This method was automatically generated."
	"void glEdgeFlagPointerEXT(GLsizei stride, GLsizei count, GLboolean* pointer);"
	<apicall: void 'glEdgeFlagPointerEXT' (long long ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEdgeFlagv: flag
	"This method was automatically generated."
	"void glEdgeFlagv(GLboolean* flag);"
	<apicall: void 'glEdgeFlagv' (ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEnable: cap
	"This method was automatically generated."
	"void glEnable(GLenum cap);"
	<apicall: void 'glEnable' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEnableClientState: array
	"This method was automatically generated."
	"void glEnableClientState(GLenum array);"
	<apicall: void 'glEnableClientState' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glEnableVertexAttribArrayARB: index
	"This method was automatically generated"
	^glExt glEnableVertexAttribArrayARB: index! !

!OpenGL methodsFor: 'Keyword API' stamp: 'das 1/19/2005 23:55'!
glEnd
	inGLBlock := false.
	self realglEnd.
! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEndList
	"This method was automatically generated."
	"void glEndList();"
	<apicall: void 'glEndList' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalCoord1d: u
	"This method was automatically generated."
	"void glEvalCoord1d(GLdouble u);"
	<apicall: void 'glEvalCoord1d' (double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalCoord1dv: u
	"This method was automatically generated."
	"void glEvalCoord1dv(GLdouble* u);"
	<apicall: void 'glEvalCoord1dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalCoord1f: u
	"This method was automatically generated."
	"void glEvalCoord1f(GLfloat u);"
	<apicall: void 'glEvalCoord1f' (float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalCoord1fv: u
	"This method was automatically generated."
	"void glEvalCoord1fv(GLfloat* u);"
	<apicall: void 'glEvalCoord1fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalCoord2d: u with: v
	"This method was automatically generated."
	"void glEvalCoord2d(GLdouble u, GLdouble v);"
	<apicall: void 'glEvalCoord2d' (double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalCoord2dv: u
	"This method was automatically generated."
	"void glEvalCoord2dv(GLdouble* u);"
	<apicall: void 'glEvalCoord2dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalCoord2f: u with: v
	"This method was automatically generated."
	"void glEvalCoord2f(GLfloat u, GLfloat v);"
	<apicall: void 'glEvalCoord2f' (float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalCoord2fv: u
	"This method was automatically generated."
	"void glEvalCoord2fv(GLfloat* u);"
	<apicall: void 'glEvalCoord2fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalMesh1: mode with: i1 with: i2
	"This method was automatically generated."
	"void glEvalMesh1(GLenum mode, GLint i1, GLint i2);"
	<apicall: void 'glEvalMesh1' (ulong long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalMesh2: mode with: i1 with: i2 with: j1 with: j2
	"This method was automatically generated."
	"void glEvalMesh2(GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2);"
	<apicall: void 'glEvalMesh2' (ulong long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalPoint1: i
	"This method was automatically generated."
	"void glEvalPoint1(GLint i);"
	<apicall: void 'glEvalPoint1' (long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glEvalPoint2: i with: j
	"This method was automatically generated."
	"void glEvalPoint2(GLint i, GLint j);"
	<apicall: void 'glEvalPoint2' (long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'extensions' stamp: 'ar 1/4/2003 00:50'!
glExtCallingConvention
	"Answer the calling convention for extension functions"
	^ExternalFunction callTypeCDecl! !

!OpenGL methodsFor: 'extensions' stamp: 'ar 1/4/2003 00:50'!
glExtGetProcAddress: aString
	"Answer the function address for the given extension function"
	^self subclassResponsibility! !

!OpenGL methodsFor: 'extensions' stamp: 'bf 10/28/2004 14:29'!
glExtModule
	"GL extension functions are usually looked up per glExtGetProcAddress, so this MUST be nil. Except for Apple OpenGL where there is no glExtGetProcAddress and we bind to the actual module using a regular FFI call"
	^nil! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFeedbackBuffer: size with: type with: buffer
	"This method was automatically generated."
	"void glFeedbackBuffer(GLsizei size, GLenum type, GLfloat* buffer);"
	<apicall: void 'glFeedbackBuffer' (long ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFinish
	"This method was automatically generated."
	"void glFinish();"
	<apicall: void 'glFinish' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFlush
	"This method was automatically generated."
	"void glFlush();"
	<apicall: void 'glFlush' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFogf: pname with: param
	"This method was automatically generated."
	"void glFogf(GLenum pname, GLfloat param);"
	<apicall: void 'glFogf' (ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFogfv: pname with: params
	"This method was automatically generated."
	"void glFogfv(GLenum pname, GLfloat* params);"
	<apicall: void 'glFogfv' (ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFogi: pname with: param
	"This method was automatically generated."
	"void glFogi(GLenum pname, GLint param);"
	<apicall: void 'glFogi' (ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFogiv: pname with: params
	"This method was automatically generated."
	"void glFogiv(GLenum pname, GLint* params);"
	<apicall: void 'glFogiv' (ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glFramebufferRenderbufferEXT: target with: attachment with: renderbuffertarget with: renderbuffer
	"This method was automatically generated"
	^glExt glFramebufferRenderbufferEXT: target with: attachment with: renderbuffertarget with: renderbuffer! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glFramebufferTexture1DEXT: target with: attachment with: textarget with: texture with: level
	"This method was automatically generated"
	^glExt glFramebufferTexture1DEXT: target with: attachment with: textarget with: texture with: level! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glFramebufferTexture2DEXT: target with: attachment with: textarget with: texture with: level
	"This method was automatically generated"
	^glExt glFramebufferTexture2DEXT: target with: attachment with: textarget with: texture with: level! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glFramebufferTexture3DEXT: target with: attachment with: textarget with: texture with: level with: zoffset
	"This method was automatically generated"
	^glExt glFramebufferTexture3DEXT: target with: attachment with: textarget with: texture with: level with: zoffset! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFrontFace: mode
	"This method was automatically generated."
	"void glFrontFace(GLenum mode);"
	<apicall: void 'glFrontFace' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glFrustum: left with: right with: bottom with: top with: zNear with: zFar
	"This method was automatically generated."
	"void glFrustum(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);"
	<apicall: void 'glFrustum' (double double double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glGenBuffersARB: n with: buffers
	"This method was automatically generated"
	^glExt glGenBuffersARB: n with: buffers! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGenFramebuffersEXT: n with: buffers
	"This method was automatically generated"
	^glExt glGenFramebuffersEXT: n with: buffers! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGenLists: range
	"This method was automatically generated."
	"GLuint glGenLists(GLsizei range);"
	<apicall: ulong 'glGenLists' (long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGenProgramsARB: n with: programs
	"This method was automatically generated"
	^glExt glGenProgramsARB: n with: programs! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGenRenderbuffersEXT: n with: buffers
	"This method was automatically generated"
	^glExt glGenRenderbuffersEXT: n with: buffers! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGenTextures: n with: textures
	"This method was automatically generated."
	"void glGenTextures(GLsizei n, GLuint* textures);"
	<apicall: void 'glGenTextures' (long ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGenTexturesEXT: n with: textures
	"This method was automatically generated."
	"void glGenTexturesEXT(GLsizei n, GLuint* textures);"
	<apicall: void 'glGenTexturesEXT' (long ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGenerateMipmapEXT: target
	"This method was automatically generated"
	^glExt glGenerateMipmapEXT: target! !

!OpenGL methodsFor: 'GL_ARB_vertex_shader' stamp: 'daf 4/1/2007 20:24'!
glGetActiveAttribARB: programObj with: index with: maxLength with: length with: size with: type with: name
	"This method was automatically generated"
	^glExt glGetActiveAttribARB: programObj with: index with: maxLength with: length with: size with: type with: name! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetActiveUniformARB: programObj with: index with: maxLength with: length with: size with: type with: name
	"This method was automatically generated"
	^glExt glGetActiveUniformARB: programObj with: index with: maxLength with: length with: size with: type with: name! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetAttachedObjectsARB: containerObj with: maxCount with: count with: obj
	"This method was automatically generated"
	^glExt glGetAttachedObjectsARB: containerObj with: maxCount with: count with: obj! !

!OpenGL methodsFor: 'GL_ARB_vertex_shader' stamp: 'daf 4/1/2007 20:24'!
glGetAttribLocationARB: programObj with: name
	"This method was automatically generated"
	^glExt glGetAttribLocationARB: programObj with: name! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 6/30/2002 23:39'!
glGetBoolean: param
	| value |
	value := WordArray new: 1.
	self glGetBooleanv: param with: value.
	^(value at: 1) == GLTrue! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetBooleanv: pname with: params
	"This method was automatically generated."
	"void glGetBooleanv(GLenum pname, GLboolean* params);"
	<apicall: void 'glGetBooleanv' (ulong ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glGetBufferParameterivARB: target with: pname with: params
	"This method was automatically generated"
	^glExt glGetBufferParameterivARB: target with: pname with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glGetBufferPointervARB: target with: pname with: params
	"This method was automatically generated"
	^glExt glGetBufferPointervARB: target with: pname with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glGetBufferSubDataARB: target with: offset with: size with: data
	"This method was automatically generated"
	^glExt glGetBufferSubDataARB: target with: offset with: size with: data! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetClipPlane: plane with: equation
	"This method was automatically generated."
	"void glGetClipPlane(GLenum plane, GLdouble* equation);"
	<apicall: void 'glGetClipPlane' (ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetColorTable: target with: format with: type with: table
	"This method was automatically generated."
	"void glGetColorTable(GLenum target, GLenum format, GLenum type, GLvoid* table);"
	<apicall: void 'glGetColorTable' (ulong ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetColorTableParameterfv: target with: pname with: params
	"This method was automatically generated."
	"void glGetColorTableParameterfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetColorTableParameterfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetColorTableParameteriv: target with: pname with: params
	"This method was automatically generated."
	"void glGetColorTableParameteriv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glGetColorTableParameteriv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_texture_compression' stamp: 'ar 4/5/2006 21:55'!
glGetCompressedTexImageARB: target with: lod with: img
	"This method was automatically generated"
	^glExt glGetCompressedTexImageARB: target with: lod with: img! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetConvolutionFilter: target with: format with: type with: image
	"This method was automatically generated."
	"void glGetConvolutionFilter(GLenum target, GLenum format, GLenum type, GLvoid* image);"
	<apicall: void 'glGetConvolutionFilter' (ulong ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetConvolutionParameterfv: target with: pname with: params
	"This method was automatically generated."
	"void glGetConvolutionParameterfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetConvolutionParameterfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetConvolutionParameteriv: target with: pname with: params
	"This method was automatically generated."
	"void glGetConvolutionParameteriv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glGetConvolutionParameteriv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetDoublev: pname with: params
	"This method was automatically generated."
	"void glGetDoublev(GLenum pname, GLdouble* params);"
	<apicall: void 'glGetDoublev' (ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetError
	"This method was automatically generated."
	"GLenum glGetError();"
	<apicall: ulong 'glGetError' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 6/30/2002 23:40'!
glGetFloat: param
	| value |
	value := FloatArray new: 1.
	self glGetFloatv: param with: value.
	^value at: 1! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetFloatv: pname with: params
	"This method was automatically generated."
	"void glGetFloatv(GLenum pname, GLfloat* params);"
	<apicall: void 'glGetFloatv' (ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGetFramebufferAttachmentParameterivEXT: target with: attachment with: pname with: params
	"This method was automatically generated"
	^glExt glGetFramebufferAttachmentParameterivEXT: target with: attachment with: pname with: params! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetHandleARB: pname
	"This method was automatically generated"
	^glExt glGetHandleARB: pname! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetHistogram: target with: reset with: format with: type with: values
	"This method was automatically generated."
	"void glGetHistogram(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid* values);"
	<apicall: void 'glGetHistogram' (ulong bool ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetHistogramParameterfv: target with: pname with: params
	"This method was automatically generated."
	"void glGetHistogramParameterfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetHistogramParameterfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetHistogramParameteriv: target with: pname with: params
	"This method was automatically generated."
	"void glGetHistogramParameteriv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glGetHistogramParameteriv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetInfoLogARB: obj with: maxLength with: length with: infoLog
	"This method was automatically generated"
	^glExt glGetInfoLogARB: obj with: maxLength with: length with: infoLog! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 6/30/2002 23:40'!
glGetInteger: param
	| value |
	value := IntegerArray new: 1.
	self glGetIntegerv: param with: value.
	^value at: 1! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetIntegerv: pname with: params
	"This method was automatically generated."
	"void glGetIntegerv(GLenum pname, GLint* params);"
	<apicall: void 'glGetIntegerv' (ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetLightfv: light with: pname with: params
	"This method was automatically generated."
	"void glGetLightfv(GLenum light, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetLightfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetLightiv: light with: pname with: params
	"This method was automatically generated."
	"void glGetLightiv(GLenum light, GLenum pname, GLint* params);"
	<apicall: void 'glGetLightiv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetMapdv: target with: query with: v
	"This method was automatically generated."
	"void glGetMapdv(GLenum target, GLenum query, GLdouble* v);"
	<apicall: void 'glGetMapdv' (ulong ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetMapfv: target with: query with: v
	"This method was automatically generated."
	"void glGetMapfv(GLenum target, GLenum query, GLfloat* v);"
	<apicall: void 'glGetMapfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetMapiv: target with: query with: v
	"This method was automatically generated."
	"void glGetMapiv(GLenum target, GLenum query, GLint* v);"
	<apicall: void 'glGetMapiv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetMaterialfv: face with: pname with: params
	"This method was automatically generated."
	"void glGetMaterialfv(GLenum face, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetMaterialfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetMaterialiv: face with: pname with: params
	"This method was automatically generated."
	"void glGetMaterialiv(GLenum face, GLenum pname, GLint* params);"
	<apicall: void 'glGetMaterialiv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetMinmax: target with: reset with: format with: type with: values
	"This method was automatically generated."
	"void glGetMinmax(GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid* values);"
	<apicall: void 'glGetMinmax' (ulong bool ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetMinmaxParameterfv: target with: pname with: params
	"This method was automatically generated."
	"void glGetMinmaxParameterfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetMinmaxParameterfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetMinmaxParameteriv: target with: pname with: params
	"This method was automatically generated."
	"void glGetMinmaxParameteriv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glGetMinmaxParameteriv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetObjectParameterfvARB: obj with: pname with: params
	"This method was automatically generated"
	^glExt glGetObjectParameterfvARB: obj with: pname with: params! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetObjectParameterivARB: obj with: pname with: params
	"This method was automatically generated"
	^glExt glGetObjectParameterivARB: obj with: pname with: params! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetPixelMapfv: map with: values
	"This method was automatically generated."
	"void glGetPixelMapfv(GLenum map, GLfloat* values);"
	<apicall: void 'glGetPixelMapfv' (ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetPixelMapuiv: map with: values
	"This method was automatically generated."
	"void glGetPixelMapuiv(GLenum map, GLuint* values);"
	<apicall: void 'glGetPixelMapuiv' (ulong ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetPixelMapusv: map with: values
	"This method was automatically generated."
	"void glGetPixelMapusv(GLenum map, GLushort* values);"
	<apicall: void 'glGetPixelMapusv' (ulong ushort*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetPointerv: pname with: params
	"This method was automatically generated."
	"void glGetPointerv(GLenum pname, GLvoid** params);"
	<apicall: void 'glGetPointerv' (ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetPointervEXT: pname with: params
	"This method was automatically generated."
	"void glGetPointervEXT(GLenum pname, GLvoid** params);"
	<apicall: void 'glGetPointervEXT' (ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetPolygonStipple: mask
	"This method was automatically generated."
	"void glGetPolygonStipple(GLubyte* mask);"
	<apicall: void 'glGetPolygonStipple' (byte*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetProgramEnvParameterdvARB: target with: index with: params
	"This method was automatically generated"
	^glExt glGetProgramEnvParameterdvARB: target with: index with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetProgramEnvParameterfvARB: target with: index with: params
	"This method was automatically generated"
	^glExt glGetProgramEnvParameterfvARB: target with: index with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetProgramLocalParameterdvARB: target with: index with: params
	"This method was automatically generated"
	^glExt glGetProgramLocalParameterdvARB: target with: index with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetProgramLocalParameterfvARB: target with: index with: params
	"This method was automatically generated"
	^glExt glGetProgramLocalParameterfvARB: target with: index with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetProgramStringARB: target with: pname with: string
	"This method was automatically generated"
	^glExt glGetProgramStringARB: target with: pname with: string! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetProgramivARB: target with: pname with: params
	"This method was automatically generated"
	^glExt glGetProgramivARB: target with: pname with: params! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glGetRenderbufferParameterivEXT: target with: pnames with: params
	"This method was automatically generated"
	^glExt glGetRenderbufferParameterivEXT: target with: pnames with: params! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetSeparableFilter: target with: format with: type with: row with: column with: span
	"This method was automatically generated."
	"void glGetSeparableFilter(GLenum target, GLenum format, GLenum type, GLvoid* row, GLvoid* column, GLvoid* span);"
	<apicall: void 'glGetSeparableFilter' (ulong ulong ulong void* void* void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetShaderSourceARB: obj with: maxLength with: length with: source
	"This method was automatically generated"
	^glExt glGetShaderSourceARB: obj with: maxLength with: length with: source! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetString: name
	"This method was automatically generated."
	"GLubyte* glGetString(GLenum name);"
	<apicall: byte* 'glGetString' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexEnvfv: target with: pname with: params
	"This method was automatically generated."
	"void glGetTexEnvfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetTexEnvfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexEnviv: target with: pname with: params
	"This method was automatically generated."
	"void glGetTexEnviv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glGetTexEnviv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexGendv: coord with: pname with: params
	"This method was automatically generated."
	"void glGetTexGendv(GLenum coord, GLenum pname, GLdouble* params);"
	<apicall: void 'glGetTexGendv' (ulong ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexGenfv: coord with: pname with: params
	"This method was automatically generated."
	"void glGetTexGenfv(GLenum coord, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetTexGenfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexGeniv: coord with: pname with: params
	"This method was automatically generated."
	"void glGetTexGeniv(GLenum coord, GLenum pname, GLint* params);"
	<apicall: void 'glGetTexGeniv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexImage: target with: level with: format with: type with: pixels
	"This method was automatically generated."
	"void glGetTexImage(GLenum target, GLint level, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glGetTexImage' (ulong long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 4/26/2003 03:07'!
glGetTexLevelParameterf: aTarget with: level with: param
	| value |
	value := FloatArray new: 1.
	self glGetTexLevelParameterfv: aTarget with: level with: param with: value.
	^value at: 1! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexLevelParameterfv: target with: level with: pname with: params
	"This method was automatically generated."
	"void glGetTexLevelParameterfv(GLenum target, GLint level, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetTexLevelParameterfv' (ulong long ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 4/26/2003 03:07'!
glGetTexLevelParameteri: aTarget with: level with: param
	| value |
	value := IntegerArray new: 1.
	self glGetTexLevelParameteriv: aTarget with: level with: param with: value.
	^value at: 1! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexLevelParameteriv: target with: level with: pname with: params
	"This method was automatically generated."
	"void glGetTexLevelParameteriv(GLenum target, GLint level, GLenum pname, GLint* params);"
	<apicall: void 'glGetTexLevelParameteriv' (ulong long ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 4/26/2003 04:30'!
glGetTexParameterf: aTarget with: param
	| value |
	value := FloatArray new: 1.
	self glGetTexParameterfv: aTarget with: param with: value.
	^value at: 1! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexParameterfv: target with: pname with: params
	"This method was automatically generated."
	"void glGetTexParameterfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glGetTexParameterfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 4/26/2003 04:31'!
glGetTexParameteri: aTarget with: param
	| value |
	value := IntegerArray new: 1.
	self glGetTexParameteriv: aTarget with: param with: value.
	^value at: 1! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glGetTexParameteriv: target with: pname with: params
	"This method was automatically generated."
	"void glGetTexParameteriv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glGetTexParameteriv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetUniformLocationARB: programObj with: name
	"This method was automatically generated"
	^glExt glGetUniformLocationARB: programObj with: name! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetUniformfvARB: programObj with: location with: params
	"This method was automatically generated"
	^glExt glGetUniformfvARB: programObj with: location with: params! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glGetUniformivARB: programObj with: location with: params
	"This method was automatically generated"
	^glExt glGetUniformivARB: programObj with: location with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetVertexAttribPointervARB: index with: pname with: pointer
	"This method was automatically generated"
	^glExt glGetVertexAttribPointervARB: index with: pname with: pointer! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetVertexAttribdvARB: index with: pname with: params
	"This method was automatically generated"
	^glExt glGetVertexAttribdvARB: index with: pname with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetVertexAttribfvARB: index with: pname with: params
	"This method was automatically generated"
	^glExt glGetVertexAttribfvARB: index with: pname with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glGetVertexAttribivARB: index with: pname with: params
	"This method was automatically generated"
	^glExt glGetVertexAttribivARB: index with: pname with: params! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glHint: target with: mode
	"This method was automatically generated."
	"void glHint(GLenum target, GLenum mode);"
	<apicall: void 'glHint' (ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glHistogram: target with: width with: internalformat with: sink
	"This method was automatically generated."
	"void glHistogram(GLenum target, GLsizei width, GLenum internalformat, GLboolean sink);"
	<apicall: void 'glHistogram' (ulong long ulong bool) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexMask: mask
	"This method was automatically generated."
	"void glIndexMask(GLuint mask);"
	<apicall: void 'glIndexMask' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexPointer: type with: stride with: pointer
	"This method was automatically generated."
	"void glIndexPointer(GLenum type, GLsizei stride, GLvoid* pointer);"
	<apicall: void 'glIndexPointer' (ulong long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexPointerEXT: type with: stride with: count with: pointer
	"This method was automatically generated."
	"void glIndexPointerEXT(GLenum type, GLsizei stride, GLsizei count, GLvoid* pointer);"
	<apicall: void 'glIndexPointerEXT' (ulong long long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexd: c
	"This method was automatically generated."
	"void glIndexd(GLdouble c);"
	<apicall: void 'glIndexd' (double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexdv: c
	"This method was automatically generated."
	"void glIndexdv(GLdouble* c);"
	<apicall: void 'glIndexdv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexf: c
	"This method was automatically generated."
	"void glIndexf(GLfloat c);"
	<apicall: void 'glIndexf' (float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexfv: c
	"This method was automatically generated."
	"void glIndexfv(GLfloat* c);"
	<apicall: void 'glIndexfv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexi: c
	"This method was automatically generated."
	"void glIndexi(GLint c);"
	<apicall: void 'glIndexi' (long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexiv: c
	"This method was automatically generated."
	"void glIndexiv(GLint* c);"
	<apicall: void 'glIndexiv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexs: c
	"This method was automatically generated."
	"void glIndexs(GLshort c);"
	<apicall: void 'glIndexs' (short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexsv: c
	"This method was automatically generated."
	"void glIndexsv(GLshort* c);"
	<apicall: void 'glIndexsv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexub: c
	"This method was automatically generated."
	"void glIndexub(GLubyte c);"
	<apicall: void 'glIndexub' (byte) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIndexubv: c
	"This method was automatically generated."
	"void glIndexubv(GLubyte* c);"
	<apicall: void 'glIndexubv' (byte*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glInitNames
	"This method was automatically generated."
	"void glInitNames();"
	<apicall: void 'glInitNames' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glInterleavedArrays: format with: stride with: pointer
	"This method was automatically generated."
	"void glInterleavedArrays(GLenum format, GLsizei stride, GLvoid* pointer);"
	<apicall: void 'glInterleavedArrays' (ulong long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glIsBufferARB: buffer
	"This method was automatically generated"
	^glExt glIsBufferARB: buffer! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIsEnabled: cap
	"This method was automatically generated."
	"GLboolean glIsEnabled(GLenum cap);"
	<apicall: bool 'glIsEnabled' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glIsFramebufferEXT: buffer
	"This method was automatically generated"
	^glExt glIsFramebufferEXT: buffer! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIsList: list
	"This method was automatically generated."
	"GLboolean glIsList(GLuint list);"
	<apicall: bool 'glIsList' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glIsProgramARB: program
	"This method was automatically generated"
	^glExt glIsProgramARB: program! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glIsRenderbufferEXT: buffer
	"This method was automatically generated"
	^glExt glIsRenderbufferEXT: buffer! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIsTexture: texture
	"This method was automatically generated."
	"GLboolean glIsTexture(GLuint texture);"
	<apicall: bool 'glIsTexture' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glIsTextureEXT: texture
	"This method was automatically generated."
	"GLboolean glIsTextureEXT(GLuint texture);"
	<apicall: bool 'glIsTextureEXT' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLightModelf: pname with: param
	"This method was automatically generated."
	"void glLightModelf(GLenum pname, GLfloat param);"
	<apicall: void 'glLightModelf' (ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLightModelfv: pname with: params
	"This method was automatically generated."
	"void glLightModelfv(GLenum pname, GLfloat* params);"
	<apicall: void 'glLightModelfv' (ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLightModeli: pname with: param
	"This method was automatically generated."
	"void glLightModeli(GLenum pname, GLint param);"
	<apicall: void 'glLightModeli' (ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLightModeliv: pname with: params
	"This method was automatically generated."
	"void glLightModeliv(GLenum pname, GLint* params);"
	<apicall: void 'glLightModeliv' (ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLightf: light with: pname with: param
	"This method was automatically generated."
	"void glLightf(GLenum light, GLenum pname, GLfloat param);"
	<apicall: void 'glLightf' (ulong ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLightfv: light with: pname with: params
	"This method was automatically generated."
	"void glLightfv(GLenum light, GLenum pname, GLfloat* params);"
	<apicall: void 'glLightfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLighti: light with: pname with: param
	"This method was automatically generated."
	"void glLighti(GLenum light, GLenum pname, GLint param);"
	<apicall: void 'glLighti' (ulong ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLightiv: light with: pname with: params
	"This method was automatically generated."
	"void glLightiv(GLenum light, GLenum pname, GLint* params);"
	<apicall: void 'glLightiv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLineStipple: factor with: pattern
	"This method was automatically generated."
	"void glLineStipple(GLint factor, GLushort pattern);"
	<apicall: void 'glLineStipple' (long ushort) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLineWidth: width
	"This method was automatically generated."
	"void glLineWidth(GLfloat width);"
	<apicall: void 'glLineWidth' (float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glLinkProgramARB: programObj
	"This method was automatically generated"
	^glExt glLinkProgramARB: programObj! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glListBase: base
	"This method was automatically generated."
	"void glListBase(GLuint base);"
	<apicall: void 'glListBase' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLoadIdentity
	"This method was automatically generated."
	"void glLoadIdentity();"
	<apicall: void 'glLoadIdentity' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLoadMatrixd: m
	"This method was automatically generated."
	"void glLoadMatrixd(GLdouble* m);"
	<apicall: void 'glLoadMatrixd' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLoadMatrixf: m
	"This method was automatically generated."
	"void glLoadMatrixf(GLfloat* m);"
	<apicall: void 'glLoadMatrixf' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLoadName: name
	"This method was automatically generated."
	"void glLoadName(GLuint name);"
	<apicall: void 'glLoadName' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_transpose_matrix' stamp: 'ar 4/5/2006 21:55'!
glLoadTransposeMatrixf: m
	"This method was automatically generated"
	^glExt glLoadTransposeMatrixf: m! !

!OpenGL methodsFor: 'GL_EXT_compiled_vertex_array' stamp: 'ar 4/5/2006 21:55'!
glLockArraysEXT: first with: count
	"This method was automatically generated"
	^glExt glLockArraysEXT: first with: count! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glLogicOp: opcode
	"This method was automatically generated."
	"void glLogicOp(GLenum opcode);"
	<apicall: void 'glLogicOp' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMap1d: target with: u1 with: u2 with: stride with: order with: points
	"This method was automatically generated."
	"void glMap1d(GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, GLdouble* points);"
	<apicall: void 'glMap1d' (ulong double double long long double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMap1f: target with: u1 with: u2 with: stride with: order with: points
	"This method was automatically generated."
	"void glMap1f(GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, GLfloat* points);"
	<apicall: void 'glMap1f' (ulong float float long long float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMap2d: target with: u1 with: u2 with: ustride with: uorder with: v1 with: v2 with: vstride with: vorder with: points
	"This method was automatically generated."
	"void glMap2d(GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, GLdouble* points);"
	<apicall: void 'glMap2d' (ulong double double long long double double long long double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMap2f: target with: u1 with: u2 with: ustride with: uorder with: v1 with: v2 with: vstride with: vorder with: points
	"This method was automatically generated."
	"void glMap2f(GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, GLfloat* points);"
	<apicall: void 'glMap2f' (ulong float float long long float float long long float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glMapBufferARB: target with: access
	"This method was automatically generated"
	^glExt glMapBufferARB: target with: access! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMapGrid1d: un with: u1 with: u2
	"This method was automatically generated."
	"void glMapGrid1d(GLint un, GLdouble u1, GLdouble u2);"
	<apicall: void 'glMapGrid1d' (long double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMapGrid1f: un with: u1 with: u2
	"This method was automatically generated."
	"void glMapGrid1f(GLint un, GLfloat u1, GLfloat u2);"
	<apicall: void 'glMapGrid1f' (long float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMapGrid2d: un with: u1 with: u2 with: vn with: v1 with: v2
	"This method was automatically generated."
	"void glMapGrid2d(GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2);"
	<apicall: void 'glMapGrid2d' (long double double long double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMapGrid2f: un with: u1 with: u2 with: vn with: v1 with: v2
	"This method was automatically generated."
	"void glMapGrid2f(GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2);"
	<apicall: void 'glMapGrid2f' (long float float long float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMaterialf: face with: pname with: param
	"This method was automatically generated."
	"void glMaterialf(GLenum face, GLenum pname, GLfloat param);"
	<apicall: void 'glMaterialf' (ulong ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMaterialfv: face with: pname with: params
	"This method was automatically generated."
	"void glMaterialfv(GLenum face, GLenum pname, GLfloat* params);"
	<apicall: void 'glMaterialfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMateriali: face with: pname with: param
	"This method was automatically generated."
	"void glMateriali(GLenum face, GLenum pname, GLint param);"
	<apicall: void 'glMateriali' (ulong ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMaterialiv: face with: pname with: params
	"This method was automatically generated."
	"void glMaterialiv(GLenum face, GLenum pname, GLint* params);"
	<apicall: void 'glMaterialiv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMatrixMode: mode
	"This method was automatically generated."
	"void glMatrixMode(GLenum mode);"
	<apicall: void 'glMatrixMode' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMinmax: target with: internalformat with: sink
	"This method was automatically generated."
	"void glMinmax(GLenum target, GLenum internalformat, GLboolean sink);"
	<apicall: void 'glMinmax' (ulong ulong bool) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultMatrixd: m
	"This method was automatically generated."
	"void glMultMatrixd(GLdouble* m);"
	<apicall: void 'glMultMatrixd' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultMatrixf: m
	"This method was automatically generated."
	"void glMultMatrixf(GLfloat* m);"
	<apicall: void 'glMultMatrixf' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_transpose_matrix' stamp: 'ar 4/5/2006 21:55'!
glMultTransposeMatrixf: m
	"This method was automatically generated"
	^glExt glMultTransposeMatrixf: m! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord1dARB: target with: s
	"This method was automatically generated."
	"void glMultiTexCoord1dARB(GLenum target, GLdouble s);"
	<apicall: void 'glMultiTexCoord1dARB' (ulong double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord1dvARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord1dvARB(GLenum target, GLdouble* v);"
	<apicall: void 'glMultiTexCoord1dvARB' (ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord1fARB: target with: s
	"This method was automatically generated."
	"void glMultiTexCoord1fARB(GLenum target, GLfloat s);"
	<apicall: void 'glMultiTexCoord1fARB' (ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord1fvARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord1fvARB(GLenum target, GLfloat* v);"
	<apicall: void 'glMultiTexCoord1fvARB' (ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord1iARB: target with: s
	"This method was automatically generated."
	"void glMultiTexCoord1iARB(GLenum target, GLint s);"
	<apicall: void 'glMultiTexCoord1iARB' (ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord1ivARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord1ivARB(GLenum target, GLint* v);"
	<apicall: void 'glMultiTexCoord1ivARB' (ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord1sARB: target with: s
	"This method was automatically generated."
	"void glMultiTexCoord1sARB(GLenum target, GLshort s);"
	<apicall: void 'glMultiTexCoord1sARB' (ulong short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord1svARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord1svARB(GLenum target, GLshort* v);"
	<apicall: void 'glMultiTexCoord1svARB' (ulong short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord2dARB: target with: s with: t
	"This method was automatically generated."
	"void glMultiTexCoord2dARB(GLenum target, GLdouble s, GLdouble t);"
	<apicall: void 'glMultiTexCoord2dARB' (ulong double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord2dvARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord2dvARB(GLenum target, GLdouble* v);"
	<apicall: void 'glMultiTexCoord2dvARB' (ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord2fARB: target with: s with: t
	"This method was automatically generated."
	"void glMultiTexCoord2fARB(GLenum target, GLfloat s, GLfloat t);"
	<apicall: void 'glMultiTexCoord2fARB' (ulong float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord2fvARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord2fvARB(GLenum target, GLfloat* v);"
	<apicall: void 'glMultiTexCoord2fvARB' (ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord2iARB: target with: s with: t
	"This method was automatically generated."
	"void glMultiTexCoord2iARB(GLenum target, GLint s, GLint t);"
	<apicall: void 'glMultiTexCoord2iARB' (ulong long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord2ivARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord2ivARB(GLenum target, GLint* v);"
	<apicall: void 'glMultiTexCoord2ivARB' (ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord2sARB: target with: s with: t
	"This method was automatically generated."
	"void glMultiTexCoord2sARB(GLenum target, GLshort s, GLshort t);"
	<apicall: void 'glMultiTexCoord2sARB' (ulong short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord2svARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord2svARB(GLenum target, GLshort* v);"
	<apicall: void 'glMultiTexCoord2svARB' (ulong short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord3dARB: target with: s with: t with: r
	"This method was automatically generated."
	"void glMultiTexCoord3dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r);"
	<apicall: void 'glMultiTexCoord3dARB' (ulong double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord3dvARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord3dvARB(GLenum target, GLdouble* v);"
	<apicall: void 'glMultiTexCoord3dvARB' (ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord3fARB: target with: s with: t with: r
	"This method was automatically generated."
	"void glMultiTexCoord3fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r);"
	<apicall: void 'glMultiTexCoord3fARB' (ulong float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord3fvARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord3fvARB(GLenum target, GLfloat* v);"
	<apicall: void 'glMultiTexCoord3fvARB' (ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord3iARB: target with: s with: t with: r
	"This method was automatically generated."
	"void glMultiTexCoord3iARB(GLenum target, GLint s, GLint t, GLint r);"
	<apicall: void 'glMultiTexCoord3iARB' (ulong long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord3ivARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord3ivARB(GLenum target, GLint* v);"
	<apicall: void 'glMultiTexCoord3ivARB' (ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord3sARB: target with: s with: t with: r
	"This method was automatically generated."
	"void glMultiTexCoord3sARB(GLenum target, GLshort s, GLshort t, GLshort r);"
	<apicall: void 'glMultiTexCoord3sARB' (ulong short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord3svARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord3svARB(GLenum target, GLshort* v);"
	<apicall: void 'glMultiTexCoord3svARB' (ulong short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord4dARB: target with: s with: t with: r with: q
	"This method was automatically generated."
	"void glMultiTexCoord4dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q);"
	<apicall: void 'glMultiTexCoord4dARB' (ulong double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord4dvARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord4dvARB(GLenum target, GLdouble* v);"
	<apicall: void 'glMultiTexCoord4dvARB' (ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord4fARB: target with: s with: t with: r with: q
	"This method was automatically generated."
	"void glMultiTexCoord4fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q);"
	<apicall: void 'glMultiTexCoord4fARB' (ulong float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord4fvARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord4fvARB(GLenum target, GLfloat* v);"
	<apicall: void 'glMultiTexCoord4fvARB' (ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord4iARB: target with: s with: t with: r with: q
	"This method was automatically generated."
	"void glMultiTexCoord4iARB(GLenum target, GLint s, GLint t, GLint r, GLint q);"
	<apicall: void 'glMultiTexCoord4iARB' (ulong long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord4ivARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord4ivARB(GLenum target, GLint* v);"
	<apicall: void 'glMultiTexCoord4ivARB' (ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord4sARB: target with: s with: t with: r with: q
	"This method was automatically generated."
	"void glMultiTexCoord4sARB(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q);"
	<apicall: void 'glMultiTexCoord4sARB' (ulong short short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glMultiTexCoord4svARB: target with: v
	"This method was automatically generated."
	"void glMultiTexCoord4svARB(GLenum target, GLshort* v);"
	<apicall: void 'glMultiTexCoord4svARB' (ulong short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNewList: list with: mode
	"This method was automatically generated."
	"void glNewList(GLuint list, GLenum mode);"
	<apicall: void 'glNewList' (ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3b: nx with: ny with: nz
	"This method was automatically generated."
	"void glNormal3b(GLbyte nx, GLbyte ny, GLbyte nz);"
	<apicall: void 'glNormal3b' (byte byte byte) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3bv: v
	"This method was automatically generated."
	"void glNormal3bv(GLbyte* v);"
	<apicall: void 'glNormal3bv' (byte*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3d: nx with: ny with: nz
	"This method was automatically generated."
	"void glNormal3d(GLdouble nx, GLdouble ny, GLdouble nz);"
	<apicall: void 'glNormal3d' (double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3dv: v
	"This method was automatically generated."
	"void glNormal3dv(GLdouble* v);"
	<apicall: void 'glNormal3dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3f: nx with: ny with: nz
	"This method was automatically generated."
	"void glNormal3f(GLfloat nx, GLfloat ny, GLfloat nz);"
	<apicall: void 'glNormal3f' (float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3fv: v
	"This method was automatically generated."
	"void glNormal3fv(GLfloat* v);"
	<apicall: void 'glNormal3fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3i: nx with: ny with: nz
	"This method was automatically generated."
	"void glNormal3i(GLint nx, GLint ny, GLint nz);"
	<apicall: void 'glNormal3i' (long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3iv: v
	"This method was automatically generated."
	"void glNormal3iv(GLint* v);"
	<apicall: void 'glNormal3iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3s: nx with: ny with: nz
	"This method was automatically generated."
	"void glNormal3s(GLshort nx, GLshort ny, GLshort nz);"
	<apicall: void 'glNormal3s' (short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormal3sv: v
	"This method was automatically generated."
	"void glNormal3sv(GLshort* v);"
	<apicall: void 'glNormal3sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormalPointer: type with: stride with: pointer
	"This method was automatically generated."
	"void glNormalPointer(GLenum type, GLsizei stride, GLvoid* pointer);"
	<apicall: void 'glNormalPointer' (ulong long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glNormalPointerEXT: type with: stride with: count with: pointer
	"This method was automatically generated."
	"void glNormalPointerEXT(GLenum type, GLsizei stride, GLsizei count, GLvoid* pointer);"
	<apicall: void 'glNormalPointerEXT' (ulong long long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glOrtho: left with: right with: bottom with: top with: zNear with: zFar
	"This method was automatically generated."
	"void glOrtho(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);"
	<apicall: void 'glOrtho' (double double double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPassThrough: token
	"This method was automatically generated."
	"void glPassThrough(GLfloat token);"
	<apicall: void 'glPassThrough' (float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPixelMapfv: map with: mapsize with: values
	"This method was automatically generated."
	"void glPixelMapfv(GLenum map, GLint mapsize, GLfloat* values);"
	<apicall: void 'glPixelMapfv' (ulong long float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPixelMapuiv: map with: mapsize with: values
	"This method was automatically generated."
	"void glPixelMapuiv(GLenum map, GLint mapsize, GLuint* values);"
	<apicall: void 'glPixelMapuiv' (ulong long ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPixelMapusv: map with: mapsize with: values
	"This method was automatically generated."
	"void glPixelMapusv(GLenum map, GLint mapsize, GLushort* values);"
	<apicall: void 'glPixelMapusv' (ulong long ushort*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPixelStoref: pname with: param
	"This method was automatically generated."
	"void glPixelStoref(GLenum pname, GLfloat param);"
	<apicall: void 'glPixelStoref' (ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPixelStorei: pname with: param
	"This method was automatically generated."
	"void glPixelStorei(GLenum pname, GLint param);"
	<apicall: void 'glPixelStorei' (ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPixelTransferf: pname with: param
	"This method was automatically generated."
	"void glPixelTransferf(GLenum pname, GLfloat param);"
	<apicall: void 'glPixelTransferf' (ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPixelTransferi: pname with: param
	"This method was automatically generated."
	"void glPixelTransferi(GLenum pname, GLint param);"
	<apicall: void 'glPixelTransferi' (ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPixelZoom: xfactor with: yfactor
	"This method was automatically generated."
	"void glPixelZoom(GLfloat xfactor, GLfloat yfactor);"
	<apicall: void 'glPixelZoom' (float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPointSize: size
	"This method was automatically generated."
	"void glPointSize(GLfloat size);"
	<apicall: void 'glPointSize' (float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPolygonMode: face with: mode
	"This method was automatically generated."
	"void glPolygonMode(GLenum face, GLenum mode);"
	<apicall: void 'glPolygonMode' (ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPolygonOffset: factor with: units
	"This method was automatically generated."
	"void glPolygonOffset(GLfloat factor, GLfloat units);"
	<apicall: void 'glPolygonOffset' (float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPolygonStipple: mask
	"This method was automatically generated."
	"void glPolygonStipple(GLubyte* mask);"
	<apicall: void 'glPolygonStipple' (byte*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPopAttrib
	"This method was automatically generated."
	"void glPopAttrib();"
	<apicall: void 'glPopAttrib' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPopClientAttrib
	"This method was automatically generated."
	"void glPopClientAttrib();"
	<apicall: void 'glPopClientAttrib' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPopMatrix
	"This method was automatically generated."
	"void glPopMatrix();"
	<apicall: void 'glPopMatrix' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPopName
	"This method was automatically generated."
	"void glPopName();"
	<apicall: void 'glPopName' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPrioritizeTextures: n with: textures with: priorities
	"This method was automatically generated."
	"void glPrioritizeTextures(GLsizei n, GLuint* textures, GLclampf* priorities);"
	<apicall: void 'glPrioritizeTextures' (long ulong* float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramEnvParameter4dARB: target with: index with: x with: y with: z with: w
	"This method was automatically generated"
	^glExt glProgramEnvParameter4dARB: target with: index with: x with: y with: z with: w! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramEnvParameter4dvARB: target with: index with: params
	"This method was automatically generated"
	^glExt glProgramEnvParameter4dvARB: target with: index with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramEnvParameter4fARB: target with: index with: x with: y with: z with: w
	"This method was automatically generated"
	^glExt glProgramEnvParameter4fARB: target with: index with: x with: y with: z with: w! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramEnvParameter4fvARB: target with: index with: params
	"This method was automatically generated"
	^glExt glProgramEnvParameter4fvARB: target with: index with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramLocalParameter4dARB: target with: index with: x with: y with: z with: w
	"This method was automatically generated"
	^glExt glProgramLocalParameter4dARB: target with: index with: x with: y with: z with: w! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramLocalParameter4dvARB: target with: index with: params
	"This method was automatically generated"
	^glExt glProgramLocalParameter4dvARB: target with: index with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramLocalParameter4fARB: target with: index with: x with: y with: z with: w
	"This method was automatically generated"
	^glExt glProgramLocalParameter4fARB: target with: index with: x with: y with: z with: w! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramLocalParameter4fvARB: target with: index with: params
	"This method was automatically generated"
	^glExt glProgramLocalParameter4fvARB: target with: index with: params! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glProgramStringARB: target with: format with: len with: string
	"This method was automatically generated"
	^glExt glProgramStringARB: target with: format with: len with: string! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPushAttrib: mask
	"This method was automatically generated."
	"void glPushAttrib(GLbitfield mask);"
	<apicall: void 'glPushAttrib' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPushClientAttrib: mask
	"This method was automatically generated."
	"void glPushClientAttrib(GLbitfield mask);"
	<apicall: void 'glPushClientAttrib' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPushMatrix
	"This method was automatically generated."
	"void glPushMatrix();"
	<apicall: void 'glPushMatrix' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glPushName: name
	"This method was automatically generated."
	"void glPushName(GLuint name);"
	<apicall: void 'glPushName' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos2d: x with: y
	"This method was automatically generated."
	"void glRasterPos2d(GLdouble x, GLdouble y);"
	<apicall: void 'glRasterPos2d' (double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos2dv: v
	"This method was automatically generated."
	"void glRasterPos2dv(GLdouble* v);"
	<apicall: void 'glRasterPos2dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos2f: x with: y
	"This method was automatically generated."
	"void glRasterPos2f(GLfloat x, GLfloat y);"
	<apicall: void 'glRasterPos2f' (float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos2fv: v
	"This method was automatically generated."
	"void glRasterPos2fv(GLfloat* v);"
	<apicall: void 'glRasterPos2fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos2i: x with: y
	"This method was automatically generated."
	"void glRasterPos2i(GLint x, GLint y);"
	<apicall: void 'glRasterPos2i' (long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos2iv: v
	"This method was automatically generated."
	"void glRasterPos2iv(GLint* v);"
	<apicall: void 'glRasterPos2iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos2s: x with: y
	"This method was automatically generated."
	"void glRasterPos2s(GLshort x, GLshort y);"
	<apicall: void 'glRasterPos2s' (short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos2sv: v
	"This method was automatically generated."
	"void glRasterPos2sv(GLshort* v);"
	<apicall: void 'glRasterPos2sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos3d: x with: y with: z
	"This method was automatically generated."
	"void glRasterPos3d(GLdouble x, GLdouble y, GLdouble z);"
	<apicall: void 'glRasterPos3d' (double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos3dv: v
	"This method was automatically generated."
	"void glRasterPos3dv(GLdouble* v);"
	<apicall: void 'glRasterPos3dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos3f: x with: y with: z
	"This method was automatically generated."
	"void glRasterPos3f(GLfloat x, GLfloat y, GLfloat z);"
	<apicall: void 'glRasterPos3f' (float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos3fv: v
	"This method was automatically generated."
	"void glRasterPos3fv(GLfloat* v);"
	<apicall: void 'glRasterPos3fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos3i: x with: y with: z
	"This method was automatically generated."
	"void glRasterPos3i(GLint x, GLint y, GLint z);"
	<apicall: void 'glRasterPos3i' (long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos3iv: v
	"This method was automatically generated."
	"void glRasterPos3iv(GLint* v);"
	<apicall: void 'glRasterPos3iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos3s: x with: y with: z
	"This method was automatically generated."
	"void glRasterPos3s(GLshort x, GLshort y, GLshort z);"
	<apicall: void 'glRasterPos3s' (short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos3sv: v
	"This method was automatically generated."
	"void glRasterPos3sv(GLshort* v);"
	<apicall: void 'glRasterPos3sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos4d: x with: y with: z with: w
	"This method was automatically generated."
	"void glRasterPos4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w);"
	<apicall: void 'glRasterPos4d' (double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos4dv: v
	"This method was automatically generated."
	"void glRasterPos4dv(GLdouble* v);"
	<apicall: void 'glRasterPos4dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos4f: x with: y with: z with: w
	"This method was automatically generated."
	"void glRasterPos4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w);"
	<apicall: void 'glRasterPos4f' (float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos4fv: v
	"This method was automatically generated."
	"void glRasterPos4fv(GLfloat* v);"
	<apicall: void 'glRasterPos4fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos4i: x with: y with: z with: w
	"This method was automatically generated."
	"void glRasterPos4i(GLint x, GLint y, GLint z, GLint w);"
	<apicall: void 'glRasterPos4i' (long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos4iv: v
	"This method was automatically generated."
	"void glRasterPos4iv(GLint* v);"
	<apicall: void 'glRasterPos4iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos4s: x with: y with: z with: w
	"This method was automatically generated."
	"void glRasterPos4s(GLshort x, GLshort y, GLshort z, GLshort w);"
	<apicall: void 'glRasterPos4s' (short short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRasterPos4sv: v
	"This method was automatically generated."
	"void glRasterPos4sv(GLshort* v);"
	<apicall: void 'glRasterPos4sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glReadBuffer: mode
	"This method was automatically generated."
	"void glReadBuffer(GLenum mode);"
	<apicall: void 'glReadBuffer' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glReadPixels: x with: y with: width with: height with: format with: type with: pixels
	"This method was automatically generated."
	"void glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glReadPixels' (long long long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRectd: x1 with: y1 with: x2 with: y2
	"This method was automatically generated."
	"void glRectd(GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2);"
	<apicall: void 'glRectd' (double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRectdv: v1 with: v2
	"This method was automatically generated."
	"void glRectdv(GLdouble* v1, GLdouble* v2);"
	<apicall: void 'glRectdv' (double* double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRectf: x1 with: y1 with: x2 with: y2
	"This method was automatically generated."
	"void glRectf(GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2);"
	<apicall: void 'glRectf' (float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRectfv: v1 with: v2
	"This method was automatically generated."
	"void glRectfv(GLfloat* v1, GLfloat* v2);"
	<apicall: void 'glRectfv' (float* float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRecti: x1 with: y1 with: x2 with: y2
	"This method was automatically generated."
	"void glRecti(GLint x1, GLint y1, GLint x2, GLint y2);"
	<apicall: void 'glRecti' (long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRectiv: v1 with: v2
	"This method was automatically generated."
	"void glRectiv(GLint* v1, GLint* v2);"
	<apicall: void 'glRectiv' (long* long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRects: x1 with: y1 with: x2 with: y2
	"This method was automatically generated."
	"void glRects(GLshort x1, GLshort y1, GLshort x2, GLshort y2);"
	<apicall: void 'glRects' (short short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRectsv: v1 with: v2
	"This method was automatically generated."
	"void glRectsv(GLshort* v1, GLshort* v2);"
	<apicall: void 'glRectsv' (short* short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRenderMode: mode
	"This method was automatically generated."
	"GLint glRenderMode(GLenum mode);"
	<apicall: long 'glRenderMode' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_EXT_framebuffer_object' stamp: 'daf 4/1/2007 20:24'!
glRenderbufferStorageEXT: target with: internalformat with: width with: height
	"This method was automatically generated"
	^glExt glRenderbufferStorageEXT: target with: internalformat with: width with: height! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glResetHistogram: target
	"This method was automatically generated."
	"void glResetHistogram(GLenum target);"
	<apicall: void 'glResetHistogram' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glResetMinmax: target
	"This method was automatically generated."
	"void glResetMinmax(GLenum target);"
	<apicall: void 'glResetMinmax' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRotated: angle with: x with: y with: z
	"This method was automatically generated."
	"void glRotated(GLdouble angle, GLdouble x, GLdouble y, GLdouble z);"
	<apicall: void 'glRotated' (double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glRotatef: angle with: x with: y with: z
	"This method was automatically generated."
	"void glRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z);"
	<apicall: void 'glRotatef' (float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glScaled: x with: y with: z
	"This method was automatically generated."
	"void glScaled(GLdouble x, GLdouble y, GLdouble z);"
	<apicall: void 'glScaled' (double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glScalef: x with: y with: z
	"This method was automatically generated."
	"void glScalef(GLfloat x, GLfloat y, GLfloat z);"
	<apicall: void 'glScalef' (float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glScissor: x with: y with: width with: height
	"This method was automatically generated."
	"void glScissor(GLint x, GLint y, GLsizei width, GLsizei height);"
	<apicall: void 'glScissor' (long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glSelectBuffer: size with: buffer
	"This method was automatically generated."
	"void glSelectBuffer(GLsizei size, GLuint* buffer);"
	<apicall: void 'glSelectBuffer' (long ulong*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glSeparableFilter2D: target with: internalformat with: width with: height with: format with: type with: row with: column
	"This method was automatically generated."
	"void glSeparableFilter2D(GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid* row, GLvoid* column);"
	<apicall: void 'glSeparableFilter2D' (ulong ulong long long ulong ulong void* void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glShadeModel: mode
	"This method was automatically generated."
	"void glShadeModel(GLenum mode);"
	<apicall: void 'glShadeModel' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glShaderSourceARB: shaderObj with: count with: string with: length
	"This method was automatically generated"
	^glExt glShaderSourceARB: shaderObj with: count with: string with: length! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glStencilFunc: func with: ref with: mask
	"This method was automatically generated."
	"void glStencilFunc(GLenum func, GLint ref, GLuint mask);"
	<apicall: void 'glStencilFunc' (ulong long ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glStencilMask: mask
	"This method was automatically generated."
	"void glStencilMask(GLuint mask);"
	<apicall: void 'glStencilMask' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glStencilOp: fail with: zfail with: zpass
	"This method was automatically generated."
	"void glStencilOp(GLenum fail, GLenum zfail, GLenum zpass);"
	<apicall: void 'glStencilOp' (ulong ulong ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord1d: s
	"This method was automatically generated."
	"void glTexCoord1d(GLdouble s);"
	<apicall: void 'glTexCoord1d' (double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord1dv: v
	"This method was automatically generated."
	"void glTexCoord1dv(GLdouble* v);"
	<apicall: void 'glTexCoord1dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord1f: s
	"This method was automatically generated."
	"void glTexCoord1f(GLfloat s);"
	<apicall: void 'glTexCoord1f' (float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord1fv: v
	"This method was automatically generated."
	"void glTexCoord1fv(GLfloat* v);"
	<apicall: void 'glTexCoord1fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord1i: s
	"This method was automatically generated."
	"void glTexCoord1i(GLint s);"
	<apicall: void 'glTexCoord1i' (long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord1iv: v
	"This method was automatically generated."
	"void glTexCoord1iv(GLint* v);"
	<apicall: void 'glTexCoord1iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord1s: s
	"This method was automatically generated."
	"void glTexCoord1s(GLshort s);"
	<apicall: void 'glTexCoord1s' (short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord1sv: v
	"This method was automatically generated."
	"void glTexCoord1sv(GLshort* v);"
	<apicall: void 'glTexCoord1sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord2d: s with: t
	"This method was automatically generated."
	"void glTexCoord2d(GLdouble s, GLdouble t);"
	<apicall: void 'glTexCoord2d' (double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord2dv: v
	"This method was automatically generated."
	"void glTexCoord2dv(GLdouble* v);"
	<apicall: void 'glTexCoord2dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord2f: s with: t
	"This method was automatically generated."
	"void glTexCoord2f(GLfloat s, GLfloat t);"
	<apicall: void 'glTexCoord2f' (float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord2fv: v
	"This method was automatically generated."
	"void glTexCoord2fv(GLfloat* v);"
	<apicall: void 'glTexCoord2fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord2i: s with: t
	"This method was automatically generated."
	"void glTexCoord2i(GLint s, GLint t);"
	<apicall: void 'glTexCoord2i' (long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord2iv: v
	"This method was automatically generated."
	"void glTexCoord2iv(GLint* v);"
	<apicall: void 'glTexCoord2iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord2s: s with: t
	"This method was automatically generated."
	"void glTexCoord2s(GLshort s, GLshort t);"
	<apicall: void 'glTexCoord2s' (short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord2sv: v
	"This method was automatically generated."
	"void glTexCoord2sv(GLshort* v);"
	<apicall: void 'glTexCoord2sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord3d: s with: t with: r
	"This method was automatically generated."
	"void glTexCoord3d(GLdouble s, GLdouble t, GLdouble r);"
	<apicall: void 'glTexCoord3d' (double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord3dv: v
	"This method was automatically generated."
	"void glTexCoord3dv(GLdouble* v);"
	<apicall: void 'glTexCoord3dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord3f: s with: t with: r
	"This method was automatically generated."
	"void glTexCoord3f(GLfloat s, GLfloat t, GLfloat r);"
	<apicall: void 'glTexCoord3f' (float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord3fv: v
	"This method was automatically generated."
	"void glTexCoord3fv(GLfloat* v);"
	<apicall: void 'glTexCoord3fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord3i: s with: t with: r
	"This method was automatically generated."
	"void glTexCoord3i(GLint s, GLint t, GLint r);"
	<apicall: void 'glTexCoord3i' (long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord3iv: v
	"This method was automatically generated."
	"void glTexCoord3iv(GLint* v);"
	<apicall: void 'glTexCoord3iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord3s: s with: t with: r
	"This method was automatically generated."
	"void glTexCoord3s(GLshort s, GLshort t, GLshort r);"
	<apicall: void 'glTexCoord3s' (short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord3sv: v
	"This method was automatically generated."
	"void glTexCoord3sv(GLshort* v);"
	<apicall: void 'glTexCoord3sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord4d: s with: t with: r with: q
	"This method was automatically generated."
	"void glTexCoord4d(GLdouble s, GLdouble t, GLdouble r, GLdouble q);"
	<apicall: void 'glTexCoord4d' (double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord4dv: v
	"This method was automatically generated."
	"void glTexCoord4dv(GLdouble* v);"
	<apicall: void 'glTexCoord4dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord4f: s with: t with: r with: q
	"This method was automatically generated."
	"void glTexCoord4f(GLfloat s, GLfloat t, GLfloat r, GLfloat q);"
	<apicall: void 'glTexCoord4f' (float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord4fv: v
	"This method was automatically generated."
	"void glTexCoord4fv(GLfloat* v);"
	<apicall: void 'glTexCoord4fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord4i: s with: t with: r with: q
	"This method was automatically generated."
	"void glTexCoord4i(GLint s, GLint t, GLint r, GLint q);"
	<apicall: void 'glTexCoord4i' (long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord4iv: v
	"This method was automatically generated."
	"void glTexCoord4iv(GLint* v);"
	<apicall: void 'glTexCoord4iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord4s: s with: t with: r with: q
	"This method was automatically generated."
	"void glTexCoord4s(GLshort s, GLshort t, GLshort r, GLshort q);"
	<apicall: void 'glTexCoord4s' (short short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoord4sv: v
	"This method was automatically generated."
	"void glTexCoord4sv(GLshort* v);"
	<apicall: void 'glTexCoord4sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoordPointer: size with: type with: stride with: pointer
	"This method was automatically generated."
	"void glTexCoordPointer(GLint size, GLenum type, GLsizei stride, GLvoid* pointer);"
	<apicall: void 'glTexCoordPointer' (long ulong long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexCoordPointerEXT: size with: type with: stride with: count with: pointer
	"This method was automatically generated."
	"void glTexCoordPointerEXT(GLint size, GLenum type, GLsizei stride, GLsizei count, GLvoid* pointer);"
	<apicall: void 'glTexCoordPointerEXT' (long ulong long long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexEnvf: target with: pname with: param
	"This method was automatically generated."
	"void glTexEnvf(GLenum target, GLenum pname, GLfloat param);"
	<apicall: void 'glTexEnvf' (ulong ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexEnvfv: target with: pname with: params
	"This method was automatically generated."
	"void glTexEnvfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glTexEnvfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexEnvi: target with: pname with: param
	"This method was automatically generated."
	"void glTexEnvi(GLenum target, GLenum pname, GLint param);"
	<apicall: void 'glTexEnvi' (ulong ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexEnviv: target with: pname with: params
	"This method was automatically generated."
	"void glTexEnviv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glTexEnviv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexGend: coord with: pname with: param
	"This method was automatically generated."
	"void glTexGend(GLenum coord, GLenum pname, GLdouble param);"
	<apicall: void 'glTexGend' (ulong ulong double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexGendv: coord with: pname with: params
	"This method was automatically generated."
	"void glTexGendv(GLenum coord, GLenum pname, GLdouble* params);"
	<apicall: void 'glTexGendv' (ulong ulong double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexGenf: coord with: pname with: param
	"This method was automatically generated."
	"void glTexGenf(GLenum coord, GLenum pname, GLfloat param);"
	<apicall: void 'glTexGenf' (ulong ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexGenfv: coord with: pname with: params
	"This method was automatically generated."
	"void glTexGenfv(GLenum coord, GLenum pname, GLfloat* params);"
	<apicall: void 'glTexGenfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexGeni: coord with: pname with: param
	"This method was automatically generated."
	"void glTexGeni(GLenum coord, GLenum pname, GLint param);"
	<apicall: void 'glTexGeni' (ulong ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexGeniv: coord with: pname with: params
	"This method was automatically generated."
	"void glTexGeniv(GLenum coord, GLenum pname, GLint* params);"
	<apicall: void 'glTexGeniv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexImage1D: target with: level with: internalformat with: width with: border with: format with: type with: pixels
	"This method was automatically generated."
	"void glTexImage1D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glTexImage1D' (ulong long long long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexImage2D: target with: level with: internalformat with: width with: height with: border with: format with: type with: pixels
	"This method was automatically generated."
	"void glTexImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glTexImage2D' (ulong long long long long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexImage3D: target with: level with: internalformat with: width with: height with: depth with: border with: format with: type with: pixels
	"This method was automatically generated."
	"void glTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glTexImage3D' (ulong long ulong long long long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexParameterf: target with: pname with: param
	"This method was automatically generated."
	"void glTexParameterf(GLenum target, GLenum pname, GLfloat param);"
	<apicall: void 'glTexParameterf' (ulong ulong float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexParameterfv: target with: pname with: params
	"This method was automatically generated."
	"void glTexParameterfv(GLenum target, GLenum pname, GLfloat* params);"
	<apicall: void 'glTexParameterfv' (ulong ulong float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexParameteri: target with: pname with: param
	"This method was automatically generated."
	"void glTexParameteri(GLenum target, GLenum pname, GLint param);"
	<apicall: void 'glTexParameteri' (ulong ulong long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexParameteriv: target with: pname with: params
	"This method was automatically generated."
	"void glTexParameteriv(GLenum target, GLenum pname, GLint* params);"
	<apicall: void 'glTexParameteriv' (ulong ulong long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexSubImage1D: target with: level with: xoffset with: width with: format with: type with: pixels
	"This method was automatically generated."
	"void glTexSubImage1D(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glTexSubImage1D' (ulong long long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexSubImage2D: target with: level with: xoffset with: yoffset with: width with: height with: format with: type with: pixels
	"This method was automatically generated."
	"void glTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glTexSubImage2D' (ulong long long long long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTexSubImage3D: target with: level with: xoffset with: yoffset with: zoffset with: width with: height with: depth with: format with: type with: pixels
	"This method was automatically generated."
	"void glTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, GLvoid* pixels);"
	<apicall: void 'glTexSubImage3D' (ulong long long long long long long long ulong ulong void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTranslated: x with: y with: z
	"This method was automatically generated."
	"void glTranslated(GLdouble x, GLdouble y, GLdouble z);"
	<apicall: void 'glTranslated' (double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glTranslatef: x with: y with: z
	"This method was automatically generated."
	"void glTranslatef(GLfloat x, GLfloat y, GLfloat z);"
	<apicall: void 'glTranslatef' (float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform1fARB: location with: v0
	"This method was automatically generated"
	^glExt glUniform1fARB: location with: v0! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform1fvARB: location with: count with: value
	"This method was automatically generated"
	^glExt glUniform1fvARB: location with: count with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform1iARB: location with: v0
	"This method was automatically generated"
	^glExt glUniform1iARB: location with: v0! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform1ivARB: location with: count with: value
	"This method was automatically generated"
	^glExt glUniform1ivARB: location with: count with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform2fARB: location with: v0 with: v1
	"This method was automatically generated"
	^glExt glUniform2fARB: location with: v0 with: v1! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform2fvARB: location with: count with: value
	"This method was automatically generated"
	^glExt glUniform2fvARB: location with: count with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform2iARB: location with: v0 with: v1
	"This method was automatically generated"
	^glExt glUniform2iARB: location with: v0 with: v1! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform2ivARB: location with: count with: value
	"This method was automatically generated"
	^glExt glUniform2ivARB: location with: count with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform3fARB: location with: v0 with: v1 with: v2
	"This method was automatically generated"
	^glExt glUniform3fARB: location with: v0 with: v1 with: v2! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform3fvARB: location with: count with: value
	"This method was automatically generated"
	^glExt glUniform3fvARB: location with: count with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform3iARB: location with: v0 with: v1 with: v2
	"This method was automatically generated"
	^glExt glUniform3iARB: location with: v0 with: v1 with: v2! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform3ivARB: location with: count with: value
	"This method was automatically generated"
	^glExt glUniform3ivARB: location with: count with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform4fARB: location with: v0 with: v1 with: v2 with: v3
	"This method was automatically generated"
	^glExt glUniform4fARB: location with: v0 with: v1 with: v2 with: v3! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform4fvARB: location with: count with: value
	"This method was automatically generated"
	^glExt glUniform4fvARB: location with: count with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform4iARB: location with: v0 with: v1 with: v2 with: v3
	"This method was automatically generated"
	^glExt glUniform4iARB: location with: v0 with: v1 with: v2 with: v3! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniform4ivARB: location with: count with: value
	"This method was automatically generated"
	^glExt glUniform4ivARB: location with: count with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniformMatrix2fvARB: location with: count with: transpose with: value
	"This method was automatically generated"
	^glExt glUniformMatrix2fvARB: location with: count with: transpose with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniformMatrix3fvARB: location with: count with: transpose with: value
	"This method was automatically generated"
	^glExt glUniformMatrix3fvARB: location with: count with: transpose with: value! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUniformMatrix4fvARB: location with: count with: transpose with: value
	"This method was automatically generated"
	^glExt glUniformMatrix4fvARB: location with: count with: transpose with: value! !

!OpenGL methodsFor: 'GL_EXT_compiled_vertex_array' stamp: 'ar 4/5/2006 21:55'!
glUnlockArraysEXT
	"This method was automatically generated"
	^glExt glUnlockArraysEXT! !

!OpenGL methodsFor: 'GL_ARB_vertex_buffer_object' stamp: 'ar 4/5/2006 21:55'!
glUnmapBufferARB: target
	"This method was automatically generated"
	^glExt glUnmapBufferARB: target! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glUseProgramObjectARB: programObj
	"This method was automatically generated"
	^glExt glUseProgramObjectARB: programObj! !

!OpenGL methodsFor: 'GL_ARB_shader_objects' stamp: 'daf 4/1/2007 20:24'!
glValidateProgramARB: programObj
	"This method was automatically generated"
	^glExt glValidateProgramARB: programObj! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex2d: x with: y
	"This method was automatically generated."
	"void glVertex2d(GLdouble x, GLdouble y);"
	<apicall: void 'glVertex2d' (double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex2dv: v
	"This method was automatically generated."
	"void glVertex2dv(GLdouble* v);"
	<apicall: void 'glVertex2dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex2f: x with: y
	"This method was automatically generated."
	"void glVertex2f(GLfloat x, GLfloat y);"
	<apicall: void 'glVertex2f' (float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex2fv: v
	"This method was automatically generated."
	"void glVertex2fv(GLfloat* v);"
	<apicall: void 'glVertex2fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex2i: x with: y
	"This method was automatically generated."
	"void glVertex2i(GLint x, GLint y);"
	<apicall: void 'glVertex2i' (long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex2iv: v
	"This method was automatically generated."
	"void glVertex2iv(GLint* v);"
	<apicall: void 'glVertex2iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex2s: x with: y
	"This method was automatically generated."
	"void glVertex2s(GLshort x, GLshort y);"
	<apicall: void 'glVertex2s' (short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex2sv: v
	"This method was automatically generated."
	"void glVertex2sv(GLshort* v);"
	<apicall: void 'glVertex2sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex3d: x with: y with: z
	"This method was automatically generated."
	"void glVertex3d(GLdouble x, GLdouble y, GLdouble z);"
	<apicall: void 'glVertex3d' (double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex3dv: v
	"This method was automatically generated."
	"void glVertex3dv(GLdouble* v);"
	<apicall: void 'glVertex3dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex3f: x with: y with: z
	"This method was automatically generated."
	"void glVertex3f(GLfloat x, GLfloat y, GLfloat z);"
	<apicall: void 'glVertex3f' (float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex3fv: v
	"This method was automatically generated."
	"void glVertex3fv(GLfloat* v);"
	<apicall: void 'glVertex3fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex3i: x with: y with: z
	"This method was automatically generated."
	"void glVertex3i(GLint x, GLint y, GLint z);"
	<apicall: void 'glVertex3i' (long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex3iv: v
	"This method was automatically generated."
	"void glVertex3iv(GLint* v);"
	<apicall: void 'glVertex3iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex3s: x with: y with: z
	"This method was automatically generated."
	"void glVertex3s(GLshort x, GLshort y, GLshort z);"
	<apicall: void 'glVertex3s' (short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex3sv: v
	"This method was automatically generated."
	"void glVertex3sv(GLshort* v);"
	<apicall: void 'glVertex3sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex4d: x with: y with: z with: w
	"This method was automatically generated."
	"void glVertex4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w);"
	<apicall: void 'glVertex4d' (double double double double) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex4dv: v
	"This method was automatically generated."
	"void glVertex4dv(GLdouble* v);"
	<apicall: void 'glVertex4dv' (double*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex4f: x with: y with: z with: w
	"This method was automatically generated."
	"void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w);"
	<apicall: void 'glVertex4f' (float float float float) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex4fv: v
	"This method was automatically generated."
	"void glVertex4fv(GLfloat* v);"
	<apicall: void 'glVertex4fv' (float*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex4i: x with: y with: z with: w
	"This method was automatically generated."
	"void glVertex4i(GLint x, GLint y, GLint z, GLint w);"
	<apicall: void 'glVertex4i' (long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex4iv: v
	"This method was automatically generated."
	"void glVertex4iv(GLint* v);"
	<apicall: void 'glVertex4iv' (long*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex4s: x with: y with: z with: w
	"This method was automatically generated."
	"void glVertex4s(GLshort x, GLshort y, GLshort z, GLshort w);"
	<apicall: void 'glVertex4s' (short short short short) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertex4sv: v
	"This method was automatically generated."
	"void glVertex4sv(GLshort* v);"
	<apicall: void 'glVertex4sv' (short*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib1dARB: index with: x
	"This method was automatically generated"
	^glExt glVertexAttrib1dARB: index with: x! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib1dvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib1dvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib1fARB: index with: x
	"This method was automatically generated"
	^glExt glVertexAttrib1fARB: index with: x! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib1fvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib1fvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib1sARB: index with: x
	"This method was automatically generated"
	^glExt glVertexAttrib1sARB: index with: x! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib1svARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib1svARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib2dARB: index with: x with: y
	"This method was automatically generated"
	^glExt glVertexAttrib2dARB: index with: x with: y! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib2dvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib2dvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib2fARB: index with: x with: y
	"This method was automatically generated"
	^glExt glVertexAttrib2fARB: index with: x with: y! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib2fvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib2fvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib2sARB: index with: x with: y
	"This method was automatically generated"
	^glExt glVertexAttrib2sARB: index with: x with: y! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib2svARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib2svARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib3dARB: index with: x with: y with: z
	"This method was automatically generated"
	^glExt glVertexAttrib3dARB: index with: x with: y with: z! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib3dvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib3dvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib3fARB: index with: x with: y with: z
	"This method was automatically generated"
	^glExt glVertexAttrib3fARB: index with: x with: y with: z! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib3fvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib3fvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib3sARB: index with: x with: y with: z
	"This method was automatically generated"
	^glExt glVertexAttrib3sARB: index with: x with: y with: z! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib3svARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib3svARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4NbvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4NbvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4NivARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4NivARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4NsvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4NsvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4NubARB: index with: x with: y with: z with: w
	"This method was automatically generated"
	^glExt glVertexAttrib4NubARB: index with: x with: y with: z with: w! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4NubvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4NubvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4NuivARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4NuivARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4NusvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4NusvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4bvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4bvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4dARB: index with: x with: y with: z with: w
	"This method was automatically generated"
	^glExt glVertexAttrib4dARB: index with: x with: y with: z with: w! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4dvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4dvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4fARB: index with: x with: y with: z with: w
	"This method was automatically generated"
	^glExt glVertexAttrib4fARB: index with: x with: y with: z with: w! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4fvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4fvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4ivARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4ivARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4sARB: index with: x with: y with: z with: w
	"This method was automatically generated"
	^glExt glVertexAttrib4sARB: index with: x with: y with: z with: w! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4svARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4svARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4ubvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4ubvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4uivARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4uivARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttrib4usvARB: index with: v
	"This method was automatically generated"
	^glExt glVertexAttrib4usvARB: index with: v! !

!OpenGL methodsFor: 'GL_ARB_vertex_program' stamp: 'ar 4/5/2006 21:55'!
glVertexAttribPointerARB: index with: size with: type with: normalized with: stride with: pointer
	"This method was automatically generated"
	^glExt glVertexAttribPointerARB: index with: size with: type with: normalized with: stride with: pointer! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertexPointer: size with: type with: stride with: pointer
	"This method was automatically generated."
	"void glVertexPointer(GLint size, GLenum type, GLsizei stride, GLvoid* pointer);"
	<apicall: void 'glVertexPointer' (long ulong long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glVertexPointerEXT: size with: type with: stride with: count with: pointer
	"This method was automatically generated."
	"void glVertexPointerEXT(GLint size, GLenum type, GLsizei stride, GLsizei count, GLvoid* pointer);"
	<apicall: void 'glVertexPointerEXT' (long ulong long long void*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'ar 12/4/2003 20:59'!
glViewport: x with: y with: width with: height
	"This method was automatically generated."
	"void glViewport(GLint x, GLint y, GLsizei width, GLsizei height);"
	<apicall: void 'glViewport' (long long long long) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'dsa 6/12/2007 16:54'!
gluErrorString: error
	"GLubyte* gluErrorString(GLenum error);"
	| explainedError |
	
	explainedError := error.

	(error = 1280) ifTrue: [ explainedError := 'invalid enum' ].
	(error = 1281) ifTrue: [ explainedError := 'invalid value' ].
	(error = 1282) ifTrue: [ explainedError := 'invalid operation' ].
	(error = 1283) ifTrue: [ explainedError := 'stack overflow' ].
	(error = 1284) ifTrue: [ explainedError := 'stack underflow' ].
	(error = 1285) ifTrue: [ explainedError := 'out of memory' ].

	^ 'gl error: ', explainedError! !

!OpenGL methodsFor: 'project' stamp: 'das 5/23/2005 10:55'!
gluLookAtEyeX: eyex aEyeY: eyey aEyeZ: eyez aCenterX: centerx aCenterY: centery aCenterZ: centerz aUpX: upx aUpY: upy aUpZ: upz

 	| forward up side m |
	forward := Vector3 x: centerx - eyex 
						  y: centery - eyey
						  z: centerz - eyez.
	forward normalize.
	up := Vector3 x: upx y: upy z: upz.
	side := (forward cross: up) normalized.
	up := (side cross: forward) normalized.
	m := Matrix4x4 identity.
	m a11: side x.
	m a21: side y.
	m a31: side z.
	m a12: up x.
	m a22: up y.
	m a32: up z.
	m a13: forward x negated.
	m a23: forward y negated.
	m a33: forward z negated.

	self glMultMatrixf m.
	self glTranslated: eyex negated with: eyey  negated with: eyez negated.

"void GLAPIENTRY
gluLookAt(GLdouble eyex, GLdouble eyey, GLdouble eyez, GLdouble centerx,
	  GLdouble centery, GLdouble centerz, GLdouble upx, GLdouble upy,
	  GLdouble upz)
{
    
	int i;
    
	float forward[3], 
	side[3], up[3];
    
	GLfloat m[4][4];

    

	forward[0] = centerx - eyex;
    
	forward[1] = centery - eyey;
    
	forward[2] = centerz - eyez;

    
	up[0] = upx;
    
	up[1] = upy;
    
	up[2] = upz;

    
	normalize(forward);

    
/* Side = forward x up */
    
	cross(forward, up, side);
    
	normalize(side);

    
/* Recompute up as: up = side x forward */
    
	cross(side, forward, up);

    
	:=:=gluMakeIdentityf(&m[0][0]);
    
	m[0][0] = side[0];
    
	m[1][0] = side[1];
    
	m[2][0] = side[2];

    
	m[0][1] = up[0];
    
	m[1][1] = up[1];
    
	m[2][1] = up[2];

    
	m[0][2] = -forward[0];
    
	m[1][2] = -forward[1];
    
	m[2][2] = -forward[2];

    
	glMultMatrixf(&m[0][0]);
    
	glTranslated(-eyex, -eyey, -eyez);

}
"! !

!OpenGL methodsFor: 'project' stamp: 'das 5/23/2005 10:55'!
gluPerspective: aFOVY aspect: aAspect zNear: aZNear zFar: aZFar

| m sine cotangent deltaZ radians |

m := Matrix4x4 identity.

radians := (aFOVY/2.0)*(Float pi/180.0).
sine := radians sin.
deltaZ := aZFar - aZNear.
cotangent := radians cos/sine.
m a11: cotangent/aAspect.
m a22: cotangent.
m a33: ((aZFar + aZNear) negated)/deltaZ.
m a34: -1.
m a43: -2 * aZNear * aZFar / deltaZ.
m a44: 0.
self glMultMatrixf: m.



"void GLAPIENTRY
gluPerspective(GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar)

{
    
	GLdouble m[4][4];
    	double sine, cotangent, deltaZ;
    	double radians = fovy / 2 * :=:=glPi / 180;

    
	deltaZ = zFar - zNear;
    	sine = sin(radians);
    	if ((deltaZ == 0) || (sine == 0) || (aspect == 0)) 
	{
	return;
    }
    	cotangent = COS(radians) / sine;
	:=:=gluMakeIdentityd(&m[0][0]);
    	m[0][0] = cotangent / aspect;
    	m[1][1] = cotangent;
    	m[2][2] = -(zFar + zNear) / deltaZ;
    	m[2][3] = -1;
    	m[3][2] = -2 * zNear * zFar / deltaZ;
    	m[3][3] = 0;
    	glMultMatrixd(&m[0][0]);
}"! !

!OpenGL methodsFor: 'as yet unclassified' stamp: 'pb 3/27/2010 22:04'!
gluUnProjectNearX: x y: y 
	| winPoint model projection viewport finalMatrix |
	self flag: #pbfix. "not working"
	model := self modelviewMatrix.
	projection := self projectionMatrix.
	viewport := self viewport.
	winPoint := Vector4
				x: ((x
						- (viewport origin x))
						/ (viewport extent x)) * 2 - 1
				y: ((y
						- (viewport origin y))
						/ (viewport extent y)) * 2 - 1
				z: 0
				w: 1.0.
	"Transcript show: 'modelview: ' , model asString;
		 cr.
	Transcript show: 'projection: ' , projection asString;
		 cr.
	Transcript show: 'viewport: ' , viewport asString;
		 cr.
	Transcript show: 'winpoint: ' , winPoint asString;
		 cr."
	finalMatrix := (model * projection) inverseTransformation.
	^ finalMatrix productFromVector4: winPoint "winPoint x: (winPoint x - modelView"! !

!OpenGL methodsFor: 'accessing' stamp: 'das 11/13/2005 17:41'!
harness

	^ harness.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 11/13/2005 17:41'!
harness: h

	harness := h.! !

!OpenGL methodsFor: 'testing' stamp: 'jcg 5/23/2005 18:56'!
hasArbVertexBufferObject
	"Answer whether we support vertex buffer objects"
	^self hasExtension: #'GL_ARB_vertex_buffer_object'! !

!OpenGL methodsFor: 'testing' stamp: 'bf 10/28/2004 12:05'!
hasArbVertexProgram
	"Answer whether we support vertex programs"
	^self hasExtension: #'GL_ARB_vertex_program'! !

!OpenGL methodsFor: 'extensions' stamp: 'DPR 7/18/2004 21:26'!
hasExtension: extName
	^self extensions includes: extName! !

!OpenGL methodsFor: 'accessing' stamp: 'das 1/19/2005 23:56'!
inGLBlock

	^ inGLBlock.! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 8/3/2006 19:37'!
inPortal2D
	"Are we rendering through a 2D portal?"
	^ portalDepth > 0! !

!OpenGL methodsFor: 'accessing' stamp: 'das 7/24/2003 14:39'!
inPortal3D

	^ inPortal3D.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 7/24/2003 14:40'!
inPortal3D: bool

	inPortal3D := bool.! !

!OpenGL methodsFor: 'initialize' stamp: 'das 9/23/2002 14:48'!
initFrustum: angle bounds: bounds zNear: nVal zFar: fVal

	| aspect |
	aspect := (bounds width/bounds height) asFloat.
  	self glMatrixMode: GLProjection.
  	self glLoadIdentity.
	self gluPerspective:angle aspect: aspect zNear: nVal zFar: fVal.
  	self glMatrixMode: GLModelview.
  	self glLoadIdentity.
  	self glViewport:bounds left with:bounds top with: bounds width with: bounds height.
! !

!OpenGL methodsFor: 'initialize' stamp: 'dsa 7/25/2007 15:53'!
initialize
	"the following makes sure that the FFI link library name is set correctly for this platform"
	self privateInstallLibrary: self openGLLibraryName.
	textureManager := OGLTextureManager new initialize: self.
	glListRegistry := WeakIdentityKeyDictionary new.
	fontManager := OGLFontManager new initialize: self.
	shaderManager := OGLShaderManager new initialize: self.
	timeStamp := 0.
	frontFace := GLCcw.
	maxPortalDepth := 5. "arbitrary value, probably should be less than 8."
	changeTexture := true.
	test := false.
	"instance := 	instance := HPTime getRealTimeAsMilliseconds." "timestamp of this ogl instance"
	isMirror := false.
	portalDepth := 0.		"Counts 2D portal render depth."
	inPortal3D := false.
	self forceWire: false.
	inGLBlock := false.
	forcePick := false.
	forceHilite := nil.
	suppressPortals := false.
	noSwap := false.
	fogOn := false.
	transparency := 1.0.
	bufferObjects := Dictionary new.
	super initialize.! !

!OpenGL methodsFor: 'initialize' stamp: 'pb 7/5/2015 22:21'!
initializeIn: bounds
	handle := self
		primCreateRenderer: 1 + 2 + 4
		x: bounds left
		y: bounds top
		w: bounds width
		h: bounds height.
	"B3DSoftwareRenderer"
	"B3DHardwareRenderer"
	"B3DStencilBuffer"
	bufRect := bounds.
	handle ifNil: [self error: 'nil'].
	handle ifNotNil: [ glExt := OGLExtManager new ogl: self ].
	^ handle ifNotNil: [ self ].! !

!OpenGL methodsFor: 'textures' stamp: 'ar 1/30/2006 12:54'!
installTexture: aTForm
	| tf |
	changeTexture ifFalse:[^false].
	"find the real texture if necessary"
	aTForm isThumb 
		ifTrue:[tf := formManager resolve: aTForm distance: distance]
		ifFalse:[tf := aTForm].
	textureManager bindTexture: tf.
	textureManager uploadTexture: tf.
	^ true.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 4/1/2003 12:19'!
isMirror

	^ isMirror.! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 8/3/2006 19:40'!
leavePortal2D

	"note that we've finished rendering through a 2D portal."
	portalDepth := ((portalDepth - 1) max: 0).! !

!OpenGL methodsFor: 'extensions' stamp: 'ar 1/4/2003 17:35'!
loadExtension: extName
	^glExt loadExtension: extName.! !

!OpenGL methodsFor: 'initialize'!
makeCurrent
	"Hack!! We can't make it explicitly but getting a property requires to have me current, so..."
	self primRender: handle getProperty: 1.! !

!OpenGL methodsFor: 'testing'!
maxIndices

	|mxInd|

	mxInd := IntegerArray ofSize: 1.
	self glGetIntegerv: GLMaxElementsIndices with: mxInd.
	^ mxInd at: 1.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 4/1/2003 12:19'!
maxPortalDepth

	^ maxPortalDepth.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 4/1/2003 12:19'!
maxPortalDepth: mpd

	maxPortalDepth := mpd.! !

!OpenGL methodsFor: 'testing'!
maxVertices

	|mxVrt|

	mxVrt := IntegerArray ofSize: 1.
	self glGetIntegerv: GLMaxElementsVertices with: mxVrt.
	^ mxVrt at: 1.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 4/1/2003 12:20'!
mirrorFlip

	isMirror := isMirror not.! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 2/23/2006 13:33'!
modelviewMatrix
	| matrix |
	matrix := Matrix4x4 new.
	self glGetFloatv: GLModelview with: matrix.
	^matrix transposed! !

!OpenGL methodsFor: 'accessing' stamp: 'das 3/17/2006 16:47'!
noSwap

	noSwap := true.! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 10/20/2004 15:00'!
numPrims
	^numPrims! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 10/20/2004 14:57'!
numVtx
	^numVtx! !

!OpenGL methodsFor: 'initialize' stamp: 'ar 12/4/2003 20:50'!
openGLLibraryName
	^self subclassResponsibility! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 6/30/2002 21:32'!
origin
	^bufRect origin! !

!OpenGL methodsFor: 'as yet unclassified' stamp: 'pb 6/9/2012 14:31'!
pbPerspective: aFOVY aspect: aAspect zNear: aZNear zFar: aZFar
	"iirc, this was originally created to address problems I was having with gluPerspective:aspect:zNear:zFar:... need to take a fresh look at it as well as the code that depends on this to see if there's still a valid reason for it."
	| xmin xmax ymin ymax |
	self flag: #pbfix.
	ymax := aZNear * (aFOVY * Float pi / 360.0) tan.
	ymin := ymax negated.
	xmin := ymin * aAspect.
	xmax := ymax * aAspect.
	self
		glFrustum: xmin
		with: xmax
		with: ymin
		with: ymax
		with: aZNear
		with: aZFar.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 6/6/2006 21:27'!
peekMatrix

	| matrix |
	matrix := Matrix4x4 new.
	self glGetFloatv: GLModelviewMatrix with: matrix.
	^ matrix.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 5/24/2006 20:43'!
popFog

	fogOn ifTrue:[self glEnable: GLFog]..! !

!OpenGL methodsFor: 'accessing' stamp: 'das 6/6/2006 21:00'!
popMatrix

	| matrix |
	matrix :=  matrixStack removeLast.
	self glLoadMatrixf: matrix.! !

!OpenGL methodsFor: 'private' stamp: 'kayef 10/31/2008 06:10'!
primCount: indexCount forMode: mode

	mode == GLTriangles ifTrue: [^ indexCount // 3].
	mode == GLQuads ifTrue: [^ indexCount // 4].
	mode == GLPoints ifTrue: [^ indexCount].
	mode == GLTriangleStrip ifTrue: [^ indexCount - 2].
	mode == GLTriangleFan ifTrue: [^ indexCount - 2].
	mode == GLLineStrip ifTrue: [^ indexCount - 1].
	mode == GLLines ifTrue: [^indexCount // 2].
	mode == GLPolygon ifTrue:[^1].
	mode == GLLineLoop ifTrue:[^1].

	"It's easy to add additional modes if necessary."
	self error: 'Unexpected mode'.
	^ 0! !

!OpenGL methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:51'!
primCreateRenderer: flags x: x y: y w: w h: h
	<primitive: 'primitiveCreateRendererFlags' module:'B3DAcceleratorPlugin'>
	^nil! !

!OpenGL methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:51'!
primDestroyRenderer: aHandle
	<primitive: 'primitiveDestroyRenderer' module:'B3DAcceleratorPlugin'>
	^nil! !

!OpenGL methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:50'!
primRender: aHandle getProperty: propNumber
	"Primitive. Get some property."
	<primitive:'primitiveGetIntProperty' module:'B3DAcceleratorPlugin'>
	^nil! !

!OpenGL methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:50'!
primRender: aHandle setBufferRectX: left y: top w: width h: height
	<primitive:'primitiveSetBufferRect' module:'B3DAcceleratorPlugin'>
	^nil! !

!OpenGL methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:50'!
primRender: aHandle setProperty: propNumber toInteger: value
	"Primitive. Set some property."
	<primitive:'primitiveSetIntProperty' module:'B3DAcceleratorPlugin'>
	^nil! !

!OpenGL methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:50'!
primSwapBuffers: aHandle
	"Primitive. Finish all rendering operations on the receiver.
	Do not return before all rendering operations have taken effect."
	<primitive: 'primitiveSwapRendererBuffers' module:'B3DAcceleratorPlugin'>
	^nil! !

!OpenGL methodsFor: 'private' stamp: 'das 3/12/2004 15:19'!
privateInstallLibrary: glLibraryName
	"Do a bit of reflective hacking so that we don't have to carry around lots and lots of duplicate methods for the OpenGL API on each platform. The only 'real' difference is the name of the appropriate shared library here. Indeed, we could (nay... we SHOULD) have done this through a subclass of ExternalLibrary which handles this for us but since we haven't, get out the reflective hammer and nail down the problem."
	| lit |
	InstalledOpenGLLibrary = glLibraryName ifTrue:[^self].
	OpenGL methodsDo:[:meth|
		lit := meth numLiterals > 0 ifTrue:[meth literalAt: 1].
		lit class == ExternalLibraryFunction ifTrue:[lit setModule: glLibraryName].
	].
	InstalledOpenGLLibrary := glLibraryName.! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 2/23/2006 13:33'!
projectionMatrix
	| matrix |
	matrix := Matrix4x4 new.
	self glGetFloatv: GLProjectionMatrix with: matrix.
	^matrix transposed! !

!OpenGL methodsFor: 'accessing' stamp: 'das 5/24/2006 20:43'!
pushFog

	self glDisable: GLFog.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 6/6/2006 20:56'!
pushMatrix

	| matrix |
	matrix := Matrix4x4 new.
	self glGetFloatv: GLModelviewMatrix with: matrix.
	matrixStack add: matrix.
! !

!OpenGL methodsFor: 'Keyword API' stamp: 'das 8/12/2004 11:28'!
realglBegin: mode
	"This method was automatically generated."
	"void glBegin(GLenum mode);"
	<apicall: void 'glBegin' (ulong) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'Keyword API' stamp: 'das 1/19/2005 23:54'!
realglEnd
	"This method was automatically generated."
	"void glEnd();"
	<apicall: void 'glEnd' (void) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OpenGL methodsFor: 'textures' stamp: 'das 1/2/2006 23:31'!
releaseTexture: aTForm

	textureManager releaseTexture: aTForm.
! !

!OpenGL methodsFor: 'accessing' stamp: 'ar 6/21/2002 19:05'!
screenShot
	"Make a screen shot with the given extent"
	^self screenShot: (0@0 extent: bufRect extent)! !

!OpenGL methodsFor: 'accessing' stamp: 'das 9/24/2002 00:42'!
screenShot: aRectangle
	"Make a screen shot with the given extent"
	| form |
	form := Form extent: aRectangle extent depth: 32.
	self glReadPixels: aRectangle left with: bufRect height - aRectangle bottom with: aRectangle width with: aRectangle height with: self imagePixelFormat32 with: self imagePixelType32 with: form bits.
	form flipVertically.
	^form! !

!OpenGL methodsFor: 'rendering' stamp: 'das 7/28/2003 13:38'!
setCull

	self glEnable: GLCullFace.
	self glFrontFace: self frontFace.! !

!OpenGL methodsFor: 'textures' stamp: 'das 1/2/2006 23:30'!
setTextureSizeOf: aTForm to: aPoint
	^textureManager setTextureSizeOf: aTForm to: aPoint
	! !

!OpenGL methodsFor: 'accessing' stamp: 'dsa 7/25/2007 15:50'!
shaderManager
	^shaderManager! !

!OpenGL methodsFor: 'accessing' stamp: 'dsa 6/13/2007 10:44'!
shaderObjects
	^ shaderObjects! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 3/14/2006 11:36'!
suppressPortals
	"Has rendering been suppressed for the far side of portals?"
	^ suppressPortals! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 3/14/2006 11:35'!
suppressPortals: bool
	"Supress rendering the far side of portals.  
	(Useful when using render to identify pointer targets for manipulation.)"
	suppressPortals := bool.! !

!OpenGL methodsFor: 'initialize' stamp: 'das 3/17/2006 16:45'!
swapBuffers
	"Swap the receiver's buffers. Return true if successful, false otherwise"
	noSwap ifTrue:[noSwap := false. ^ self.].
	^(self primSwapBuffers: handle) notNil! !

!OpenGL methodsFor: 'accessing'!
test
	
	^ test.! !

!OpenGL methodsFor: 'private' stamp: 'das 8/26/2004 20:21'!
test2D

| s |

true ifTrue:[
	s := 10.
	self glMatrixMode: GLModelview.
	self glLoadIdentity.
	self glDisable: GLDepthTest.
	self glTranslated: 0 with: 0 with: -1.0.
	self glEnable: GLBlend.
	self glBlendFunc: GLSrcAlpha with: GLOneMinusSrcAlpha.
	self glColor4f: 0 with: 0 with: 0 with: 1.
	self
		glBegin: GLQuads;
			glVertex2f: 0.0 with: 0.0;
			glVertex2f: 0.0 with: s;
			glVertex2f: s with: s;
			glVertex2f: s with: 0.0;
		glEnd.
	self 
		glEnable: GLBlend;
		glBlendFunc: GLSrcAlpha with: GLOneMinusSrcAlpha;
		glColor4f: 0.0 with: 0.0 with: 0.0 with: 0.5;
		glLineWidth: 1;
		glBegin: GLLineLoop;
			glVertex2f: 0 with: s;
			glVertex2f: 0 with: 0;
			glVertex2f: s with: s;
			glVertex2f:s with: 0;
		glEnd;
		glColor4f: 1.0 with: 1.0 with: 1.0 with: 0.5;
		glBegin: GLQuads;
			glVertex2f: 0 + 0.1 with: s - 0.1;
			glVertex2f: 0 + 0.1 with: 0 + 0.1;
			glVertex2f: s - 0.1 with: 0 + 0.1;
			glVertex2f: s - 0.1 with: s - 0.1;
		glEnd;
		glDisable: GLBlend.
].
! !

!OpenGL methodsFor: 'accessing'!
test: bool
	
	test := bool.! !

!OpenGL methodsFor: 'textures' stamp: 'ar 4/26/2003 05:22'!
textureInternalFormatCompressed
	^GLCompressedRgbaARB! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 4/27/2006 22:13'!
textureManager
	^textureManager! !

!OpenGL methodsFor: 'accessing' stamp: 'das 6/29/2004 10:56'!
timeStamp

	^ timeStamp.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 6/29/2004 10:56'!
timeStamp: ts

	timeStamp := ts.! !

!OpenGL methodsFor: 'accessing' stamp: 'bgf 7/15/2006 16:13'!
transparency

	forceHilite ifNotNil: [
		^ forceHilite alpha * transparency.
	].

	^ transparency.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 5/29/2006 14:04'!
transparency: trans

	transparency := trans.! !

!OpenGL methodsFor: 'rendering' stamp: 'das 1/5/2006 21:41'!
updateTextures: updateList

	textureManager updateTextures:updateList.! !

!OpenGL methodsFor: 'accessing' stamp: 'das 6/13/2005 13:11'!
version
	
	^ (self glGetString: GLVersion) fromCString "eg: '1.5 ATI-1.3.42' " asNumber! !

!OpenGL methodsFor: 'accessing' stamp: 'pb 3/27/2010 19:36'!
viewport
	| vals |
	vals := WordArray new: 4.
	self glGetIntegerv: GLViewport with: vals.
	^ (vals at: 1) @ (vals at: 2) extent: (vals at: 3) @ (vals at: 4)! !

!OpenGL class methodsFor: 'compiling' stamp: 'ar 5/18/2004 16:50'!
addSelector: aSelector withMethod: aMethod
	"Flush the installed OGL library to force relinking"
	InstalledOpenGLLibrary := nil.
	^super addSelector: aSelector withMethod: aMethod! !

!OpenGL class methodsFor: 'as yet unclassified' stamp: 'pb 3/16/2010 19:26'!
errorStringWith: glErr
	^ 'OpenGL error: ' , glErr printString , ' description: ' , ((self glErrors) at: glErr ifAbsent: 'Unknown error') printString! !

!OpenGL class methodsFor: 'as yet unclassified' stamp: 'pb 3/16/2010 19:40'!
glErrors
	glErrors
		ifNil: [glErrors := Dictionary new at: GLInvalidEnum put: 'Invalid Enum';
						 at: GLInvalidValue put: 'Invalid Value';
						 at: GLInvalidOperation put: 'Invalid Operation';
						 at: GLStackOverflow put: 'Stack Overflow';
						 at: GLStackUnderflow put: 'Stack Underflow';
						 at: GLOutOfMemory put: 'Out of Memory';
						 at: GLNoError put: 'No Error';
						 yourself].
	^ glErrors! !

!OpenGL class methodsFor: 'islands' stamp: 'ar 6/16/2005 19:24'!
howToPassAsArgument
	^#passByIdentity:! !

!OpenGL class methodsFor: 'class initialization' stamp: 'ar 3/7/2006 19:26'!
initialize
	"OpenGL initialize"
	Smalltalk addToStartUpList: self.! !

!OpenGL class methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:45'!
isAvailable
	^self pluginVersion > 0! !

!OpenGL class methodsFor: 'instance creation' stamp: 'pb 6/6/2012 00:08'!
new
	Smalltalk platformName = 'Win32' ifTrue:[^OGLWin32 basicNew initialize].
	Smalltalk platformName = 'unix'
		ifTrue:
		"
			[(Smalltalk windowSystemName = 'Quartz')
				"
			[((Smalltalk getSystemAttribute: 1005) = 'Quartz')
				"implicitly big endian"
				ifTrue: [^OGLUnixQuartz basicNew initialize].
				"default to X11 window system"
				Smalltalk isLittleEndian
					ifTrue: [^OGLUnixX11LE basicNew initialize]
					ifFalse: [^OGLUnixX11BE basicNew initialize]].
	Smalltalk platformName = 'Mac OS' ifTrue:[
		Smalltalk osVersion asNumber < 1000 
			ifTrue: [^OGLMacOS9 basicNew initialize]
			ifFalse:[^OGLMacOSX basicNew initialize].
	].
	^self error:'Cannot identify platform'! !

!OpenGL class methodsFor: 'instance creation' stamp: 'ar 5/6/2005 14:43'!
newIn: bounds
	^(self new) initializeIn: bounds! !

!OpenGL class methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:44'!
pluginVersion
	"OpenGL pluginVersion"
	<primitive:'primitiveRendererVersion' module:'B3DAcceleratorPlugin'>
	^0! !

!OpenGL class methodsFor: 'primitives' stamp: 'ar 5/6/2005 14:44'!
primitiveSetVerboseLevel: newLevel
	"Primitive. Set the reporting level for the engine. Values are
		0 - print NO information ever
		1 - print critical debug errors
		2 - print debug warnings
		3 - print extra information
		4 - print extra warnings
		5 - print information about primitive execution

	   10 - print information about each vertex and face
	"
	<primitive:'primitiveSetVerboseLevel' module:'B3DAcceleratorPlugin'>! !

!OpenGL class methodsFor: 'class initialization' stamp: 'ar 3/7/2006 19:26'!
startUp
	"Flush the installed OGLLib"
	InstalledOpenGLLibrary := nil.! !

!OGLMacOSX methodsFor: 'accessing' stamp: 'bf 10/27/2004 02:59'!
glExtCallingConvention
	"Answer the calling convention for extension functions"
	^ExternalFunction callTypeAPI! !

!OGLMacOSX methodsFor: 'accessing' stamp: 'bf 10/28/2004 13:38'!
glExtGetProcAddress: aString
	"AGL does not support dynamic lookup of extension functions. We lookup in glExtModule instead"
	^nil! !

!OGLMacOSX methodsFor: 'accessing' stamp: 'bf 10/28/2004 14:30'!
glExtModule
	"GL extension functions are usually looked up per glExtGetProcAddress, so this MUST be nil. Except for Apple OpenGL where there is no glExtGetProcAddress and we bind to the actual module using a regular FFI call"
	^self openGLLibraryName
! !

!OGLMacOSX methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:13'!
imagePixelFormat32
	^GLBgra! !

!OGLMacOSX methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:13'!
imagePixelType32
	^GLUnsignedInt8888Rev! !

!OGLMacOSX methodsFor: 'initialize' stamp: 'ar 12/4/2003 21:01'!
openGLLibraryName
	^'OpenGL.framework'! !

!OGLMacOSX methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:16'!
textureInternalFormat
	^GLRgba! !

!OGLMacOSX methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:13'!
texturePixelFormat
	^GLBgra! !

!OGLMacOSX methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:14'!
texturePixelType
	^GLUnsignedInt8888Rev! !

!OGLMacOS9 methodsFor: 'initialize' stamp: 'ar 12/4/2003 21:00'!
openGLLibraryName
	^'OpenGLLibrary'! !

!OGLUnix methodsFor: 'accessing' stamp: 'pb 6/9/2012 14:05'!
glExtGetProcAddress: aString
	"Answer the function address for the given extension function.  pbfix - module: needs to be changed to 'GL' for Unix systems"
	<cdecl: ulong 'glXGetProcAddressARB' (char*) module: 'libGL.so.1'>
	^self externalCallFailed! !

!OGLUnix methodsFor: 'accessing' stamp: 'bf 10/21/2002 19:10'!
imagePixelFormat32
	^GLBgra! !

!OGLUnix methodsFor: 'accessing' stamp: 'bf 10/21/2002 20:03'!
imagePixelType32
	^GLUnsignedByte! !

!OGLUnix methodsFor: 'initialize' stamp: 'pb 6/9/2012 18:44'!
openGLLibraryName
	self flag: #pbfix.
	"
	6/2012 Linux specfic notes:
	1) Currently, the VM downloaded via the Ubuntu Software Center is a different build than what comes from the squeak.org site and will segfault so use the one from squeak.org instead.
	2) The X11 VM support that relates to OpenGL behaves differently on Linux vs. other platforms.  There appears to be a fix for: http://lists.squeakfoundation.org/pipermail/vm-dev/2012-June/010797.html that will hopefully be merged in soon.
	3) If you're on a Unix system rather than a Linux system, make the change suggested by the comment in glExtGetProcAddress:
	"
	(Smalltalk osVersion = 'linux' or: Smalltalk osVersion = 'linux-gnu')
		ifTrue: [ ^ 'libGL.so.1' ]
		ifFalse: [ ^ 'GL' ].! !

!OGLUnix methodsFor: 'accessing' stamp: 'bf 10/21/2002 19:15'!
textureInternalFormat
	^GLRgba! !

!OGLUnix methodsFor: 'accessing' stamp: 'bf 10/21/2002 19:09'!
texturePixelFormat
	^GLBgra! !

!OGLUnix methodsFor: 'accessing' stamp: 'bf 10/21/2002 19:10'!
texturePixelType
	^GLUnsignedByte! !

!OGLUnixX11BE methodsFor: 'accessing' stamp: 'ikp 2/3/2003 17:01'!
imagePixelType32
	^GLUnsignedInt8888Rev! !

!OGLUnixX11BE methodsFor: 'accessing' stamp: 'ikp 2/3/2003 17:02'!
texturePixelType
	^GLUnsignedInt8888Rev! !

!OGLUnixX11LE methodsFor: 'intitialize' stamp: 'ikp 2/3/2003 17:00'!
beginFrame
	super beginFrame.
	self glPixelStorei: GLUnpackLsbFirst with: 0.
! !

!OGLUnixX11LE methodsFor: 'accessing' stamp: 'ikp 2/3/2003 17:01'!
imagePixelType32
	^GLUnsignedByte! !

!OGLUnixX11LE methodsFor: 'accessing' stamp: 'ikp 2/3/2003 17:02'!
texturePixelType
	^GLUnsignedByte! !

!OGLUnixQuartz methodsFor: 'accessing' stamp: 'ikp 1/7/2003 21:14'!
imagePixelFormat32
	^GLBgra! !

!OGLUnixQuartz methodsFor: 'accessing' stamp: 'ikp 1/7/2003 21:14'!
imagePixelType32
	^GLUnsignedInt8888Rev! !

!OGLUnixQuartz methodsFor: 'initialize' stamp: 'ar 12/4/2003 21:01'!
openGLLibraryName
	^'OpenGL'! !

!OGLUnixQuartz methodsFor: 'accessing' stamp: 'ikp 1/7/2003 21:14'!
textureInternalFormat
	^GLRgba! !

!OGLUnixQuartz methodsFor: 'accessing' stamp: 'ikp 1/7/2003 21:14'!
texturePixelFormat
	^GLBgra! !

!OGLUnixQuartz methodsFor: 'accessing' stamp: 'ikp 1/7/2003 21:14'!
texturePixelType
	^GLUnsignedInt8888Rev! !

!OGLUnixQuartz class methodsFor: 'as yet unclassified' stamp: 'pb 6/9/2012 13:32'!
test
	"OGLUnixOSX test"

	self flag: #pbfix.
"
	<cdecl: void 'ffiTest2' (long long) module: 'B3DAcceleratorPlugin'>
"
	^self error: 'test failed'! !

!OGLUnixQuartz class methodsFor: 'as yet unclassified' stamp: 'das 3/12/2004 12:55'!
test: x with: y
	"OGLUnixOSX test: 6 with: 7"

	self flag: #pbfix.
"
	<cdecl: void 'ffiTest2' (long long) module: 'B3DAcceleratorPlugin'>
"
	^self error: 'test failed'! !

!OGLWin32 methodsFor: 'initialize' stamp: 'ar 6/26/2002 19:08'!
beginFrame
	super beginFrame.
	self glPixelStorei: GLUnpackLsbFirst with: 0.
! !

!OGLWin32 methodsFor: 'accessing' stamp: 'ar 1/4/2003 00:07'!
glExtCallingConvention
	"Answer the calling convention for extension functions"
	^ExternalFunction callTypeAPI! !

!OGLWin32 methodsFor: 'accessing' stamp: 'pb 6/9/2012 13:29'!
glExtGetProcAddress: aString
	"Answer the function address for the given extension function"
	<apicall: ulong 'wglGetProcAddress' (char*) module: '#openGLLibraryName'>
	^self externalCallFailed! !

!OGLWin32 methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:10'!
imagePixelFormat32
	^GLBgra! !

!OGLWin32 methodsFor: 'accessing' stamp: 'ar 6/26/2002 20:03'!
imagePixelType32
	^GLUnsignedByte! !

!OGLWin32 methodsFor: 'initialize' stamp: 'ar 12/4/2003 21:01'!
openGLLibraryName
	^'opengl32.dll'! !

!OGLWin32 methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:15'!
textureInternalFormat
	^GLRgba! !

!OGLWin32 methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:09'!
texturePixelFormat
	^GLBgra! !

!OGLWin32 methodsFor: 'accessing' stamp: 'ar 6/26/2002 19:10'!
texturePixelType
	^GLUnsignedByte! !
GLExtConstants initialize!
OpenGLConstants initialize!
OGLExtManager initialize!
OpenGL initialize!
