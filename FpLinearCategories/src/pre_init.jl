# To avoid name clashes, we exclude some names from the global namespace of the extension module.
empty!(ExcludedNames)

append!(ExcludedNames, [
  :HasPseudoInverse,
  :SetPseudoInverse,
  :PseudoInverse,
  :PseudoInverse_OPERATION,
  :TheJuliaAttributeTypePseudoInverse,
  :HasSize,
  :SetSize,
  :Size,
  :Size_OPERATION,
  :TheJuliaAttributeTypeSize,
  :HasUnderlyingCategory,
  :SetUnderlyingCategory,
  :UnderlyingCategory,
  :UnderlyingCategory_OPERATION,
  :TheJuliaAttributeTypeUnderlyingCategory,
])
