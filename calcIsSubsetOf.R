calcIsSubsetOf <- function(orderedSet, orderedSubset)
{
  identical(intersect(orderedSet, orderedSubset), orderedSubset)
}