calcIsSubsetOf <- function(orderedSubset, orderedSet)
{
  identical(intersect(orderedSet, orderedSubset), orderedSet)
}