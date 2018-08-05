## local attach, not added to search path

attach.local = function(object, fields=NULL, excludeFields=NULL, overwrite=TRUE, envir=parent.frame(), ...) {
  if (is.null(fields)) {
    fields <- names(object)
    # Don't try to attach non-named elements
    fields <- setdiff(fields, "")
  }

  # Note: we cannot do 'fields <- setdiff(fields, excludeFields)', because
  # that will also remove duplicates!
  attachedFields <- character(0L)
  for (field in fields) {
    if (field %in% excludeFields)
      next
    if (overwrite || !exists(field, envir=envir, inherits=FALSE)) {
      assign(field, object[[field]], envir=envir)
      # Remove field this way a 2nd field of the same name can
      # be attached (and overwrite the first one)
      object[[field]] <- NULL
      attachedFields <- c(attachedFields, field)
    }
  }

  invisible(attachedFields)

}
