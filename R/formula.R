is.formula = function(x) {
  inherits(x, 'formula')
}


hasResponse = function(f) {
  if (is.formula(f)) {
    tt = terms(f)
  } else {
    tt = f
  }
  attr(tt, 'response') != 0
}


hasIntercept = function(f) {
  if (is.formula(f)) {
    tt = terms(f)
  } else {
    tt = f
  }
  attr(tt, 'intercept') != 0
}


hasSingleResponse = function(f) {
  hasResponse(f) && length(getResponse(f)) == 1
}


getResponse = function(f) {
  if (hasResponse(f)) {
    update(f, . ~ 1) %>% all.vars()
  }
  else {
    return(NULL)
  }
}


getREterms = function(f) {
  .loadOptionalPackage('lme4')
  terms = lme4::findbars(f)
}


REtermAsFormula = function(term) {
  assert_that(is.call(term))
  assert_that(!is.formula(term))
  as.character(term)[2] %>% reformulate
}


getREGroupName = function(term) {
  assert_that(is.call(term))
  as.character(term)[3]
}


getCovariates = function(f) {
  if(is.null(f)) {
    character()
  } else {
    dropResponse(f) %>% all.vars()
  }
}


hasCovariates = function(f) {
  length(getCovariates(f)) > 0
}


hasRE = function(f) {
  length(getREterms(f)) > 0
}


addInteraction = function(f, var) {
  assert_that(is.formula(f))
  assert_that(is.character(var))
  vars = terms(f) %>% labels()

  if (length(vars) == 0) {
    if (hasIntercept(f)) {
      reformulate(
        var,
        response = getResponse(f),
        intercept = TRUE,
        env = environment(f)
      )
    } else {
      f
    }
  } else {
    reformulate(
      paste(vars, var, sep = '*'),
      response = getResponse(f),
      intercept = hasIntercept(f),
      env = environment(f)
    )
  }
}


#' @exportS3Method base::merge
merge.formula = function(x, y, ...) {
  assert_that(is.formula(x))
  assert_that(is.formula(y))
  assert_that(!hasResponse(y))
  xlabels = terms(x) %>% labels()
  ylabels = terms(y) %>% labels()

  allLabels = union(xlabels, ylabels)
  if (length(allLabels) == 0) {
    if (!hasIntercept(x) && hasIntercept(y)) {
      update(x, ~ 1)
    } else {
      x
    }
  } else {
    reformulate(
      allLabels,
      response = getResponse(x),
      intercept = hasIntercept(x) || hasIntercept(y),
      env = environment(x)
    ) #TODO: merge environments of x and y
  }
}


dropResponse = function(f) {
  if (hasResponse(f)) {
    update(f, NULL ~ .)
  } else {
    f
  }
}


dropIntercept = function(f) {
  if (hasIntercept(f)) {
    update(f, ~ .+-1)
  } else {
    f
  }
}


#' @noRd
#' @importFrom stats drop.terms
#' @title Drop random-effects component from a formula
#' @description Remove the random-effects components specified by "(. | .)" from a formula
#' @keywords internal
dropRE = function(f) {
  reStrings = getREterms(f) %>% as.character
  if (length(reStrings) == 0) {
    f
  } else {
    labs = labels(terms(f))
    reIdx = match(reStrings, labs)
    assert_that(noNA(reIdx))

    if (length(reIdx) == length(labs)) {
      if (hasIntercept(f)) {
        update(f, ~ 1)
      } else {
        update(f, ~ 0)
      }
    } else {
      newf = drop.terms(terms(f), reIdx, keep.response = hasResponse(f)) %>%
        formula
      environment(newf) = environment(f)
      newf
    }
  }
}


#' @noRd
#' @title Get special terms as character vector
#' @param f A function containing special function terms, e.g. A ~ B + time(1) + time(I(B^2))
#' @param special The special function, e.g. time
#' @return A character vector of the terms encapsulated in a special function
#' @keywords internal
getSpecialTerms = function(f, special) {
  assert_that(is.scalar(special))
  tt = terms(f, specials = special)

  vars = attr(tt, 'variables')
  specialIdx = attr(tt, 'specials')[[special]]

  if (length(specialIdx) == 0) {
    return(character())
  }

  vars[specialIdx + 1] %>%
    as.list %>%
    lapply('[[', -1) %>%
    vapply(deparse, FUN.VALUE = '')
}


#' @noRd
#' @title Get special terms as formula
#' @details An intercept is added unless the formula contains a special removing it, e.g. time(0)
#' @keywords internal
getSpecialFormula = function(f, special) {
  specialTerms = getSpecialTerms(f, special)

  if (length(specialTerms) == 0) {
    update(f, ~ 1)
  } else {
    reformulate(
      specialTerms,
      response = getResponse(f),
      intercept = TRUE,
      env = environment(f)
    )
  }
}


dropSpecial = function(f, special) {
  assert_that(is.scalar(special))
  tt = terms(f, specials = special)

  vars = attr(tt, 'variables')
  specialIdx = attr(tt, 'specials')[[special]]

  if (length(specialIdx) == 0) {
    return(f)
  }

  newTerms = labels(tt)[-(specialIdx - 1)]
  if (length(newTerms) == 0) {
    if (hasIntercept(f)) {
      update(f, ~ 1)
    } else {
      update(f, ~ 0)
    }
  } else {
    reformulate(
      newTerms,
      response = getResponse(f),
      intercept = hasIntercept(f),
      env = environment(f)
    )
  }
}
