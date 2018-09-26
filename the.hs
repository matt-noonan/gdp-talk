class The decorated bare | decorated -> bare where
  the :: decorated -> bare

  -- Default instance can be used in the module
  -- where `decorated` is defined.
  default the ::
    Coercible decorated bare => decorated -> bare
  the = coerce
