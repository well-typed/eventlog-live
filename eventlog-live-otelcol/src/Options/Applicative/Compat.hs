{-# LANGUAGE CPP #-}

module Options.Applicative.Compat (
  parserOptionGroup,
  simpleVersioner,
) where

#if MIN_VERSION_optparse_applicative(0,19,0)
import Options.Applicative (parserOptionGroup)
import Options.Applicative (simpleVersioner)
#else
#if MIN_VERSION_optparse_applicative(0,18,1)
import Options.Applicative (simpleVersioner)
#else
import Options.Applicative (infoOption, long, help, hidden)
#endif
import Options.Applicative (Parser)
#endif

#if MIN_VERSION_optparse_applicative(0,19,0)
#else
-- Prior to optparse-applicative-0.19.0.0, option groups were not supported,
-- so this definition simply drops the group.
parserOptionGroup :: String -> Parser a -> Parser a
parserOptionGroup _ p = p
#if MIN_VERSION_optparse_applicative(0,18,1)
#else
-- Prior to optparse-applicative-0.18.1.0, simpleVersioner was not defined,
-- so this definition is taken verbatim from optparse-applicative-0.18.1.0.
simpleVersioner :: String -> Parser (a -> a)
simpleVersioner version = infoOption version $
  mconcat [long "version", help "Show version information", hidden]
#endif
#endif
