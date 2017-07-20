{ mkDerivation, base, checkers, lens, QuickCheck, semigroupoids
, stdenv, tasty, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "exitcode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens semigroupoids transformers ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/exitcode";
  description = "Monad transformer for exit codes";
  license = stdenv.lib.licenses.bsd3;
}
