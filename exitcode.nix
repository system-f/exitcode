{ mkDerivation, base, checkers, lens, QuickCheck, semigroupoids
, stdenv
}:
mkDerivation {
  pname = "exitcode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens semigroupoids ];
  testHaskellDepends = [ base checkers QuickCheck ];
  homepage = "https://github.com/qfpl/exitcode";
  description = "Monad transformer for exit codes";
  license = stdenv.lib.licenses.bsd3;
}
