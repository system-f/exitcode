{ mkDerivation, base, checkers, hedgehog, lens, mtl, semigroupoids
, semigroups, stdenv, tasty, tasty-hedgehog, tasty-hunit
, transformers
}:
mkDerivation {
  pname = "exitcode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base lens mtl semigroupoids semigroups transformers
  ];
  testHaskellDepends = [
    base checkers hedgehog lens tasty tasty-hedgehog tasty-hunit
    transformers
  ];
  homepage = "https://github.com/qfpl/exitcode";
  description = "Monad transformer for exit codes";
  license = stdenv.lib.licenses.bsd3;
}
