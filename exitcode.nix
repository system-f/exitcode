{ mkDerivation, base, checkers, hedgehog, lens, mmorph, mtl
, QuickCheck, semigroupoids, semigroups, stdenv, tasty
, tasty-hedgehog, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "exitcode";
  version = "0.1.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base lens mmorph mtl semigroupoids semigroups transformers
  ];
  testHaskellDepends = [
    base checkers hedgehog lens QuickCheck tasty tasty-hedgehog
    tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "https://github.com/qfpl/exitcode";
  description = "Monad transformer for exit codes";
  license = stdenv.lib.licenses.bsd3;
}
