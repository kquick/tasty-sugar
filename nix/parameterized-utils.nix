{ mkDerivation, base, base-orphans, constraints, containers
, deepseq, fetchgit, ghc-bignum, hashable, hashtables, hedgehog
, hedgehog-classes, indexed-traversable, lib, microlens, mtl, tasty
, tasty-ant-xml, tasty-hedgehog, tasty-hunit, template-haskell
, text, th-abstraction, vector
}:
mkDerivation {
  pname = "parameterized-utils";
  version = "2.3.1.0.99";
  src = fetchgit {
    url = "https://github.com/GaloisInc/parameterized-utils";
    sha256 = "0jrfac6bkh0pppln6chpy16dbfw34djcq58kn0yry0vwf3iyixas";
    rev = "0423981e27499d956ec084e8420b55ac49200c47";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base-orphans constraints containers deepseq ghc-bignum
    hashable hashtables indexed-traversable microlens mtl
    template-haskell text th-abstraction vector
  ];
  testHaskellDepends = [
    base hashable hashtables hedgehog hedgehog-classes
    indexed-traversable microlens mtl tasty tasty-ant-xml
    tasty-hedgehog tasty-hunit
  ];
  homepage = "https://github.com/GaloisInc/parameterized-utils";
  description = "Classes and data structures for working with data-kind indexed types";
  license = lib.licenses.bsd3;
}
