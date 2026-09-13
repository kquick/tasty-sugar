{ mkDerivation, base, fetchgit, lib }:
mkDerivation {
  pname = "microlens";
  version = "0.5.0.0";
  src = fetchgit {
    url = "https://github.com/monadfix/microlens";
    sha256 = "12hmnb63nzj49mfc3xcbyk6xgpyxayayhnkd75szqz87jxfr12lv";
    rev = "7d509d465c54dba45b182f9c60452b3b3aa71d1a";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/microlens; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/stevenfontanella/microlens";
  description = "A tiny lens library with no dependencies";
  license = lib.licenses.bsd3;
}
