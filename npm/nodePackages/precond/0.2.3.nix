{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "precond";
    version = "0.2.3";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/precond/-/precond-0.2.3.tgz";
      sha1 = "aa9591bcaa24923f1e0f4849d240f47efc1075ac";
    };
    deps = [];
    meta = {
      description = "Precondition checking utilities.";
      keywords = [
        "precondition"
        "assert"
        "invariant"
        "contract"
        "condition"
      ];
    };
  }