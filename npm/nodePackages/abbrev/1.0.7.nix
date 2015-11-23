{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "abbrev";
    version = "1.0.7";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/abbrev/-/abbrev-1.0.7.tgz";
      sha1 = "5b6035b2ee9d4fb5cf859f08a9be81b208491843";
    };
    deps = [];
    meta = {
      homepage = "https://github.com/isaacs/abbrev-js#readme";
      description = "Like ruby's abbrev module, but in js";
    };
  }