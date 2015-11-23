{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "safe-json-stringify";
    version = "1.0.3";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/safe-json-stringify/-/safe-json-stringify-1.0.3.tgz";
      sha1 = "3cb6717660a086d07cb5bd9b7a6875bcf67bd05e";
    };
    deps = [];
    meta = {
      homepage = "https://github.com/e-conomic/safe-json-stringify";
      description = "Prevent defined property getters from throwing errors";
    };
  }