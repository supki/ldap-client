{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "wrappy";
    version = "1.0.1";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/wrappy/-/wrappy-1.0.1.tgz";
      sha1 = "1e65969965ccbc2db4548c6b84a6f2c5aedd4739";
    };
    deps = [];
    meta = {
      homepage = "https://github.com/npm/wrappy";
      description = "Callback wrapping utility";
    };
  }