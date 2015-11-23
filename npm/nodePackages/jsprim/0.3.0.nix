{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "jsprim";
    version = "0.3.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/jsprim/-/jsprim-0.3.0.tgz";
      sha1 = "cd13466ea2480dbd8396a570d47d31dda476f8b1";
    };
    deps = with nodePackages; [
      json-schema_0-2-2
      extsprintf_1-0-0
      verror_1-3-3
    ];
    devDependencies = [];
    meta = {
      description = "utilities for primitive JavaScript types";
    };
  }