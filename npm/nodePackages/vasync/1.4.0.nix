{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "vasync";
    version = "1.4.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/vasync/-/vasync-1.4.0.tgz";
      sha1 = "6ea5a63582358868d8743cbdd6ffadc9083b910f";
    };
    deps = with nodePackages; [
      jsprim_0-3-0
      verror_1-1-0
    ];
    devDependencies = [];
    meta = {
      description = "utilities for observable asynchronous control flow";
    };
  }