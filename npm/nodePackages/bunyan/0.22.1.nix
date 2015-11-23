{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "bunyan";
    version = "0.22.1";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/bunyan/-/bunyan-0.22.1.tgz";
      sha1 = "020c383bed625af5c6c8834dd8c4aca0dd0f765c";
    };
    deps = with nodePackages; [
      dtrace-provider_0-2-8
      mv_0-0-5
    ];
    optionalDependencies = with nodePackages; [
      dtrace-provider_0-2-8
      mv_0-0-5
    ];
    meta = {
      description = "a JSON Logger library for node.js services";
      keywords = [
        "log"
        "logging"
        "log4j"
        "json"
      ];
    };
  }