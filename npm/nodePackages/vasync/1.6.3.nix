{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "vasync";
    version = "1.6.3";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/vasync/-/vasync-1.6.3.tgz";
      sha1 = "4a69d7052a47f4ce85503d7641df1cbf40432a94";
    };
    deps = with nodePackages; [
      verror_1-6-0
    ];
    meta = {
      homepage = "https://github.com/davepacheco/node-vasync";
      description = "utilities for observable asynchronous control flow";
    };
  }