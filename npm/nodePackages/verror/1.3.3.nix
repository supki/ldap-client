{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "verror";
    version = "1.3.3";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/verror/-/verror-1.3.3.tgz";
      sha1 = "8a6a4ac3a8c774b6f687fece49bdffd78552e2cd";
    };
    deps = with nodePackages; [
      extsprintf_1-0-0
    ];
    devDependencies = [];
    meta = {
      description = "richer JavaScript errors";
    };
  }