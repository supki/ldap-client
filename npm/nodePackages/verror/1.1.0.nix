{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "verror";
    version = "1.1.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/verror/-/verror-1.1.0.tgz";
      sha1 = "2a4b4eb14a207051e75a6f94ee51315bf173a1b0";
    };
    deps = with nodePackages; [
      extsprintf_1-0-0
    ];
    devDependencies = [];
    meta = {
      description = "richer JavaScript errors";
    };
  }