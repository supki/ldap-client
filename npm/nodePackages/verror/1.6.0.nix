{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "verror";
    version = "1.6.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/verror/-/verror-1.6.0.tgz";
      sha1 = "7d13b27b1facc2e2da90405eb5ea6e5bdd252ea5";
    };
    deps = with nodePackages; [
      extsprintf_1-2-0
    ];
    devDependencies = [];
    meta = {
      homepage = "https://github.com/davepacheco/node-verror";
      description = "richer JavaScript errors";
    };
  }