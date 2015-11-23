{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "extsprintf";
    version = "1.2.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/extsprintf/-/extsprintf-1.2.0.tgz";
      sha1 = "5ad946c22f5b32ba7f8cd7426711c6e8a3fc2529";
    };
    deps = [];
    devDependencies = [];
    meta = {
      homepage = "https://github.com/davepacheco/node-extsprintf";
      description = "extended POSIX-style sprintf";
    };
  }