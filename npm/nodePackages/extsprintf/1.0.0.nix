{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "extsprintf";
    version = "1.0.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/extsprintf/-/extsprintf-1.0.0.tgz";
      sha1 = "4d58b815ace5bebfc4ebf03cf98b0a7604a99b86";
    };
    deps = [];
    devDependencies = [];
    meta = {
      description = "extended POSIX-style sprintf";
    };
  }